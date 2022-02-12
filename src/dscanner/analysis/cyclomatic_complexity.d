// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.cyclomatic_complexity;

import dparse.ast;
import dparse.lexer;
import dsymbol.scope_ : Scope;
import dscanner.analysis.base;
import dscanner.analysis.helpers;

import std.format;

/// Implements a basic cyclomatic complexity algorithm using the AST.
///
/// Issues a warning on functions whenever the cyclomatic complexity of them
/// passed over a configurable threshold.
///
/// The complexity score starts at 1 and is increased each time on
/// - `if`
/// - switch `case`
/// - any loop
/// - `&&`
/// - `||`
/// - `?:` (ternary operator)
/// - `throw`
/// - `catch`
/// - `return`
/// - `break` (unless in case)
/// - `continue`
/// - `goto`
/// - function literals
///
/// See: https://en.wikipedia.org/wiki/Cyclomatic_complexity
/// Rules based on http://cyvis.sourceforge.net/cyclomatic_complexity.html
/// and https://github.com/fzipp/gocyclo
final class CyclomaticComplexityCheck : BaseAnalyzer
{
	/// Message key emitted when the threshold is reached
	enum string KEY = "dscanner.metric.cyclomatic_complexity";
	/// Human readable message emitted when the threshold is reached
	enum string MESSAGE = "Cyclomatic complexity of this function is %s.";
	mixin AnalyzerInfo!"cyclomatic_complexity";

	/// Maximum cyclomatic complexity. Once the cyclomatic complexity is greater
	/// than this threshold, a warning is issued.
	///
	/// By default 50 is used as threshold, which is considered almost
	/// unmaintainable / untestable.
	///
	/// For clean development a threshold like 20 can be used instead.
	int maxCyclomaticComplexity;

	///
	this(string fileName, const(Scope)* sc, bool skipTests = false,
		int maxCyclomaticComplexity = 50)
	{
		super(fileName, sc, skipTests);
		this.maxCyclomaticComplexity = maxCyclomaticComplexity;
	}

	mixin VisitComplex!IfStatement;
	mixin VisitComplex!CaseStatement;
	mixin VisitComplex!CaseRangeStatement;
	mixin VisitLoop!DoStatement;
	mixin VisitLoop!WhileStatement;
	mixin VisitLoop!ForStatement;
	mixin VisitLoop!ForeachStatement;
	mixin VisitComplex!AndAndExpression;
	mixin VisitComplex!OrOrExpression;
	mixin VisitComplex!TernaryExpression;
	mixin VisitComplex!ThrowExpression;
	mixin VisitComplex!Catch;
	mixin VisitComplex!LastCatch;
	mixin VisitComplex!ReturnStatement;
	mixin VisitComplex!FunctionLiteralExpression;
	mixin VisitComplex!GotoStatement;
	mixin VisitComplex!ContinueStatement;

	override void visit(const SwitchStatement n)
	{
		inLoop.assumeSafeAppend ~= false;
		scope (exit)
			inLoop.length--;
		n.accept(this);
	}

	override void visit(const BreakStatement b)
	{
		if (b.label !is Token.init || inLoop[$ - 1])
			complexityStack[$ - 1]++;
	}

	override void visit(const FunctionDeclaration fun)
	{
		if (!fun.functionBody)
			return;

		complexityStack.assumeSafeAppend ~= 1;
		inLoop.assumeSafeAppend ~= false;
		scope (exit)
		{
			complexityStack.length--;
			inLoop.length--;
		}
		fun.functionBody.accept(this);
		testComplexity(fun.name.line, fun.name.column);
	}

	override void visit(const Unittest unittest_)
	{
		if (!skipTests)
		{
			complexityStack.assumeSafeAppend ~= 1;
			inLoop.assumeSafeAppend ~= false;
			scope (exit)
			{
				complexityStack.length--;
				inLoop.length--;
			}
			unittest_.accept(this);
			testComplexity(unittest_.line, unittest_.column);
		}
	}

	alias visit = BaseAnalyzer.visit;
private:
	int[] complexityStack = [0];
	bool[] inLoop = [false];

	void testComplexity(size_t line, size_t column)
	{
		auto complexity = complexityStack[$ - 1];
		if (complexity > maxCyclomaticComplexity)
		{
			addErrorMessage(line, column, KEY, format!MESSAGE(complexity));
		}
	}

	template VisitComplex(NodeType, int increase = 1)
	{
		override void visit(const NodeType n)
		{
			complexityStack[$ - 1] += increase;
			n.accept(this);
		}
	}

	template VisitLoop(NodeType, int increase = 1)
	{
		override void visit(const NodeType n)
		{
			inLoop.assumeSafeAppend ~= true;
			scope (exit)
				inLoop.length--;
			complexityStack[$ - 1] += increase;
			n.accept(this);
		}
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.cyclomatic_complexity = Check.enabled;
	sac.max_cyclomatic_complexity = 0;
	assertAnalyzerWarnings(q{
unittest // [warn]: Cyclomatic complexity of this function is 1.
{
}

unittest // [warn]: Cyclomatic complexity of this function is 1.
{
	writeln("hello");
	writeln("world");
}

void main(string[] args) // [warn]: Cyclomatic complexity of this function is 3.
{
	if (!args.length)
		return;
	writeln("hello ", args);
}

unittest // [warn]: Cyclomatic complexity of this function is 1.
{
	// static if / static foreach does not increase cyclomatic complexity
	static if (stuff)
		int a;
	int a;
}

unittest // [warn]: Cyclomatic complexity of this function is 2.
{
	foreach (i; 0 .. 2)
	{
	}
	int a;
}

unittest // [warn]: Cyclomatic complexity of this function is 3.
{
	foreach (i; 0 .. 2)
	{
		break;
	}
	int a;
}

unittest // [warn]: Cyclomatic complexity of this function is 2.
{
	switch (x)
	{
	case 1:
		break;
	default:
		break;
	}
	int a;
}

bool shouldRun(check : BaseAnalyzer)( // [warn]: Cyclomatic complexity of this function is 20.
	string moduleName, const ref StaticAnalysisConfig config)
{
	enum string a = check.name;

	if (mixin("config." ~ a) == Check.disabled)
		return false;

	// By default, run the check
	if (!moduleName.length)
		return true;

	auto filters = mixin("config.filters." ~ a);

	// Check if there are filters are defined
	// filters starting with a comma are invalid
	if (filters.length == 0 || filters[0].length == 0)
		return true;

	auto includers = filters.filter!(f => f[0] == '+').map!(f => f[1..$]);
	auto excluders = filters.filter!(f => f[0] == '-').map!(f => f[1..$]);

	// exclusion has preference over inclusion
	if (!excluders.empty && excluders.any!(s => moduleName.canFind(s)))
		return false;

	if (!includers.empty)
		return includers.any!(s => moduleName.canFind(s));

	// by default: include all modules
	return true;
}

}c, sac);
	stderr.writeln("Unittest for CyclomaticComplexityCheck passed.");
}
