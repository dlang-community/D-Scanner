// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.cyclomatic_complexity;

import dscanner.analysis.base;
import dmd.location : Loc;
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
extern (C++) class CyclomaticComplexityCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"cyclomatic_complexity";

	/// Maximum cyclomatic complexity. Once the cyclomatic complexity is greater
	/// than this threshold, a warning is issued.
	///
	/// By default 50 is used as threshold, which is considered almost
	/// unmaintainable / untestable.
	///
	/// For clean development a threshold like 20 can be used instead.
	immutable int maxCyclomaticComplexity;

	private enum string KEY = "dscanner.metric.cyclomatic_complexity";
	private enum string MESSAGE = "Cyclomatic complexity of this function is %s.";

	private int[] complexityStack = [0];
	private bool[] inLoop = [false];

	extern (D) this(string fileName, bool skipTests = false, int maxCyclomaticComplexity = 50)
	{
		super(fileName, skipTests);
		this.maxCyclomaticComplexity = maxCyclomaticComplexity;
	}

	override void visit(AST.TemplateDeclaration templateDecl)
	{
		foreach (member; *templateDecl.members)
			member.accept(this);
	}

	override void visit(AST.FuncDeclaration funDecl)
	{
		if (funDecl.fbody is null)
			return;

		analyzeFunctionBody(funDecl.fbody, funDecl.loc);
	}

	override void visit(AST.UnitTestDeclaration unitTestDecl)
	{
		if (skipTests)
			return;

		analyzeFunctionBody(unitTestDecl.fbody, unitTestDecl.loc);
	}

	private void analyzeFunctionBody(AST.Statement functionBody, Loc location)
	{
		complexityStack.assumeSafeAppend ~= 1;
		inLoop.assumeSafeAppend ~= false;
		scope (exit)
		{
			complexityStack.length--;
			inLoop.length--;
		}

		functionBody.accept(this);
		testComplexity(location.linnum, location.charnum);
	}

	private void testComplexity(size_t line, size_t column)
	{
		auto complexity = complexityStack[$ - 1];
		if (complexity > maxCyclomaticComplexity)
			addErrorMessage(line, column, KEY, format!MESSAGE(complexity));
	}

	override void visit(AST.FuncExp funcExp)
	{
		if (funcExp.fd is null)
			return;

		complexityStack[$ - 1]++;
		funcExp.fd.accept(this);
	}

	mixin VisitComplex!(AST.IfStatement);
	mixin VisitComplex!(AST.LogicalExp);
	mixin VisitComplex!(AST.CondExp);
	mixin VisitComplex!(AST.CaseStatement);
	mixin VisitComplex!(AST.CaseRangeStatement);
	mixin VisitComplex!(AST.ReturnStatement);
	mixin VisitComplex!(AST.ContinueStatement);
	mixin VisitComplex!(AST.GotoStatement);
	mixin VisitComplex!(AST.TryFinallyStatement);
	mixin VisitComplex!(AST.ThrowExp);

	private template VisitComplex(NodeType, int increase = 1)
	{
		override void visit(NodeType nodeType)
		{
			complexityStack[$ - 1] += increase;
			super.visit(nodeType);
		}
	}

	override void visit(AST.SwitchStatement switchStatement)
	{
		inLoop.assumeSafeAppend ~= false;
		scope (exit)
			inLoop.length--;

		switchStatement.condition.accept(this);
		switchStatement._body.accept(this);
	}

	override void visit(AST.BreakStatement breakStatement)
	{
		if (inLoop[$ - 1])
			complexityStack[$ - 1]++;
	}

	override void visit(AST.TryCatchStatement tryCatchStatement)
	{
		tryCatchStatement._body.accept(this);

		if (tryCatchStatement.catches !is null)
		{
			foreach (catchStatement; *(tryCatchStatement.catches))
			{
				complexityStack[$ - 1]++;
				catchStatement.handler.accept(this);
			}
		}
	}

	override void visit(AST.StaticForeachStatement staticForeachStatement)
	{
		// StaticForeachStatement visit has to be overridden in order to avoid visiting
		// its forEachStatement member, which would increase the complexity.
		return;
	}

	mixin VisitLoop!(AST.DoStatement);
	mixin VisitLoop!(AST.WhileStatement);
	mixin VisitLoop!(AST.ForStatement);
	mixin VisitLoop!(AST.ForeachRangeStatement);
	mixin VisitLoop!(AST.ForeachStatement);

	private template VisitLoop(NodeType, int increase = 1)
	{
		override void visit(NodeType nodeType)
		{
			inLoop.assumeSafeAppend ~= true;
			scope (exit)
				inLoop.length--;

			complexityStack[$ - 1] += increase;
			super.visit(nodeType);
		}
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.cyclomatic_complexity = Check.enabled;
	sac.max_cyclomatic_complexity = 0;

	// TODO: Remove redundant tests and break down remaining tests in individual assertions
	assertAnalyzerWarningsDMD(q{
unittest // [warn]: Cyclomatic complexity of this function is 1.
{
}

// unit test
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

// Template, other (tested) stuff
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

	assertAnalyzerWarningsDMD(q{
		// goto, return
		void returnGoto() // [warn]: Cyclomatic complexity of this function is 3.
		{
			goto hello;
			int a = 0;
			a += 9;

		hello:
			return;
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		// if, else, ternary operator
		void ifElseTernary() // [warn]: Cyclomatic complexity of this function is 4.
		{
			if (1 > 2)
			{
				int a;
			}
			else if (2 > 1)
			{
				int b;
			}
			else
			{
				int c;
			}

			int d = true ? 1 : 2;
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		// static if and static foreach don't increase cyclomatic complexity
		void staticIfFor() // [warn]: Cyclomatic complexity of this function is 1.
		{
			static if (stuff)
				int a;

			int b;

			static foreach(i; 0 .. 10)
			{
				pragma(msg, i);
			}
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		// function literal (lambda)
		void lambda() // [warn]: Cyclomatic complexity of this function is 2.
		{
			auto x = (int a) => a + 1;
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		// loops: for, foreach, while, do - while
		void controlFlow() // [warn]: Cyclomatic complexity of this function is 7.
		{
			int x = 0;

			for (int i = 0; i < 100; i++)
			{
				i++;
			}

			foreach (i; 0 .. 2)
			{
				x += i;
				continue;
			}

			while (true)
			{
				break;
			}

			do
			{
				int x = 0;
			} while (true);
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		// switch - case
		void switchCaseCaseRange() // [warn]: Cyclomatic complexity of this function is 5.
		{
			switch (x)
			{
			case 1:
				break;
			case 2:
			case 3:
				break;
			case 7: .. case 10:
				break;
			default:
				break;
			}
			int a;
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		// if, else, logical expressions
		void ifConditions() // [warn]: Cyclomatic complexity of this function is 5.
		{
			if (true && false)
			{
				doX();
			}
			else if (true || false)
			{
				doY();
			}
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		// catch, throw
		void throwCatch() // [warn]: Cyclomatic complexity of this function is 5.
		{
			int x;
			try
			{
				x = 5;
			}
			catch (Exception e)
			{
				x = 7;
			}
			catch (Exception a)
			{
				x = 8;
			}
			catch (Exception x)
			{
				throw new Exception("Exception");
			}
			finally
			{
				x = 9;
			}
		}
	}c, sac);

	stderr.writeln("Unittest for CyclomaticComplexityCheck passed.");
}
