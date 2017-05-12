//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.auto_ref_assignment;

import dparse.lexer;
import dparse.ast;
import analysis.base;

/**
 * Checks for assignment to auto-ref function parameters.
 */
class AutoRefAssignmentCheck : BaseAnalyzer
{
	///
	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	override void visit(const Module m)
	{
		pushScope();
		m.accept(this);
		popScope();
	}

	override void visit(const FunctionDeclaration func)
	{
		if (func.parameters is null || func.parameters.parameters.length == 0)
			return;
		pushScope();
		scope (exit)
			popScope();
		func.accept(this);
	}

	override void visit(const Parameter param)
	{
		import std.algorithm.searching : canFind;

		immutable bool isAuto = param.parameterAttributes.canFind(cast(ubyte) tok!"auto");
		immutable bool isRef = param.parameterAttributes.canFind(cast(ubyte) tok!"ref");
		if (!isAuto || !isRef)
			return;
		addSymbol(param.name.text);
	}

	override void visit(const AssignExpression assign)
	{
		if (assign.operator == tok!"" || scopes.length == 0)
			return;
		interest++;
		assign.ternaryExpression.accept(this);
		interest--;
	}

	override void visit(const IdentifierOrTemplateInstance ioti)
	{
		import std.algorithm.searching : canFind;

		if (ioti.identifier == tok!"" || interest <= 0)
			return;
		if (scopes[$ - 1].canFind(ioti.identifier.text))
			addErrorMessage(ioti.identifier.line, ioti.identifier.column, KEY, MESSAGE);
	}

	override void visit(const IdentifierChain ic)
	{
		import std.algorithm.searching : canFind;

		if (ic.identifiers.length == 0 || interest <= 0)
			return;
		if (scopes[$ - 1].canFind(ic.identifiers[0].text))
			addErrorMessage(ic.identifiers[0].line, ic.identifiers[0].column, KEY, MESSAGE);
	}

	alias visit = BaseAnalyzer.visit;

private:

	enum string MESSAGE = "Assignment to auto-ref function parameter.";
	enum string KEY = "dscanner.suspicious.auto_ref_assignment";

	invariant
	{
		assert(interest >= 0);
	}

	int interest;

	void addSymbol(string symbolName)
	{
		scopes[$ - 1] ~= symbolName;
	}

	void pushScope()
	{
		scopes.length++;
	}

	void popScope()
	{
		scopes = scopes[0 .. $ - 1];
	}

	string[][] scopes;
}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.auto_ref_assignment_check = Check.enabled;
	assertAnalyzerWarnings(q{
		int doStuff(T)(auto ref int a)
		{
			a = 10; // [warn]: %s
		}

		int doStuff(T)(ref int a)
		{
			a = 10;
		}
	}c.format(AutoRefAssignmentCheck.MESSAGE), sac);
	stderr.writeln("Unittest for AutoRefAssignmentCheck passed.");
}
