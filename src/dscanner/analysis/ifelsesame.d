//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.ifelsesame;

import dscanner.analysis.base;
import dmd.hdrgen : toChars;
import dmd.tokens : EXP, TOK;
import dscanner.analysis.source_range : extentOf, findToken;
import std.conv : to;
import std.string : format;
import std.typecons : Tuple, tuple;

/**
 * Checks for duplicated code in conditional and logical expressions.
 * $(UL
 * $(LI If statements whose "then" block is the same as the "else" block)
 * $(LI || and && expressions where the left and right are the same)
 * $(LI == and != expressions where the left and right are the same)
 * $(LI >, <, >=, and <= expressions where the left and right are the same)
 * $(LI Assignments where the left and right are the same)
 * )
 */
extern (C++) class IfElseSameCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"if_else_same_check";

	private enum IF_KEY = "dscanner.bugs.if_else_same";
	private enum IF_MESSAGE = "'Else' branch is identical to 'Then' branch.";

	private enum LOGICAL_EXP_KEY = "dscanner.bugs.logic_operator_operands";
	private enum LOGICAL_EXP_MESSAGE = "Left side of logical %s is identical to right side.";

	private enum ASSIGN_KEY = "dscanner.bugs.self_assignment";
	private enum ASSIGN_MESSAGE = "Left side of assignment operation is identical to the right side.";

	private bool inAssignment = false;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.IfStatement ifStatement)
	{
		super.visit(ifStatement);

		if (ifStatement.ifbody is null || ifStatement.elsebody is null)
			return;

		auto thenBody = to!string(toChars(ifStatement.ifbody));
		auto elseBody = to!string(toChars(ifStatement.elsebody));

		if (thenBody == elseBody)
			addErrorMessage([ifStatement.loc.fileOffset, statementEnd(ifStatement.elsebody)], IF_KEY, IF_MESSAGE);
	}

	override void visit(AST.AssignExp assignExp)
	{
		bool oldInAssignment = inAssignment;
		inAssignment = true;
		super.visit(assignExp);
		inAssignment = oldInAssignment;
	}

	override void visit(AST.CondExp condExp)
	{
		super.visit(condExp);
		if (inAssignment)
			handleBinaryExpression(condExp);
	}

	override void visit(AST.LogicalExp logicalExpr)
	{
		super.visit(logicalExpr);
		handleBinaryExpression(logicalExpr);
	}

	/// End offset of a statement: its closing brace, or its `;`.
	extern (D) private size_t statementEnd(AST.Statement statement)
	{
		if (auto scopeStatement = statement.isScopeStatement())
			return scopeStatement.endloc.fileOffset + 1;
		return findToken(source, statement.loc.fileOffset, TOK.semicolon).end;
	}

	private void handleBinaryExpression(AST.BinExp expr)
	{
		auto expr1 = to!string(toChars(expr.e1));
		auto expr2 = to!string(toChars(expr.e2));

		if (expr1 == expr2)
		{
			auto errorInfo = getErrorInfo(expr.op);
			addErrorMessage(extentOf(source, expr), errorInfo[0], errorInfo[1]);
		}
	}

	private extern (D) Tuple!(string, string) getErrorInfo(EXP op)
	{
		switch (op)
		{
		case EXP.orOr:
			return tuple(LOGICAL_EXP_KEY, LOGICAL_EXP_MESSAGE.format("or"));
		case EXP.andAnd:
			return tuple(LOGICAL_EXP_KEY, LOGICAL_EXP_MESSAGE.format("and"));
		case EXP.question:
			return tuple(ASSIGN_KEY, ASSIGN_MESSAGE);
		default:
			assert(0);
		}

	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.if_else_same_check = Check.enabled;

	assertAnalyzerWarningsDMD(q{
		void testSizeT()
		{
			string person = "unknown";
			if (person == "unknown")
				person = "bobrick"; /* same */
			else
				person = "bobrick"; /* same */ /+
^^^^^^^^^^^^^^^^^^^^^^^ [warn]: 'Else' branch is identical to 'Then' branch. +/
			// note: above ^^^ line spans over multiple lines, so it starts at start of line, since we don't have any way to test this here
			// look at the tests using 1-wide tab width for accurate visuals.

			if (person == "unknown") // ok
				person = "ricky"; // not same
			else
				person = "bobby"; // not same
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		void testLogicalExp()
		{
			int a = 5, b = 5;
			if (a == b || a == b) // [warn]: Left side of logical or is identical to right side.
				a = 6;
			if (a == b && a == b) // [warn]: Left side of logical and is identical to right side.
				a = 6;
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		void testAssignExp()
		{
			int a = 5, b = 5;
			a = b > 5 ? b : b; // [warn]: Left side of assignment operation is identical to the right side.
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		void foo()
		{
			if (auto stuff = call()) {}
		}
	}c, sac);

	stderr.writeln("Unittest for IfElseSameCheck passed.");
}
