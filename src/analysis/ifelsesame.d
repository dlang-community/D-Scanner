//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.ifelsesame;

import std.stdio;
import dparse.ast;
import dparse.lexer;
import analysis.base;
import analysis.helpers;
import dsymbol.scope_ : Scope;

/**
 * Checks for duplicated code in conditional and logical expressions.
 * $(UL
 * $(LI If statements whose "then" block is the same as the "else" block)
 * $(LI || and && expressions where the left and right are the same)
 * $(LI == expressions where the left and right are the same)
 * )
 */
class IfElseSameCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const IfStatement ifStatement)
	{
		if (ifStatement.thenStatement == ifStatement.elseStatement)
			addErrorMessage(ifStatement.line, ifStatement.column,
					"dscanner.bugs.if_else_same", "'Else' branch is identical to 'Then' branch.");
		ifStatement.accept(this);
	}

	override void visit(const AssignExpression assignExpression)
	{
		auto e = cast(const AssignExpression)(cast(const Expression) assignExpression.expression)
			.items[$ - 1];
		if (e !is null && assignExpression.operator == tok!"="
				&& e.ternaryExpression == assignExpression.ternaryExpression)
		{
			addErrorMessage(assignExpression.line, assignExpression.column, "dscanner.bugs.self_assignment",
					"Left side of assignment operatior is identical to the right side.");
		}
		assignExpression.accept(this);
	}

	override void visit(const AndAndExpression andAndExpression)
	{
		if (andAndExpression.left !is null && andAndExpression.right !is null
				&& andAndExpression.left == andAndExpression.right)
		{
			addErrorMessage(andAndExpression.line, andAndExpression.column,
					"dscanner.bugs.logic_operator_operands",
					"Left side of logical and is identical to right side.");
		}
		andAndExpression.accept(this);
	}

	override void visit(const OrOrExpression orOrExpression)
	{
		if (orOrExpression.left !is null && orOrExpression.right !is null
				&& orOrExpression.left == orOrExpression.right)
		{
			addErrorMessage(orOrExpression.line, orOrExpression.column,
					"dscanner.bugs.logic_operator_operands",
					"Left side of logical or is identical to right side.");
		}
		orOrExpression.accept(this);
	}
}

unittest
{
	import analysis.config : StaticAnalysisConfig, Check;

	StaticAnalysisConfig sac;
	sac.if_else_same_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void testSizeT()
		{
			string person = "unknown";
			if (person == "unknown") // [warn]: 'Else' branch is identical to 'Then' branch.
				person = "bobrick"; // same
			else
				person = "bobrick"; // same

			if (person == "unknown") // ok
				person = "ricky"; // not same
			else
				person = "bobby"; // not same
		}
	}c, sac);
	stderr.writeln("Unittest for IfElseSameCheck passed.");
}
