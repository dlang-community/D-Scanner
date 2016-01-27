//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.logic_precedence;

import std.stdio;
import dparse.ast;
import dparse.lexer;
import analysis.base;
import analysis.helpers;
import dsymbol.scope_;

/**
 * Checks for code with confusing && and || operator precedence
 * ---
 * if (a && b || c) // bad
 * if (a && (b || c)) // good
 * ---
 */
class LogicPrecedenceCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	enum string KEY = "dscanner.confusing.logical_precedence";

	this(string fileName, const(Scope)* sc)
	{
		super(fileName, sc);
	}

	override void visit(const OrOrExpression orOr)
	{
		if (orOr.left is null || orOr.right is null)
			return;
		const AndAndExpression left = cast(AndAndExpression) orOr.left;
		const AndAndExpression right = cast(AndAndExpression) orOr.right;
		if (left is null && right is null)
			return;
		if ((left !is null && left.right is null) && (right !is null && right.right is null))
			return;
		addErrorMessage(orOr.line, orOr.column, KEY,
				"Use parenthesis to clarify this expression.");
		orOr.accept(this);
	}
}

unittest
{
	import analysis.config : StaticAnalysisConfig;

	StaticAnalysisConfig sac;
	sac.logical_precedence_check = true;
	assertAnalyzerWarnings(q{
		void testFish()
		{
			if (a && b || c) {} // [warn]: Use parenthesis to clarify this expression.
			if ((a && b) || c) {} // Good
			if (b || c && d) {} // [warn]: Use parenthesis to clarify this expression.
			if (b || (c && d)) {} // Good
		}
	}c, sac);
	stderr.writeln("Unittest for LogicPrecedenceCheck passed.");
}
