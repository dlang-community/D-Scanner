//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.comma_expression;

import dparse.ast;
import dparse.lexer;
import analysis.base;
import dsymbol.scope_;

/**
 * Check for uses of the comma expression.
 */
class CommaExpressionCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName, const(Scope)* sc)
	{
		super(fileName, sc);
	}

	override void visit(const Expression ex)
	{
		if (ex.items.length > 1 && interest > 0)
		{
			addErrorMessage(ex.line, ex.column, KEY, "Avoid using the comma expression.");
		}
		ex.accept(this);
	}

	override void visit(const AssignExpression ex)
	{
		++interest;
		ex.accept(this);
		--interest;
	}

	// Dconf 2016
	override void visit(const SynchronizedStatement ss)
	{
		++interest;
		visit(ss.expression);
		--interest;
		visit(ss.statementNoCaseNoDefault);
	}

	invariant
	{
		assert(interest >= 0);
	}

	int interest;

	private enum string KEY = "dscanner.suspicious.comma_expression";
}
