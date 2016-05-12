//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.redundant_parens;

import dparse.ast;
import dparse.lexer;
import analysis.base;
import dsymbol.scope_ : Scope;

/**
 * Checks for redundant parenthesis
 */
class RedundantParenCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	///
	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const IfStatement statement)
	{
		UnaryExpression unary;
		if (statement.expression is null || statement.expression.items.length != 1)
			goto end;
		unary = cast(UnaryExpression) statement.expression.items[0];
		if (unary is null)
			goto end;
		if (unary.primaryExpression is null)
			goto end;
		if (unary.primaryExpression.expression is null)
			goto end;
		addErrorMessage(unary.primaryExpression.expression.line,
				unary.primaryExpression.expression.column, KEY, "Redundant parenthesis.");
	end:
		statement.accept(this);
	}

	override void visit(const PrimaryExpression primaryExpression)
	{
		UnaryExpression unary;
		if (primaryExpression.expression is null)
			goto end;
		unary = cast(UnaryExpression) primaryExpression.expression.items[0];
		if (unary is null)
			goto end;
		if (unary.primaryExpression is null)
			goto end;
		if (unary.primaryExpression.expression is null)
			goto end;
		addErrorMessage(primaryExpression.expression.line,
				primaryExpression.expression.column, KEY, "Redundant parenthesis.");
	end:
		primaryExpression.accept(this);
	}

private:
	enum string KEY = "dscanner.suspicious.redundant_parens";
}
