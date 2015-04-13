//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.comma_expression;

import std.d.ast;
import std.d.lexer;
import analysis.base;

/**
 * Check for uses of the comma expression.
 */
class CommaExpressionCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const Expression ex)
	{
		if (ex.items.length > 1)
		{
			addErrorMessage(ex.line, ex.column, KEY,
				"Avoid using the comma expression.");
		}
		ex.accept(this);
	}

	private enum KEY = "dscanner.suspicious.comma_expression";
}
