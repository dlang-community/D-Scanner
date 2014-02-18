//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.del;

import stdx.d.ast;
import stdx.d.lexer;
import analysis.base;

/**
 * Checks for use of the deprecated "delete" keyword
 */
class DeleteCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(DeleteExpression d)
	{
		addErrorMessage(d.line, d.column, "Avoid using the delete keyword");
		d.accept(this);
	}
}
