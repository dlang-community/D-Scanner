//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.numbers;

import std.regex;
import stdx.d.ast;
import stdx.d.lexer;
import analysis.base;

/**
 * Checks for long and hard-to-read number literals
 */
class NumberStyleCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(Token t)
	{
		import std.algorithm;
		if (isNumberLiteral(t.type) && !t.text.startsWith("0x")
			&& ((t.text.startsWith("0b") && !t.text.matchFirst(badBinaryRegex).empty)
				|| !t.text.matchFirst(badDecimalRegex).empty))
		{
			addErrorMessage(t.line, t.column,
				"Use underscores to improve number constant readability");
		}
	}

	auto badBinaryRegex = ctRegex!(`0b[01]{9,}`);
	auto badDecimalRegex = ctRegex!(`\d{5,}`);
}
