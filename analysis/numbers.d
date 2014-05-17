//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.numbers;

import std.stdio;
import std.regex;
import std.d.ast;
import std.d.lexer;
import analysis.base;
import analysis.helpers;

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

	override void visit(const Token t)
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

	auto badBinaryRegex = ctRegex!(`^0b[01]{9,}`);
	auto badDecimalRegex = ctRegex!(`^\d{5,}`);
}

unittest
{
	shouldWarn(q{
		void testNumbers()
		{
			int a;
			a = 1; // ok
			a = 10; // ok
			a = 100; // ok
			a = 1000; // FIXME: boom
			a = 10000; // [warn]: Use underscores to improve number constant readability
			a = 100000; // [warn]: Use underscores to improve number constant readability
			a = 1000000; // [warn]: Use underscores to improve number constant readability
		}
	}c, analysis.run.AnalyzerCheck.number_style_check);

	stderr.writeln("Unittest for NumberStyleCheck passed.");
}
