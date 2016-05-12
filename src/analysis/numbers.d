//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.numbers;

import std.stdio;
import std.regex;
import dparse.ast;
import dparse.lexer;
import analysis.base;
import analysis.helpers;
import dsymbol.scope_ : Scope;

/**
 * Checks for long and hard-to-read number literals
 */
class NumberStyleCheck : BaseAnalyzer
{
public:
	alias visit = BaseAnalyzer.visit;

	/**
	 * Constructs the style checker with the given file name.
	 */
	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const Token t)
	{
		import std.algorithm : startsWith;

		if (isNumberLiteral(t.type) && !t.text.startsWith("0x")
				&& ((t.text.startsWith("0b") && !t.text.matchFirst(badBinaryRegex)
					.empty) || !t.text.matchFirst(badDecimalRegex).empty))
		{
			addErrorMessage(t.line, t.column, "dscanner.style.number_literals",
					"Use underscores to improve number constant readability.");
		}
	}

private:
	auto badBinaryRegex = ctRegex!(`^0b[01]{9,}`);
	auto badDecimalRegex = ctRegex!(`^\d{5,}`);
}

unittest
{
	import analysis.config : StaticAnalysisConfig, Check;

	StaticAnalysisConfig sac;
	sac.number_style_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void testNumbers()
		{
			int a;
			a = 1; // ok
			a = 10; // ok
			a = 100; // ok
			a = 1000; // FIXME: boom
			a = 10000; // [warn]: Use underscores to improve number constant readability.
			a = 100000; // [warn]: Use underscores to improve number constant readability.
			a = 1000000; // [warn]: Use underscores to improve number constant readability.
		}
	}c, sac);

	stderr.writeln("Unittest for NumberStyleCheck passed.");
}
