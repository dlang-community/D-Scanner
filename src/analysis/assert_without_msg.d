// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.assert_without_msg;

import analysis.base : BaseAnalyzer;
import dsymbol.scope_ : Scope;
import dparse.lexer;
import dparse.ast;

import std.stdio;

/**
 * Check that all asserts have an explanatory message.
 */
class AssertWithoutMessageCheck : BaseAnalyzer
{
	enum string KEY = "dscanner.style.assert_without_msg";
	enum string MESSAGE = "An assert should have an explanatory message";

	///
	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const AssertExpression expr)
	{
		if (expr.message is null)
			addErrorMessage(expr.line, expr.column, KEY, MESSAGE);
	}

	alias visit = BaseAnalyzer.visit;

}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.assert_without_msg = Check.enabled;

	assertAnalyzerWarnings(q{
	unittest {
		assert(0, "foo bar");
		assert(0); // [warn]: %s
	}
	}c.format(
		AssertWithoutMessageCheck.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
	unittest {
		static assert(0, "foo bar");
		static assert(0); // [warn]: %s
	}
	}c.format(
		AssertWithoutMessageCheck.MESSAGE,
	), sac);


	stderr.writeln("Unittest for AssertWithoutMessageCheck passed.");
}
