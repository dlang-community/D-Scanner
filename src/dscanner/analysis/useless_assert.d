//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.useless_assert;

import dscanner.analysis.base;
import dscanner.analysis.helpers;

import std.stdio;

/**
 * Checks for asserts that always succeed
 */
extern(C++) class UselessAssertCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"useless_assert_check";

	///
	extern(D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.AssertExp ae)
	{
		auto ie = ae.e1.isIntegerExp();
		if (ie && ie.getInteger() != 0)
			addErrorMessage(cast(ulong) ae.loc.linnum, cast(ulong) ae.loc.charnum, KEY, MESSAGE);

		auto re = ae.e1.isRealExp();
		if (re && re.value != 0)
			addErrorMessage(cast(ulong) ae.loc.linnum, cast(ulong) ae.loc.charnum, KEY, MESSAGE);
		
		if (ae.e1.isStringExp() || ae.e1.isArrayLiteralExp() || ae.e1.isAssocArrayLiteralExp())
			addErrorMessage(cast(ulong) ae.loc.linnum, cast(ulong) ae.loc.charnum, KEY, MESSAGE);
	}

private:
	enum string KEY = "dscanner.suspicious.useless_assert";
	enum string MESSAGE = "Assert condition is always true.";
}

unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.format : format;

	alias assertAnalyzerWarnings = assertAnalyzerWarningsDMD;

	StaticAnalysisConfig sac = disabledConfig();
	sac.useless_assert_check = Check.enabled;
	assertAnalyzerWarnings(q{
unittest
{
	assert(true); // [warn]: Assert condition is always true.
	assert(1); // [warn]: Assert condition is always true.
	assert([10]); // [warn]: Assert condition is always true.
	assert(false);
	assert(0);
	assert(0.0L);
}
}c, sac);
	stderr.writeln("Unittest for UselessAssertCheck passed.");
}
