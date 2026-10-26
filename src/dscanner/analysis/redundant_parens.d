//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.redundant_parens;

import dscanner.analysis.base;

/**
 * Checks for redundant parenthesis
 */
extern (C++) class RedundantParenCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"redundant_parens_check";

	///
	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.IfStatement s)
	{
		if (s.condition.parens)
			addErrorMessage(cast(ulong) s.loc.linnum, cast(ulong) s.loc.charnum, KEY, MESSAGE);
	}

private:
	enum string KEY = "dscanner.suspicious.redundant_parens";
	enum string MESSAGE = "Redundant parenthesis.";
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.redundant_parens_check = Check.enabled;

	assertAnalyzerWarningsDMD(q{
		void testRedundantParens()
		{
			int a = 0;
			bool b = true;

			if ((a + 2 == 3)) // [warn]: Redundant parenthesis.
			{

			}

			if ((b)) // [warn]: Redundant parenthesis.
			{

			}

			if (b) { }

			if (a * 2 == 0) { }
		}
	}c, sac);

	stderr.writeln("Unittest for RedundantParenthesis passed.");
}
