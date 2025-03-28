//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.range;

import std.stdio;
import dscanner.analysis.base;
import dscanner.analysis.helpers;
import std.string : format;

/**
 * Checks for .. expressions where the left side is larger than the right. This
 * is almost always a mistake.
 */
extern(C++) class BackwardsRangeCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"backwards_range_check";

	/// Key for this check in the report output
	enum string KEY = "dscanner.bugs.backwards_slices";

	/**
	 * Params:
	 *     fileName = the name of the file being analyzed
	 */
	extern(D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.IntervalExp ie)
	{
		auto lwr = ie.lwr.isIntegerExp();
		auto upr = ie.upr.isIntegerExp();

		if (lwr && upr && lwr.getInteger() > upr.getInteger())
		{
			string message = format("%d is larger than %d. This slice is likely incorrect.",
						lwr.getInteger(), upr.getInteger());
			addErrorMessage(cast(ulong) ie.loc.linnum, cast(ulong) ie.loc.charnum, KEY, message);
		}
			
	}

	override void visit(AST.ForeachRangeStatement s)
	{
		auto lwr = s.lwr.isIntegerExp();
		auto upr = s.upr.isIntegerExp();

		if (lwr && upr && lwr.getInteger() > upr.getInteger())
		{
			string message = format(
						"%d is larger than %d. Did you mean to use 'foreach_reverse( ... ; %d .. %d)'?",
						lwr.getInteger(), upr.getInteger(), upr.getInteger(), lwr.getInteger());
			addErrorMessage(cast(ulong) s.loc.linnum, cast(ulong) s.loc.charnum, KEY, message);
		}
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.backwards_range_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{
		void testRange()
		{
			a = node.tupleof[2..T.length+1]; // ok
			foreach (a; 10 .. j + 2) {} // ok

			int[] data = [1, 2, 3, 4, 5];

			data = data[1 .. 3]; // ok
			data = data[3 .. 1]; // [warn]: 3 is larger than 1. This slice is likely incorrect.

			foreach (n; 1 .. 3) { } // ok
			foreach (n; 3 .. 1) { } // [warn]: 3 is larger than 1. Did you mean to use 'foreach_reverse( ... ; 1 .. 3)'?
		}
	}c, sac);

	stderr.writeln("Unittest for BackwardsRangeCheck passed.");
}
