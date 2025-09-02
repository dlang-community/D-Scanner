//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.length_subtraction;

import dscanner.analysis.base;
import dscanner.analysis.helpers;

/**
 * Checks for subtraction from a .length property. This is usually a bug.
 */
extern (C++) class LengthSubtractionCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"length_subtraction_check";

	private enum KEY = "dscanner.suspicious.length_subtraction";
	private enum MSG = "Avoid subtracting from '.length' as it may be unsigned.";

	extern(D) this(string fileName)
	{
		super(fileName);
	}

	override void visit(AST.MinExp minExpr)
	{
		super.visit(minExpr);

		auto left = minExpr.e1.isDotIdExp();
		if (left is null || left.ident is null)
			return;

		if (left.ident.toString() == "length")
			addErrorMessage(
				cast(ulong) left.loc.linnum, cast(ulong) left.loc.charnum, KEY, MSG,
				[AutoFix.insertionAt(left.e1.loc.fileOffset, "cast(ptrdiff_t) ")]
			);
	}
}

unittest
{
	import dscanner.analysis.config : Check, disabledConfig, StaticAnalysisConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD, assertAutoFix;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.length_subtraction_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{
		void testSizeT()
		{
			if (i < a.length - 1) // [warn]: Avoid subtracting from '.length' as it may be unsigned.
				writeln("something");
		}
	}c, sac);

	assertAutoFix(q{
		void testSizeT()
		{
			if (i < a.length - 1) // fix
				writeln("something");
		}
	}c, q{
		void testSizeT()
		{
			if (i < cast(ptrdiff_t) a.length - 1) // fix
				writeln("something");
		}
	}c, sac);

	stderr.writeln("Unittest for IfElseSameCheck passed.");
}
