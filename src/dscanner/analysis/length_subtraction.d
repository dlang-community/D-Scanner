//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.length_subtraction;

import std.stdio;

import dscanner.analysis.base;
import dscanner.analysis.helpers;
import dsymbol.scope_;

/**
 * Checks for subtraction from a .length property. This is usually a bug.
 */
extern(C++) class LengthSubtractionCheck(AST) : BaseAnalyzerDmd
{
	// alias visit = BaseAnalyzerDmd!AST.visit;
	alias visit = BaseAnalyzerDmd.visit;

	mixin AnalyzerInfo!"length_subtraction_check";

	extern(D) this(string fileName)
	{
		super(fileName);
	}

	override void visit(AST.BinExp be)
	{
		import dmd.tokens : EXP;

		if (auto de = be.e1.isDotIdExp())
		{
			if (be.op == EXP.min && de.ident.toString() == "length")
				addErrorMessage(cast(size_t) de.loc.linnum, cast(size_t) de.loc.charnum + 1, KEY,
									"Avoid subtracting from '.length' as it may be unsigned.");
		}

		super.visit(be);
	}

	private enum KEY = "dscanner.suspicious.length_subtraction";
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.length_subtraction_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{
		void testSizeT()
		{
			if (i < a.length - 1) // [warn]: Avoid subtracting from '.length' as it may be unsigned.
				writeln("something");
		}
	}c, sac);

    // TODO: Check and fix if broken
	//assertAutoFix(q{
		//void testSizeT()
		//{
			//if (i < a.length - 1) // fix
				//writeln("something");
		//}
	//}c, q{
		//void testSizeT()
		//{
			//if (i < cast(ptrdiff_t) a.length - 1) // fix
				//writeln("something");
		//}
	//}c, sac);
	stderr.writeln("Unittest for IfElseSameCheck passed.");
}
