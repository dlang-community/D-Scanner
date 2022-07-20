//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.del;

import std.stdio;
import dscanner.analysis.base;
import dscanner.analysis.helpers;
import dsymbol.scope_;

/**
 * Checks for use of the deprecated 'delete' keyword
 */
extern(C++) class DeleteCheck(AST) : BaseAnalyzerDmd
{
	// alias visit = BaseAnalyzerDmd!AST.visit;
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"delete_check";

	extern(D) this(string fileName)
	{
		super(fileName);
	}

	override void visit(AST.DeleteExp d)
	{
		addErrorMessage(cast(ulong) d.loc.linnum, cast(ulong) d.loc.charnum, "dscanner.deprecated.delete_keyword",
				"Avoid using the 'delete' keyword.");
		super.visit(d);
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.delete_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{
		void testDelete()
		{
			int[int] data = [1 : 2];
			delete data[1]; // [warn]: Avoid using the 'delete' keyword.

			auto a = new Class();
			delete a; // [warn]: Avoid using the 'delete' keyword.
		}
	}c, sac);

	stderr.writeln("Unittest for DeleteCheck passed.");
}
