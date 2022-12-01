// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.assert_without_msg;

import dscanner.analysis.base;
import std.stdio;

/**
 * Check that all asserts have an explanatory message.
 */
extern(C++) class AssertWithoutMessageCheck(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"assert_without_msg";
	alias visit = BaseAnalyzerDmd.visit;

	///
	extern(D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	// Avoid visiting in/out contracts for this check
	override void visitFuncBody(AST.FuncDeclaration f)
	{
		if (f.fbody)
        {
            f.fbody.accept(this);
        }
	}

	override void visit(AST.AssertExp ae)
	{
		if (!ae.msg)
			addErrorMessage(ae.loc.linnum, ae.loc.charnum, KEY, MESSAGE);
	}

	override void visit(AST.StaticAssert ae)
	{
		if (!ae.msgs)
			addErrorMessage(ae.loc.linnum, ae.loc.charnum, KEY, MESSAGE);
	}


private:
	enum string KEY = "dscanner.style.assert_without_msg";
	enum string MESSAGE = "An assert should have an explanatory message";
}

unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;

	StaticAnalysisConfig sac = disabledConfig();
	sac.assert_without_msg = Check.enabled;

	assertAnalyzerWarningsDMD(q{
	unittest {
		assert(0, "foo bar");
		assert(0); // [warn]: An assert should have an explanatory message
	}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
	unittest {
		static assert(0, "foo bar");
		static assert(0); // [warn]: An assert should have an explanatory message
	}
	}c, sac);

	stderr.writeln("Unittest for AssertWithoutMessageCheck passed.");
}
