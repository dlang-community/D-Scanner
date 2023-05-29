//          Copyright The dlang community - 2018
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.trust_too_much;

import dscanner.analysis.base;
import dmd.astenums : STC;

/**
 * Checks that `@trusted` is only applied to a a single function
 */
extern(C++) class TrustTooMuchCheck(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"trust_too_much";
	alias visit = BaseAnalyzerDmd.visit;

private:
	extern(D) static immutable MESSAGE = "Trusting a whole scope is a bad idea, " ~
		"`@trusted` should only be attached to the functions individually";
	extern(D) static immutable string KEY = "dscanner.trust_too_much";

public:
	///
	extern(D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.StorageClassDeclaration scd)
	{
		if (scd.stc & STC.trusted)
			addErrorMessage(cast(ulong) scd.loc.linnum, cast(ulong) scd.loc.charnum,
					KEY, MESSAGE);
	
			super.visit(scd);
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings = assertAnalyzerWarningsDMD;
	import std.format : format;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.trust_too_much = Check.enabled;
	const msg = "Trusting a whole scope is a bad idea, " ~
		"`@trusted` should only be attached to the functions individually";

	//--- fail cases ---//

	assertAnalyzerWarnings(q{
	@trusted: // [warn]: %s
		void test();
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@trusted @nogc: // [warn]: %s
		void test();
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@trusted { // [warn]: %s
		void test();
		void test();
	}
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@safe {
		@trusted @nogc { // [warn]: %s
		void test();
		void test();
	}}
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@nogc @trusted { // [warn]: %s
		void test();
		void test();
	}
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@trusted template foo(){ // [warn]: %s
	}
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	struct foo{
	@trusted:  // [warn]: %s
	}
	}c.format(msg), sac);
	//--- pass cases ---//

	assertAnalyzerWarnings(q{
	void test() @trusted {}
	}c, sac);

	assertAnalyzerWarnings(q{
	@trusted void test();
	}c, sac);

	assertAnalyzerWarnings(q{
	@nogc template foo(){
	}
	}c , sac);

	assertAnalyzerWarnings(q{
	alias nothrow @trusted uint F4();
	}c , sac);

	assertAnalyzerWarnings(q{
	@trusted ~this();
	@trusted this();
	}c , sac);

	stderr.writeln("Unittest for TrustTooMuchCheck passed.");
}