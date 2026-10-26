//          Copyright The dlang community - 2018
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.trust_too_much;

import dscanner.analysis.base;
import dscanner.analysis.source_range : declarationStart, findIdentifier;
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
		{
			// with several attributes dmd may locate the declaration after `@trusted`,
			// and a local declaration has no location, so search from its first member
			const located = scd.loc.isValid || scd.decl is null || scd.decl.length == 0
				? scd.loc : (*scd.decl)[0].loc;
			if (located.isValid)
			{
				const trusted = findIdentifier(source, declarationStart(source, located.fileOffset), "trusted");
				addErrorMessage([trusted.start - "@".length, trusted.end], KEY, MESSAGE);
			}
		}
	
			super.visit(scd);
	}
}

unittest
{
	import dmd.astcodegen : ASTCodegen;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings = assertAnalyzerWarningsDMD;
	import dmd.astcodegen : ASTCodegen;
	import std.stdio : stderr;
	import std.format : format;

	StaticAnalysisConfig sac = disabledConfig();
	sac.trust_too_much = Check.enabled;
	const msg = TrustTooMuchCheck!ASTCodegen.MESSAGE;

	//--- fail cases ---//

	assertAnalyzerWarnings(q{
	@trusted: /+
	^^^^^^^^ [warn]: %s +/
		void test();
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@trusted @nogc: /+
	^^^^^^^^ [warn]: %s +/
		void test();
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@trusted { /+
	^^^^^^^^ [warn]: %s +/
		void test();
		void test();
	}
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@safe {
		@trusted @nogc { /+
		^^^^^^^^ [warn]: %s +/
		void test();
		void test();
	}}
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@nogc @trusted { /+
	      ^^^^^^^^ [warn]: %s +/
		void test();
		void test();
	}
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@trusted template foo(){ /+
	^^^^^^^^ [warn]: %s +/
	}
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	struct foo{
	@trusted:  /+
	^^^^^^^^ [warn]: %s +/
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

	// a local declaration, which dmd gives no location of its own
	assertAnalyzerWarnings(q{
	void foo()
	{
		@trusted struct S {} /+
		^^^^^^^^ [warn]: %s +/
	}
	}c.format(msg), sac);

	stderr.writeln("Unittest for TrustTooMuchCheck passed.");
}
