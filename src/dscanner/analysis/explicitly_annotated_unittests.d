// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.explicitly_annotated_unittests;

import dscanner.analysis.base;
import dscanner.analysis.helpers;

/**
 * Requires unittests to be explicitly annotated with either @safe or @system
 */
extern(C++) class ExplicitlyAnnotatedUnittestCheck(AST) : BaseAnalyzerDmd
{
    mixin AnalyzerInfo!"explicitly_annotated_unittests";
	alias visit = BaseAnalyzerDmd.visit;

	extern(D) this(string fileName)
	{
		super(fileName);
	}

	override void visit(AST.UnitTestDeclaration d)
	{
		import dmd.astenums : STC;

		if (skipTests)
			return;

		if (!(d.storage_class & STC.safe || d.storage_class & STC.system))
			addErrorMessage(
				cast(ulong) d.loc.linnum, cast(ulong) d.loc.charnum, KEY, MESSAGE,
				[
					AutoFix.insertionAt(d.loc.fileOffset, "@safe "),
					AutoFix.insertionAt(d.loc.fileOffset, "@system ")
				]
			);

		super.visit(d);
	}

private:
	enum string KEY = "dscanner.style.explicitly_annotated_unittest";
	enum string MESSAGE = "A unittest should be annotated with at least @safe or @system";
}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.explicitly_annotated_unittests = Check.enabled;

	assertAnalyzerWarningsDMD(q{

		@disable foo() {}

		@safe unittest {}
		@system unittest {}
		pure nothrow @system @nogc unittest {}

		unittest {} // [warn]: A unittest should be annotated with at least @safe or @system
		pure nothrow @nogc unittest {} // [warn]: A unittest should be annotated with at least @safe or @system
	}c, sac);

	// nested
	assertAnalyzerWarningsDMD(q{
		struct Foo
		{
			@safe unittest {}
			@system unittest {}

			unittest {} // [warn]: A unittest should be annotated with at least @safe or @system
			pure nothrow @nogc unittest {} // [warn]: A unittest should be annotated with at least @safe or @system
		}
	}c, sac);

	//// nested
	assertAutoFix(q{
		unittest {} // fix:0
		pure nothrow @nogc unittest {} // fix:0

		struct Foo
		{
			unittest {} // fix:1
			pure nothrow @nogc unittest {} // fix:1
		}
	}c, q{
		@safe unittest {} // fix:0
		pure nothrow @nogc @safe unittest {} // fix:0

		struct Foo
		{
			@system unittest {} // fix:1
			pure nothrow @nogc @system unittest {} // fix:1
		}
	}c, sac, true);

	stderr.writeln("Unittest for ExplicitlyAnnotatedUnittestCheck passed.");
}
