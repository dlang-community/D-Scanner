// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.explicitly_annotated_unittests;

import dparse.lexer;
import dparse.ast;
import dscanner.analysis.base;

import std.stdio;

/**
 * Requires unittests to be explicitly annotated with either @safe or @system
 */
final class ExplicitlyAnnotatedUnittestCheck : BaseAnalyzer
{
	enum string KEY = "dscanner.style.explicitly_annotated_unittest";
	enum string MESSAGE = "A unittest should be annotated with at least @safe or @system";
    mixin AnalyzerInfo!"explicitly_annotated_unittests";

	///
	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	override void visit(const Declaration decl)
	{
		if (decl.unittest_ !is null)
		{
			bool isSafeOrSystem;
			if (decl.attributes !is null)
			foreach (attribute; decl.attributes)
			{
				if (attribute.atAttribute !is null)
				{
					const token = attribute.atAttribute.identifier.text;
					if (token == "safe" || token == "system")
					{
						isSafeOrSystem = true;
						break;
					}
				}
			}
			if (!isSafeOrSystem)
			{
				auto token = decl.unittest_.findTokenForDisplay(tok!"unittest");
				addErrorMessage(token, KEY, MESSAGE,
					[
						AutoFix.insertionBefore(token[0], "@safe ", "Mark unittest @safe"),
						AutoFix.insertionBefore(token[0], "@system ", "Mark unittest @system")
					]);
			}
		}
		decl.accept(this);
	}

	alias visit = BaseAnalyzer.visit;

}

unittest
{
	import dscanner.analysis.config : Check, disabledConfig, StaticAnalysisConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings, assertAutoFix;
	import std.format : format;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.explicitly_annotated_unittests = Check.enabled;

	assertAnalyzerWarnings(q{
		@safe unittest {}
		@system unittest {}
		pure nothrow @system @nogc unittest {}

		unittest {} /+
		^^^^^^^^ [warn]: %s +/
		pure nothrow @nogc unittest {} /+
		                   ^^^^^^^^ [warn]: %s +/
	}c.format(
		ExplicitlyAnnotatedUnittestCheck.MESSAGE,
		ExplicitlyAnnotatedUnittestCheck.MESSAGE,
	), sac);

	// nested
	assertAnalyzerWarnings(q{
		struct Foo
		{
			@safe unittest {}
			@system unittest {}

			unittest {} /+
			^^^^^^^^ [warn]: %s +/
			pure nothrow @nogc unittest {} /+
			                   ^^^^^^^^ [warn]: %s +/
		}
	}c.format(
		ExplicitlyAnnotatedUnittestCheck.MESSAGE,
		ExplicitlyAnnotatedUnittestCheck.MESSAGE,
	), sac);


	// nested
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
	}c, sac);

	stderr.writeln("Unittest for ExplicitlyAnnotatedUnittestCheck passed.");
}
