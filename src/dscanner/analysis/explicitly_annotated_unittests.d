// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.explicitly_annotated_unittests;

import dparse.lexer;
import dparse.ast;
import dscanner.analysis.base : BaseAnalyzer;

import std.stdio;

/**
 * Requires unittests to be explicitly annotated with either @safe or @system
 */
final class ExplicitlyAnnotatedUnittestCheck : BaseAnalyzer
{
	enum string KEY = "dscanner.style.explicitly_annotated_unittest";
	enum string MESSAGE = "A unittest should be annotated with at least @safe or @system";

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
				addErrorMessage(decl.unittest_.line, decl.unittest_.column, KEY, MESSAGE);
		}
		decl.accept(this);
	}

	alias visit = BaseAnalyzer.visit;

}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.explicitly_annotated_unittests = Check.enabled;

	assertAnalyzerWarnings(q{
		@safe unittest {}
		@system unittest {}
		pure nothrow @system @nogc unittest {}

		unittest {} // [warn]: %s
		pure nothrow @nogc unittest {} // [warn]: %s
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

			unittest {} // [warn]: %s
			pure nothrow @nogc unittest {} // [warn]: %s
		}
	}c.format(
		ExplicitlyAnnotatedUnittestCheck.MESSAGE,
		ExplicitlyAnnotatedUnittestCheck.MESSAGE,
	), sac);

	stderr.writeln("Unittest for ExplicitlyAnnotatedUnittestCheck passed.");
}
