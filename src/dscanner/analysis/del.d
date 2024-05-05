//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.del;

import std.stdio;
import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import dsymbol.scope_;

/**
 * Checks for use of the deprecated 'delete' keyword
 */
final class DeleteCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	mixin AnalyzerInfo!"delete_check";

	this(BaseAnalyzerArguments args)
	{
		super(args);
	}

	override void visit(const DeleteExpression d)
	{
		addErrorMessage(d.tokens[0], KEY,
				"Avoid using the 'delete' keyword.",
				[AutoFix.replacement(d.tokens[0], `destroy(`, "Replace delete with destroy()")
					.concat(AutoFix.insertionAfter(d.tokens[$ - 1], ")"))]);
		d.accept(this);
	}

	private enum string KEY = "dscanner.deprecated.delete_keyword";
}

unittest
{
	import dscanner.analysis.config : Check, disabledConfig, StaticAnalysisConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings, assertAutoFix;

	StaticAnalysisConfig sac = disabledConfig();
	sac.delete_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void testDelete()
		{
			int[int] data = [1 : 2];
			delete data[1]; /+
			^^^^^^ [warn]: Avoid using the 'delete' keyword. +/

			auto a = new Class();
			delete a; /+
			^^^^^^ [warn]: Avoid using the 'delete' keyword. +/
		}
	}c, sac);

	assertAutoFix(q{
		void testDelete()
		{
			int[int] data = [1 : 2];
			delete data[1]; // fix

			auto a = new Class();
			delete a; // fix
		}
	}c, q{
		void testDelete()
		{
			int[int] data = [1 : 2];
			destroy(data[1]); // fix

			auto a = new Class();
			destroy(a); // fix
		}
	}c, sac);

	stderr.writeln("Unittest for DeleteCheck passed.");
}
