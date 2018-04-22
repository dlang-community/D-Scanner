//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.asm_style;

import std.stdio;
import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import dscanner.analysis.helpers;
import dsymbol.scope_ : Scope;

/**
 * Checks for confusing asm expressions.
 * See_also: $(LINK https://issues.dlang.org/show_bug.cgi?id=9738)
 */
final class AsmStyleCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const AsmBrExp brExp)
	{
		if (brExp.asmBrExp !is null && brExp.asmBrExp.asmUnaExp !is null
				&& brExp.asmBrExp.asmUnaExp.asmPrimaryExp !is null)
		{
			addErrorMessage(brExp.line, brExp.column, "dscanner.confusing.brexp",
					"This is confusing because it looks like an array index. Rewrite a[1] as [a + 1] to clarify.");
		}
		brExp.accept(this);
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.asm_style_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void testAsm()
		{
			asm
			{
				mov a, someArray[1]; // [warn]: This is confusing because it looks like an array index. Rewrite a[1] as [a + 1] to clarify.
				add near ptr [EAX], 3;
			}
		}
	}c, sac);

	stderr.writeln("Unittest for AsmStyleCheck passed.");
}
