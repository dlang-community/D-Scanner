//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.logic_precedence;

import dscanner.analysis.base;
import dscanner.analysis.helpers;

/**
 * Checks for code with confusing && and || operator precedence
 * ---
 * if (a && b || c) // bad
 * if (a && (b || c)) // good
 * ---
 */
extern(C++) class LogicPrecedenceCheck(AST) : BaseAnalyzerDmd
{
	enum string KEY = "dscanner.confusing.logical_precedence";
	mixin AnalyzerInfo!"logical_precedence_check";
	alias visit = BaseAnalyzerDmd.visit;

	extern(D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.LogicalExp le)
	{
		import dmd.tokens : EXP;

		auto left = le.e1.isLogicalExp();
		auto right = le.e2.isLogicalExp();

		if (left)
			left = left.op == EXP.andAnd ? left : null;
		if (right)
			right = right.op == EXP.andAnd ? right : null;

		if (le.op != EXP.orOr)
			goto END;

		if (!left && !right)
			goto END;
		
		if ((left && left.parens) || (right && right.parens))
			goto END;

		if ((left !is null && left.e2 is null) && (right !is null && right.e2 is null))
			goto END;

		addErrorMessage(cast(ulong) le.loc.linnum, cast(ulong) le.loc.charnum, KEY,
				"Use parenthesis to clarify this expression.");
		
END:
		super.visit(le);
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.logical_precedence_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{
		void testFish()
		{
			if (a && b || c) {} // [warn]: Use parenthesis to clarify this expression.
			if ((a && b) || c) {} // Good
			if (b || c && d) {} // [warn]: Use parenthesis to clarify this expression.
			if (b || (c && d)) {} // Good
		}
	}c, sac);
	stderr.writeln("Unittest for LogicPrecedenceCheck passed.");
}