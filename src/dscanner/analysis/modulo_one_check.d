//          Copyright Brian Schott (Hackerpilot) 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.modulo_one_check;

import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import std.stdio;

/**
 * Checks for 'x' modulo 1 syntax.
 */
final class ModuloOneCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	///
	this(string fileName, bool skipTest = false)
	{
		super(fileName, null, skipTest);
	}

	override void visit(const MulExpression mulExp)
	{
		UnaryExpression unary = cast(UnaryExpression) mulExp.right;
		if (mulExp.operator == tok!"%" &&
			isNumberLiteral(unary.primaryExpression.primary.type) &&
			unary.primaryExpression.primary.text == "1")
		{
			addErrorMessage(mulExp.line, mulExp.column, KEY,
							"Any number modulo 1 will be 0.");
		}
		mulExp.accept(this);
	}

private:
	enum KEY = "dscanner.confusing.mudulo_one_check";
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.modulo_one_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void testModuloOne()
		{
			int a;
			a = 10 % 2;
			a = 10 % 1; // [warn]: Any number modulo 1 will be 0.
		}
	}c, sac);

	stderr.writeln("Unittest for ModuloOneCheck passed.");
}
