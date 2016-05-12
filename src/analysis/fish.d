//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.fish;

import std.stdio;
import dparse.ast;
import dparse.lexer;
import analysis.base;
import analysis.helpers;
import dsymbol.scope_ : Scope;

/**
 * Checks for use of the deprecated floating point comparison operators.
 */
class FloatOperatorCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	enum string KEY = "dscanner.deprecated.floating_point_operators";

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const RelExpression r)
	{
		if (r.operator == tok!"<>" || r.operator == tok!"<>="
				|| r.operator == tok!"!<>" || r.operator == tok!"!>"
				|| r.operator == tok!"!<" || r.operator == tok!"!<>="
				|| r.operator == tok!"!>=" || r.operator == tok!"!<=")
		{
			addErrorMessage(r.line, r.column, KEY,
					"Avoid using the deprecated floating-point operators.");
		}
		r.accept(this);
	}
}

unittest
{
	import analysis.config : StaticAnalysisConfig, Check;

	StaticAnalysisConfig sac;
	sac.float_operator_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void testFish()
		{
			float z = 1.5f;
			bool a;
			a = z !<>= z; // [warn]: Avoid using the deprecated floating-point operators.
			a = z !<> z; // [warn]: Avoid using the deprecated floating-point operators.
			a = z <> z; // [warn]: Avoid using the deprecated floating-point operators.
			a = z <>= z; // [warn]: Avoid using the deprecated floating-point operators.
			a = z !> z; // [warn]: Avoid using the deprecated floating-point operators.
			a = z !>= z; // [warn]: Avoid using the deprecated floating-point operators.
			a = z !< z; // [warn]: Avoid using the deprecated floating-point operators.
			a = z !<= z; // [warn]: Avoid using the deprecated floating-point operators.
		}
	}c, sac);

	stderr.writeln("Unittest for FloatOperatorCheck passed.");
}
