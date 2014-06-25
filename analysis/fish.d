//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.fish;

import std.stdio;
import std.d.ast;
import std.d.lexer;
import analysis.base;
import analysis.helpers;

/**
 * Checks for use of the deprecated floating point comparison operators.
 */
class FloatOperatorCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const RelExpression r)
	{
		if (r.operator == tok!"<>"
			|| r.operator == tok!"<>="
			|| r.operator == tok!"!<>"
			|| r.operator == tok!"!>"
			|| r.operator == tok!"!<"
			|| r.operator == tok!"!<>="
			|| r.operator == tok!"!>="
			|| r.operator == tok!"!<=")
		{
			addErrorMessage(r.line, r.column, "Avoid using the deprecated floating-point operators.");
		}
		r.accept(this);
	}
}

unittest
{
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
	}c, analysis.run.AnalyzerCheck.float_operator_check);

	stderr.writeln("Unittest for FloatOperatorCheck passed.");
}

