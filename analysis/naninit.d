//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.naninit;

import std.stdio;

import std.d.ast;
import std.d.lexer;
import analysis.base;
import analysis.helpers;

/**
 * Checks for floating-point variables default-initialized to NaN.
 */
class NanInitCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const VariableDeclaration vd)
	{
		switch (vd.type.type2.builtinType)
		{
			case tok!"double":
			case tok!"idouble":
			case tok!"float":
			case tok!"ifloat":
			case tok!"real":
			case tok!"ireal":
			{
				foreach (dec; vd.declarators)
				{
					if (dec.initializer is null)
						addErrorMessage(dec.name.line, dec.name.column,
							"Variable is default-initialized to NaN");
				}
			}

			default:
		}

		vd.accept(this);
	}
}

unittest
{
	assertAnalyzerWarnings(q{
		void testNan()
		{
			float x;
			float[3] arr;
		}
	}c, analysis.run.AnalyzerCheck.nan_init_check);
	stderr.writeln("Unittest for NanInitCheck passed.");
}
