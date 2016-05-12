//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.length_subtraction;

import std.stdio;

import dparse.ast;
import dparse.lexer;
import analysis.base;
import analysis.helpers;
import dsymbol.scope_;

/**
 * Checks for subtraction from a .length property. This is usually a bug.
 */
class LengthSubtractionCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const AddExpression addExpression)
	{
		if (addExpression.operator == tok!"-")
		{
			const UnaryExpression l = cast(const UnaryExpression) addExpression.left;
			const UnaryExpression r = cast(const UnaryExpression) addExpression.right;
			if (l is null || r is null)
			{
				//				stderr.writeln(__FILE__, " ", __LINE__);
				goto end;
			}
			if (r.primaryExpression is null || r.primaryExpression.primary.type != tok!"intLiteral")
			{
				//				stderr.writeln(__FILE__, " ", __LINE__);
				goto end;
			}
			if (l.identifierOrTemplateInstance is null
					|| l.identifierOrTemplateInstance.identifier.text != "length")
			{
				//				stderr.writeln(__FILE__, " ", __LINE__);
				goto end;
			}
			const(Token) token = l.identifierOrTemplateInstance.identifier;
			addErrorMessage(token.line, token.column, "dscanner.suspicious.length_subtraction",
					"Avoid subtracting from '.length' as it may be unsigned.");
		}
	end:
		addExpression.accept(this);
	}
}

unittest
{
	import analysis.config : StaticAnalysisConfig, Check;

	StaticAnalysisConfig sac;
	sac.length_subtraction_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void testSizeT()
		{
			if (i < a.length - 1) // [warn]: Avoid subtracting from '.length' as it may be unsigned.
				writeln("something");
		}
	}c, sac);
	stderr.writeln("Unittest for IfElseSameCheck passed.");
}
