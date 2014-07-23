//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.length_subtraction;

import std.stdio;

import std.d.ast;
import std.d.lexer;
import analysis.base;
import analysis.helpers;


/**
 * Checks for subtraction from a .length property. This is usually a bug.
 */
class LengthSubtractionCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const AddExpression addExpression)
	{
		if (addExpression.operator == tok!"-")
		{
			UnaryExpression l = cast(UnaryExpression) addExpression.left;
			UnaryExpression r = cast(UnaryExpression) addExpression.right;
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
			addErrorMessage(token.line, token.column,
				"Avoid subtracting from '.length' as it may be unsigned.");
		}
	end:
		addExpression.accept(this);
	}
}

unittest
{
	import analysis.config;
	StaticAnalysisConfig sac;
	sac.if_else_same_check = true;
	assertAnalyzerWarnings(q{
		void testSizeT()
		{
			if (i < a.length - 1) // [warn]: Avoid subtracting from '.length' as it may be unsigned.
				writeln("something");
		}
	}c, sac);
	stderr.writeln("Unittest for IfElseSameCheck passed.");
}

