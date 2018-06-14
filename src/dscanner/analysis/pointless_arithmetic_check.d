//          Copyright Hiroki Noda 2018.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.pointless_arithmetic_check;

import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import std.stdio;
import std.typetuple;

/**
 * Checks for Pointless Arithmetic.
 */
final class PointlessArithmeticCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	///
	this(string fileName, bool skipTest = false)
	{
		super(fileName, null, skipTest);
	}

	override void visit(const AssignExpression asgExp)
	{
		auto left = asgExp.ternaryExpression;
		UnaryExpression right = cast(UnaryExpression) asgExp.expression;

		if (left && right)
		{
			if (asgExp.operator == tok!"*=" &&
				isNumberLiteral(right.primaryExpression.primary.type) &&
				right.primaryExpression.primary.text == "1")
			{
				addErrorMessage(asgExp.line, asgExp.column, KEY,
								"Any number 'x' mul 1 will be 'x'.");
			}
			else if (asgExp.operator == tok!"/=" &&
					 isNumberLiteral(right.primaryExpression.primary.type) &&
					 right.primaryExpression.primary.text == "1")
			{
				addErrorMessage(asgExp.line, asgExp.column, KEY,
								"Any number 'x' div 1 will be 'x'.");
			}
			else if (asgExp.operator == tok!"%=" &&
					 isNumberLiteral(right.primaryExpression.primary.type) &&
					 right.primaryExpression.primary.text == "1")
			{
				addErrorMessage(asgExp.line, asgExp.column, KEY,
								"Any number modulo 1 will be 0.");
			}
		}
		asgExp.accept(this);
	}

	override void visit(const PowExpression powExp)
	{
		UnaryExpression left = cast(UnaryExpression) powExp.left;
		UnaryExpression right = cast(UnaryExpression) powExp.right;

		if (left && right)
		{
			if (isNumberLiteral(right.primaryExpression.primary.type) &&
				right.primaryExpression.primary.text == "0")
			{
				addErrorMessage(powExp.line, powExp.column, KEY,
								"Any number pow 0 will be 1.");
			}
		}
		powExp.accept(this);
	}

	override void visit(const MulExpression mulExp)
	{
		UnaryExpression left = cast(UnaryExpression) mulExp.left;
		UnaryExpression right = cast(UnaryExpression) mulExp.right;

		if (left && right)
		{
			if (mulExp.operator == tok!"*" &&
				isNumberLiteral(right.primaryExpression.primary.type) &&
				right.primaryExpression.primary.text == "1")
			{
				addErrorMessage(mulExp.line, mulExp.column, KEY,
								"Any number 'x' mul 1 will be 'x'.");
			}
			else if (mulExp.operator == tok!"/" &&
				isNumberLiteral(right.primaryExpression.primary.type) &&
				right.primaryExpression.primary.text == "1")
			{
				addErrorMessage(mulExp.line, mulExp.column, KEY,
								"Any number 'x' div 1 will be 'x'.");
			}
			else if (mulExp.operator == tok!"%" &&
					 isIntegerLiteral(left.primaryExpression.primary.type) &&
					 isNumberLiteral(right.primaryExpression.primary.type) &&
					 right.primaryExpression.primary.text == "1")
			{
				addErrorMessage(mulExp.line, mulExp.column, KEY,
								"Any number modulo 1 will be 0.");
			}
		}
		mulExp.accept(this);
	}

private:
	enum KEY = "dscanner.confusing.pointless_arithmetic_check";

	static bool isIntegerLiteral(IdType type) nothrow pure
	{
		alias IntegerLiterals = AliasSeq!(tok!"intLiteral", tok!"longLiteral",
										  tok!"uintLiteral", tok!"ulongLiteral");
		switch (type)
		{
		foreach (T; IntegerLiterals)
		{
		case T:
			return true;
		}
		default:
			return false;
		}
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.pointless_arithmetic_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void testPointlessArithmetic()
		{
			int a = 2;

			a = a * 2;
			a = a * 1; // [warn]: Any number 'x' mul 1 will be 'x'.
			a *= 2;
			a *= 1; // [warn]: Any number 'x' mul 1 will be 'x'.

			a = a / 2;
			a = a / 1; // [warn]: Any number 'x' div 1 will be 'x'.

			a /= 1; // [warn]: Any number 'x' div 1 will be 'x'.

			a = 10 % 2;
			a = 10 % 1; // [warn]: Any number modulo 1 will be 0.

			a %= 1; // [warn]: Any number modulo 1 will be 0.

			a = a ^^ 0; // [warn]: Any number pow 0 will be 1.

			float b = 1.1;
			b = b * 1; // [warn]: Any number 'x' mul 1 will be 'x'.
			b = b / 1; // [warn]: Any number 'x' div 1 will be 'x'.
			b /= 1; // [warn]: Any number 'x' div 1 will be 'x'.
		}
	}c, sac);

	stderr.writeln("Unittest for PointlessArithmeticCheck passed.");
}
