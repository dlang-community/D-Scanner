//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.useless_assert;

import analysis.base;
import analysis.helpers;
import dparse.ast;
import dparse.lexer;

import std.stdio;

/**
 * Checks for asserts that always succeed
 */
class UselessAssertCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	///
	this(string fileName)
	{
		super(fileName, null);
	}

	override void visit(const AssertExpression ae)
	{
		import std.conv : to;

		UnaryExpression unary = cast(UnaryExpression) ae.assertion;
		if (unary is null)
		{
			stderr.writeln("unary is null");
			return;
		}

		if (unary.primaryExpression is null)
			return;
		immutable token = unary.primaryExpression.primary;
		immutable skipSwitch = unary.primaryExpression.arrayLiteral !is null
			|| unary.primaryExpression.assocArrayLiteral !is null
			|| unary.primaryExpression.functionLiteralExpression !is null;
		if (!skipSwitch) switch (token.type)
		{
		case tok!"doubleLiteral":
			if (!token.text.to!double)
				return;
			break;
		case tok!"floatLiteral":
			if (!token.text.to!float)
				return;
			break;
		case tok!"idoubleLiteral":
		case tok!"ifloatLiteral":
		case tok!"irealLiteral":
			return; // `to` doesn't support imaginary numbers
		case tok!"intLiteral":
			if (!token.text.to!int)
				return;
			break;
		case tok!"longLiteral":
			if (!token.text.to!long)
				return;
			break;
		case tok!"realLiteral":
			if (!token.text.to!real)
				return;
			break;
		case tok!"uintLiteral":
			if (!token.text.to!uint)
				return;
			break;
		case tok!"ulongLiteral":
			if (!token.text.to!ulong)
				return;
			break;
		case tok!"characterLiteral":
			if (token.text == `'\0'`)
				return;
			break;
		case tok!"dstringLiteral":
		case tok!"stringLiteral":
		case tok!"wstringLiteral":
		case tok!"true":
			break;
		default:
			return;
		}
		addErrorMessage(ae.line, ae.column, KEY, "Assert condition is always true");
	}

private:
	enum string KEY = "dscanner.suspicious.useless_assert";
}
