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

auto filterChars(string chars, S)(S str)
{
	import std.algorithm.comparison : among;
	import std.algorithm.iteration : filter;
	import std.meta : aliasSeqOf;
	return str.filter!(c => !c.among(aliasSeqOf!chars));
}

/**
 * Checks for asserts that always succeed
 */
class UselessAssertCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	///
	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	override void visit(const AssertExpression ae)
	{
		import std.conv : to;

		UnaryExpression unary = cast(UnaryExpression) ae.assertion;
		if (unary is null)
			return;
		if (unary.primaryExpression is null)
			return;
		immutable token = unary.primaryExpression.primary;
		immutable skipSwitch = unary.primaryExpression.arrayLiteral !is null
			|| unary.primaryExpression.assocArrayLiteral !is null
			|| unary.primaryExpression.functionLiteralExpression !is null;
		if (!skipSwitch) switch (token.type)
		{
		case tok!"doubleLiteral":
			if (!token.text.filterChars!"Ll".to!double)
				return;
			break;
		case tok!"floatLiteral":
			if (!token.text.filterChars!"Ff".to!float)
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
			if (!token.text.filterChars!"Ll".to!long)
				return;
			break;
		case tok!"realLiteral":
			if (!token.text.to!real)
				return;
			break;
		case tok!"uintLiteral":
			if (!token.text.filterChars!"Uu".to!uint)
				return;
			break;
		case tok!"ulongLiteral":
			if (!token.text.filterChars!"UuLl".to!ulong)
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
		addErrorMessage(ae.line, ae.column, KEY, MESSAGE);
	}

private:
	enum string KEY = "dscanner.suspicious.useless_assert";
	enum string MESSAGE = "Assert condition is always true.";
}

unittest
{
	import std.stdio : stderr;
	import analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.format : format;

	StaticAnalysisConfig sac = disabledConfig();
	sac.useless_assert_check = Check.enabled;
	assertAnalyzerWarnings(q{
unittest
{
	assert(true); // [warn]: %1$s
	assert(1); // [warn]: %1$s
	assert([10]); // [warn]: %1$s
	assert(false);
	assert(0);
	assert(0.0L);
}

}c
			.format(UselessAssertCheck.MESSAGE), sac);
	stderr.writeln("Unittest for UselessAssertCheck passed.");
}
