//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module langutils;

import std.array;
import std.algorithm;
import std.d.lexer;


/**
 * Returns: true if input is a access attribute
 */
pure nothrow bool isAccessAttribute(TokenType input)
{
	return input > TokenType.PROTECTION_BEGIN && input < TokenType.PROTECTION_END;
}

/**
 * See_also: isAttribute(TokenType)
 */
pure nothrow bool isAttribute(ref const Token token)
{
	return isAttribute(token.type);
}

/**
 * Returns: true if the given token type is an attribute, false otherwise
 */
pure nothrow bool isAttribute(TokenType input)
{
	if (isAccessAttribute(input))
		return true;
	return input > TokenType.ATTRIBUTES_BEGIN && input < TokenType.ATTRIBUTES_END;
}

string combineTokens(ref const Token[] tokens)
{
	auto app = appender!string();
	foreach (t; tokens)
		app.put(t.value);
	return app.data;
}

pure nothrow string getTypeFromToken(const Token t)
{
	switch (t.type)
	{

	case TokenType.DoubleLiteral:
		return "double";
	case TokenType.FloatLiteral:
		return "float";
	case TokenType.IntLiteral:
		return "int";
	case TokenType.RealLiteral:
		return "real";
	case TokenType.UnsignedIntLiteral:
		return "uint";
	case TokenType.UnsignedLongLiteral:
		return "ulong";
	case TokenType.LongLiteral:
		return "long";
	case TokenType.DStringLiteral:
		return "dstring";
	case TokenType.StringLiteral:
		return "string";
	case TokenType.WStringLiteral:
		return "wstring";
	default:
		return null;
	}
}

pure bool isIdentifierOrType(const Token t)
{
	return t.type == TokenType.Identifier || (t.type > TokenType.TYPES_BEGIN
		&& TokenType.TYPES_END);
}

pure bool isDocComment(ref const Token t)
{
    return t.value.startsWith("///") || t.value.startsWith("/**")
        || t.value.startsWith("/++");
}
