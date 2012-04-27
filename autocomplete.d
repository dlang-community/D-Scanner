//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module autocomplete;

import std.range;
import std.algorithm;
import std.array;
import std.conv;
import std.stdio;
import std.typecons;

import parser;
import langutils;
import types;
import tokenizer;

immutable string[] versions = ["AIX", "all", "Alpha", "ARM", "BigEndian", "BSD",
	"Cygwin", "D_Coverage", "D_Ddoc", "DigitalMars", "D_InlineAsm_X86",
	"D_InlineAsm_X86_64", "D_LP64", "D_NET", "D_PIC", "D_Version2",
	"FreeBSD", "GNU", "HPPA", "HPPA64", "Hurd", "IA64", "LDC", "linux",
	"LittleEndian", "MinGW", "MIPS", "MIPS64", "none", "OpenBSD", "OSX",
	"Posix", "PPC", "PPC64", "S390", "S390X", "SDC", "SH", "SH64", "SkyOS",
	"Solaris", "SPARC", "SPARC64", "SysV3", "SysV4", "unittest", "Win32",
	"Win64", "Windows", "X86", "X86_64"
];

/**
 * Returns: indicies into the token array
 */
size_t findEndOfExpression(const Token[] tokens, size_t index)
{
	size_t i = index;
	while (i < tokens.length)
	{
		switch (tokens[i].type)
		{
		case TokenType.RBrace:
		case TokenType.RParen:
		case TokenType.RBracket:
		case TokenType.Semicolon:
			break;
		case TokenType.LParen:
			skipParens(tokens, index);
			break;
		case TokenType.LBrace:
			skipBraces(tokens, index);
			break;
		case TokenType.LBracket:
			skipBrackets(tokens, index);
			break;
		default:
			++i;
			break;
		}
	}
	return i;
}

size_t findBeginningOfExpression(const Token[] tokens, size_t index)
{
	return index;
}

struct AutoComplete
{
	this(const (Token)[] tokens, CompletionContext context)
	{
		this.tokens = tokens;
		this.context = context;
	}

	string getTypeOfExpression(const(Token)[] expression, const Token[] tokens, size_t cursor)
	{
		return "void";
	}

	/**
	 * This is where the magic happens
	 */
	string typeOfVariable(Token symbol, size_t cursor)
	{
		// int is of type int, double of type double, and so on
		if (symbol.value in typeProperties)
			return symbol.value;

		switch (symbol.type)
		{
			case TokenType.FloatLiteral:
				return "float";
			case TokenType.DoubleLiteral:
				return "double";
			case TokenType.RealLiteral:
				return "real";
			case TokenType.IntLiteral:
				return "int";
			case TokenType.UnsignedIntLiteral:
				return "uint";
			case TokenType.LongLiteral:
				return "long";
			case TokenType.UnsignedLongLiteral:
				return "ulong";
			default:
				break;
		}

		if (context.getMembersOfType(symbol.value))
			return symbol.value;

		// Arbitrarily define the depth of the cursor position as zero
		// iterate backwards through the code to try to find the variable
		int depth = 0;
		auto preceedingTokens = assumeSorted(tokens).lowerBound(cursor);
		auto index = preceedingTokens.length - 1;
		while (true)
		{
			if (preceedingTokens[index] == TokenType.LBrace)
				--depth;
			else if (preceedingTokens[index] == TokenType.RBrace)
				++depth;
			else if (depth <= 0 && preceedingTokens[index].value == symbol)
			{
				// Found the symbol, now determine if it was declared here.
				auto p = preceedingTokens[index - 1];
				if ((p == TokenType.Auto || p == TokenType.Immutable
					|| p == TokenType.Const)
					&& preceedingTokens[index + 1] == TokenType.Assign)
				{
					return null;
				}
				else if (p == TokenType.Identifier
					|| (p.type > TokenType.TYPES_BEGIN
					&& p.type < TokenType.TYPES_END))
				{
					return p.value;
				}
			}
			if (index == 0)
				break;
			else
				--index;
		}

		// Find all struct or class bodies that we're in.
		// Check for the symbol in those class/struct/interface bodies
		// if match is found, return it
		auto structs = context.getStructsContaining(cursor);
		if (symbol == "this" && structs.length > 0)
			return minCount!("a.bodyStart > b.bodyStart")(structs)[0].name;
		foreach (s; structs)
		{
			auto t = s.getMemberType(symbol.value);
			if (t !is null)
				return t;
		}
		return "void";
	}

	string symbolAt(size_t cursor) const
	{
		auto r = assumeSorted(tokens).lowerBound(cursor)[$ - 1];
		if (r.value.length + r.startIndex > cursor)
			return r.value;
		else
			return null;
	}

	string parenComplete(size_t cursor)
	{
		stderr.writeln("parenComplete");
		auto index = assumeSorted(tokens).lowerBound(cursor).length - 2;
		Token t = tokens[index];
		stderr.writeln(t);
		if (t.startIndex + t.value.length + 1 != cursor)
			return "";
		switch (tokens[index].type)
		{
		case TokenType.Version:
			return to!string(join(map!`a ~ "?1"`(versions), " ").array());
		case TokenType.If:
		case TokenType.Cast:
		case TokenType.While:
		case TokenType.For:
		case TokenType.Foreach:
		case TokenType.Switch:
			return "";
		default:
			return "";
		}
	}

	string dotComplete(size_t cursor)
	{
		auto index = assumeSorted(tokens).lowerBound(cursor).length - 2;
		Token t = tokens[index];
		if (t.startIndex + t.value.length + 1 != cursor)
			return "";
		auto type = typeOfVariable(t, cursor);

		const Tuple!(string, string)[string] typeMap = context.getMembersOfType(type);
		if (typeMap is null)
			return "";
		auto app = appender!(string[])();
		foreach (k, t; typeMap)
			app.put(k ~ t[1]);
		return to!string(array(join(sort!"a.toLower() < b.toLower()"(app.data), " ")));
	}

	const(Token)[] tokens;
	CompletionContext context;
}
