
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

string[] callChainBackwards(const Token[] tokens, size_t index)
{
	if (index == 0)
		return [tokens[index].value];
	string[] callChain;
	string current;
	loop: while(true)
	{
		switch(tokens[index].type)
		{
		case TokenType.tThis:
		case TokenType.identifier:
		case TokenType.TYPES_BEGIN: .. case TokenType.TYPES_END:
			current = tokens[index].value ~ current;
			callChain = current ~ callChain;
			current = "";
			if (index == 0)
				break loop;
			else
				--index;
			if (tokens[index] == TokenType.not)
				callChain = callChain[1 .. $];
			break;
		case TokenType.rBracket:
			tokens.skipBrackets(index);
			current ~= "[]";
			break;
		case TokenType.rParen:
			tokens.skipParens(index);
			break;
		case TokenType.not:
		case TokenType.dot:
			if (index == 0)
				break loop;
			else
				--index;
			break;
		default:
			break loop;
		}
	}
	return callChain;
}


string[] callChainForwards(const Token[] tokens, size_t index)
{
	string[] callChain;
	while (index < tokens.length)
	{
		switch(tokens[index].type)
		{
		case TokenType.tNew:
			++index;
			break;
		case TokenType.tThis:
		case TokenType.identifier:
		case TokenType.TYPES_BEGIN: .. case TokenType.TYPES_END:
			callChain ~= tokens[index++].value;
			break;
		case TokenType.lParen:
			tokens.skipParens(index);
			break;
		case TokenType.lBracket:
			tokens.skipBrackets(index);
			callChain[$ - 1] ~= "[i]";
			break;
		case TokenType.not:
			++index;
			if (tokens.startsWith(TokenType.lParen))
				tokens.skipParens(index);
			else
				++index;
			break;
		default:
			break;
		}
		if (index >= tokens.length || tokens[index] != TokenType.dot)
			break;
		else
			++index;
	}
	return callChain;
}


struct AutoComplete
{
	this(const (Token)[] tokens, CompletionContext context)
	{
		this.tokens = tokens;
		this.context = context;
	}

	string getTypeOfExpression(string[] chain, const Token[] tokens, size_t cursor)
	{
		if (chain.length == 0)
			return "void";
		auto type = typeOfVariable(chain[0], cursor);
		if (type == "void")
			return type;
		chain = chain[1 .. $];
		while (chain.length >= 1)
		{
			auto typeMap = context.getMembersOfType(type);
			if (typeMap is null)
				return "void";
			auto memberType = typeMap[chain[0]][0];
			if (memberType is null)
				return "void";
			type = memberType;
			chain = chain[1 .. $];
		}
		return type;
	}

	/**
	 * This is where the magic happens
	 */
	string typeOfVariable(string symbol, size_t cursor)
	{
		// int is of type int, double of type double, and so on
		if (symbol in typeProperties)
			return symbol;

		if (context.getMembersOfType(symbol))
			return symbol;

		// Arbitrarily define the depth of the cursor position as zero
		// iterate backwards through the code to try to find the variable
		int depth = 0;
		auto preceedingTokens = assumeSorted(tokens).lowerBound(cursor);
		auto index = preceedingTokens.length - 1;
		while (true)
		{
			if (preceedingTokens[index] == TokenType.lBrace)
				--depth;
			else if (preceedingTokens[index] == TokenType.rBrace)
				++depth;
			else if (depth <= 0 && preceedingTokens[index].value == symbol)
			{
				// Found the symbol, now determine if it was declared here.
				auto p = preceedingTokens[index - 1];
				if ((p == TokenType.tAuto || p == TokenType.tImmutable
					|| p == TokenType.tConst)
					&& preceedingTokens[index + 1] == TokenType.assign)
				{
					auto chain = callChainForwards(tokens, index + 2);
					return getTypeOfExpression(chain, tokens, cursor);
				}
				if (p == TokenType.identifier
					|| (p.type > TokenType.TYPES_BEGIN
					&& p.type < TokenType.TYPES_END))
				{
					return preceedingTokens[index - 1].value;
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
			auto t = s.getMemberType(symbol);
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
		auto index = assumeSorted(tokens).lowerBound(cursor).length;
		if (index > 2)
			index -= 2;
		else
			return [];
		if (tokens[index] == TokenType.tVersion)
		{
			return to!string(array(join(map!`a ~ "?1"`(versions), " ")));
		}
		return "";
	}

	string dotComplete(size_t cursor)
	{
		auto index = assumeSorted(tokens).lowerBound(cursor).length;
		if (index > 2)
			index -= 2;
		else
			return "";
		auto t = tokens[index];
		string[] chain = callChainBackwards(tokens, index);
		auto type = getTypeOfExpression(chain, tokens, cursor);

		if (type && type in typeProperties)
		{
			string r;
			foreach (i, prop; typeProperties[type])
				if (i == typeProperties.length)
					r = r ~ prop;
				else
					r = r ~ prop ~ " ";
			return r;
		}

		const Tuple!(string, string)[string] typeMap = context.getMembersOfType(type);
		if (typeMap is null)
			return "";
		auto app = appender!(string[])();
		foreach (k, t; typeMap)
			app.put(k ~ t[1]);
		return to!string(array(join(sort(app.data), " ")));
	}

	const(Token)[] tokens;
	CompletionContext context;
}
