// Written in the D programming language

/**
 * This module contains a range-based _lexer generator.
 *
 * Copyright: Brian Schott 2013
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt Boost, License 1.0)
 * Authors: Brian Schott, with ideas shamelessly stolen from Andrei Alexandrescu
 * Source: $(PHOBOSSRC std/_lexer.d)
 */

module stdx.lexer;

import std.typecons;
import std.algorithm;
import std.range;
import std.traits;
import std.conv;
import std.math;
import dpick.buffer.buffer;
import dpick.buffer.traits;

template TokenIdType(alias staticTokens, alias dynamicTokens,
	alias possibleDefaultTokens)
{
  static if ((staticTokens.length + dynamicTokens.length + possibleDefaultTokens.length) <= ubyte.max)
		alias TokenIdType = ubyte;
	else static if ((staticTokens.length + dynamicTokens.length + possibleDefaultTokens.length) <= ushort.max)
		alias TokenIdType = ushort;
	else static if ((staticTokens.length + dynamicTokens.length + possibleDefaultTokens.length) <= uint.max)
		alias TokenIdType = uint;
	else
		static assert (false);
}

string tokenStringRepresentation(IdType, alias staticTokens, alias dynamicTokens, alias possibleDefaultTokens)(IdType type) @property
{
	if (type == 0)
		return "!ERROR!";
	else if (type < staticTokens.length + 1)
		return staticTokens[type - 1];
	else if (type < staticTokens.length + possibleDefaultTokens.length + 1)
		return possibleDefaultTokens[type - staticTokens.length - 1];
    else if (type < staticTokens.length + possibleDefaultTokens.length + dynamicTokens.length + 1)
		return dynamicTokens[type - staticTokens.length - possibleDefaultTokens.length - 1];
	else
		return null;
}

template TokenId(IdType, alias staticTokens, alias dynamicTokens,
	alias possibleDefaultTokens, string symbol)
{
	static if (symbol == "")
	{
	  enum id = 0;
		alias id TokenId;
	}
	else static if (symbol == "\0")
	{
		enum id = 1 + staticTokens.length + dynamicTokens.length + possibleDefaultTokens.length;
		alias id TokenId;
	}
	else
	{
		enum i = staticTokens.countUntil(symbol);
		static if (i >= 0)
		{
			enum id = i + 1;
			alias id TokenId;
		}
		else
		{
			enum ii = possibleDefaultTokens.countUntil(symbol);
			static if (ii >= 0)
			{
				enum id = ii + staticTokens.length + 1;
				static assert (id >= 0 && id < IdType.max, "Invalid token: " ~ symbol);
				alias id TokenId;
			}
			else
			{
				enum dynamicId = dynamicTokens.countUntil(symbol);
				enum id = dynamicId >= 0
					? i + staticTokens.length + possibleDefaultTokens.length + dynamicId + 1
					: -1;
				static assert (id >= 0 && id < IdType.max, "Invalid token: " ~ symbol);
				alias id TokenId;
			}
		}
	}
}

struct TokenStructure(IDType, string extraFields = "")
{
	bool opEquals(IDType type) const pure nothrow @safe
	{
		return this.type == type;
	}

	this(IDType type)
	{
		this.type = type;
	}

	this(IDType type, string text, size_t line, size_t column, size_t index)
	{
		this.text = text;
		this.line = line;
		this.column = column;
		this.type = type;
		this.index = index;
	}

	string text;
	size_t line;
	size_t column;
	size_t index;
	IDType type;
	mixin (extraFields);
}

mixin template Lexer(R, IDType, Token, alias defaultTokenFunction,
	alias staticTokens, alias dynamicTokens, alias pseudoTokens,
	alias pseudoTokenHandlers, alias possibleDefaultTokens)
{
	static string generateCaseStatements(string[] tokens, size_t offset = 0)
	{
		string code;
		for (size_t i = 0; i < tokens.length; i++)
		{
			auto indent = "";
			foreach (k; 0 .. offset)
				indent ~= "    ";
			size_t j = i + 1;

			if (offset < tokens[i].length)
			{
				while (j < tokens.length && offset < tokens[j].length
					&& tokens[i][offset] == tokens[j][offset]) j++;
				code ~= indent ~ "case " ~ text(cast(ubyte) tokens[i][offset]) ~ ":\n";
				if (i + 1 >= j)
				{
					if (offset + 1 == tokens[i].length)
						code ~= generateLeaf(tokens[i], indent ~ "    ");
					else
					{
						code ~= indent ~ "    if (range.lookahead(" ~ text(tokens[i].length) ~ ").length == 0)\n";
						code ~= indent ~ "        goto outer_default;\n";
						code ~= indent ~ "    if (range.lookahead(" ~ text(tokens[i].length) ~ ") == \"" ~ escape(tokens[i]) ~ "\")\n";
						code ~= indent ~ "    {\n";
						code ~= generateLeaf(tokens[i], indent ~ "        ");
						code ~= indent ~ "    }\n";
						code ~= indent ~ "    else\n";
						code ~= indent ~ "        goto outer_default;\n";
					}
				}
				else
				{
					code ~= indent ~ "    if (range.lookahead(" ~ text(offset + 2) ~ ").length == 0)\n";
					code ~= indent ~ "    {\n";
					code ~= generateLeaf(tokens[i][0 .. offset + 1], indent ~ "        ");
					code ~= indent ~ "    }\n";
					code ~= indent ~ "    switch (range.lookahead(" ~ text(offset + 2) ~ ")[" ~ text(offset + 1) ~ "])\n";
					code ~= indent ~ "    {\n";
					code ~= generateCaseStatements(tokens[i .. j], offset + 1);
					code ~= indent ~ "    default:\n";
					code ~= generateLeaf(tokens[i][0 .. offset + 1], indent ~ "        ");
					code ~= indent ~ "    }\n";
				}
			}
			i = j - 1;
		}
		return code;
	}

	static string generateLeaf(string token, string indent)
	{
		static assert (pseudoTokenHandlers.length % 2 == 0,
			"Each pseudo-token must have a matching function name.");
		string code;
		if (staticTokens.countUntil(token) >= 0)
		{
			if (token.length == 1)
				code ~= indent ~ "range.popFront();\n";
			else
				code ~= indent ~ "range.popFrontN(" ~ text(token.length) ~ ");\n";
			code ~= indent ~ "return Token(tok!\"" ~ escape(token) ~ "\", null, line, column, index);\n";
		}
		else if (pseudoTokens.countUntil(token) >= 0)
			code ~= indent ~ "return " ~ pseudoTokenHandlers[pseudoTokenHandlers.countUntil(token) + 1] ~ "();\n";
		else if (possibleDefaultTokens.countUntil(token) >= 0)
		{
			code ~= indent ~ "if (range.lookahead(" ~ text(token.length + 1) ~ ").length == 0 || isSeparating(range.lookahead(" ~ text(token.length + 1) ~ ")[" ~ text(token.length) ~ "]))\n";
			code ~= indent ~ "{\n";
			if (token.length == 1)
				code ~= indent ~ "    range.popFront();\n";
			else
				code ~= indent ~ "    range.popFrontN(" ~ text(token.length) ~ ");\n";
			code ~= indent ~ "    return Token(tok!\"" ~ escape(token) ~"\", null, line, column, index);\n";
			code ~= indent ~ "}\n";
			code ~= indent ~ "else\n";
			code ~= indent ~ "    goto outer_default;\n";
		}
		else
			code ~= indent ~ "goto outer_default;\n";
		return code;
	}

	const(Token) front() pure nothrow const @property
	{
		return _front;
	}

	void _popFront() pure
	{
		_front = advance();
	}

	bool empty() pure const nothrow @property
	{
		return _front.type == tok!"\0";
	}

	static string escape(string input)
	{
		string rVal;
		foreach (ubyte c; cast(ubyte[]) input)
		{
			switch (c)
			{
			case '\\': rVal ~= `\\`; break;
			case '"': rVal ~= `\"`; break;
			case '\'': rVal ~= `\'`; break;
			case '\t': rVal ~= `\t`; break;
			case '\n': rVal ~= `\n`; break;
			case '\r': rVal ~= `\r`; break;
			default: rVal ~= c; break;
			}
		}
		return rVal;
	}

	Token advance() pure
	{
		if (range.empty)
			return Token(tok!"\0");
		immutable size_t index = range.index;
		immutable size_t column = range.column;
		immutable size_t line = range.line;
		lexerLoop: switch (range.front)
		{
		mixin(generateCaseStatements(stupidToArray(sort(staticTokens ~ pseudoTokens ~ possibleDefaultTokens))));
//		pragma(msg, generateCaseStatements(stupidToArray(sort(staticTokens ~ pseudoTokens ~ possibleDefaultTokens))));
		outer_default:
		default:
			return defaultTokenFunction();
		}
	}

	/**
	 * This only exists because the real array() can't be called at compile-time
	 */
	static T[] stupidToArray(R, T = ElementType!R)(R range)
	{
		T[] rVal;
		foreach (v; range)
			rVal ~= v;
		return rVal;
	}

	LexerRange!(typeof(buffer(R.init))) range;
	Token _front;
}

struct LexerRange(BufferType) if (isBuffer!BufferType)
{
	this(BufferType r)
	{
		this.range = r;
		index = 0;
		column = 1;
		line = 1;
	}

	void popFront() pure
	{
		index++;
		column++;
		range.popFront();
	}

	void incrementLine() pure nothrow
	{
		column = 1;
		line++;
	}

	BufferType range;
	alias range this;
	size_t index;
	size_t column;
	size_t line;
}
