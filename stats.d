//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module stats;

import std.stdio;
import std.d.lexer;

pure nothrow bool isLineOfCode(TokenType t)
{
	switch(t)
	{
	case TokenType.semicolon:
	case TokenType.while_:
	case TokenType.if_:
	case TokenType.for_:
	case TokenType.foreach_:
	case TokenType.foreach_reverse_:
	case TokenType.case_:
		return true;
	default:
		return false;
	}
}

void printTokenCount(Tokens)(File output, ref Tokens tokens, size_t fileSize)
{
	ulong count;
	while(!tokens.empty)
	{
		tokens.popFront();
		++count;
	}
	output.writefln("%f", cast(float) fileSize / cast(float) count);
	//output.writefln("%d", count);
}

void printLineCount(Tokens)(File output, ref Tokens tokens)
{
	ulong count;
	foreach (t; tokens)
	{
		if (isLineOfCode(t.type))
			++count;
	}
	output.writefln("%d", count);
}
