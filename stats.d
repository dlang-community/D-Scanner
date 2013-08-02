//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module stats;

import std.stdio;
import stdx.d.lexer;

pure nothrow bool isLineOfCode(TokenType t)
{
	with (TokenType) switch(t)
	{
	case semicolon:
	case while_:
	case if_:
	case do_:
	case else_:
	case switch_:
	case for_:
	case foreach_:
	case foreach_reverse_:
	case default_:
	case case_:
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
	output.writefln("%d", count);
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
