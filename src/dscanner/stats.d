//          Copyright Brian Schott (Hackerpilot) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.stats;

import std.stdio;
import std.algorithm;
import dparse.lexer;

pure nothrow bool isLineOfCode(IdType t)
{
	switch (t)
	{
	case tok!";":
	case tok!"while":
	case tok!"if":
	case tok!"do":
	case tok!"else":
	case tok!"switch":
	case tok!"for":
	case tok!"foreach":
	case tok!"foreach_reverse":
	case tok!"default":
	case tok!"case":
		return true;
	default:
		return false;
	}
}

ulong printTokenCount(Tokens)(File output, string fileName, ref Tokens tokens)
{
	ulong c;
	foreach (ref t; tokens)
	{
		c++;
	}
	output.writefln("%s:\t%d", fileName, c);
	return c;
}

ulong printLineCount(Tokens)(File output, string fileName, ref Tokens tokens)
{
	ulong count;
	foreach (ref t; tokens)
	{
		if (isLineOfCode(t.type))
			++count;
	}
	output.writefln("%s:\t%d", fileName, count);
	return count;
}
