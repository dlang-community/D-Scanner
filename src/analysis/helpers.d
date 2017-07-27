// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.helpers;

import core.exception : AssertError;
import std.string;
import std.traits;
import std.stdio;

import dparse.ast;
import dparse.rollback_allocator;
import dsymbol.modulecache : ModuleCache;
import analysis.config;
import analysis.run;
import analysis.base;
import std.experimental.allocator.mallocator;
import std.experimental.allocator;

S between(S)(S value, S before, S after) if (isSomeString!S)
{
	return value.after(before).before(after);
}

S before(S)(S value, S separator) if (isSomeString!S)
{
	immutable i = indexOf(value, separator);
	if (i == -1)
		return value;
	return value[0 .. i];
}

S after(S)(S value, S separator) if (isSomeString!S)
{
	immutable i = indexOf(value, separator);
	if (i == -1)
		return "";
	return value[i + separator.length .. $];
}

/**
 * This assert function will analyze the passed in code, get the warnings,
 * and make sure they match the warnings in the comments. Warnings are
 * marked like so: // [warn]: Failed to do somethings.
 */
void assertAnalyzerWarnings(string code, const StaticAnalysisConfig config,
		string file = __FILE__, size_t line = __LINE__)
{
	import analysis.run : parseModule;
	import dparse.lexer : StringCache, Token;
	import std.ascii : newline;

	StringCache cache = StringCache(StringCache.defaultBucketCount);
	RollbackAllocator r;
	const(Token)[] tokens;
	const(Module) m = parseModule(file, cast(ubyte[]) code, &r, cache, false, tokens);

	auto moduleCache = ModuleCache(new CAllocatorImpl!Mallocator);

	// Run the code and get any warnings
	MessageSet rawWarnings = analyze("test", m, config, moduleCache, tokens);
	string[] codeLines = code.split(newline);

	// Get the warnings ordered by line
	string[size_t] warnings;
	foreach (rawWarning; rawWarnings[])
	{
		// Skip the warning if it is on line zero
		immutable size_t rawLine = rawWarning.line;
		if (rawLine == 0)
		{
			stderr.writefln("!!! Skipping warning because it is on line zero:\n%s",
					rawWarning.message);
			continue;
		}

		size_t warnLine = line - 1 + rawLine;
		warnings[warnLine] = format("[warn]: %s", rawWarning.message);
	}

	// Get all the messages from the comments in the code
	string[size_t] messages;
	foreach (i, codeLine; codeLines)
	{
		// Skip if no [warn] comment
		if (codeLine.indexOf("// [warn]:") == -1)
			continue;

		// Skip if there is no comment or code
		immutable string codePart = codeLine.before("// ");
		immutable string commentPart = codeLine.after("// ");
		if (!codePart.length || !commentPart.length)
			continue;

		// Get the line of this code line
		size_t lineNo = i + line;

		// Get the message
		messages[lineNo] = commentPart;
	}

	// Throw an assert error if any messages are not listed in the warnings
	foreach (lineNo, message; messages)
	{
		// No warning
		if (lineNo !in warnings)
		{
			immutable string errors = "Expected warning:\n%s\nFrom source code at (%s:?):\n%s".format(messages[lineNo],
					lineNo, codeLines[lineNo - line]);
			throw new AssertError(errors, file, lineNo);
		}
		// Different warning
		else if (warnings[lineNo] != messages[lineNo])
		{
			immutable string errors = "Expected warning:\n%s\nBut was:\n%s\nFrom source code at (%s:?):\n%s".format(
					messages[lineNo], warnings[lineNo], lineNo, codeLines[lineNo - line]);
			throw new AssertError(errors, file, lineNo);
		}
	}

	// Throw an assert error if there were any warnings that were not expected
	string[] unexpectedWarnings;
	foreach (lineNo, warning; warnings)
	{
		// Unexpected warning
		if (lineNo !in messages)
		{
			unexpectedWarnings ~= "%s\nFrom source code at (%s:?):\n%s".format(warning,
					lineNo, codeLines[lineNo - line]);
		}
	}
	if (unexpectedWarnings.length)
	{
		immutable string message = "Unexpected warnings:\n" ~ unexpectedWarnings.join("\n");
		throw new AssertError(message, file, line);
	}
}
