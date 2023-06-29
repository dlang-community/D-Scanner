// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.helpers;

import core.exception : AssertError;
import std.string;
import std.traits;
import std.stdio;

import dparse.ast;
import dparse.rollback_allocator;
import dsymbol.modulecache : ModuleCache;
import dscanner.analysis.config;
import dscanner.analysis.run;
import dscanner.analysis.base;
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
 * marked like so if range doesn't matter: // [warn]: Failed to do somethings.
 *
 * To test for start and end column, mark warnings as multi-line comments like
 * this: /+
 * ^^^^^ [warn]: Failed to do somethings. +/
 */
void assertAnalyzerWarnings(string code, const StaticAnalysisConfig config,
		string file = __FILE__, size_t line = __LINE__)
{
	import dscanner.analysis.run : parseModule;
	import dparse.lexer : StringCache, Token;

	StringCache cache = StringCache(StringCache.defaultBucketCount);
	RollbackAllocator r;
	const(Token)[] tokens;
	const(Module) m = parseModule(file, cast(ubyte[]) code, &r, defaultErrorFormat, cache, false, tokens);

	ModuleCache moduleCache;

	// Run the code and get any warnings
	MessageSet rawWarnings = analyze("test", m, config, moduleCache, tokens);
	string[] codeLines = code.splitLines();

	struct FoundWarning
	{
		string msg;
		size_t startColumn, endColumn;
	}

	// Get the warnings ordered by line
	FoundWarning[size_t] warnings;
	foreach (rawWarning; rawWarnings[])
	{
		// Skip the warning if it is on line zero
		immutable size_t rawLine = rawWarning.endLine;
		if (rawLine == 0)
		{
			stderr.writefln("!!! Skipping warning because it is on line zero:\n%s",
					rawWarning.message);
			continue;
		}

		size_t warnLine = line - 1 + rawLine;
		warnings[warnLine] = FoundWarning(
			format("[warn]: %s", rawWarning.message),
			rawWarning.startLine != rawWarning.endLine ? 1 : rawWarning.startColumn,
			rawWarning.endColumn,
		);
	}

	// Get all the messages from the comments in the code
	FoundWarning[size_t] messages;
	bool lastLineStartedComment = false;
	foreach (i, codeLine; codeLines)
	{
		scope (exit)
			lastLineStartedComment = codeLine.stripRight.endsWith("/+", "/*") > 0;

		// Get the line of this code line
		size_t lineNo = i + line;

		if (codeLine.stripLeft.startsWith("^") && lastLineStartedComment)
		{
			auto start = codeLine.indexOf("^") + 1;
			assert(start != 0);
			auto end = codeLine.indexOfNeither("^", start) + 1;
			assert(end != 0);
			auto warn = codeLine.indexOf("[warn]:");
			assert(warn != -1, "malformed line, expected `[warn]: text` after `^^^^^` part");
			auto message = codeLine[warn .. $].stripRight;
			if (message.endsWith("+/", "*/"))
				message = message[0 .. $ - 2].stripRight;
			messages[lineNo - 1] = FoundWarning(message, start, end);
		}
		// Skip if no [warn] comment
		else if (codeLine.indexOf("// [warn]:") != -1)
		{
			// Skip if there is no comment or code
			immutable string codePart = codeLine.before("// ");
			immutable string commentPart = codeLine.after("// ");
			if (!codePart.length || !commentPart.length)
				continue;

			// Get the message
			messages[lineNo] = FoundWarning(commentPart);
		}
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
		else if (warnings[lineNo].msg != messages[lineNo].msg)
		{
			immutable string errors = "Expected warning:\n%s\nBut was:\n%s\nFrom source code at (%s:?):\n%s".format(
					messages[lineNo], warnings[lineNo], lineNo, codeLines[lineNo - line]);
			throw new AssertError(errors, file, lineNo);
		}

		// specified column range
		if ((message.startColumn || message.endColumn)
			&& warnings[lineNo] != message)
		{
			import std.algorithm : max;
			import std.array : array;
			import std.range : repeat;
			import std.string : replace;

			const(char)[] expectedRange = ' '.repeat(max(0, cast(int)message.startColumn - 1)).array
				~ '^'.repeat(max(0, cast(int)(message.endColumn - message.startColumn))).array;
			const(char)[] actualRange;
			if (!warnings[lineNo].startColumn || warnings[lineNo].startColumn == warnings[lineNo].endColumn)
				actualRange = "no column range defined!";
			else
				actualRange = ' '.repeat(max(0, cast(int)warnings[lineNo].startColumn - 1)).array
					~ '^'.repeat(max(0, cast(int)(warnings[lineNo].endColumn - warnings[lineNo].startColumn))).array;
			size_t paddingWidth = max(expectedRange.length, actualRange.length);
			immutable string errors = "Wrong warning range: expected %s, but was %s\nFrom source code at (%s:?):\n%s\n%-*s <-- expected\n%-*s <-- actual".format(
					[message.startColumn, message.endColumn],
					[warnings[lineNo].startColumn, warnings[lineNo].endColumn],
					lineNo, codeLines[lineNo - line].replace("\t", " "),
					paddingWidth, expectedRange,
					paddingWidth, actualRange);
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
