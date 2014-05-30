// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.helpers;

import std.string;
import std.traits;
import std.stdio;

import std.d.ast;
import analysis.run;


S between(S)(S value, S before, S after) 
	if (isSomeString!S)
{
	return value.after(before).before(after);
}

S before(S)(S value, S separator)
	if (isSomeString!S)
{
	auto i = indexOf(value, separator);

	if (i == -1)
		return value;

	return value[0 .. i];
}

S after(S)(S value, S separator)
	if (isSomeString!S)
{
	auto i = indexOf(value, separator);

	if (i == -1)
		return "";

	size_t start = i + separator.length;

	return value[start .. $];
}

/**
 * This assert function will analyze the passed in code, get the warnings,
 * and make sure they match the warnings in the comments. Warnings are
 * marked like so: // [warn]: Failed to do somethings.
 */
void assertAnalyzerWarnings(string code, analysis.run.AnalyzerCheck analyzers, string file=__FILE__, size_t line=__LINE__)
{
	import analysis.run;

	// Run the code and get any warnings
	string[] rawWarnings = analyze("test", cast(ubyte[]) code, analyzers);
	string[] codeLines = code.split("\n");

	// Get the warnings ordered by line
	string[size_t] warnings;
	for (size_t i=0; i<rawWarnings.length; ++i)
	{
		// Skip the warning if it is on line zero
		size_t rawLine = std.conv.to!size_t(rawWarnings[i].between("test(", ":"));
		if (rawLine == 0)
		{
			stderr.writefln("!!! Skipping warning because it is on line zero:\n%s", rawWarnings[i]);
			continue;
		}

		size_t warnLine = line - 1 + rawLine;
		warnings[warnLine] = rawWarnings[i].after(")");
	}

	// Get all the messages from the comments in the code
	string[size_t] messages;
	foreach (i, codeLine; codeLines)
	{
		// Skip if no [warn] comment
		if (codeLine.indexOf("// [warn]:") == -1)
			continue;

		// Skip if there is no comment or code
		string codePart = codeLine.before("// ");
		string commentPart = codeLine.after("// ");
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
			string errors = "Expected warning:\n%s\nFrom source code at (%s:?):\n%s".format(
				messages[lineNo],
				lineNo,
				codeLines[lineNo - line]
			);
			throw new core.exception.AssertError(errors, file, lineNo);
		}
		// Different warning
		else if (warnings[lineNo] != messages[lineNo])
		{
			string errors = "Expected warning:\n%s\nBut was:\n%s\nFrom source code at (%s:?):\n%s".format(
				messages[lineNo],
				warnings[lineNo],
				lineNo,
				codeLines[lineNo - line]
			);
			throw new core.exception.AssertError(errors, file, lineNo);
		}
	}

	// Throw an assert error if there were any warnings that were not expected
	string[] unexpectedWarnings;
	foreach (lineNo, warning; warnings)
	{
		// Unexpected warning
		if (lineNo !in messages)
		{
			unexpectedWarnings ~= "%s\nFrom source code at (%s:?):\n%s".format(
				warning,
				lineNo,
				codeLines[lineNo - line]
			);
		}
	}
	if (unexpectedWarnings.length)
	{
		string message = "Unexpected warnings:\n" ~ unexpectedWarnings.join("\n");
		throw new core.exception.AssertError(message, file, line);
	}
}

