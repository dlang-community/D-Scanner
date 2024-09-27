// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.helpers;

import core.exception : AssertError;
import std.stdio;
import std.string;
import std.traits;

import dparse.ast;
import dparse.lexer : tok, Token;
import dparse.rollback_allocator;
import dscanner.analysis.base;
import dscanner.analysis.config;
import dscanner.analysis.run;
import dsymbol.modulecache : ModuleCache;
import std.experimental.allocator;
import std.experimental.allocator.mallocator;

import dmd.parse : Parser;
import dmd.astbase : ASTBase;
import dmd.astcodegen;
import dmd.frontend;

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

string getLineIndentation(scope const(Token)[] tokens, size_t line, const AutoFixFormatting formatting)
{
	import std.algorithm : countUntil;
	import std.array : array;
	import std.range : repeat;
	import std.string : lastIndexOfAny;

	auto idx = tokens.countUntil!(a => a.line == line);
	if (idx == -1 || tokens[idx].column <= 1 || !formatting.indentation.length)
		return "";

	auto indent = tokens[idx].column - 1;
	if (formatting.indentation[0] == '\t')
		return (cast(immutable)'\t').repeat(indent).array;
	else
		return (cast(immutable)' ').repeat(indent).array;
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

/// EOL inside this project, for tests
private static immutable fileEol = q{
};

/**
 * This assert function will analyze the passed in code, get the warnings, and
 * apply all specified autofixes all at once.
 *
 * Indicate which autofix to apply by adding a line comment at the end of the
 * line with the following content: `// fix:0`, where 0 is the index which
 * autofix to apply. There may only be one diagnostic on a line with this fix
 * comment. Alternatively you can also just write `// fix` to apply the only
 * available suggestion.
 */
void assertAutoFix(string before, string after, const StaticAnalysisConfig config, bool useDmd = false,
		const AutoFixFormatting formattingConfig = AutoFixFormatting(AutoFixFormatting.BraceStyle.otbs, "\t", 4, fileEol),
		string file = __FILE__, size_t line = __LINE__)
{
	import dparse.lexer : StringCache, Token;
	import dscanner.analysis.autofix : improveAutoFixWhitespace;
	import dscanner.analysis.run : parseModule;
	import std.algorithm : canFind, findSplit, map, sort;
	import std.conv : to;
	import std.sumtype : match;
	import std.typecons : tuple, Tuple;
	import std.file : exists, remove;
	import std.path : dirName;
	import std.stdio : File;
	import dscanner.analysis.rundmd : analyzeDmd, parseDmdModule;
	import dscanner.utils : getModuleName;

	MessageSet rawWarnings;

	if (useDmd)
	{
		auto testFileName = "test.d";
		File f = File(testFileName, "w");
		scope(exit)
		{
			assert(exists(testFileName));
			remove(testFileName);
		}

		f.write(before);
		f.close();

		auto dmdModule = parseDmdModule(file, before);
		rawWarnings = analyzeDmd(testFileName, dmdModule, getModuleName(dmdModule.md), config);
	}
	else
	{
		StringCache cache = StringCache(StringCache.defaultBucketCount);
		RollbackAllocator r;
		const(Token)[] tokens;
		const(Module) m = parseModule(file, cast(ubyte[]) before, &r, defaultErrorFormat, cache, false, tokens);

		ModuleCache moduleCache;

		rawWarnings = analyze("test", m, config, moduleCache, tokens, true, true, formattingConfig);
	}

	string[] codeLines = before.splitLines();
	Tuple!(Message, int)[] toApply;
	int[] applyLines;

	scope (failure)
	{
		if (toApply.length)
			stderr.writefln("Would have applied these fixes:%(\n- %s%)",
				toApply.map!"a[0].autofixes[a[1]].name");
		else
			stderr.writeln("Did not find any fixes at all up to this point.");
		stderr.writeln("Found warnings on lines: ", rawWarnings[].map!(a
			=> a.endLine == 0 ? 0 : a.endLine - 1 + line));
	}

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

		auto fixComment = codeLines[rawLine - 1].findSplit("// fix");
		if (fixComment[1].length)
		{
			applyLines ~= cast(int)rawLine - 1;
			if (fixComment[2].startsWith(":"))
			{
				auto i = fixComment[2][1 .. $].to!int;
				assert(i >= 0, "can't use negative autofix indices");
				if (i >= rawWarning.autofixes.length)
					throw new AssertError("autofix index out of range, diagnostic only has %s autofixes (%s)."
						.format(rawWarning.autofixes.length, rawWarning.autofixes.map!"a.name"),
							file, rawLine + line);
				toApply ~= tuple(rawWarning, i);
			}
			else
			{
				if (rawWarning.autofixes.length != 1)
					throw new AssertError("diagnostic has %s autofixes (%s), but expected exactly one."
						.format(rawWarning.autofixes.length, rawWarning.autofixes.map!"a.name"),
							file, rawLine + line);
				toApply ~= tuple(rawWarning, 0);
			}
		}
	}

	foreach (i, codeLine; codeLines)
	{
		if (!applyLines.canFind(i) && codeLine.canFind("// fix"))
			throw new AssertError("Missing expected warning for autofix on line %s"
				.format(i + line), file, i + line);
	}

	AutoFix.CodeReplacement[] replacements;

	foreach_reverse (pair; toApply)
	{
		Message message = pair[0];
		AutoFix fix = message.autofixes[pair[1]];
		replacements ~= fix.expectReplacements;
	}

	replacements.sort!"a.range[0] < b.range[0]";

	improveAutoFixWhitespace(before, replacements);

	string newCode = before;
	foreach_reverse (replacement; replacements)
	{
		newCode = newCode[0 .. replacement.range[0]] ~ replacement.newText
			~ newCode[replacement.range[1] .. $];
	}

	if (newCode != after)
	{
		bool onlyWhitespaceDiffers = newCode.replace("\t", "").replace(" ", "")
			== after.replace("\t", "").replace(" ", "").replace("\r", "");

		string formatDisplay(string code)
		{
			string ret = code.lineSplitter!(KeepTerminator.yes).map!(a => "\t" ~ a).join;
			if (onlyWhitespaceDiffers)
				ret = ret
					.replace("\r", "\x1B[2m\\r\x1B[m")
					.replace("\t", "\x1B[2m→   \x1B[m")
					.replace(" ", "\x1B[2m⸱\x1B[m");
			return ret;
		}

		throw new AssertError("Applying autofix didn't yield expected results. Expected:\n"
			~ formatDisplay(after)
			~ "\n\nActual:\n"
			~ formatDisplay(newCode),
			file, line);
	}
}

void assertAnalyzerWarningsDMD(string code, const StaticAnalysisConfig config, bool semantic = false,
		string file = __FILE__, size_t line = __LINE__)
{
	import std.file : exists, remove;
	import std.path : dirName;
	import std.stdio : File;
	import dscanner.analysis.rundmd : analyzeDmd, parseDmdModule;
	import dscanner.utils : getModuleName;

	auto testFileName = "test.d";
	File f = File(testFileName, "w");
	scope(exit)
	{
		assert(exists(testFileName));
        remove(testFileName);
	}

	f.write(code);
	f.close();

	auto dmdModule = parseDmdModule(file, code);
	if (semantic)
		dmdModule.fullSemantic();

	MessageSet rawWarnings = analyzeDmd(testFileName, dmdModule, getModuleName(dmdModule.md), config);

	string[] codeLines = code.splitLines();

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
