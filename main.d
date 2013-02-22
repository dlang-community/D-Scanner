//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module main;


import std.algorithm;
import std.array;
import std.conv;
import std.file;
import std.getopt;
import std.parallelism;
import std.path;
import std.regex;
import std.stdio;
import std.range;
import std.d.lexer;

import highlighter;

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

/**
 * Loads any import directories specified in /etc/dmd.conf.
 * Bugs: Only works on Linux
 * Returns: the paths specified as -I options in /etc/dmd.conf
 */
string[] loadDefaultImports()
{
version(linux)
{
	string path = "/etc/dmd.conf";
	if (!exists(path))
		return [];
	string[] rVal;
	auto file = File(path, "r");
	foreach(char[] line; file.byLine())
	{
		if (!line.startsWith("DFLAGS"))
			continue;
		while ((line = line.find("-I")).length > 0)
		{
			auto end = std.string.indexOf(line, " ");
			auto importDir = line[2 .. end].idup;
			rVal ~= importDir;
			line = line[end .. $];
		}
	}
	return rVal;
}
else
{
	return [];
}
}


string[] loadConfig()
{
	string path = expandTilde("~" ~ dirSeparator ~ ".dscanner");
	string[] dirs;
	if (exists(path))
	{
		auto f = File(path, "r");
		scope(exit) f.close();

		auto trimRegex = ctRegex!(`\s*$`);
		foreach(string line; lines(f))
		{
			dirs ~= replace(line, trimRegex, "");
		}
	}
	foreach(string importDir; loadDefaultImports()) {
		dirs ~= importDir;
	}
	return dirs;
}

int main(string[] args)
{
	string[] importDirs;
	bool sloc;
	bool dotComplete;
	bool json;
	bool parenComplete;
	bool highlight;
	bool ctags;
	bool recursiveCtags;
	bool format;
	bool help;
	bool tokenCount;
    bool frequencyCount;

	try
	{
		getopt(args, "I", &importDirs, "dotComplete|d", &dotComplete, "sloc|l", &sloc,
			"json|j", &json, "parenComplete|p", &parenComplete, "highlight", &highlight,
			"ctags|c", &ctags, "recursive|r|R", &recursiveCtags, "help|h", &help,
			"tokenCount", &tokenCount, "frequencyCount", &frequencyCount);
	}
	catch (Exception e)
	{
		stderr.writeln(e.msg);
	}

    if (help)
    {
        printHelp(args[0]);
        return 0;
    }

    auto optionCount = count!"a"([sloc, highlight, ctags, json, tokenCount]);
    if (optionCount > 1)
    {
        stderr.writeln("Too many options specified");
        return 1;
    }
    else if (optionCount < 1)
    {
        printHelp(args[0]);
        return 1;
    }

	if (tokenCount || sloc)
	{
		LexerConfig config;
        config.tokenStyle = TokenStyle.doNotReplaceSpecial;
		ulong[] counts = new ulong[args.length - 1];
		foreach (i, arg; parallel(args[1..$]))
		{
			config.fileName = arg;
			uint count;
            auto f = File(arg);
            import core.stdc.stdlib;
            ubyte[] buffer = (cast(ubyte*)malloc(f.size))[0..f.size];
            scope(exit) free(buffer.ptr);
            //uninitializedArray!(ubyte[])(f.size);
			foreach (t; byToken(f.rawRead(buffer), config))
            {
                if (tokenCount)
                    ++counts[i];
                else if (isLineOfCode(t.type))
                    ++counts[i];
            }

		}
		foreach(i; 0 .. counts.length)
			writefln("%s: %d", args[i + 1], counts[i]);
	}
    else if (highlight)
	{
		LexerConfig config;
		config.iterStyle = IterationStyle.everything;
		config.tokenStyle = TokenStyle.source;
        File f = args.length == 1 ? stdin : File(args[1]);
        ubyte[] buffer = uninitializedArray!(ubyte[])(f.size);
        highlighter.highlight(byToken(f.rawRead(buffer), config),
			args.length == 1 ? "stdin" : args[1]);
		return 0;
	}

	return 0;
}

void printHelp(string programName)
{
	writefln(
`
    Usage: %s options

options:
    --help | -h
        Prints this help message

    --sloc | -l [sourceFiles]
        count the number of logical lines of code in the given
        source files. If no files are specified, a file is read from stdin.

    --json | -j [sourceFile]
        Generate a JSON summary of the given source file. If no file is
        specifed, the file is read from stdin.

    --dotComplete | -d [sourceFile] cursorPosition
        Provide autocompletion for the insertion of the dot operator. The cursor
        position is the character position in the *file*, not the position in
        the line. If no file is specified, the file is read from stdin.

    --parenComplete | -p [sourceFile] cursorPosition
        Provide a listing of function parameters or pre-defined version
        identifiers at the cursor position. The cursor position is the character
        position in the *file*, not the line. If no file is specified, the
        contents are read from stdin.

    --symbolComplete | -s [sourceFile] cursorPosition
        Provide a listing of classes, structs, interfaces, variables, functions,
        and methods available in the current scope that begin with the text
        before the cursor position.

    --highlight [sourceFile] - Syntax-highlight the given source file. The
        resulting HTML will be written to standard output.

    -I includePath
        Include _includePath_ in the list of paths used to search for imports.
        By default dscanner will search in the current working directory as
        well as any paths specified in /etc/dmd.conf. This is only used for the
        --parenComplete and --dotComplete options.

    --ctags | -c sourceFile
        Generates ctags information from the given source code file. Note that
        ctags information requires a filename, so stdin cannot be used in place
        of a filename.

    --recursive | -R | -r directory
        When used with --ctags, dscanner will produce ctags output for all .d
        and .di files contained within directory and its sub-directories.`,
        programName);
}
