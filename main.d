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
import std.d.parser;

import highlighter;
import autocomplete;
import stats;
import ctags;

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
	bool parenComplete;
	bool symbolComplete;
	bool highlight;
	bool ctags;
    bool json;
    bool declaration;
	bool recursive;
	bool format;
	bool help;
	bool tokenCount;
	bool syntaxCheck;

	try
	{
		getopt(args, "I", &importDirs, "dotComplete|d", &dotComplete, "sloc|l", &sloc,
			"json|j", &json, "parenComplete|p", &parenComplete, "highlight", &highlight,
			"ctags|c", &ctags, "recursive|r|R", &recursive, "help|h", &help,
			"tokenCount", &tokenCount, "syntaxCheck", &syntaxCheck,
            "declaration|e", &declaration, "symbolComplete|s", &symbolComplete);
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

    auto optionCount = count!"a"([sloc, highlight, ctags, json, tokenCount,
		syntaxCheck]);
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

    if (highlight)
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
	else
	{
		LexerConfig config;

		bool usingStdin = args.length == 3;
		config.fileName = usingStdin ? "stdin" : args[1];
		File f = usingStdin ? stdin : File(args[1]);
		auto bytes = usingStdin ? cast(ubyte[]) [] : uninitializedArray!(ubyte[])(f.size);
		auto byteCount = f.size;
		f.rawRead(bytes);

		auto tokens = byToken(bytes, config);
		if (sloc)
		{
			printLineCount(stdout, tokens);
		}
		else if (tokenCount)
		{
			printTokenCount(stdout, tokens, f.size);
		}
		else if (dotComplete || parenComplete || symbolComplete)
		{
			auto app = appender!(Token[])();
			app.reserve(byteCount / 13);
			while (!tokens.empty)
				app.put(tokens.moveFront());
			Token[] tokenArr = app.data;
			if (dotComplete)
			{
			}
			else if (parenComplete)
			{
			}
			else if (symbolComplete)
			{
			}
		}
		else if (ctags)
		{
			printCtags(stdout, tokens, args[1]);
		}
		else if (syntaxCheck)
		{
			parseModule(tokens.array(), args[1]);
		}
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

    --declaration | -e [sourceFile] cursorPosition
        Prints the absolute path to the file in which the symbol at the cursor
        position was declared, as well as its line number.

    --highlight [sourceFile] - Syntax-highlight the given source file. The
        resulting HTML will be written to standard output.

    --imports | -i [sourceFiles]
        Prints modules imported by the given source file.

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
