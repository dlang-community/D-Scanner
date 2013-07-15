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
import stats;
import ctags;

int main(string[] args)
{
	string[] importDirs;
	bool sloc;
	bool highlight;
	bool ctags;
    bool json;
	bool recursive;
	bool format;
	bool help;
	bool tokenCount;
	bool syntaxCheck;

	try
	{
		getopt(args, "I", &importDirs, "sloc|l", &sloc,
			"json|j", &json, "highlight", &highlight,
			"ctags|c", &ctags, "recursive|r|R", &recursive, "help|h", &help,
			"tokenCount", &tokenCount, "syntaxCheck", &syntaxCheck);
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
