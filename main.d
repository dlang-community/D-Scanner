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
import stdx.d.lexer;
import stdx.d.parser;

import highlighter;
import stats;
import ctags;
import astprinter;
import imports;

int main(string[] args)
{
	bool sloc;
	bool highlight;
	bool ctags;
	bool recursive;
	bool format;
	bool help;
	bool tokenCount;
	bool syntaxCheck;
	bool ast;
	bool imports;
	bool muffin;

	try
	{
		getopt(args, "sloc|l", &sloc, "highlight", &highlight,
			"ctags|c", &ctags, "recursive|r|R", &recursive, "help|h", &help,
			"tokenCount|t", &tokenCount, "syntaxCheck|s", &syntaxCheck,
			"ast|xml", &ast, "imports|i", &imports, "muffinButton", &muffin);
	}
	catch (Exception e)
	{
		stderr.writeln(e.msg);
	}

	if (muffin)
	{
		stdout.writeln(
`       ___________
    __(#*O 0** @%*)__
  _(%*o#*O%*0 #O#%##@)_
 (*#@%#o*@ #o%O*%@ #o #)
 \=====================/
  |I|I|I|I|I|I|I|I|I|I|
  |I|I|I|I|I|I|I|I|I|I|
  |I|I|I|I|I|I|I|I|I|I|
  |I|I|I|I|I|I|I|I|I|I|`);
		return 0;
	}

	if (help)
	{
		printHelp(args[0]);
		return 0;
	}

	auto optionCount = count!"a"([sloc, highlight, ctags, tokenCount,
		syntaxCheck, ast, imports]);
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
		ubyte[] buffer = uninitializedArray!(ubyte[])(to!size_t(f.size));
		highlighter.highlight(byToken(f.rawRead(buffer), config),
			args.length == 1 ? "stdin" : args[1]);
		return 0;
	}
	else if (ctags)
	{
		if (recursive)
		{
			stdout.printCtags(dirEntries(args[1], SpanMode.depth)
				.filter!(a => a.name.endsWith(".d") || a.name.endsWith(".di"))()
				.map!(a => a.name)().array());
		}
		else
			stdout.printCtags(args[1 .. $]);
	}
	else
	{
		LexerConfig config;

		bool usingStdin = args.length == 3;
		config.fileName = usingStdin ? "stdin" : args[1];
		File f = usingStdin ? stdin : File(args[1]);
		auto bytes = usingStdin ? cast(ubyte[]) [] : uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(bytes);

		auto tokens = byToken(bytes, config);
		if (sloc)
		{
			printLineCount(stdout, tokens);
		}
		else if (tokenCount)
		{
			printTokenCount(stdout, tokens, to!size_t(f.size));
		}
		else if (syntaxCheck)
		{
			parseModule(tokens.array(), config.fileName);
		}
		else if (imports)
		{
			auto mod = parseModule(tokens.array(), config.fileName);
			auto visitor = new ImportPrinter;
			visitor.visit(mod);
		}
		else if (ast)
		{
			auto mod = parseModule(tokens.array(), config.fileName);
			auto printer = new XMLPrinter;
			printer.output = stdout;
			printer.visit(mod);
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

    --sloc | -l [sourceFile]
        Prints the number of logical lines of code in the given
        source file. If no files are specified, a file is read from stdin.

    --tokenCount | t [sourceFile]
        Prints the number of tokens in the given source file.

    --highlight [sourceFile] - Syntax-highlight the given source file. The
        resulting HTML will be written to standard output.

    --imports | -i [sourceFile]
        Prints modules imported by the given source file.

    --syntaxCheck | -s [sourceFile]
        Lexes and parses sourceFile, printing the line and column number of any
        syntax errors to stdout. One error or warning is printed per line.

    --ctags | -c sourceFile
        Generates ctags information from the given source code file. Note that
        ctags information requires a filename, so stdin cannot be used in place
        of a filename.

    --ast | --xml sourceFile
        Generates an XML representation of the source files abstract syntax tree

    --recursive | -R | -r directory
        When used with --ctags, dscanner will produce ctags output for all .d
        and .di files contained within the given directory and its
        sub-directories.`,
        programName);
}
