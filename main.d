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
import dpick.buffer.buffer;

import highlighter;
import stats;
import ctags;
import astprinter;
import imports;
import outliner;

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
	bool outline;

	try
	{
		getopt(args, "sloc|l", &sloc, "highlight", &highlight,
			"ctags|c", &ctags, "recursive|r|R", &recursive, "help|h", &help,
			"tokenCount|t", &tokenCount, "syntaxCheck|s", &syntaxCheck,
			"ast|xml", &ast, "imports|i", &imports, "outline|o", &outline,
			"muffinButton", &muffin);
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
		syntaxCheck, ast, imports, outline]);
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
		bool usingStdin = args.length == 1;
		ubyte[] bytes = usingStdin ? readStdin() : readFile(args[1]);
		auto tokens = byToken!(ubyte[], false, false)(bytes);
		highlighter.highlight(tokens, args.length == 1 ? "stdin" : args[1]);
		return 0;
	}
	else if (ctags)
	{
		stdout.printCtags(expandArgs(args, recursive));
	}
	else
	{
		bool usingStdin = args.length == 1;
		if (sloc || tokenCount)
		{
			if (usingStdin)
			{
				auto tokens = byToken!(ubyte[], false, false)(readStdin());
				if (tokenCount)
					printTokenCount(stdout, "stdin", tokens);
				else
					printLineCount(stdout, "stdin", tokens);
			}
			else
			{
				ulong count;
				foreach (f; expandArgs(args, recursive))
				{
					auto tokens = byToken!(ubyte[])(readFile(f));
					if (tokenCount)
						count += printTokenCount(stdout, f, tokens);
					else
						count += printLineCount(stdout, f, tokens);
				}
				writefln("total:\t%d", count);
			}
		}
		else if (syntaxCheck)
		{
			auto tokens = byToken(usingStdin ? readStdin() : readFile(args[1]));
			parseModule(tokens.array(), usingStdin ? "stdin" : args[1]);
		}
		else if (imports)
		{
			auto tokens = byToken(usingStdin ? readStdin() : readFile(args[1]));
			auto mod = parseModule(tokens.array(), usingStdin ? "stdin" : args[1]);
			auto visitor = new ImportPrinter;
			visitor.visit(mod);
		}
		else if (ast)
		{
			auto tokens = byToken(usingStdin ? readStdin() : readFile(args[1]));
			auto mod = parseModule(tokens.array(), usingStdin ? "stdin" : args[1]);
			auto printer = new XMLPrinter;
			printer.output = stdout;
			printer.visit(mod);
		}
		else if (outline)
		{
			auto tokens = byToken(usingStdin ? readStdin() : readFile(args[1]));
			auto mod = parseModule(tokens.array(), usingStdin ? "stdin" : args[1]);
			auto outliner = new Outliner(stdout);
			outliner.visit(mod);
		}
	}
	return 0;
}

string[] expandArgs(string[] args, bool recursive)
{
	if (recursive)
	{
		string[] rVal;
		foreach (arg; args[1 ..$])
		{
			if (isFile(arg) && arg.endsWith(`.d`) || arg.endsWith(`.di`))
				rVal ~= arg;
			else foreach (item; dirEntries(arg, SpanMode.breadth).map!(a => a.name))
			{
				if (isFile(item) && (item.endsWith(`.d`) || item.endsWith(`.di`)))
					rVal ~= item;
				else
					continue;
			}
		}
		return rVal;
	}
	else
		return args[1 .. $];
}

ubyte[] readStdin()
{
	auto sourceCode = appender!(ubyte[])();
	ubyte[4096] buf;
	while (true)
	{
		auto b = stdin.rawRead(buf);
		if (b.length == 0)
			break;
		sourceCode.put(b);
	}
	return sourceCode.data;
}

ubyte[] readFile(string fileName)
{
	if (!exists(fileName))
	{
		stderr.writefln("%s does not exist", fileName);
		return [];
	}
	File f = File(fileName);
	ubyte[] sourceCode = uninitializedArray!(ubyte[])(to!size_t(f.size));
	f.rawRead(sourceCode);
	return sourceCode;
}

void printHelp(string programName)
{
	stderr.writefln(
`
    Usage: %s options

options:
    --help | -h
        Prints this help message

    --sloc | -l [sourceFiles]
        Prints the number of logical lines of code in the given
        source files. If no files are specified, input is read from stdin.

    --tokenCount | t [sourceFiles]
        Prints the number of tokens in the given source files. If no files are
        specified, input is read from stdin.

    --highlight [sourceFile] - Syntax-highlight the given source file. The
        resulting HTML will be written to standard output. If no files are
        specified, input is read from stdin.

    --imports | -i [sourceFile]
        Prints modules imported by the given source file. If no files are
        specified, input is read from stdin.

    --syntaxCheck | -s [sourceFile]
        Lexes and parses sourceFile, printing the line and column number of any
        syntax errors to stdout. One error or warning is printed per line.
        If no files are specified, input is read from stdin.

    --ctags | -c sourceFile
        Generates ctags information from the given source code file. Note that
        ctags information requires a filename, so stdin cannot be used in place
        of a filename.

    --ast | --xml sourceFile
        Generates an XML representation of the source files abstract syntax
        tree. If no files are specified, input is read from stdin.

    --recursive | -R | -r
        When used with --ctags, --tokenCount, or --sloc, dscanner will produce
        ctags output for all .d and .di files contained within the given
        directories and its sub-directories.`,
        programName);
}
