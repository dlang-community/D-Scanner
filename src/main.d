//          Copyright Brian Schott (Hackerpilot) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module main;

import std.algorithm;
import std.array;
import std.conv;
import std.file;
import std.getopt;
import std.path;
import std.stdio;
import std.range;
import std.experimental.lexer;
import std.typecons : scoped;
import dparse.lexer;
import dparse.parser;
import dparse.rollback_allocator;

import highlighter;
import stats;
import ctags;
import etags;
import astprinter;
import imports;
import outliner;
import symbol_finder;
import analysis.run;
import analysis.config;
import dscanner_version;
import readers;

import inifiled;

import dsymbol.modulecache;

version (unittest)
	void main()
{
}
else
	int main(string[] args)
{
	bool sloc;
	bool highlight;
	bool ctags;
	bool etags;
	bool etagsAll;
	bool help;
	bool tokenCount;
	bool syntaxCheck;
	bool ast;
	bool imports;
	bool recursiveImports;
	bool muffin;
	bool outline;
	bool tokenDump;
	bool styleCheck;
	bool defaultConfig;
	bool report;
	bool skipTests;
	string symbolName;
	string configLocation;
	string[] importPaths;
	bool printVersion;
	bool explore;

	try
	{
		// dfmt off
		getopt(args, std.getopt.config.caseSensitive,
				"sloc|l", &sloc,
				"highlight", &highlight,
				"ctags|c", &ctags,
				"help|h", &help,
				"etags|e", &etags,
				"etagsAll", &etagsAll,
				"tokenCount|t", &tokenCount,
				"syntaxCheck|s", &syntaxCheck,
				"ast|xml", &ast,
				"imports|i", &imports,
				"recursiveImports", &recursiveImports,
				"outline|o", &outline,
				"tokenDump", &tokenDump,
				"styleCheck|S", &styleCheck,
				"defaultConfig", &defaultConfig,
				"declaration|d", &symbolName,
				"config", &configLocation,
				"report", &report,
				"I", &importPaths,
				"version", &printVersion,
				"muffinButton", &muffin,
				"explore", &explore,
				"skipTests", &skipTests);
		//dfmt on
	}
	catch (ConvException e)
	{
		stderr.writeln(e.msg);
		return 1;
	}

	if (muffin)
	{
		stdout.writeln(`       ___________
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

	if (explore)
	{
		stdout.writeln("D-Scanner: Scanning...");
		stderr.writeln("D-Scanner: No new astronomical objects discovered.");
		return 1;
	}

	if (help)
	{
		printHelp(args[0]);
		return 0;
	}

	if (printVersion)
	{
		version (Windows)
			writeln(DSCANNER_VERSION);
		else version (built_with_dub)
			writeln(DSCANNER_VERSION);
		else
			write(DSCANNER_VERSION, " ", GIT_HASH);
		return 0;
	}

	const(string[]) absImportPaths = importPaths.map!(a => a.absolutePath()
			.buildNormalizedPath()).array();

	auto alloc = scoped!(dsymbol.modulecache.ASTAllocator)();
	auto moduleCache = ModuleCache(alloc);

	if (absImportPaths.length)
		moduleCache.addImportPaths(absImportPaths);

	immutable optionCount = count!"a"([sloc, highlight, ctags, tokenCount, syntaxCheck, ast, imports,
			outline, tokenDump, styleCheck, defaultConfig, report,
			symbolName !is null, etags, etagsAll, recursiveImports]);
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

	// --report implies --styleCheck
	if (report)
		styleCheck = true;

	immutable usingStdin = args.length == 1;

	StringCache cache = StringCache(StringCache.defaultBucketCount);
	if (defaultConfig)
	{
		string s = getConfigurationLocation();
		mkdirRecurse(findSplitBefore(s, "dscanner.ini")[0]);
		StaticAnalysisConfig saConfig = defaultStaticAnalysisConfig();
		writeln("Writing default config file to ", s);
		writeINIFile(saConfig, s);
	}
	else if (tokenDump || highlight)
	{
		ubyte[] bytes = usingStdin ? readStdin() : readFile(args[1]);
		LexerConfig config;
		config.stringBehavior = StringBehavior.source;

		if (highlight)
		{
			auto tokens = byToken(bytes, config, &cache);
			highlighter.highlight(tokens, args.length == 1 ? "stdin" : args[1]);
			return 0;
		}
		else if (tokenDump)
		{
			auto tokens = getTokensForParser(bytes, config, &cache);
			writeln(
					"text                    \tblank\tindex\tline\tcolumn\ttype\tcomment\ttrailingComment");
			foreach (token; tokens)
			{
				writefln("<<%20s>>\t%b\t%d\t%d\t%d\t%d\t%s\t%s",
						token.text is null ? str(token.type) : token.text,
						token.text is null,
						token.index,
						token.line,
						token.column,
						token.type,
						token.comment,
						token.trailingComment);
			}
			return 0;
		}
	}
	else if (symbolName !is null)
	{
		stdout.findDeclarationOf(symbolName, expandArgs(args));
	}
	else if (ctags)
	{
		stdout.printCtags(expandArgs(args));
	}
	else if (etags || etagsAll)
	{
		stdout.printEtags(etagsAll, expandArgs(args));
	}
	else if (styleCheck)
	{
		StaticAnalysisConfig config = defaultStaticAnalysisConfig();
		string s = configLocation is null ? getConfigurationLocation() : configLocation;
		if (s.exists())
			readINIFile(config, s);
        if (skipTests)
            config.fillConfig!(Check.skipTests);
		if (report)
			generateReport(expandArgs(args), config, cache, moduleCache);
		else
			return analyze(expandArgs(args), config, cache, moduleCache, true) ? 1 : 0;
	}
	else if (syntaxCheck)
	{
		return .syntaxCheck(usingStdin ? ["stdin"] : expandArgs(args), cache, moduleCache) ? 1 : 0;
	}
	else
	{
		if (sloc || tokenCount)
		{
			if (usingStdin)
			{
				LexerConfig config;
				config.stringBehavior = StringBehavior.source;
				auto tokens = byToken(readStdin(), config, &cache);
				if (tokenCount)
					printTokenCount(stdout, "stdin", tokens);
				else
					printLineCount(stdout, "stdin", tokens);
			}
			else
			{
				ulong count;
				foreach (f; expandArgs(args))
				{

					LexerConfig config;
					config.stringBehavior = StringBehavior.source;
					auto tokens = byToken(readFile(f), config, &cache);
					if (tokenCount)
						count += printTokenCount(stdout, f, tokens);
					else
						count += printLineCount(stdout, f, tokens);
				}
				writefln("total:\t%d", count);
			}
		}
		else if (imports || recursiveImports)
		{
			printImports(usingStdin, args, importPaths, &cache, recursiveImports);
		}
		else if (ast || outline)
		{
			string fileName = usingStdin ? "stdin" : args[1];
			RollbackAllocator rba;
			LexerConfig config;
			config.fileName = fileName;
			config.stringBehavior = StringBehavior.source;
			auto tokens = getTokensForParser(usingStdin ? readStdin()
					: readFile(args[1]), config, &cache);
			auto mod = parseModule(tokens, fileName, &rba, &doNothing);

			if (ast)
			{
				auto printer = new XMLPrinter;
				printer.output = stdout;
				printer.visit(mod);
			}
			else if (outline)
			{
				auto outliner = new Outliner(stdout);
				outliner.visit(mod);
			}
		}
	}
	return 0;
}

void printHelp(string programName)
{
	stderr.writefln(`
    Usage: %s <options>

Options:
    --help, -h
        Prints this help message

    --version
        Prints the program version

    --sloc <file | directory>..., -l <file | directory>...
        Prints the number of logical lines of code in the given
        source files. If no files are specified, input is read from stdin.

    --tokenCount <file | directory>..., -t <file | directory>...
        Prints the number of tokens in the given source files. If no files are
        specified, input is read from stdin.

    --highlight <file>
        Syntax-highlight the given source file. The resulting HTML will be
        written to standard output. If no file is specified, input is read
        from stdin.

    --imports <file>, -i <file>
        Prints modules imported by the given source file. If no files are
        specified, input is read from stdin. Combine with "-I" arguments to
        resolve import locations.

    --recursive-imports <file>
        Similar to "--imports", but lists imports of imports recursively.

    --syntaxCheck <file>, -s <file>
        Lexes and parses sourceFile, printing the line and column number of any
        syntax errors to stdout. One error or warning is printed per line.
        If no files are specified, input is read from stdin. %1$s will exit with
        a status code of zero if no errors are found, 1 otherwise.

    --styleCheck|S <file | directory>..., <file | directory>...
        Lexes and parses sourceFiles, printing the line and column number of any
        static analysis check failures stdout. %1$s will exit with a status code
        of zero if no warnings or errors are found, 1 otherwise.

    --ctags <file | directory>..., -c <file | directory>...
        Generates ctags information from the given source code file. Note that
        ctags information requires a filename, so stdin cannot be used in place
        of a filename.

    --etags <file | directory>..., -e <file | directory>...
        Generates etags information from the given source code file. Note that
        etags information requires a filename, so stdin cannot be used in place
        of a filename.

    --etagsAll <file | directory>...
        Same as --etags except private and package declarations are tagged too.

    --ast <file> | --xml <file>
        Generates an XML representation of the source files abstract syntax
        tree. If no files are specified, input is read from stdin.

    --declaration <symbolName> <file | directory>...,
	-d <symbolName> <file | directory>...
        Find the location where symbolName is declared. This should be more
        accurate than "grep". Searches the given files and directories, or the
        current working directory if none are specified.

    --report <file | directory>...
        Generate a static analysis report in JSON format. Implies --styleCheck,
        however the exit code will still be zero if errors or warnings are
        found.

    --config <file>
        Use the given configuration file instead of the default located in
        $HOME/.config/dscanner/dscanner.ini

    --defaultConfig
        Generates a default configuration file for the static analysis checks,

    --skipTests
        Does not analyze in the unittests. Only works if --styleCheck.`,

    programName);
}

private void doNothing(string, size_t, size_t, string, bool)
{
}

private enum CONFIG_FILE_NAME = "dscanner.ini";
version (linux) version = useXDG;
version (BSD) version = useXDG;
version (FreeBSD) version = useXDG;
version (OSX) version = useXDG;

/**
 * Locates the default configuration file
 */
string getDefaultConfigurationLocation()
{
	version (useXDG)
	{
		import std.process : environment;

		string configDir = environment.get("XDG_CONFIG_HOME", null);
		if (configDir is null)
		{
			configDir = environment.get("HOME", null);
			if (configDir is null)
				throw new Exception("Both $XDG_CONFIG_HOME and $HOME are unset");
			configDir = buildPath(configDir, ".config", "dscanner", CONFIG_FILE_NAME);
		}
		else
			configDir = buildPath(configDir, "dscanner", CONFIG_FILE_NAME);
		return configDir;
	}
	else version (Windows)
		return CONFIG_FILE_NAME;
}

/**
 * Searches upwards from the CWD through the directory hierarchy
 */
string tryFindConfigurationLocation()
{
	auto path = pathSplitter(getcwd());
	string result;

	while (!path.empty)
	{
		result = buildPath(buildPath(path), CONFIG_FILE_NAME);

		if (exists(result))
			break;

		path.popBack();
	}

	if (path.empty)
		return null;

	return result;
}

/**
 * Tries to find a config file and returns the default one on failure
 */
string getConfigurationLocation()
{
	immutable config = tryFindConfigurationLocation();

	if (config !is null)
		return config;

	return getDefaultConfigurationLocation();
}
