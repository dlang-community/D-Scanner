
//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module main;


import std.file;
import std.stdio;
import std.algorithm;
import std.conv;
import std.array;
import std.path;
import std.regex;
import std.getopt;
import std.parallelism;
import types;
import tokenizer;
import parser;
import langutils;
import autocomplete;
import highlighter;

pure bool isLineOfCode(TokenType t)
{
	switch(t)
	{
	case TokenType.Semicolon:
	case TokenType.While:
	case TokenType.If:
	case TokenType.For:
	case TokenType.Foreach:
	case TokenType.Foreach_reverse:
	case TokenType.Case:
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

/**
 * Returns: the absolute path of the given module, or null if it could not be
 *     found.
 */
string findAbsPath(string[] dirs, string moduleName)
{
	// For file names
	if (endsWith(moduleName, ".d") || endsWith(moduleName, ".di"))
	{
		if (isAbsolute(moduleName))
			return moduleName;
		else
			return buildPath(getcwd(), moduleName);
	}

	// Try to find the file name from a module name like "std.stdio"
	foreach(dir; dirs)
	{
		string fileLocation = buildPath(dir, replace(moduleName, ".", dirSeparator));
		string dfile = fileLocation ~ ".d";
		if (exists(dfile) && isFile(dfile))
		{
			return dfile;
		}
		if (exists(fileLocation  ~ ".di") && isFile(fileLocation  ~ ".di"))
		{
			return fileLocation ~ ".di";
		}
	}
	stderr.writeln("Could not locate import ", moduleName, " in ", dirs);
	return null;
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

void main(string[] args)
{
	string[] importDirs;
	bool sloc;
	bool dotComplete;
	bool json;
	bool parenComplete;
	bool highlight;
	bool ctags;
	bool recursiveCtags;
	getopt(args, "I", &importDirs, "dotComplete", &dotComplete, "sloc", &sloc,
		"json", &json, "parenComplete", &parenComplete, "highlight", &highlight,
		"ctags", &ctags, "recursive|r|R", &recursiveCtags);

	importDirs ~= loadConfig();

	if (sloc)
	{
		writeln(args[1..$].map!(a => a.readText().tokenize())().joiner()
			.count!(a => isLineOfCode(a.type))());
		return;
	}

	if (highlight)
	{
		highlighter.highlight(args[1].readText().tokenize(IterationStyle.EVERYTHING));
		return;
	}

	if (dotComplete || parenComplete)
	{
		if (isAbsolute(args[1]))
			importDirs ~= dirName(args[1]);
		else
			importDirs ~= getcwd();
		auto tokens = args[1].readText().tokenize();
		auto mod = parseModule(tokens);
		CompletionContext context = new CompletionContext(mod);
		context.importDirectories = importDirs;
		foreach (im; parallel(mod.imports))
		{
			auto p = findAbsPath(importDirs, im);
			if (p is null || !p.exists())
				continue;
			context.addModule(p.readText().tokenize().parseModule());
		}
		auto complete = AutoComplete(tokens, context);
		if (parenComplete)
			writeln(complete.parenComplete(to!size_t(args[2])));
		else if (dotComplete)
			writeln(complete.dotComplete(to!size_t(args[2])));
		return;
	}

	if (json || ctags)
	{

		if (json)
		{
			auto tokens = tokenize(readText(args[1]));
			auto mod = parseModule(tokens);
			mod.writeJSONTo(stdout);
		}
		else
		{
			if (!recursiveCtags)
			{
				auto tokens = tokenize(readText(args[1]));
				auto mod = parseModule(tokens);
				mod.writeCtagsTo(stdout, args[1]);
			}
			else
			{
				Module m;
				foreach (dirEntry; dirEntries(args[1], SpanMode.breadth))
				{
					if (!dirEntry.name.endsWith(".d", ".di"))
						continue;
					stderr.writeln("Generating tags for ", dirEntry.name);
					auto tokens = tokenize(readText(dirEntry.name));
					if (m is null)
						m = parseModule(tokens);
					else
					{
						auto mod = parseModule(tokens);
						m.merge(mod);
					}
				}
				m.writeCtagsTo(stdout, "");
			}
		}
	}
}
