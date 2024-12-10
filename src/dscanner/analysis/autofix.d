module dscanner.analysis.autofix;

import std.algorithm : filter, findSplit;
import std.conv : to;
import std.file : exists, remove;
import std.functional : toDelegate;
import std.stdio;

import dscanner.analysis.base : AutoFix, AutoFixFormatting, BaseAnalyzer, BaseAnalyzerDmd, Message;
import dscanner.analysis.config : StaticAnalysisConfig;
import dscanner.analysis.run : analyze, doNothing;
import dscanner.analysis.rundmd;
import dscanner.utils : getModuleName, readFile, readStdin;

void listAutofixes(
	StaticAnalysisConfig config,
	string resolveMessage,
	bool usingStdin,
	string fileName
)
{
	import std.format : format;
	import std.json : JSONValue;

	union RequestedLocation
	{
		struct
		{
			uint line, column;
		}
		ulong bytes;
	}

	RequestedLocation req;
	bool isBytes = resolveMessage[0] == 'b';
	if (isBytes)
		req.bytes = resolveMessage[1 .. $].to!ulong;
	else
	{
		auto parts = resolveMessage.findSplit(":");
		req.line = parts[0].to!uint;
		req.column = parts[2].to!uint;
	}

	bool matchesCursor(Message m)
	{
		return isBytes ? req.bytes >= m.startIndex && req.bytes <= m.endIndex
			: req.line >= m.startLine && req.line <= m.endLine
			&& (req.line > m.startLine || req.column >= m.startColumn)
			&& (req.line < m.endLine || req.column <= m.endColumn);
	}

	ubyte[] code;
	if (usingStdin)
	{
		code = readStdin();
		fileName = "stdin.d";
		File f = File(fileName, "w");
		f.rawWrite(code);
		f.close();
	}
	else
	{
		code = readFile(fileName);
	}

	auto dmdModule = parseDmdModule(fileName, cast(string) code);
	auto moduleName = getModuleName(dmdModule.md);
	auto messages = analyzeDmd(fileName, dmdModule, moduleName, config);

	with (stdout.lockingTextWriter)
	{
		put("[");
		foreach (message; messages[].filter!matchesCursor)
		{
			foreach (i, autofix; message.autofixes)
			{
				put(i == 0 ? "\n" : ",\n");
				put("\t{\n");
				put(format!"\t\t\"name\": %s,\n"(JSONValue(autofix.name)));
				put("\t\t\"replacements\": [");
				foreach (j, replacement; autofix.expectReplacements)
				{
					put(j == 0 ? "\n" : ",\n");
					put(format!"\t\t\t{\"range\": [%d, %d], \"newText\": %s}"(
						replacement.range[0],
						replacement.range[1],
						JSONValue(replacement.newText)));
				}
				put("\n");
				put("\t\t]\n");
				put("\t}");
			}
		}
		put("\n]");
	}
	stdout.flush();

	if (usingStdin)
	{
		assert(exists(fileName));
		remove(fileName);
	}
}

void improveAutoFixWhitespace(scope const(char)[] code, AutoFix.CodeReplacement[] replacements)
{
	import std.algorithm : endsWith, startsWith;
	import std.ascii : isWhite;
	import std.string : strip;
	import std.utf : stride, strideBack;

	enum WS
	{
		none, tab, space, newline
	}

	WS getWS(size_t i)
	{
		if (cast(ptrdiff_t) i < 0 || i >= code.length)
			return WS.newline;
		switch (code[i])
		{
			case '\n':
			case '\r':
				return WS.newline;
			case '\t':
				return WS.tab;
			case ' ':
				return WS.space;
			default:
				return WS.none;
		}
	}

	foreach (ref replacement; replacements)
	{
		assert(replacement.range[0] >= 0 && replacement.range[0] < code.length
		&& replacement.range[1] >= 0 && replacement.range[1] < code.length
		&& replacement.range[0] <= replacement.range[1],
			"trying to autofix whitespace on code that doesn't match with what the replacements were generated for");

		void growRight()
		{
			// this is basically: replacement.range[1]++;
			if (code[replacement.range[1] .. $].startsWith("\r\n"))
				replacement.range[1] += 2;
			else if (replacement.range[1] < code.length)
				replacement.range[1] += code.stride(replacement.range[1]);
		}

		void growLeft()
		{
			// this is basically: replacement.range[0]--;
			if (code[0 .. replacement.range[0]].endsWith("\r\n"))
				replacement.range[0] -= 2;
			else if (replacement.range[0] > 0)
				replacement.range[0] -= code.strideBack(replacement.range[0]);
		}

		if (replacement.newText.strip.length)
		{
			if (replacement.newText.startsWith(" "))
			{
				// we insert with leading space, but there is a space/NL/SOF before
				// remove to-be-inserted space
				if (getWS(replacement.range[0] - 1))
					replacement.newText = replacement.newText[1 .. $];
			}
			if (replacement.newText.startsWith("]", ")"))
			{
				// when inserting `)`, consume regular space before
				if (getWS(replacement.range[0] - 1) == WS.space)
					growLeft();
			}
			if (replacement.newText.endsWith(" "))
			{
				// we insert with trailing space, but there is a space/NL/EOF after, chomp off
				if (getWS(replacement.range[1]))
					replacement.newText = replacement.newText[0 .. $ - 1];
			}
			if (replacement.newText.endsWith("[", "("))
			{
				if (getWS(replacement.range[1]))
					growRight();
			}
		}
		else if (!replacement.newText.length)
		{
			// after removing code and ending up with whitespace on both sides,
			// collapse 2 whitespace into one
			switch (getWS(replacement.range[1]))
			{
				case WS.newline:
					switch (getWS(replacement.range[0] - 1))
					{
						case WS.newline:
						// after removal we have NL ~ NL or SOF ~ NL,
						// remove right NL
							growRight();
							break;
						case WS.space:
						case WS.tab:
						// after removal we have space ~ NL,
						// remove the space
							growLeft();
							break;
						default:
							break;
					}
					break;
				case WS.space:
				case WS.tab:
				// for NL ~ space, SOF ~ space, space ~ space, tab ~ space,
				// for NL ~ tab, SOF ~ tab, space ~ tab, tab ~ tab
				// remove right space/tab
					if (getWS(replacement.range[0] - 1))
						growRight();
					break;
				default:
					break;
			}
		}
	}
}

unittest
{
	import std.algorithm : sort;

	AutoFix.CodeReplacement r(int start, int end, string s)
	{
		return AutoFix.CodeReplacement([start, end], s);
	}

	string test(string code, AutoFix.CodeReplacement[] replacements...)
	{
		replacements.sort!"a.range[0] < b.range[0]";
		improveAutoFixWhitespace(code, replacements);
		foreach_reverse (r; replacements)
			code = code[0 .. r.range[0]] ~ r.newText ~ code[r.range[1] .. $];
		return code;
	}

	assert(test("import a;\nimport b;", r(0, 9, "")) == "import b;");
	assert(test("import a;\r\nimport b;", r(0, 9, "")) == "import b;");
	assert(test("import a;\nimport b;", r(8, 9, "")) == "import a\nimport b;");
	assert(test("import a;\nimport b;", r(7, 8, "")) == "import ;\nimport b;");
	assert(test("import a;\r\nimport b;", r(7, 8, "")) == "import ;\r\nimport b;");
	assert(test("a b c", r(2, 3, "")) == "a c");
}
