//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.line_length;

import analysis.base : BaseAnalyzer;

import dparse.ast;
import dparse.lexer;

import std.typecons : tuple, Tuple;

/**
 * Checks for lines longer than 120 characters
 */
class LineLengthCheck : BaseAnalyzer
{
	///
	this(string fileName, const(Token)[] tokens, bool skipTests = false)
	{
		super(fileName, null, skipTests);
		this.tokens = tokens;
	}

	override void visit(const Module)
	{
		ulong lastErrorLine = ulong.max;
		size_t endColumn = 0;
		foreach (i, token; tokens)
		{
			immutable info = tokenLength(token, i > 0 ? tokens[i - 1].line : 0);
			if (info[1])
				endColumn = info[0] + token.column;
			else
			{
				immutable wsChange = i > 0
						? token.column - (tokens[i - 1].column + tokenByteLength(tokens[i - 1]))
						: 0;
				endColumn += wsChange + info[0];
			}
			if (endColumn > MAX_LINE_LENGTH && token.line != lastErrorLine)
			{
				addErrorMessage(token.line, token.column, KEY, MESSAGE);
				lastErrorLine = token.line;
			}
		}
	}

	alias visit = BaseAnalyzer.visit;

private:

	static size_t tokenByteLength(ref const Token tok)
	{
		return tok.text is null ? str(tok.type).length : tok.text.length;
	}

	static Tuple!(size_t, bool) tokenLength(ref const Token tok, size_t prevLine)
	{
		import std.uni : lineSep, paraSep;

		size_t endColumn = 0;
		if (tok.text is null)
			endColumn += str(tok.type).length;
		else
			foreach (dchar c; tok.text)
			{
				if (c == lineSep || c == '\n' || c == '\v' || c == '\r' || c == paraSep)
					endColumn = 0;
				else
					endColumn++;
			}
		return tuple(endColumn, tok.line > prevLine);
	}

	import std.conv : to;

	enum string KEY = "dscanner.style.long_line";
	enum string MESSAGE = "Line is longer than " ~ to!string(MAX_LINE_LENGTH) ~ " characters";
	enum MAX_LINE_LENGTH = 120;
	const(Token)[] tokens;
}

@system unittest
{
	import analysis.config : Check, StaticAnalysisConfig;
	import analysis.helpers : assertAnalyzerWarnings;
	import std.stdio : stderr;

	StaticAnalysisConfig sac;
	sac.long_line_check = Check.enabled;
	assertAnalyzerWarnings(q{
Window window = Platform.instance.createWindow("Дистанционное управление сварочным оборудованием", null);
	}c, sac);

	stderr.writeln("Unittest for LineLengthCheck passed.");
}
