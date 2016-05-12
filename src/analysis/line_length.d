//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.line_length;

import dparse.lexer;
import dparse.ast;
import analysis.base : BaseAnalyzer;

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
		foreach (token; tokens)
		{
			if (tokenEndColumn(token) > MAX_LINE_LENGTH && token.line != lastErrorLine)
			{
				addErrorMessage(token.line, token.column, KEY, MESSAGE);
				lastErrorLine = token.line;
			}
		}
	}

	alias visit = BaseAnalyzer.visit;

private:

	static ulong tokenEndColumn(ref const Token tok)
	{
		import std.uni : lineSep, paraSep;

		ulong endColumn = tok.column;
		foreach (dchar c; tok.text)
		{
			if (c == lineSep || c == '\n' || c == '\v' || c == '\r' || c == paraSep)
				endColumn = 0;
			else
				endColumn++;
		}
		return endColumn;
	}

	import std.conv : to;

	enum string KEY = "dscanner.style.long_line";
	enum string MESSAGE = "Line is longer than " ~ to!string(MAX_LINE_LENGTH) ~ " characters";
	enum MAX_LINE_LENGTH = 120;
	const(Token)[] tokens;
}
