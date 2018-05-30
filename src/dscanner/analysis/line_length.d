//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.line_length;

import dscanner.analysis.base : BaseAnalyzer;

import dparse.ast;
import dparse.lexer;

import std.typecons : tuple, Tuple;

/**
 * Checks for lines longer than 120 characters
 */
final class LineLengthCheck : BaseAnalyzer
{
	///
	this(string fileName, const(Token)[] tokens, bool skipTests = false)
	{
		super(fileName, null, skipTests);
		this.tokens = tokens;
	}

	override void visit(const Module)
	{
		size_t endColumn;
		lastErrorLine = ulong.max;
		foreach (i, token; tokens)
		{
			immutable info = tokenLength(token, i > 0 ? tokens[i - 1].line : 0);
			if (info.multiLine)
				endColumn = checkMultiLineToken(token, endColumn);
			else if (info.newLine)
				endColumn = info.length + token.column - 1;
			else
			{
				immutable wsChange = i > 0
						? token.column - (tokens[i - 1].column + tokenByteLength(tokens[i - 1]))
						: 0;
				endColumn += wsChange + info.length;
			}
			if (endColumn > MAX_LINE_LENGTH)
				triggerError(token);
		}
	}

	alias visit = BaseAnalyzer.visit;

private:

	ulong lastErrorLine = ulong.max;

	void triggerError(ref const Token tok)
	{
		if (tok.line != lastErrorLine)
		{
			addErrorMessage(tok.line, tok.column, KEY, MESSAGE);
			lastErrorLine = tok.line;
		}
	}

	static bool isLineSeparator(dchar c)
	{
		import std.uni : lineSep, paraSep;
		return c == lineSep || c == '\n' || c == '\v' || c == '\r' || c == paraSep;
	}

	size_t checkMultiLineToken()(auto ref const Token tok, size_t startColumn = 0)
	{
		import std.utf : byDchar;

		auto col = startColumn;
		foreach (c; tok.text.byDchar)
		{
			if (isLineSeparator(c))
			{
				if (col > MAX_LINE_LENGTH)
					triggerError(tok);
				col = 1;
			}
			else
				col += getEditorLength(c);
		}
		return col;
	}

	unittest
	{
		assert(new LineLengthCheck(null, null).checkMultiLineToken(Token(tok!"stringLiteral", "		", 0, 0, 0)) == 8);
		assert(new LineLengthCheck(null, null).checkMultiLineToken(Token(tok!"stringLiteral", "		\na", 0, 0, 0)) == 2);
		assert(new LineLengthCheck(null, null).checkMultiLineToken(Token(tok!"stringLiteral", "		\n	", 0, 0, 0)) == 5);
	}

	static size_t tokenByteLength()(auto ref const Token tok)
	{
		return tok.text is null ? str(tok.type).length : tok.text.length;
	}

	unittest
	{
		assert(tokenByteLength(Token(tok!"stringLiteral", "aaa", 0, 0, 0)) == 3);
		assert(tokenByteLength(Token(tok!"stringLiteral", "Дистан", 0, 0, 0)) == 12);
		// tabs and whitespace
		assert(tokenByteLength(Token(tok!"stringLiteral", "	", 0, 0, 0)) == 1);
		assert(tokenByteLength(Token(tok!"stringLiteral", "    ", 0, 0, 0)) == 4);
	}

	// D Style defines tabs to have a width of four spaces
	static size_t getEditorLength(C)(C c)
	{
		if (c == '\t')
			return 4;
		else
			return 1;
	}

	alias TokenLength = Tuple!(size_t, "length", bool, "newLine", bool, "multiLine");
	static TokenLength tokenLength()(auto ref const Token tok, size_t prevLine)
	{
		import std.utf : byDchar;

		size_t length;
		const newLine = tok.line > prevLine;
		bool multiLine;
		if (tok.text is null)
			length += str(tok.type).length;
		else
			foreach (c; tok.text.byDchar)
			{
				if (isLineSeparator(c))
				{
					length = 1;
					multiLine = true;
				}
				else
					length += getEditorLength(c);
			}

		return TokenLength(length, newLine, multiLine);
	}

	unittest
	{
		assert(tokenLength(Token(tok!"stringLiteral", "aaa", 0, 0, 0), 0).length == 3);
		assert(tokenLength(Token(tok!"stringLiteral", "Дистан", 0, 0, 0), 0).length == 6);
		// tabs and whitespace
		assert(tokenLength(Token(tok!"stringLiteral", "	", 0, 0, 0), 0).length == 4);
		assert(tokenLength(Token(tok!"stringLiteral", "    ", 0, 0, 0), 0).length == 4);
	}

	import std.conv : to;

	enum string KEY = "dscanner.style.long_line";
	enum string MESSAGE = "Line is longer than " ~ to!string(MAX_LINE_LENGTH) ~ " characters";
	enum MAX_LINE_LENGTH = 120;
	const(Token)[] tokens;
}

@system unittest
{
	import dscanner.analysis.config : Check, StaticAnalysisConfig, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.long_line_check = Check.enabled;

	assertAnalyzerWarnings(q{
Window window = Platform.instance.createWindow("Дистанционное управление сварочным оборудованием			   ", null);
Window window = Platform.instance.createWindow("Дистанционное управление сварочным оборудованием				", null); // [warn]: Line is longer than 120 characters
unittest {
// with tabs
assert("foo" == "foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo1");
assert("foo" == "fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo2"); // [warn]: Line is longer than 120 characters
// with whitespace (don't overwrite)
    assert("foo" == "boooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo3");
    assert("foo" == "booooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo4"); // [warn]: Line is longer than 120 characters
}
	}c, sac);

// TODO: libdparse counts columns bytewise
	//assert("foo" == "boooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo5");
	//assert("foo" == "booooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo6"); // [warn]: Line is longer than 120 characters

	// reduced from std/regex/internal/thompson.d
	assertAnalyzerWarnings(q{
			// whitespace on purpose, do not remove!
            mixin(`case IR.`~e~`:
                    opCacheTrue[pc] = &Ops!(true).op!(IR.`~e~`);
                    opCacheBackTrue[pc] = &BackOps!(true).op!(IR.`~e~`);
                `);
                                                                               mixin(`case IR.`~e~`:
                                                                            opCacheTrue[pc] = &Ops!(true).op!(IR.`~e~`);
                                                                            opCacheTrue[pc] = &Ops!(true).op!(IR.`~e~`);
                                                                     opCacheBackTrue[pc] = &BackOps!(true).op!(IR.`~e~`); // [warn]: Line is longer than 120 characters
                `);
	}c, sac);

	stderr.writeln("Unittest for LineLengthCheck passed.");
}
