//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.line_length;

import dscanner.analysis.base;
import dmd.tokens : Token, TOK;

/**
 * Checks for lines longer than `max_line_length` characters
 */
extern (C++) class LineLengthCheck : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"long_line_check";
	private enum KEY = "dscanner.style.long_line";
	immutable string msg;

	private Token[] tokens;
	private immutable int maxLineLength;
	private uint currentLine = 1;
	private int currentLineLen;

	extern (D) this(string fileName, bool skipTests = false, int maxLineLength = 120)
	{
		import std.conv : to;

		super(fileName, skipTests);
		this.maxLineLength = maxLineLength;
		msg = "Line is longer than " ~ to!string(maxLineLength) ~ " characters";

		lexFile();
		checkFile();
	}

	private void lexFile()
	{
		import dscanner.utils : readFile;
		import dmd.errorsink : ErrorSinkNull;
		import dmd.globals : global;
		import dmd.lexer : Lexer;

		auto bytes = readFile(fileName) ~ '\0';

		__gshared ErrorSinkNull errorSinkNull;
		if (!errorSinkNull)
			errorSinkNull = new ErrorSinkNull;

		scope lexer = new Lexer(null, cast(char*) bytes, 0, bytes.length, true,
				true, true, errorSinkNull, &global.compileEnv);

		while (lexer.token.value != TOK.endOfFile)
		{
			lexer.nextToken();
			tokens ~= lexer.token;
		}
	}

	private void checkFile()
	{
		import std.conv : to;

		foreach (i, token; tokens)
		{
			switch (token.value)
			{
			case TOK.whitespace:
				switch (token.ptr[0])
				{
				case '\t':
					currentLineLen += 4;
					break;
				case '\r':
					break;
				case '\n', '\v':
					checkCurrentLineLength();
					break;
				default:
					for (auto p = token.ptr; *p == ' '; p++)
						currentLineLen++;
				}
				break;
			case TOK.comment:
				if (i == tokens.length - 1)
					skipComment(to!string(token.ptr));
				else
					skipComment(token.ptr[0 .. tokens[i + 1].ptr - token.ptr]);
				break;
			case TOK.string_:
				if (i == tokens.length - 1)
					checkStringLiteral(to!string(token.ptr));
				else
					checkStringLiteral(token.ptr[0 .. tokens[i + 1].ptr - token.ptr]);
				break;
			default:
				currentLineLen += token.toString().length;
			}
		}
	}

	private extern (D) void skipComment(const(char)[] commentStr)
	{
		import std.utf : byDchar;

		foreach (dchar c; commentStr.byDchar)
			if (c == '\n' || c == '\v')
				checkCurrentLineLength();
	}

	private extern (D) void checkStringLiteral(const(char)[] str)
	{
		import std.utf : byDchar;

		foreach (dchar c; str.byDchar)
		{
			if (c == '\t')
				currentLineLen += 4;
			else if (c == '\n' || c == '\v')
				checkCurrentLineLength();
			else if (c != '\r')
				currentLineLen++;
		}
	}

	void checkCurrentLineLength()
	{
		if (currentLineLen > maxLineLength)
			addErrorMessage(cast(ulong) currentLine, 0uL, KEY, msg);

		currentLine++;
		currentLineLen = 0;
	}
}

@system unittest
{
	import dscanner.analysis.config : Check, StaticAnalysisConfig, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.long_line_check = Check.enabled;

	assertAnalyzerWarningsDMD(q{
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

	assertAnalyzerWarningsDMD(q{
	assert("foo" == "boooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo5");
	assert("foo" == "booooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo6"); // [warn]: Line is longer than 120 characters
	}c, sac);

	// reduced from std/regex/internal/thompson.d
	assertAnalyzerWarningsDMD(q{
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

	// Test customizing max_line_length.
	sac.max_line_length = 115;
	assertAnalyzerWarningsDMD(q{
Window window = Platform.instance.createWindow("Дистанционное управлсварочным оборудованием			   ", null);
Window window = Platform.instance.createWindow("Дистанционное управлсварочным оборудованием				", null); // [warn]: Line is longer than 115 characters
unittest {
// with tabs
assert("foo" == "fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo1");
assert("foo" == "foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo2"); // [warn]: Line is longer than 115 characters
// with whitespace (don't overwrite)
    assert("foo" == "booooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo3");
    assert("foo" == "boooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo4"); // [warn]: Line is longer than 115 characters
}
	}c, sac);

	stderr.writeln("Unittest for LineLengthCheck passed.");
}
