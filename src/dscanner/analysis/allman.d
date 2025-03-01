// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.allman;

import dscanner.analysis.base;
import dmd.tokens : Token, TOK;
import std.algorithm : canFind, until;
import std.range : retro;

/**
Checks for the allman style (braces should be on their own line)
------------
if (param < 0) {
}
------------
should be
------------
if (param < 0)
{
}
------------
*/
extern (C++) class AllManCheck : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"allman_braces_check";

	private enum string KEY = "dscanner.style.allman";
	private enum string MESSAGE = "Braces should be on their own line";

	private Token[] tokens;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
		lexFile();
		checkBraces();
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

		scope lexer = new Lexer(null, cast(char*) bytes, 0, bytes.length, 0, 0,  errorSinkNull, &global.compileEnv);

		do
		{
			lexer.nextToken();
			tokens ~= lexer.token;
		}
		while (lexer.token.value != TOK.endOfFile);
	}

	private void checkBraces()
	{
		foreach (i; 1 .. tokens.length - 1)
		{
			const curLine = tokens[i].loc.linnum;
			const prevTokenLine = tokens[i - 1].loc.linnum;

			if (tokens[i].value == TOK.leftCurly && curLine == prevTokenLine)
			{
				// ignore struct initialization
				if (tokens[i - 1].value == TOK.assign)
					continue;

				// ignore duplicate braces
				if (tokens[i - 1].value == TOK.leftCurly && tokens[i - 2].loc.linnum != curLine)
					continue;

				// ignore inline { } braces
				if (curLine != tokens[i + 1].loc.linnum)
					addErrorMessage(cast(ulong) tokens[i].loc.linnum, cast(ulong) tokens[i].loc.charnum, KEY, MESSAGE);
			}

			if (tokens[i].value == TOK.rightCurly && curLine == prevTokenLine)
			{
				// ignore duplicate braces
				if (tokens[i-1].value == TOK.rightCurly && tokens[i - 2].loc.linnum != curLine)
					continue;

				// ignore inline { } braces
				if (!tokens[0 .. i].retro.until!(t => t.loc.linnum != curLine).canFind!(t => t.value == TOK.leftCurly))
					addErrorMessage(cast(ulong) tokens[i].loc.linnum, cast(ulong) tokens[i].loc.charnum, KEY, MESSAGE);
			}
		}
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import std.format : format;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.allman_braces_check = Check.enabled;

	// check common allman style violation
	assertAnalyzerWarningsDMD(q{
		void testAllman()
		{
			while (true) { // [warn]: %s
				auto f = 1;
			}

			do { // [warn]: %s
				auto f = 1;
			} while (true);

			// inline braces are OK
			while (true) { auto f = 1; }

			if (true) { // [warn]: %s
				auto f = 1;
			}
			if (true)
			{
				auto f = 1; } // [warn]: %s
			if (true) { auto f = 1; }
			foreach (r; [1]) { // [warn]: %s
			}
			foreach (r; [1]) {	}
			foreach_reverse (r; [1]) { // [warn]: %s
			}
			foreach_reverse (r; [1]) {	}
			for (int i = 0; i < 10; i++) { // [warn]: %s
			}
			for (int i = 0; i < 10; i++) { }

			// nested check
			while (true) { // [warn]: %s
				while (true) { // [warn]: %s
					auto f = 1;
				}
			}
		}
	}c.format(
		AllManCheck.MESSAGE,
		AllManCheck.MESSAGE,
		AllManCheck.MESSAGE,
		AllManCheck.MESSAGE,
		AllManCheck.MESSAGE,
		AllManCheck.MESSAGE,
		AllManCheck.MESSAGE,
		AllManCheck.MESSAGE,
		AllManCheck.MESSAGE,
	), sac);

	// check struct initialization
	assertAnalyzerWarningsDMD(q{
unittest
{
	struct Foo { int a; }
	Foo foo = {
		a: 1;
	};
}
	}, sac);

	// allow duplicate braces
	assertAnalyzerWarningsDMD(q{
unittest
{{
}}
	}, sac);

	stderr.writeln("Unittest for Allman passed.");
}
