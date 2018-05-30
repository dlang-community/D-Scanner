// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.allman;

import dparse.lexer;
import dparse.ast;
import dscanner.analysis.base : BaseAnalyzer;
import dsymbol.scope_ : Scope;

import std.algorithm;
import std.range;

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
final class AllManCheck : BaseAnalyzer
{
	///
	this(string fileName, const(Token)[] tokens, bool skipTests = false)
	{
		super(fileName, null, skipTests);
		foreach (i; 1 .. tokens.length - 1)
		{
			const curLine = tokens[i].line;
			const prevTokenLine = tokens[i-1].line;
			if (tokens[i].type == tok!"{" && curLine == prevTokenLine)
			{
				// ignore struct initialization
				if (tokens[i-1].type == tok!"=")
					continue;
				// ignore duplicate braces
				if (tokens[i-1].type == tok!"{" && tokens[i - 2].line != curLine)
					continue;
				// ignore inline { } braces
				if (curLine != tokens[i + 1].line)
					addErrorMessage(tokens[i].line, tokens[i].column, KEY, MESSAGE);
			}
			if (tokens[i].type == tok!"}" && curLine == prevTokenLine)
			{
				// ignore duplicate braces
				if (tokens[i-1].type == tok!"}" && tokens[i - 2].line != curLine)
					continue;
				// ignore inline { } braces
				if (!tokens[0 .. i].retro.until!(t => t.line != curLine).canFind!(t => t.type == tok!"{"))
					addErrorMessage(tokens[i].line, tokens[i].column, KEY, MESSAGE);
			}
		}
	}

	enum string KEY = "dscanner.style.allman";
	enum string MESSAGE = "Braces should be on their own line";
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;
	import std.format : format;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.allman_braces_check = Check.enabled;

	// check common allman style violation
	assertAnalyzerWarnings(q{
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
	assertAnalyzerWarnings(q{
unittest
{
	struct Foo { int a; }
	Foo foo = {
		a: 1;
	};
}
	}, sac);

	// allow duplicate braces
	assertAnalyzerWarnings(q{
unittest
{{
}}
	}, sac);


	stderr.writeln("Unittest for Allman passed.");
}
