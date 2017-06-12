// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module analysis.trailing_whitespace;

import dparse.lexer;
import dparse.ast;
import analysis.base : BaseAnalyzer, Message;
import dsymbol.scope_ : Scope;

import std.algorithm;
import std.range;

/**
Checks for trailing whitespace
*/
class TrailingWhitespaceCheck : BaseAnalyzer
{
	///
	this(string fileName, const(Token)[] tokens, bool skipTests = false)
	{
		super(fileName, null, skipTests);
		import std.stdio;
		foreach (t; tokens)
			writeln("type", t.type.str,  " line: ", t.line);

		foreach (i; 1 .. tokens.length)
		{
			addErrorMessage(tokens[i].line, tokens[i].column, KEY, MESSAGE);
		}
    }

	enum string KEY = "dscanner.style.trailing_whitespace";
	enum string MESSAGE = "Trailing whitespace detected.";
}

unittest
{
	import analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import analysis.helpers;
	import std.stdio;

	StaticAnalysisConfig sac = disabledConfig();
	sac.trailing_whitespace = Check.enabled;

	// between functions
	auto msgs = getAnalyzerWarnings(q{
		void testConsecutiveEmptyLines(){}


		void foo(){}
	}c, sac);
	assert(msgs.length == 1);
	Message msg = Message("test", 5, 3, TrailingWhitespaceCheck.KEY, TrailingWhitespaceCheck.MESSAGE);
	assert(msgs.front == msg);

	stderr.writeln("Unittest for TrailingWhitespaceCheck passed.");
}
