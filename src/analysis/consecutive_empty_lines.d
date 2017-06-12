// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module analysis.consecutive_empty_lines;

import dparse.lexer;
import dparse.ast;
import analysis.base : BaseAnalyzer, Message;
import dsymbol.scope_ : Scope;

import std.algorithm;
import std.range;

/**
Checks whether a file contains two or more consecutive empty lines
*/
class ConsecutiveEmptyLinesCheck : BaseAnalyzer
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
			auto curLine = tokens[i].line;
			auto prevTokenLine = tokens[i-1].line;
			if (curLine >= prevTokenLine + 2)
			{
				addErrorMessage(tokens[i].line, tokens[i].column, KEY, MESSAGE);
			}
		}
    }

	enum string KEY = "dscanner.style.consecutive_empty_lines";
	enum string MESSAGE = "Consecutive empty lines detected";
}

unittest
{
	import analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import analysis.helpers;
	import std.stdio;

	StaticAnalysisConfig sac = disabledConfig();
	sac.consecutive_empty_lines = Check.enabled;

	// between functions
	auto msgs = getAnalyzerWarnings(q{
		void testConsecutiveEmptyLines(){}


		void foo(){}
	}c, sac);
	assert(msgs.length == 1);
	Message msg = Message("test", 5, 3, ConsecutiveEmptyLinesCheck.KEY, ConsecutiveEmptyLinesCheck.MESSAGE);
	assert(msgs.front == msg);

	// between functions (with comments)
	msgs = getAnalyzerWarnings(q{
		void testConsecutiveEmptyLines(){}

		///
		void foo(){}
	}c, sac);
	assert(msgs.length == 0);

	msgs = getAnalyzerWarnings(q{
		void testConsecutiveEmptyLines(){}


		///
		void foo(){}
	}c, sac);
	assert(msgs.length == 0);
	msgs.writeln;
	msg = Message("test", 5, 3, ConsecutiveEmptyLinesCheck.KEY, ConsecutiveEmptyLinesCheck.MESSAGE);
	assert(msgs.front == msg);


	// within a function
	msgs = getAnalyzerWarnings(q{
		void testConsecutiveEmptyLines()
		{
			int a;


			int b;
		}
	}c, sac);
	assert(msgs.length == 1);
	msg = Message("test", 7, 4, ConsecutiveEmptyLinesCheck.KEY, ConsecutiveEmptyLinesCheck.MESSAGE);
	assert(msgs.front == msg);

	stderr.writeln("Unittest for ConsecutiveEmptyLines passed.");
}
