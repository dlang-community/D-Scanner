// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.consecutive_empty_lines;

import dparse.lexer;
import dparse.ast;
import analysis.base : BaseAnalyzer, Message;
import dsymbol.scope_ : Scope;

/**
Checks whether a file contains two or more consecutive empty lines
*/
class ConsecutiveEmptyLines: BaseAnalyzer
{
    ubyte[] code;

    ///
	this(string fileName, ubyte[] code, bool skipTests = false)
	{
		super(fileName, null, skipTests);
		this.code = code;
    }

	override void visit(const Module)
    {
        findConsecutive();
    }

	alias visit = ASTVisitor.visit;

private:

    /**
    Searches for trailing whitespace
    */
    void findConsecutive()
    {
        import std.utf: byCodeUnit;
        import std.ascii: isWhite;
        size_t line = 0;
        size_t newLineCount = 0;
        foreach (s; (cast(char[]) code).byCodeUnit)
        {
            if (s == '\n')
            {
                if (newLineCount >= 2)
                    addErrorMessage(line, 0, KEY, MESSAGE);
                line++;
                newLineCount++;
            }
            // ignore carriage returns for windows compatibility
            else if (!(s == '\r' || isWhite(s)))
            {
                newLineCount = 0;
            }
        }
    }

	enum string KEY = "dscanner.style.consecutive_empty_lines";
	enum string MESSAGE = "Consecutive empty lines detected";
}

unittest
{
	import analysis.config : StaticAnalysisConfig;
    import analysis.helpers;
    import std.stdio;

	StaticAnalysisConfig sac;
	sac.consecutive_empty_lines = "enabled";

	auto msgs = getAnalyzerWarnings(q{
        void testConsecutiveEmptyLines(){


        }

        void foo(){

        }
	}c, sac);
    assert(msgs.length == 1);
    Message msg = Message("test", 3, 0, "dscanner.style.consecutive_empty_lines",
                          "Consecutive empty lines detected");
    assert(msgs.front == msg);

	stderr.writeln("Unittest for ConsecutiveEmptyLines passed.");
}
