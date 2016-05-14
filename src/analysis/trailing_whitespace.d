// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.trailing_whitespace;

import dparse.lexer;
import dparse.ast;
import analysis.base : BaseAnalyzer, Message;
import dsymbol.scope_ : Scope;

/**
Checks whether a file contains trailing whitespace
*/
class TrailingWhitespaceCheck : BaseAnalyzer
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
        findTrailingWS();
    }

	alias visit = ASTVisitor.visit;

private:

    /**
    Searches for trailing whitespace
    */
    void findTrailingWS()
    {
        import std.utf: byCodeUnit;
        import std.ascii: isWhite;
        import std.typecons: tuple;
        auto text = (cast(char[]) code).byCodeUnit;
        size_t line = 0;
        size_t column = 0;
        bool hasWhitespace;
        foreach (s; text)
        {
            if (s == '\n')
            {
                if (hasWhitespace)
                    addErrorMessage(line, column, KEY, MESSAGE);
                line++;
                column = 0;
            }
            else
            {
                if (isWhite(s))
                    hasWhitespace = true;
                else
                    hasWhitespace = false;
                column++;
            }
        }
    }

	enum string KEY = "dscanner.style.trailing_whitespace";
	enum string MESSAGE = "Trailing whitespace detected";
}

unittest
{
	import analysis.config : StaticAnalysisConfig;
    import analysis.helpers;
    import std.stdio;

	StaticAnalysisConfig sac;
	sac.trailing_whitespace_check = "enabled";

	auto msgs = getAnalyzerWarnings(q{
        void testTrailing()
        {
            a = 1;  
        }
	}c, sac);
    assert(msgs.length == 1);
    Message msg = Message("test", 3, 20, "dscanner.style.trailing_whitespace", "Trailing whitespace detected");
    assert(msgs.front == msg);

	stderr.writeln("Unittest for TrailingWhitespaceCheck passed.");
}
