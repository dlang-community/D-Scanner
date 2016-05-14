// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.allman;

import dparse.lexer;
import dparse.ast;
import analysis.base : BaseAnalyzer;
import dsymbol.scope_ : Scope;

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
class AllManCheck : BaseAnalyzer
{
    ubyte[] code;

    ///
	this(string fileName, ubyte[] code, bool skipTests = false)
	{
		super(fileName, null, skipTests);
		this.code = code;
    }

	override void visit(const WhileStatement st)
	{
        if (st.declarationOrStatement !is null)
            checkForBrace(st.declarationOrStatement, st.expression.line, st.expression.column);
    }

    override void visit(const ForeachStatement st)
    {
        checkForBrace(st.declarationOrStatement, st.low.line, st.low.column);
    }

    override void visit(const ForStatement st)
    {
        checkForBrace(st.declarationOrStatement, st.test.line, st.test.column);
    }

    override void visit(const DoStatement st)
    {
        // the DoStatement only knows about the line and column of the expression
        checkForBrace(st.statementNoCaseNoDefault, 0, 0);
        st.statementNoCaseNoDefault.accept(this);
    }

	override void visit(const IfStatement st)
	{
        checkForBrace(st.thenStatement, st.expression.line, st.expression.column);
        if (st.elseStatement !is null)
            checkForBrace(st.elseStatement, st.expression.line, st.expression.column);
    }

	alias visit = ASTVisitor.visit;

private:

    void checkForBrace(const DeclarationOrStatement declOrSt, size_t line, size_t column)
    {
        if(auto stst = declOrSt.statement)
        {
            checkForBrace(stst.statementNoCaseNoDefault, line, column);
        }
        declOrSt.accept(this);
    }

    void checkForBrace(const StatementNoCaseNoDefault stNo, size_t line, size_t column)
    {
        if(stNo !is null)
        {
            findBraceOrNewLine(stNo.startLocation, stNo.endLocation, line, column);
        }
    }

    /**
    Counts all matches of an symbol and the number of iterated characters
    */
    auto findLast(size_t start, char symbol)
    {
        import std.utf: byCodeUnit;
        import std.typecons: tuple;
        auto text_before = (cast(char[]) code[0..start]).byCodeUnit;
        size_t offset = 0;
        size_t matches = 0;
        foreach (s; text_before)
        {
            if (s == symbol)
            {
                matches++;
            }
            offset++;
        }
        return tuple!("matches", "offset")(matches, offset);
    }

    /**
    Checks whether a brace or newline comes first
    */
    void findBraceOrNewLine(size_t start, size_t end, size_t line, size_t column)
    {
        import std.stdio;
		import std.algorithm : canFind;
        import std.utf: byCodeUnit;
        auto stRange = (cast(char[]) code[start..end]).byCodeUnit;
        // inline statements are allowed - search for newline
        size_t charsToBrace = 0;
        if (canFind(stRange, '\n'))
        {
            foreach (s; stRange)
            {
                // brace first
                if (s == '{')
                {
                    // DoStatement doesn't has proper line and column attched
                    // -> calulcate ourselves
                    if (line == 0 && column == 0)
                    {
                        // find line & column of brace
                        auto t = findLast(start, '\n');
                        line = t.matches + 1;
                        column = (start + charsToBrace) - t.offset;
                    }
                    addErrorMessage(line, column, KEY, MESSAGE);
                    break;
                }
                // newline - test passed
                else if (s == '\n')
                {
                    break;
                }
            }
        }
    }

	enum string KEY = "dscanner.style.allman";
	enum string MESSAGE = "Brace should be on their own line";
}

unittest
{
	import analysis.config : StaticAnalysisConfig;
    import analysis.helpers;
    import std.stdio;

	StaticAnalysisConfig sac;
	sac.allman_braces_check = "enabled";

	assertAnalyzerWarnings(q{
        void testAllman()
        {
            while (true) { // [warn]: Brace should be on their own line
                auto f = 1;
            }

		    do { // [warn]: Brace should be on their own line
                auto f = 1;
            } while (true);

            // inline is OK
            while (true) { auto f = 1; }

            if (true) { // [warn]: Brace should be on their own line
                auto f = 1;
            }
            if (true) { auto f = 1; }
            foreach (r; [1]) { // [warn]: Brace should be on their own line
            }
            foreach (r; [1]) {  }
            foreach_reverse (r; [1]) { // [warn]: Brace should be on their own line
            }
            foreach_reverse (r; [1]) {  }
            for (int i = 0; i < 10; i++) { // [warn]: Brace should be on their own line
            }
            for (int i = 0; i < 10; i++) { }

            // nested check
            while (true) { // [warn]: Brace should be on their own line
                while (true) { // [warn]: Brace should be on their own line
                    auto f = 1;
                }
            }
        }
	}c, sac);

	stderr.writeln("Unittest for Allman passed.");
}
