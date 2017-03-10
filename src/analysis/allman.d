// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

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
	///
	this(string fileName, const(ubyte)[] code, bool skipTests = false)
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

	const(ubyte)[] code;

	enum string KEY = "dscanner.style.allman";
	enum string MESSAGE = "Braces should be on their own line";

	void checkForBrace(const DeclarationOrStatement declOrSt, size_t line, size_t column)
	{
		if(auto stst = declOrSt.statement)
		{
			checkForBrace(stst.statementNoCaseNoDefault, line, column);
		}
		declOrSt.accept(this);
	}

	void checkForBrace(const StatementNoCaseNoDefault st, size_t line, size_t column)
	{
		if(st !is null)
		{
			findBraceOrNewLine(st.startLocation, st.endLocation, line, column);
		}
	}

	/**
	Checks whether a brace or newline comes first
	*/
	void findBraceOrNewLine(size_t start, size_t end, size_t line, size_t column)
	{
		import std.algorithm : canFind;
		import std.utf : byCodeUnit;

		auto codeRange = (cast(char[]) code[start..end]).byCodeUnit;

		// inline statements are allowed -> search for newline
		if (codeRange.canFind('\n'))
		{
			foreach (s; codeRange)
			{
				// first brace
				if (s == '{')
				{
					// DoStatement hasn't a proper line and column attached
					// -> calculate ourselves
					if (line == 0 && column == 0)
					{
						// find line & column of brace
						auto t = findLineAndColumnForPos(start);
						line = t.line + 1; // Dscanner starts lines at 1
						column = t.column;
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

	/**
	Counts all matches of an symbol and the number of iterated characters
	*/
	auto findLineAndColumnForPos(size_t pos)
	{
		import std.utf : byCodeUnit;
		import std.typecons : tuple;

		auto textBefore = (cast(char[]) code[0..pos]).byCodeUnit;
		size_t line = 0;
		size_t column = 0;

		foreach (s; textBefore)
		{
			if (s == '\n')
			{
				line++;
				column = 0;
			}
			else if (s != '\r')
			{
				// ignore carriage return
				column++;
			}
		}
		return tuple!("line", "column")(line, column);
	}

}

unittest
{
	import analysis.config : StaticAnalysisConfig, Check;
	import analysis.helpers : assertAnalyzerWarnings;
	import std.format : format;
	import std.stdio : stderr;

	StaticAnalysisConfig sac;
	sac.allman_braces_check = Check.enabled;

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
	), sac);

	stderr.writeln("Unittest for Allman passed.");
}
