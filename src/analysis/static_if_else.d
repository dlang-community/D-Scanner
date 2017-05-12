// Copyright Chris Wright (dhasenan) 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.static_if_else;

import dparse.ast;
import dparse.lexer;
import analysis.base;

/**
 * Checks for potentially mistaken static if / else if.
 *
 * It's potentially valid to write:
 * ---
 * static if (foo) {
 * } else if (bar) {
 * }
 * ---
 * 
 * However, it's more likely that this is a mistake.
 */
class StaticIfElse : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	override void visit(const ConditionalStatement cc)
	{
		cc.accept(this);
		if (cc.falseStatement is null)
		{
			return;
		}
		const(IfStatement) ifStmt = getIfStatement(cc);
		if (!ifStmt)
		{
			return;
		}
		addErrorMessage(ifStmt.line, ifStmt.column, KEY, "Mismatched static if. Use 'else static if' here.");
	}

	const(IfStatement) getIfStatement(const ConditionalStatement cc)
	{
		if (cc.falseStatement.statement)
		{
			if (cc.falseStatement.statement.statementNoCaseNoDefault)
			{
				if (cc.falseStatement.statement.statementNoCaseNoDefault.ifStatement)
				{
					return cc.falseStatement.statement.statementNoCaseNoDefault.ifStatement;
				}
			}
		}
		return null;
	}

	enum KEY = "dscanner.suspicious.static_if_else";
}

unittest
{
	import analysis.helpers : assertAnalyzerWarnings;
	import analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.static_if_else_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void foo() {
			static if (false)
				auto a = 0;
			else if (true) // [warn]: Mismatched static if. Use 'else static if' here.
				auto b = 1;
		}
	}c, sac);
	// Explicit braces, so no warning.
	assertAnalyzerWarnings(q{
		void foo() {
			static if (false)
				auto a = 0;
			else {
				if (true)
					auto b = 1;
			}
		}
	}c, sac);

	stderr.writeln("Unittest for StaticIfElse passed.");
}
