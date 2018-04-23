// Copyright Chris Wright (dhasenan) 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.static_if_else;

import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import dscanner.utils : safeAccess;

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
final class StaticIfElse : BaseAnalyzer
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
		return safeAccess(cc).falseStatement.statement.statementNoCaseNoDefault.ifStatement;
	}

	enum KEY = "dscanner.suspicious.static_if_else";
}

unittest
{
	import dscanner.analysis.helpers : assertAnalyzerWarnings;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
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
