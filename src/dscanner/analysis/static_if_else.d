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

	mixin AnalyzerInfo!"static_if_else_check";

	this(BaseAnalyzerArguments args)
	{
		super(args);
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
		auto tokens = ifStmt.tokens[0 .. 1];
		// extend one token to include `else` before this if
		tokens = (tokens.ptr - 1)[0 .. 2];
		addErrorMessage(tokens, KEY, "Mismatched static if. Use 'else static if' here.",
			[
				AutoFix.insertionBefore(tokens[$ - 1], "static "),
				AutoFix.resolveLater("Wrap '{}' block around 'if'", [tokens[0].index, ifStmt.tokens[$ - 1].index, 0])
			]);
	}

	const(IfStatement) getIfStatement(const ConditionalStatement cc)
	{
		return safeAccess(cc).falseStatement.statement.statementNoCaseNoDefault.ifStatement;
	}

	override AutoFix.CodeReplacement[] resolveAutoFix(
		const Module mod,
		scope const(Token)[] tokens,
		const AutoFix.ResolveContext context,
		const AutoFixFormatting formatting,
	)
	{
		import dscanner.analysis.helpers : getLineIndentation;
		import std.algorithm : countUntil;

		auto beforeElse = tokens.countUntil!(a => a.index == context.params[0]);
		auto lastToken = tokens.countUntil!(a => a.index == context.params[1]);
		if (beforeElse == -1 || lastToken == -1)
			throw new Exception("got different tokens than what was used to generate this autofix");

		auto indentation = getLineIndentation(tokens, tokens[beforeElse].line, formatting);

		string beforeIf = formatting.getWhitespaceBeforeOpeningBrace(indentation, false)
			~ "{" ~ formatting.eol ~ indentation;
		string afterIf = formatting.eol ~ indentation ~ "}";

		return AutoFix.replacement([tokens[beforeElse].index + 4, tokens[beforeElse + 1].index], beforeIf, "")
			.concat(AutoFix.indentLines(tokens[beforeElse + 1 .. lastToken + 1], formatting))
			.concat(AutoFix.insertionAfter(tokens[lastToken], afterIf))
			.expectReplacements;
	}

	enum KEY = "dscanner.suspicious.static_if_else";
}

unittest
{
	import dscanner.analysis.config : Check, disabledConfig, StaticAnalysisConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings, assertAutoFix;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.static_if_else_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void foo() {
			static if (false)
				auto a = 0;
			else if (true) /+
			^^^^^^^ [warn]: Mismatched static if. Use 'else static if' here. +/
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

	assertAutoFix(q{
		void foo() {
			static if (false)
				auto a = 0;
			else if (true) // fix:0
				auto b = 1;
		}
		void bar() {
			static if (false)
				auto a = 0;
			else if (true) // fix:1
				auto b = 1;
		}
		void baz() {
			static if (false)
				auto a = 0;
			else if (true) { // fix:1
				auto b = 1;
			}
		}
	}c, q{
		void foo() {
			static if (false)
				auto a = 0;
			else static if (true) // fix:0
				auto b = 1;
		}
		void bar() {
			static if (false)
				auto a = 0;
			else {
				if (true) // fix:1
					auto b = 1;
			}
		}
		void baz() {
			static if (false)
				auto a = 0;
			else {
				if (true) { // fix:1
					auto b = 1;
				}
			}
		}
	}c, sac);

	stderr.writeln("Unittest for StaticIfElse passed.");
}
