//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.del;

import dscanner.analysis.base;
import dscanner.analysis.source_range : SourceToken;
import dmd.tokens : TOK;

/**
 * Checks for use of the deprecated 'delete' keyword
 */
extern(C++) class DeleteCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"delete_check";

	private enum KEY = "dscanner.deprecated.delete_keyword";
	private enum MSG = "Avoid using the 'delete' keyword.";

	extern(D) this(string fileName)
	{
		super(fileName);
	}

	/**
	 * dmd no longer knows the `delete` keyword and reads `delete a;` as a
	 * declaration, so the statements are found in the source tokens.
	 */
	override void visit(AST.Module m)
	{
		import dscanner.analysis.source_range : tokensFrom;

		auto previous = SourceToken(TOK.semicolon);
		foreach (token; tokensFrom(source, 0))
		{
			if (startsStatement(previous.value) && source[token.start .. token.end] == "delete")
				reportDelete(token);
			previous = token;
		}
	}

	extern (D) private void reportDelete(SourceToken deleteToken)
	{
		import dscanner.analysis.source_range : tokensFrom;
		import std.range : drop;

		size_t depth, operandEnd = deleteToken.end;
		foreach (token; tokensFrom(source, deleteToken.start).drop(1))
		{
			if (token.value == TOK.semicolon && depth == 0)
				break;
			if (token.value == TOK.leftParenthesis || token.value == TOK.leftBracket)
				depth++;
			else if (token.value == TOK.rightParenthesis || token.value == TOK.rightBracket)
				depth--;
			operandEnd = token.end;
		}

		addErrorMessage([deleteToken.start, deleteToken.end], KEY, MSG,
			[AutoFix.replacement(deleteToken.start, deleteToken.end, `destroy(`, "Replace delete with destroy()")
				.concat(AutoFix.insertionAt(operandEnd, ")"))]);
	}
}

unittest
{
	import dscanner.analysis.config : Check, disabledConfig, StaticAnalysisConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD, assertAutoFix;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.delete_check = Check.enabled;
	// dmd no longer parses `delete`; `delete data[1]` is a syntax error, so only forms it accepts are used here
	assertAnalyzerWarningsDMD(q{
		void testDelete()
		{
			auto a = new Class();
			delete a; /+
			^^^^^^ [warn]: Avoid using the 'delete' keyword. +/

			int* p;
			if (p) delete *p; /+
			       ^^^^^^ [warn]: Avoid using the 'delete' keyword. +/

			int delete_;
			a.delete_();
		}
	}c, sac);

	assertAutoFix(q{
		void testDelete()
		{
			int[int] data = [1 : 2];
			delete data[1]; // fix

			auto a = new Class();
			delete a; // fix
		}
	}c, q{
		void testDelete()
		{
			int[int] data = [1 : 2];
			destroy(data[1]); // fix

			auto a = new Class();
			destroy(a); // fix
		}
	}c, sac);

	stderr.writeln("Unittest for DeleteCheck passed.");
}

private bool startsStatement(TOK previous)
{
	return previous == TOK.semicolon || previous == TOK.leftCurly || previous == TOK.rightCurly
		|| previous == TOK.rightParenthesis || previous == TOK.colon || previous == TOK.else_
		|| previous == TOK.do_;
}
