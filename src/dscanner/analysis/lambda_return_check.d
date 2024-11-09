//          Copyright Brian Schott (Hackerpilot) 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.lambda_return_check;

import dscanner.analysis.base;
import dmd.tokens : Token, TOK;

extern (C++) class LambdaReturnCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"lambda_return_check";

	private enum KEY = "dscanner.confusing.lambda_returns_lambda";
	private enum MSG = "This lambda returns a lambda. Add parenthesis to clarify.";

	private Token[] tokens;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
		lexFile();
	}

	private void lexFile()
	{
		import dscanner.utils : readFile;
		import dmd.errorsink : ErrorSinkNull;
		import dmd.globals : global;
		import dmd.lexer : Lexer;

		auto bytes = readFile(fileName) ~ '\0';
		__gshared ErrorSinkNull errorSinkNull;
		if (!errorSinkNull)
			errorSinkNull = new ErrorSinkNull;

		scope lexer = new Lexer(null, cast(char*) bytes, 0, bytes.length, 0, 0, errorSinkNull, &global.compileEnv);
		while (lexer.nextToken() != TOK.endOfFile)
			tokens ~= lexer.token;
	}

	override void visit(AST.FuncLiteralDeclaration lambda)
	{
		import std.algorithm.iteration : filter;
		import std.algorithm.searching : canFind, find, until;

		super.visit(lambda);

		if (lambda.fbody.isReturnStatement() is null)
			return;

		auto lambdaRange = tokens.filter!(t => t.loc.fileOffset > lambda.loc.fileOffset)
			.filter!(t => t.loc.fileOffset < lambda.endloc.fileOffset);
		auto tokenRange = lambdaRange.find!(t => t.value == TOK.goesTo);

		if (!tokenRange.canFind!(t => t.value == TOK.leftCurly))
			return;

		if (!tokenRange.canFind!(t => t.value == TOK.leftParenthesis))
		{
			auto start = tokenRange.front.loc.fileOffset;
			AutoFix fix0;
			auto firstParam = (*(lambda.getParameterList().parameters))[0];

			if (hasParensOnParams((*(lambda.getParameterList().parameters))[0]))
				fix0 = AutoFix.replacement(start, start + 3, "", "Remove arrow (use function body)");
			else
				fix0 = AutoFix.insertionAt(firstParam.loc.fileOffset, "(")
					.concat(AutoFix.insertionAt(start - 1, ")"))
					.concat(AutoFix.replacement(start, start + 3, "", "Remove arrow (use function body)"));

			addErrorMessage(
				cast(ulong) lambda.loc.linnum, cast(ulong) lambda.loc.charnum, KEY, MSG,
				[fix0, AutoFix.insertionAt(start + 2, " ()")]
			);
		}
	}

	private bool hasParensOnParams(AST.Parameter param)
	{
		int idx;

		foreach (token; tokens)
		{
			if (token.loc.fileOffset == param.loc.fileOffset)
				break;
			idx++;
		}

		return tokens[idx - 1].value == TOK.leftParenthesis && tokens[idx - 2].value == TOK.leftParenthesis;
	}
}

unittest
{
	import dscanner.analysis.config : Check, disabledConfig, StaticAnalysisConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD, assertAutoFix;
	import std.stdio : stderr;
	import std.format : format;

	StaticAnalysisConfig sac = disabledConfig();
	sac.lambda_return_check = Check.enabled;
	auto msg = "This lambda returns a lambda. Add parenthesis to clarify.";

	assertAnalyzerWarningsDMD(q{
		void main()
		{
			int[] b;
			auto a = b.map!(a => { return a * a + 2; }).array(); // [warn]: %s
			pragma(msg, typeof(a => { return a; })); // [warn]: %s
			pragma(msg, typeof((a) => { return a; })); // [warn]: %s
			pragma(msg, typeof({ return a; }));
			pragma(msg, typeof(a => () { return a; }));
			b.map!(a => a * 2);
		}
	}c.format(msg, msg, msg), sac);

	assertAutoFix(q{
		void main()
		{
			int[] b;
			auto a = b.map!(a => { return a * a + 2; }).array(); // fix:0
			auto a = b.map!(a => { return a * a + 2; }).array(); // fix:1
			pragma(msg, typeof(a => { return a; })); // fix:0
			pragma(msg, typeof(a => { return a; })); // fix:1
			pragma(msg, typeof((a) => { return a; })); // fix:0
			pragma(msg, typeof((a) => { return a; })); // fix:1
		}
	}c, q{
		void main()
		{
			int[] b;
			auto a = b.map!((a) { return a * a + 2; }).array(); // fix:0
			auto a = b.map!(a => () { return a * a + 2; }).array(); // fix:1
			pragma(msg, typeof((a) { return a; })); // fix:0
			pragma(msg, typeof(a => () { return a; })); // fix:1
			pragma(msg, typeof((a) { return a; })); // fix:0
			pragma(msg, typeof((a) => () { return a; })); // fix:1
		}
	}c, sac, true);

	stderr.writeln("Unittest for LambdaReturnCheck passed.");
}
