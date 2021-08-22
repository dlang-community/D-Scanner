//          Copyright Brian Schott (Hackerpilot) 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.lambda_return_check;

import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import dscanner.utils : safeAccess;

final class LambdaReturnCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	mixin AnalyzerInfo!"lambda_return_check";

	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	override void visit(const FunctionLiteralExpression fLit)
	{
		auto fe = safeAccess(fLit).assignExpression.as!UnaryExpression
			.primaryExpression.functionLiteralExpression.unwrap;

		if (fe is null || fe.parameters !is null || fe.identifier != tok!"" ||
			fe.specifiedFunctionBody is null || fe.specifiedFunctionBody.blockStatement is null)
		{
			return;
		}
		addErrorMessage(fLit.line, fLit.column, KEY, "This lambda returns a lambda. Add parenthesis to clarify.");
	}

private:
	enum KEY = "dscanner.confusing.lambda_returns_lambda";
}

version(Windows) {/*because of newline in code*/} else
unittest
{
	import dscanner.analysis.helpers : assertAnalyzerWarnings;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.lambda_return_check = Check.enabled;

	auto code = `
		void main()
		{
			int[] b;
			auto a = b.map!(a => { return a * a + 2; }).array(); // [warn]: This lambda returns a lambda. Add parenthesis to clarify.
			pragma(msg, typeof(a => { return a; })); // [warn]: This lambda returns a lambda. Add parenthesis to clarify.
			pragma(msg, typeof((a) => { return a; })); // [warn]: This lambda returns a lambda. Add parenthesis to clarify.
			pragma(msg, typeof({ return a; }));
			pragma(msg, typeof(a => () { return a; }));
		}`c;
	assertAnalyzerWarnings(code, sac);
	stderr.writeln("Unittest for LambdaReturnCheck passed.");
}
