//          Copyright Brian Schott (Hackerpilot) 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.lambda_return_check;

import dparse.ast;
import dparse.lexer;
import analysis.base;

class LambdaReturnCheck : BaseAnalyzer
{
    alias visit = BaseAnalyzer.visit;

	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

    override void visit(const FunctionLiteralExpression fLit)
    {
        if (fLit.assignExpression is null)
            return;
        const UnaryExpression unary = cast(const UnaryExpression) fLit.assignExpression;
        if (unary is null)
            return;
        if (unary.primaryExpression is null)
            return;
        if (unary.primaryExpression.functionLiteralExpression is null)
            return;
        if (unary.primaryExpression.functionLiteralExpression.parameters !is null)
            return;
        if (unary.primaryExpression.functionLiteralExpression.identifier != tok!"")
            return;
        if (unary.primaryExpression.functionLiteralExpression.functionBody is null)
            return;
        if (unary.primaryExpression.functionLiteralExpression.functionBody.blockStatement is null)
            return;
        addErrorMessage(fLit.line, fLit.column, KEY, "This lambda returns a lambda. Add parenthesis to clarify.");
    }

private:
    enum KEY = "dscanner.confusing.lambda_returns_lambda";
}

unittest
{
	import analysis.helpers : assertAnalyzerWarnings;
	import analysis.config : StaticAnalysisConfig, Check;
	import std.stdio : stderr;

	StaticAnalysisConfig sac;
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
