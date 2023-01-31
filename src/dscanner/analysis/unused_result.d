//          Copyright Vladimir Panteleev 2020
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.unused_result;

import dscanner.analysis.base;
import dscanner.analysis.mismatched_args : resolveSymbol, IdentVisitor;
import dscanner.utils;
import dsymbol.scope_;
import dsymbol.symbol;
import dparse.ast, dparse.lexer;
import std.algorithm.searching : canFind;
import std.range: retro;

/**
 * Checks for function call statements which call non-void functions.
 *
 * In case the function returns a value indicating success/failure,
 * ignoring this return value and continuing execution can lead to
 * undesired results.
 *
 * When the return value is intentionally discarded, `cast(void)` can
 * be prepended to silence the check.
 */
final class UnusedResultChecker : BaseAnalyzer
{
    alias visit = BaseAnalyzer.visit;

    mixin AnalyzerInfo!"unused_result";

private:

    enum string KEY = "dscanner.unused_result";
    enum string MSG = "Function return value is discarded";

public:

    const(DSymbol)* void_;
    const(DSymbol)* noreturn_;

    ///
    this(string fileName, const(Scope)* sc, bool skipTests = false)
    {
        super(fileName, sc, skipTests);
        void_ = sc.getSymbolsByName(internString("void"))[0];
        auto symbols = sc.getSymbolsByName(internString("noreturn"));
        if (symbols.length > 0)
            noreturn_ = symbols[0];
    }

    override void visit(const(ExpressionStatement) decl)
    {
        import std.typecons : scoped;

        super.visit(decl);
        if (!decl.expression)
            return;
        if (decl.expression.items.length != 1)
            return;
        auto ue = cast(UnaryExpression) decl.expression.items[0];
        if (!ue)
            return;
        auto fce = ue.functionCallExpression;
        if (!fce)
            return;

        auto identVisitor = scoped!IdentVisitor;
        if (fce.unaryExpression !is null)
            identVisitor.visit(fce.unaryExpression);
        else if (fce.type !is null)
            identVisitor.visit(fce.type);

        if (!identVisitor.names.length)
            return;

        const(DSymbol)*[] symbols = resolveSymbol(sc, identVisitor.names);

        if (!symbols.length)
            return;

        foreach (sym; symbols)
        {
            if (!sym)
                return;
            if (!sym.type)
                return;
            if (sym.kind != CompletionKind.functionName)
                return;
            if (sym.type is void_)
                return;
            if (noreturn_ && sym.type is noreturn_)
                return;
        }

        addErrorMessage(decl.expression.line, decl.expression.column, KEY, MSG);
    }
}

unittest
{
    import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
    import dscanner.analysis.helpers : assertAnalyzerWarnings;
    import std.stdio : stderr;
    import std.format : format;

    StaticAnalysisConfig sac = disabledConfig();
    sac.unused_result = Check.enabled;

    assertAnalyzerWarnings(q{
        void fun() {}
        void main()
        {
            fun();
        }
    }c, sac);

    assertAnalyzerWarnings(q{
        alias noreturn = typeof(*null);
        noreturn fun() { while (1) {} }
        noreturn main()
        {
            fun();
        }
    }c, sac);

    assertAnalyzerWarnings(q{
        int fun() { return 1; }
        void main()
        {
            fun(); // [warn]: %s
        }
    }c.format(UnusedResultChecker.MSG), sac);

    assertAnalyzerWarnings(q{
        void main()
        {
            void fun() {}
            fun();
        }
    }c, sac);

    version (none) // TODO: local functions
    assertAnalyzerWarnings(q{
        void main()
        {
            int fun() { return 1; }
            fun(); // [warn]: %s
        }
    }c.format(UnusedResultChecker.MSG), sac);

    assertAnalyzerWarnings(q{
        int fun() { return 1; }
        void main()
        {
            cast(void) fun();
        }
    }c, sac);

    assertAnalyzerWarnings(q{
        void fun() { }
        alias gun = fun;
        void main()
        {
            gun();
        }
    }c, sac);

    import std.stdio: writeln;
    writeln("Unittest for UnusedResultChecker passed");
}

