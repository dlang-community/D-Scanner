//          Copyright Basile Burg 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//
module dscanner.analysis.vcall_in_ctor;

import dscanner.analysis.base;
import dmd.astenums : STC;

/**
 * Checks virtual calls from the constructor to methods defined in the same class.
 *
 * When not used carefully, virtual calls from constructors can lead to a call
 * in a derived instance that's not yet constructed.
 */
extern (C++) class VcallCtorChecker(AST) : BaseAnalyzerDmd
{
    alias visit = BaseAnalyzerDmd.visit;
    mixin AnalyzerInfo!"vcall_in_ctor";

    private enum string KEY = "dscanner.vcall_ctor";
    private enum string MSG = "a virtual call inside a constructor may lead to unexpected results in the derived classes";

    private static struct FuncContext
    {
        bool canBeVirtual;
        bool hasNonVirtualVis;
        bool hasNonVirtualStg;
        bool inCtor;
    }

    private static struct CallContext
    {
        string funcName;
        ulong lineNum;
        ulong charNum;
    }

    private FuncContext[] contexts;
    private bool[string] virtualFuncs;
    private CallContext[] ctorCalls;
    private bool isFinal;

    extern (D) this(string filename, bool skipTests = false)
    {
        super(filename, skipTests);
    }

    override void visit(AST.ClassDeclaration classDecl)
    {
        pushContext((classDecl.storage_class & STC.final_) == 0 && !isFinal);
        super.visit(classDecl);
        checkForVirtualCalls();
        popContext();
    }

    override void visit(AST.StructDeclaration structDecl)
    {
        pushContext(false);
        super.visit(structDecl);
        checkForVirtualCalls();
        popContext();
    }

    private void checkForVirtualCalls()
    {
        import std.algorithm : each, filter;

        ctorCalls.filter!(call => call.funcName in virtualFuncs)
            .each!(call => addErrorMessage(call.lineNum, call.charNum, KEY, MSG));
    }

    override void visit(AST.VisibilityDeclaration visDecl)
    {
        import dmd.dsymbol : Visibility;

        if (contexts.length == 0)
        {
            super.visit(visDecl);
            return;
        }

        bool oldVis = currentContext.hasNonVirtualVis;
        currentContext.hasNonVirtualVis = visDecl.visibility.kind == Visibility.Kind.private_
            || visDecl.visibility.kind == Visibility.Kind.package_;
        super.visit(visDecl);
        currentContext.hasNonVirtualVis = oldVis;
    }

    override void visit(AST.StorageClassDeclaration stgDecl)
    {
        bool oldFinal = isFinal;
        isFinal = (stgDecl.stc & STC.final_) != 0;

        bool oldStg;
        if (contexts.length > 0)
        {
            oldStg = currentContext.hasNonVirtualStg;
            currentContext.hasNonVirtualStg = !(stgDecl.stc & STC.static_ || stgDecl.stc & STC.final_);
        }

        super.visit(stgDecl);

        isFinal = oldFinal;
        if (contexts.length > 0)
            currentContext.hasNonVirtualStg = oldStg;
    }

    override void visit(AST.FuncDeclaration funcDecl)
    {
        if (contexts.length == 0)
        {
            super.visit(funcDecl);
            return;
        }

        bool hasVirtualBody;
        if (funcDecl.fbody !is null)
        {
            auto funcBody = funcDecl.fbody.isCompoundStatement();
            hasVirtualBody = funcBody !is null && funcBody.statements !is null && (*funcBody.statements).length == 0;
        }
        else
        {
            hasVirtualBody = true;
        }

        bool hasNonVirtualStg = currentContext.hasNonVirtualStg
            || funcDecl.storage_class & STC.static_ || funcDecl.storage_class & STC.final_;

        if (!currentContext.canBeVirtual || currentContext.hasNonVirtualVis || hasNonVirtualStg || !hasVirtualBody)
        {
            super.visit(funcDecl);
            return;
        }

        string funcName = cast(string) funcDecl.ident.toString();
        virtualFuncs[funcName] = true;
    }

    override void visit(AST.CtorDeclaration ctorDecl)
    {
        if (contexts.length == 0)
        {
            super.visit(ctorDecl);
            return;
        }

        currentContext.inCtor = true;
        super.visit(ctorDecl);
        currentContext.inCtor = false;
    }

    override void visit(AST.CallExp callExp)
    {
        super.visit(callExp);

        if (contexts.length == 0)
            return;

        auto identExp = callExp.e1.isIdentifierExp();
        if (!currentContext.inCtor || identExp is null)
            return;

        string funcCall = cast(string) identExp.ident.toString();
        ctorCalls ~= CallContext(funcCall, callExp.loc.linnum, callExp.loc.charnum);
    }

    private ref currentContext() @property
    {
        return contexts[$ - 1];
    }

    private void pushContext(bool inClass)
    {
        contexts ~= FuncContext(inClass);
    }

    private void popContext()
    {
        contexts.length--;
    }
}

unittest
{
    import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
    import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
    import std.format : format;
    import std.stdio : stderr;

    StaticAnalysisConfig sac = disabledConfig();
    sac.vcall_in_ctor = Check.enabled;
    string MSG = "a virtual call inside a constructor may lead to unexpected results in the derived classes";

    // fails
    assertAnalyzerWarningsDMD(q{
        class Bar
        {
            this(){foo();} // [warn]: %s
            private:
            public void foo(){}
        }
    }c.format(MSG), sac);

    assertAnalyzerWarningsDMD(q{
        class Bar
        {
            this()
            {
                foo(); // [warn]: %s
                foo(); // [warn]: %s
                bar();
            }
            private: void bar();
            public{void foo(){}}
        }
    }c.format(MSG, MSG), sac);

    assertAnalyzerWarningsDMD(q{
        class Bar
        {
            this()
            {
                foo();
                bar(); // [warn]: %s
            }
            private: public void bar();
            private {void foo(){}}
        }
    }c.format(MSG), sac);

    // passes
    assertAnalyzerWarningsDMD(q{
        class Bar
        {
            this(){foo();}
            private void foo(){}
        }
    }, sac);

    assertAnalyzerWarningsDMD(q{
        class Bar
        {
            this(){foo();}
            private {void foo(){}}
        }
    }, sac);

    assertAnalyzerWarningsDMD(q{
        class Bar
        {
            this(){foo();}
            final void foo(){}
        }
    }, sac);

    assertAnalyzerWarningsDMD(q{
        final class Bar
        {
            public:
            this(){foo();}
            void foo(){}
        }
    }, sac);

    assertAnalyzerWarningsDMD(q{
        class Bar
        {
            public:
            this(){foo();}
            void foo(T)(){}
        }
    }, sac);

    assertAnalyzerWarningsDMD(q{
        class Foo
        {
            static void nonVirtual();
            this(){nonVirtual();}
        }
    }, sac);

    assertAnalyzerWarningsDMD(q{
        class Foo
        {
            package void nonVirtual();
            this(){nonVirtual();}
        }
    }, sac);

    assertAnalyzerWarningsDMD(q{
        class C {
            static struct S {
            public:
                this(int) {
                    foo();
                }
                void foo() {}
            }
        }
    }, sac);

    stderr.writeln("Unittest for VcallCtorChecker passed");
}
