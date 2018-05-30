//          Copyright Basile Burg 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//
module dscanner.analysis.vcall_in_ctor;

import dscanner.analysis.base;
import dscanner.utils;
import dparse.ast, dparse.lexer;
import std.algorithm.searching : canFind;
import std.range: retro;

/**
 * Checks virtual calls from the constructor to methods defined in the same class.
 *
 * When not used carefully, virtual calls from constructors can lead to a call
 * in a derived instance that's not yet constructed.
 */
final class VcallCtorChecker : BaseAnalyzer
{
    alias visit = BaseAnalyzer.visit;

private:

    enum string KEY = "dscanner.vcall_ctor";
    enum string MSG = "a virtual call inside a constructor may lead to"
        ~ " unexpected results in the derived classes";

    // what's called in the ctor
    Token[][] _ctorCalls;
    // the virtual method in the classes
    Token[][] _virtualMethods;


    // The problem only happens in classes
    bool[] _inClass = [false];
    // The problem only happens in __ctor
    bool[] _inCtor = [false];
    // The problem only happens with call to virtual methods
    bool[] _isVirtual = [true];
    // The problem only happens with call to virtual methods
    bool[] _isNestedFun = [false];
    // The problem only happens in derived classes that override
    bool[] _isFinal = [false];

    void pushVirtual(bool value)
    {
        _isVirtual ~= value;
    }

    void pushInClass(bool value)
    {
        _inClass ~= value;
        _ctorCalls.length += 1;
        _virtualMethods.length += 1;
    }

    void pushInCtor(bool value)
    {
        _inCtor ~= value;
    }

    void pushNestedFunc(bool value)
    {
        _isNestedFun ~= value;
    }

    void pushIsFinal(bool value)
    {
        _isFinal ~= value;
    }

    void popVirtual()
    {
        _isVirtual.length -= 1;
    }

    void popInClass()
    {
        _inClass.length -= 1;
        _ctorCalls.length -= 1;
        _virtualMethods.length -= 1;
    }

    void popInCtor()
    {
        _inCtor.length -= 1;
    }

    void popNestedFunc()
    {
        _isNestedFun.length -= 1;
    }

    void popIsFinal()
    {
        _isFinal.length -= 1;
    }

    void overwriteVirtual(bool value)
    {
        _isVirtual[$-1] = value;
    }

    bool isVirtual()
    {
        return _isVirtual[$-1];
    }

    bool isInClass()
    {
        return _inClass[$-1];
    }

    bool isInCtor()
    {
        return _inCtor[$-1];
    }

    bool isFinal()
    {
        return _isFinal[$-1];
    }

    bool isInNestedFunc()
    {
        return _isNestedFun[$-1];
    }

    void check()
    {
        foreach (call; _ctorCalls[$-1])
            foreach (vm; _virtualMethods[$-1])
        {
            if (call == vm)
            {
                addErrorMessage(call.line, call.column, KEY, MSG);
                break;
            }
        }
    }

public:

    ///
    this(string fileName, bool skipTests = false)
    {
        super(fileName, null, skipTests);
    }

    override void visit(const(ClassDeclaration) decl)
    {
        pushVirtual(true);
        pushInClass(true);
        pushNestedFunc(false);
        decl.accept(this);
        check();
        popVirtual();
        popInClass();
        popNestedFunc();
    }

    override void visit(const(Constructor) ctor)
    {
        pushInCtor(isInClass);
        ctor.accept(this);
        popInCtor();
    }

    override void visit(const(Declaration) d)
    {
        // "<protection>:"
        if (d.attributeDeclaration && d.attributeDeclaration.attribute)
        {
            const tp = d.attributeDeclaration.attribute.attribute.type;
            overwriteVirtual(isProtection(tp) & (tp != tok!"private"));
        }

        // "protection {}"
        bool pop;
        scope(exit) if (pop)
            popVirtual;

        const bool hasAttribs = d.attributes !is null;
        const bool hasStatic = hasAttribs ? d.attributes.canFind!(a => a.attribute.type == tok!"static") : false;
        const bool hasFinal = hasAttribs ? d.attributes.canFind!(a => a.attribute.type == tok!"final") : false;

        if (d.attributes) foreach (attr; d.attributes.retro)
        {
            if (!hasStatic &&
               (attr.attribute == tok!"public" || attr.attribute == tok!"protected"))
            {
                pushVirtual(true);
                pop = true;
                break;
            }
            else if (hasStatic || attr.attribute == tok!"private" || attr.attribute == tok!"package")
            {
                pushVirtual(false);
                pop = true;
                break;
            }
        }

        // final class... final function
        if ((d.classDeclaration || d.functionDeclaration) && hasFinal)
            pushIsFinal(true);

        d.accept(this);

        if ((d.classDeclaration || d.functionDeclaration) && hasFinal)
            popIsFinal;
    }

    override void visit(const(FunctionCallExpression) exp)
    {
        // nested function are not virtual
        pushNestedFunc(true);
        exp.accept(this);
        popNestedFunc();
    }

    override void visit(const(UnaryExpression) exp)
    {
        if (isInCtor)
        // get function identifier for a call, only for this member (so no ident chain)
        if (const IdentifierOrTemplateInstance iot = safeAccess(exp)
            .functionCallExpression.unaryExpression.primaryExpression.identifierOrTemplateInstance)
        {
            const Token t = iot.identifier;
            if (t != tok!"")
            {
                _ctorCalls[$-1] ~= t;
            }
        }
        exp.accept(this);
    }

    override void visit(const(FunctionDeclaration) d)
    {
        if (isInClass() && !isInNestedFunc() && !isFinal() && !d.templateParameters)
        {
            bool virtualOnce;
            bool notVirtualOnce;

            const bool hasAttribs = d.attributes !is null;
            const bool hasStatic = hasAttribs ? d.attributes.canFind!(a => a.attribute.type == tok!"static") : false;

            // handle "private", "public"... for this declaration
            if (d.attributes) foreach (attr; d.attributes.retro)
            {
                if (!hasStatic &&
                   (attr.attribute == tok!"public" || attr.attribute == tok!"protected"))
                {
                    if (!isVirtual)
                    {
                        virtualOnce = true;
                        break;
                    }
                }
                else if (hasStatic || attr.attribute == tok!"private" || attr.attribute == tok!"package")
                {
                    if (isVirtual)
                    {
                        notVirtualOnce = true;
                        break;
                    }
                }
            }

            if (!isVirtual && virtualOnce)
                _virtualMethods[$-1] ~= d.name;
            else if (isVirtual && !virtualOnce)
                _virtualMethods[$-1] ~= d.name;

        }
        d.accept(this);
    }
}

unittest
{
    import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
    import dscanner.analysis.helpers : assertAnalyzerWarnings;
    import std.stdio : stderr;
    import std.format : format;

    StaticAnalysisConfig sac = disabledConfig();
    sac.vcall_in_ctor = Check.enabled;

    // fails
    assertAnalyzerWarnings(q{
        class Bar
        {
            this(){foo();} // [warn]: %s
            private:
            public
            void foo(){}

        }
    }c.format(VcallCtorChecker.MSG), sac);

    assertAnalyzerWarnings(q{
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
    }c.format(VcallCtorChecker.MSG, VcallCtorChecker.MSG), sac);

    assertAnalyzerWarnings(q{
        class Bar
        {
            this()
            {
                foo();
                bar(); // [warn]: %s
            }
            private: public void bar();
            public private {void foo(){}}
        }
    }c.format(VcallCtorChecker.MSG), sac);

    // passes
    assertAnalyzerWarnings(q{
        class Bar
        {
            this(){foo();}
            private void foo(){}
        }
    }, sac);

    assertAnalyzerWarnings(q{
        class Bar
        {
            this(){foo();}
            private {void foo(){}}
        }
    }, sac);

    assertAnalyzerWarnings(q{
        class Bar
        {
            this(){foo();}
            private public protected private void foo(){}
        }
    }, sac);

    assertAnalyzerWarnings(q{
        class Bar
        {
            this(){foo();}
            final private public protected void foo(){}
        }
    }, sac);

    assertAnalyzerWarnings(q{
        class Bar
        {
            this(){foo();}
            final void foo(){}
        }
    }, sac);

    assertAnalyzerWarnings(q{
        final class Bar
        {
            public:
            this(){foo();}
            void foo(){}
        }
    }, sac);

    assertAnalyzerWarnings(q{
        class Bar
        {
            public:
            this(){foo();}
            void foo(T)(){}
        }
    }, sac);

    assertAnalyzerWarnings(q{
        class Foo
        {
            static void nonVirtual();
            this(){nonVirtual();}
        }
    }, sac);

    assertAnalyzerWarnings(q{
        class Foo
        {
            package void nonVirtual();
            this(){nonVirtual();}
        }
    }, sac);

    import std.stdio: writeln;
    writeln("Unittest for VcallCtorChecker passed");
}

