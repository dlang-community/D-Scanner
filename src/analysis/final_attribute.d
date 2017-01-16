//          Copyright Basile Burg 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.final_attribute;

import analysis.base;
import analysis.helpers;
import dparse.ast;
import dparse.lexer;

import std.stdio;

/**
 * Checks for useless usage of the final attribute.
 *
 * There several cases where the compiler allows them even if it's a noop.
 */
final class FinalAttributeChecker : BaseAnalyzer
{

private:

	enum string KEY = "dscanner.useless.final";
	enum string MSGB = "Useless final attribute, %s";

    static struct MESSAGE
    {
        static immutable struct_i    = "structs inherit of anything";
        static immutable union_i     = "unions inherit of anything";
        static immutable class_t     = "templatized class member functions are never virtual";
        static immutable class_p     = "private class member functions are never virtual";
        static immutable class_f     = "final class member functions are never virtual";
        static immutable interface_t = "templatized interface functions are never virtual";
        static immutable struct_f    = "struct member functions are never virtual";
        static immutable union_f     = "union member functions are never virtual";
        static immutable func_n      = "nested functions are never virtual";
        static immutable func_g      = "global functions are never virtual";
    }

    enum Parent
    {
        module_,
        struct_,
        union_,
        class_,
        function_,
        interface_
    }

    bool[] _private;
    bool _finalAggregate;
    Parent _parent = Parent.module_;

    void addError(T)(T t, string msg)
    {
        import std.format : format;
        const size_t lne = t.name.line;
        const size_t col = t.name.column;
        addErrorMessage(lne, col, KEY, MSGB.format(msg));
    }

public:

	alias visit = BaseAnalyzer.visit;

	///
	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
        _private.length = 1;
	}

    override void visit(const(StructDeclaration) sd)
    {
        const Parent saved = _parent;
        _parent = Parent.struct_;
        _private.length += 1;
        sd.accept(this);
        _private.length -= 1;
        _parent = saved;
    }

    override void visit(const(InterfaceDeclaration) id)
    {
        const Parent saved = _parent;
        _parent = Parent.interface_;
        _private.length += 1;
        id.accept(this);
        _private.length -= 1;
        _parent = saved;
    }

    override void visit(const(UnionDeclaration) ud)
    {
        const Parent saved = _parent;
        _parent = Parent.union_;
        _private.length += 1;
        ud.accept(this);
        _private.length -= 1;
        _parent = saved;
    }

    override void visit(const(ClassDeclaration) cd)
    {
        const Parent saved = _parent;
        _parent = Parent.class_;
        _private.length += 1;
        cd.accept(this);
        _private.length -= 1;
        _parent = saved;
    }

	override void visit(const(Declaration) d)
	{
        const Parent savedParent = _parent;
        bool privatePushed;
        _parent = Parent.function_;

        scope(exit)
        {
            d.accept(this);
            _parent = savedParent;
        }

        import std.algorithm.searching : find;
        import std.algorithm.iteration: filter;
        import std.range.primitives : empty;

        if (d.attributeDeclaration && d.attributeDeclaration.attribute)
        {
            const tp = d.attributeDeclaration.attribute.attribute.type;
            _private[$-1] = isProtection(tp) & (tp == tok!"private");
        }

        const bool isFinal = !d.attributes
            .find!(a => a.attribute.type == tok!"final")
            .empty;

        // determine if private
        const bool changeProtectionOnce = !d.attributes
            .filter!(a => a.attribute.type.isProtection)
            .empty;

        const bool isPrivateOnce = !d.attributes
            .find!(a => a.attribute.type == tok!"private")
            .empty;

        bool isPrivate;
        if (_private[$-1] && isPrivateOnce)
            isPrivate = true;
        else if (!_private[$-1] && isPrivateOnce)
            isPrivate = true;
        else if (_private[$-1] && !changeProtectionOnce)
            isPrivate = true;

        // check final aggregate type
        if (d.classDeclaration || d.structDeclaration || d.unionDeclaration)
        {
            _finalAggregate = isFinal;
            if (savedParent == Parent.module_)
            {
                if (d.structDeclaration)
                    addError(d.structDeclaration, MESSAGE.struct_i);
                else if (d.unionDeclaration)
                    addError(d.unionDeclaration, MESSAGE.union_i);
            }
        }

        if (!d.functionDeclaration)
            return;

        // check final functions
        const(FunctionDeclaration) fd = d.functionDeclaration;

        if (isFinal) final switch(savedParent)
        {
        case Parent.class_:
            if (fd.templateParameters)
                addError(fd, MESSAGE.class_t);
            if (isPrivate)
                addError(fd, MESSAGE.class_p);
            else if (_finalAggregate)
                addError(fd, MESSAGE.class_f);
            break;
        case Parent.interface_:
            if (fd.templateParameters)
                addError(fd, MESSAGE.interface_t);
            break;
        case Parent.struct_:
            addError(fd, MESSAGE.struct_f);
            break;
        case Parent.union_:
            addError(fd, MESSAGE.union_f);
            break;
        case Parent.function_:
            addError(fd, MESSAGE.func_n);
            break;
        case Parent.module_:
            addError(fd, MESSAGE.func_g);
            break;
        }
	}
}

@system unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import analysis.config : StaticAnalysisConfig, Check;
	import analysis.helpers : assertAnalyzerWarnings;

    StaticAnalysisConfig sac;
    sac.final_attribute_check = Check.enabled;

    // pass

    assertAnalyzerWarnings(q{
        void foo(){}
    }, sac);

    assertAnalyzerWarnings(q{
        void foo(){void foo(){}}
    }, sac);

    assertAnalyzerWarnings(q{
        class Foo{public final void foo(){}}
    }, sac);

    assertAnalyzerWarnings(q{
        final class Foo{static struct Bar{}}
    }, sac);

    assertAnalyzerWarnings(q{
        class Foo{private: public final void foo(){}}
    }, sac);

    assertAnalyzerWarnings(q{
        class Foo{private: public: final void foo(){}}
    }, sac);

    // fail

    assertAnalyzerWarnings(q{
        final void foo(){} // [warn]: %s
    }c.format(
        FinalAttributeChecker.MSGB.format(FinalAttributeChecker.MESSAGE.func_g)
    ), sac);

    assertAnalyzerWarnings(q{
        void foo(){final void foo(){}} // [warn]: %s
    }c.format(
        FinalAttributeChecker.MSGB.format(FinalAttributeChecker.MESSAGE.func_n)
    ), sac);

    assertAnalyzerWarnings(q{
        final struct Foo{} // [warn]: %s
    }c.format(
        FinalAttributeChecker.MSGB.format(FinalAttributeChecker.MESSAGE.struct_i)
    ), sac);

    assertAnalyzerWarnings(q{
        final union Foo{} // [warn]: %s
    }c.format(
        FinalAttributeChecker.MSGB.format(FinalAttributeChecker.MESSAGE.union_i)
    ), sac);

    assertAnalyzerWarnings(q{
        class Foo{private final void foo(){}} // [warn]: %s
    }c.format(
        FinalAttributeChecker.MSGB.format(FinalAttributeChecker.MESSAGE.class_p)
    ), sac);

    assertAnalyzerWarnings(q{
        class Foo{private: final void foo(){}} // [warn]: %s
    }c.format(
        FinalAttributeChecker.MSGB.format(FinalAttributeChecker.MESSAGE.class_p)
    ), sac);

    assertAnalyzerWarnings(q{
        interface Foo{final void foo(T)(){}} // [warn]: %s
    }c.format(
        FinalAttributeChecker.MSGB.format(FinalAttributeChecker.MESSAGE.interface_t)
    ), sac);

    assertAnalyzerWarnings(q{
        final class Foo{final void foo(){}} // [warn]: %s
    }c.format(
        FinalAttributeChecker.MSGB.format(FinalAttributeChecker.MESSAGE.class_f)
    ), sac);

	stderr.writeln("Unittest for FinalAttributeChecker passed.");
}
