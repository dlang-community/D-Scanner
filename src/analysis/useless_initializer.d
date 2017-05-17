//          Copyright Basile Burg 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module analysis.useless_initializer;

import analysis.base;
import dparse.ast;
import dparse.lexer;
import std.stdio;

/*
Limitations:
    - Stuff = Stuff.init doesnot work with type with * []
*/

/**
 * Check that detects the initializers that are
 * not different from the implcit initializer.
 */
final class UselessInitializerChecker : BaseAnalyzer
{
    alias visit = BaseAnalyzer.visit;

private:

    enum key = "dscanner.useless-initializer";
    version(unittest)
        enum msg = "X";
    else
        enum msg = `Variable %s initializer is useless because it does not differ from the default value`;

    static immutable strDefs = [`""`, `""c`, `""w`, `""d`, "``", "``c", "``w", "``d", "q{}"];
    static immutable intDefs = ["0", "0L", "0UL", "0uL", "0U", "0x0", "0b0"];

public:

    ///
    this(string fileName, bool skipTests = false)
    {
        super(fileName, null, skipTests);
    }

    override void visit(const(VariableDeclaration) decl)
    {
        foreach (declarator; decl.declarators)
        {
            if (!decl.type || !decl.type.type2)
                continue;
            if (!declarator.initializer || !declarator.initializer.nonVoidInitializer)
                continue;

            import std.format : format;

            version(unittest)
                enum warn = q{addErrorMessage(declarator.name.line, declarator.name.column,
                    key, msg);};
            else
                enum warn = q{addErrorMessage(declarator.name.line, declarator.name.column,
                    key, msg.format(declarator.name.text));};

            // ---  Info about the declaration type --- //
            import std.algorithm : among, canFind;
            import std.algorithm.iteration : filter;
            import std.range : empty;

            const bool isPtr = decl.type.typeSuffixes && !decl.type.typeSuffixes
                .filter!(a => a.star != tok!"").empty;
            const bool isArr = decl.type.typeSuffixes && !decl.type.typeSuffixes
                .filter!(a => a.array).empty;

            bool isStr, isSzInt;
            Token customType;

            if (decl.type.type2.symbol && decl.type.type2.symbol.identifierOrTemplateChain &&
                decl.type.type2.symbol.identifierOrTemplateChain.identifiersOrTemplateInstances.length == 1)
            {
                const IdentifierOrTemplateInstance idt =
                    decl.type.type2.symbol.identifierOrTemplateChain.identifiersOrTemplateInstances[0];

                customType = idt.identifier;
                isStr = customType.text.among("string", "wstring", "dstring") != 0;
                isSzInt = customType.text.among("size_t", "ptrdiff_t") != 0;
            }

            // --- 'BasicType/Symbol AssignExpression' ---//
            const NonVoidInitializer nvi = declarator.initializer.nonVoidInitializer;
            const UnaryExpression ue = cast(UnaryExpression) nvi.assignExpression;
            if (ue && ue.primaryExpression)
            {
                const Token value = ue.primaryExpression.primary;

                if (!isPtr && !isArr && !isStr && decl.type.type2.builtinType != tok!"")
                {
                    switch(decl.type.type2.builtinType)
                    {
                    // check for common cases of default values
                    case tok!"byte",    tok!"ubyte":
                    case tok!"short",   tok!"ushort":
                    case tok!"int",     tok!"uint":
                    case tok!"long",    tok!"ulong":
                    case tok!"cent",    tok!"ucent":
                        if (intDefs.canFind(value.text))
                            mixin(warn);
                        goto default;
                    default:
                    // check for BasicType.init
                        if (ue.primaryExpression.basicType.type == decl.type.type2.builtinType &&
                            ue.primaryExpression.primary.text == "init" &&
                            !ue.primaryExpression.expression)
                            mixin(warn);
                    }
                }
                else if (isSzInt)
                {
                    if (intDefs.canFind(value.text))
                        mixin(warn);
                }
                else if (isPtr)
                {
                    if (str(value.type) == "null")
                        mixin(warn);
                }
                else if (isArr)
                {
                    if (str(value.type) == "null")
                        mixin(warn);
                    else if (nvi.arrayInitializer && nvi.arrayInitializer.arrayMemberInitializations.length == 0)
                        mixin(warn);
                    else if (decl.type.type2.builtinType != tok!"")
                    {
                        switch(decl.type.type2.builtinType)
                        {
                        case tok!"char", tok!"wchar", tok!"dchar":
                            if (strDefs.canFind(value.text))
                                mixin(warn);
                            break;
                        default:
                        }
                    }
                }
                else if (isStr)
                {
                    if (strDefs.canFind(value.text))
                        mixin(warn);
                    else if (nvi.arrayInitializer && nvi.arrayInitializer.arrayMemberInitializations.length == 0)
                        mixin(warn);
                }
            }

            // Symbol s = Symbol.init
            else if (ue && customType != tok!"" && ue.unaryExpression && ue.unaryExpression.primaryExpression &&
                ue.unaryExpression.primaryExpression.identifierOrTemplateInstance &&
                ue.unaryExpression.primaryExpression.identifierOrTemplateInstance.identifier == customType &&
                ue.identifierOrTemplateInstance && ue.identifierOrTemplateInstance.identifier.text == "init")
            {
                mixin(warn);
            }

            // 'Symbol ArrayInitializer' : assumes Symbol is an array b/c of the Init
            else if (nvi.arrayInitializer && (isArr || isStr))
            {
                if (nvi.arrayInitializer.arrayMemberInitializations.length == 0)
                    mixin(warn);
            }
        }

        decl.accept(this);
    }
}

@system unittest
{
    import analysis.config : Check, disabledConfig, StaticAnalysisConfig;
    import analysis.helpers: assertAnalyzerWarnings;
    import std.stdio : stderr;

    StaticAnalysisConfig sac = disabledConfig;
    sac.useless_initializer = Check.enabled;

    // fails
    assertAnalyzerWarnings(q{
        ubyte a = 0x0;      // [warn]: X
        int a = 0;          // [warn]: X
        ulong a = 0;        // [warn]: X
        int* a = null;      // [warn]: X
        Foo* a = null;      // [warn]: X
        int[] a = null;     // [warn]: X
        int[] a = [];       // [warn]: X
        string a = "";      // [warn]: X
        string a = ""c;     // [warn]: X
        wstring a = ""w;    // [warn]: X
        dstring a = ""d;    // [warn]: X
        string a = q{};     // [warn]: X
        size_t a = 0;       // [warn]: X
        ptrdiff_t a = 0;    // [warn]: X
        string a = [];      // [warn]: X
        char[] a = "";      // [warn]: X
        int a = int.init;   // [warn]: X
        char a = char.init; // [warn]: X
        S s = S.init;       // [warn]: X
    }, sac);

    // passes
    assertAnalyzerWarnings(q{
        ubyte a = 0xFE;
        int a = 1;
        ulong a = 1;
        int* a = &a;
        Foo* a = &a;
        int[] a = &a;
        int[] a = [0];
        string a = "sdf";
        string a = "sdg"c;
        wstring a = "sdg"w;
        dstring a = "fgh"d;
        string a = q{int a;};
        size_t a = 1;
        ptrdiff_t a = 1;
        string a = ['a'];
        char[] a = "ze";
        S s = S(0,1);
        S s = s.call();
    }, sac);

    stderr.writeln("Unittest for UselessInitializerChecker passed.");
}

