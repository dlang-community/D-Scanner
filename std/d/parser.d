// Written in the D programming language

/**
 * <script type="text/javascript">inhibitQuickIndex = 1</script>
 * This module contains a _parser for D source code.
 *
 * Grammar:
 * The grammar format used in the documentation of this module generally follows
 * the format used by the ANTLR _parser generator.
 * $(UL
 * $(LI Literals are highlighted in green.)
 * $(LI Rules are links to their definitions.)
 * $(LI Tokens and rules can be grouped by parenthesis.)
 * $(LI An asterisk (*) indicates that the previous rule, token, or group
 * can repeat 0 or more times.)
 * $(LI A question mark (?) indicates that the previous rule, token, or group
 * will be present either 0 or 1 times.)
 * $(LI A plus sign (+) indicates that the previous rule, token, or group
 * repeats one or more times. (i.e. it is optional))
 * $(LI If there is more than one way to match a rule, the alternatives will be
 * separated by a pipe character (|).)
 * $(LI Rule definitions begin with the rule name followed by a colon (:). Rule
 * definitions end with a semicolon (;).)
 * )
 *
 * The grammar for D starts with the $(LINK2 #module, module) rule.
 *
 * Examples:
 * ---
 * import std.d.lexer;
 * import std.d.parser;
 * import std.d.ast;
 * import std.array;
 *
 * string sourceCode = q{
 * import std.stdio;
 *
 * void main()
 * {
 *     writeln("Hello, World.");
 * }
 * }c;
 * void main()
 * {
 *     LexerConfig config;
 *     auto tokens = byToken(cast(ubyte[]) sourceCode, config).array();
 *     Module mod = parseModule(tokens);
 *     // Use module here...
 * }
 * ---
 *
 * Copyright: Brian Schott 2013
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt Boost, License 1.0)
 * Authors: Brian Schott
 * Source: $(PHOBOSSRC std/d/_parser.d)
 * MACROS:
 *     GRAMMAR = $(D_CODE $0)
 *     RULEDEF = $(DDOC_ANCHOR $0) $(B $0)
 *     RULE = $(LINK2 #$0, $(B $0))
 *     LITERAL = $(D_STRING $0)
 */

module std.d.parser;

import std.d.lexer;
import std.d.ast;
import std.conv;
import std.algorithm;
import std.array;
version (unittest) import std.stdio;

version = development;
version = verbose;
version(development) import std.stdio;
import std.stdio;

/**
 * Params:
 *     tokens = the tokens parsed by std.d.lexer
 * Returns: the parsed module
 */
Module parseModule(const(Token)[] tokens, string fileName)
{
    auto parser = new Parser();
    parser.fileName = fileName;
    parser.tokens = tokens;
    auto mod = parser.parseModule();
    version (development) writeln("Parsing finished with ", parser.errorCount,
        " errors and ", parser.warningCount, " warnings.");
    return mod;
}

/**
 * Parser structure
 */
struct Parser
{
    /**
     * Parses an AddExpression.
     *
     * $(GRAMMAR $(RULEDEF addExpression):
     *       $(RULE mulExpression)
     *     | $(RULE addExpression) $(LPAREN)$(LITERAL '+') | $(LITERAL'-') | $(LITERAL'~')$(RPAREN) $(RULE mulExpression)
     *     ;)
     */
    AddExpression parseAddExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AddExpression, MulExpression,
            TokenType.plus, TokenType.minus, TokenType.tilde)();
    }

    /**
     * Parses an AliasDeclaration.
     *
     * $(GRAMMAR $(RULEDEF aliasDeclaration):
     *     $(LITERAL 'alias') $(LPAREN)$(RULE aliasInitializer) $(LPAREN)$(LITERAL ',') $(RULE aliasInitializer)$(RPAREN)* | $(RULE type) $(RULE declarator)$(RPAREN) $(LITERAL ';')
     *     ;)
     */
    AliasDeclaration parseAliasDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new AliasDeclaration;
        if (expect(TokenType.alias_) is null) return null;
        if (startsWith(TokenType.identifier, TokenType.assign))
        {
            do
            {
                auto initializer = parseAliasInitializer();
                if (initializer is null) return null;
                node.initializers ~= initializer;
                if (currentIs(TokenType.comma))
                    advance();
                else
                    break;
            }
            while (true);
        }
        else
        {
            if ((node.type = parseType()) is null) return null;
            if ((node.declarator = parseDeclarator()) is null) return null;
        }
        if (expect(TokenType.semicolon) is null) return null;
        return node;
    }

    unittest
    {
        auto sourceCode =
q{
alias core.sys.posix.stdio.fileno fileno;
}c;

        Parser p = getParserForUnittest(sourceCode, "parseAliasDeclaration");

        AliasDeclaration d = p.parseAliasDeclaration();
        assert (d !is null);
        assert (p.errorCount == 0);

        stderr.writeln("Unittest for parseAliasDeclaration() passed.");
    }

    /**
     * Parses an AliasInitializer
     * $(GRAMMAR $(RULEDEF aliasInitializer):
     *     $(LITERAL Identifier) $(LITERAL '=') $(RULE type)
     *     ;)
     */
    AliasInitializer parseAliasInitializer()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new AliasInitializer;
        auto i = expect(TokenType.identifier);
        if (i is null) return null;
        node.identifier = *i;
        if (expect(TokenType.assign) is null) return null;
        node.type = parseType();
        return node;
    }

    /**
     * Parses an AliasThisDeclaration
     * $(GRAMMAR $(RULEDEF aliasThisDeclaration):
     *     $(LITERAL 'alias') $(LITERAL Identifier) $(LITERAL 'this') $(LITERAL ';')
     *     ;)
     */
    AliasThisDeclaration parseAliasThisDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new AliasThisDeclaration;
        if (expect(TokenType.alias_) is null) return null;
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        if (expect(TokenType.this_) is null) return null;
        if (expect(TokenType.semicolon) is null) return null;
        return node;
    }

    /**
     * Parses an AlignAttribute.
     * $(GRAMMAR $(RULEDEF alignAttribute):
     *     $(LITERAL 'align') ($(LITERAL '$(LPAREN)') $(LITERAL IntegerLiteral) $(LITERAL '$(RPAREN)'))?
     *     ;)
     */
    AlignAttribute parseAlignAttribute()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new AlignAttribute;
        expect(TokenType.align_);
        if (currentIs(TokenType.lParen))
        {
            if (expect(TokenType.lParen) is null) return null;
            auto intLit = expect(TokenType.intLiteral);
            if (intLit is null) return null;
            node.intLiteral = *intLit;
            if (expect(TokenType.rParen) is null) return null;
        }
        return node;
    }

    /**
     * Parses an AndAndExpression
     * $(GRAMMAR $(RULEDEF andAndExpression):
     *       $(RULE orExpression)
     *     | $(RULE andAndExpression) $(LITERAL '&&') $(RULE orExpression)
     *     ;)
     */
    AndAndExpression parseAndAndExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AndAndExpression, OrExpression,
            TokenType.logicAnd)();
    }

    /**
     * Parses an AndExpression
     *
     * $(GRAMMAR $(RULEDEF andExpression):
     *       $(RULE cmpExpression)
     *     | $(RULE andExpression) $(LITERAL '&') $(RULE cmpExpression)
     *     ;)
     */
    AndExpression parseAndExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AndExpression, CmpExpression,
            TokenType.bitAnd)();
    }

    /**
     * Parses an ArgumentList
     *
     * $(GRAMMAR $(RULEDEF argumentList):
     *     $(RULE assignExpression) ($(LITERAL ',') $(RULE assignExpression)?)*
     *     ;)
     */
    ArgumentList parseArgumentList()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseCommaSeparatedRule!(ArgumentList, AssignExpression)();
    }

    /**
     * Parses Arguments
     *
     * $(GRAMMAR $(RULEDEF arguments):
     *     $(LITERAL '$(LPAREN)') $(RULE argumentList)? $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Arguments parseArguments()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Arguments;
        if (expect(TokenType.lParen) is null) return null;
        if (!currentIs(TokenType.rParen))
            node.argumentList = parseArgumentList();
        if (expect(TokenType.rParen) is null) return null;
        return node;
    }

    /**
     * Parses an ArrayInitializer
     *
     * $(GRAMMAR $(RULEDEF arrayInitializer):
     *       $(LITERAL '[') $(LITERAL ']')
     *     | $(LITERAL '[') $(RULE arrayMemberInitialization) ($(LITERAL ',') $(RULE arrayMemberInitialization))* $(LITERAL ']')
     *     ;)
     */
    ArrayInitializer parseArrayInitializer()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new ArrayInitializer;
        if (expect(TokenType.lBracket) is null) return null;
        while (true)
        {
            node.arrayMemberInitializations ~= parseArrayMemberInitialization();
            if (currentIs(TokenType.comma))
            {
                advance();
                continue;
            }
            else
                break;
        }

        if (expect(TokenType.rBracket) is null) return null;
        return node;
    }

    /**
     * Parses an ArrayLiteral
     *
     * $(GRAMMAR $(RULEDEF arrayLiteral):
     *     $(LITERAL '[') $(RULE argumentList) $(LITERAL ']')
     *     ;)
     */
    ArrayLiteral parseArrayLiteral()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new ArrayLiteral;
        if (expect(TokenType.lBracket) is null) return null;
        node.argumentList = parseArgumentList();
        if (expect(TokenType.rBracket) is null) return null;
        return node;
    }

    /**
     * Parses an ArrayMemberInitialization
     *
     * $(GRAMMAR $(RULEDEF arrayMemberInitialization):
     *     ($(RULE assignExpression) $(LITERAL ':'))? $(RULE nonVoidInitializer)
     *     ;)
     */
    ArrayMemberInitialization parseArrayMemberInitialization()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new ArrayMemberInitialization;
        with (TokenType) switch (current.type)
        {
        case lBrace:
        case lBracket:
            node.nonVoidInitializer = parseNonVoidInitializer();
            if (node.nonVoidInitializer is null) return null;
            break;
        default:
            auto assignExpression = parseAssignExpression();
            if (assignExpression is null) return null;
            if (currentIs(colon))
            {
                node.assignExpression = assignExpression;
                advance();
                node.nonVoidInitializer = parseNonVoidInitializer();
                if (node.nonVoidInitializer is null) return null;
            }
            else
            {
                node.nonVoidInitializer = new NonVoidInitializer;
                node.nonVoidInitializer.assignExpression = assignExpression;
            }
        }
        return node;
    }

    /**
     * Parses an AsmAddExp
     *
     * $(GRAMMAR $(RULEDEF asmAddExp):
     *       $(RULE asmMulExp)
     *     | $(RULE asmAddExp) ($(LITERAL '+') | $(LITERAL '-')) $(RULE asmMulExp)
     *     ;)
     */
    AsmAddExp parseAsmAddExp()
    {
        return parseLeftAssocBinaryExpression!(AsmAddExp, AsmMulExp,
            TokenType.plus, TokenType.minus)();
    }

    /**
     * Parses an AsmAndExp
     *
     * $(GRAMMAR $(RULEDEF asmAndExp):
     *     $(RULE asmEqualExp) ($(LITERAL '&') $(RULE asmEqualExp))?
     *     ;)
     */
    AsmAndExp parseAsmAndExp()
    {
        auto node = new AsmAndExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmBrExp
     *
     * $(GRAMMAR $(RULEDEF asmBrExp):
     *     $(RULE asmUnaExp)
     *     | $(RULE asmBrExp) $(LITERAL '[') $(RULE asmExp) $(LITERAL ']')
     *     ;)
     */
    AsmBrExp parseAsmBrExp()
    {
        auto node = new AsmBrExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmEqualExp
     *
     * $(GRAMMAR $(RULEDEF asmEqualExp):
     *     $(RULE asmRelExp) (('==' | '!=') $(RULE asmRelExp))?
     *     ;)
     */
    AsmEqualExp parseAsmEqualExp()
    {
        auto node = new AsmEqualExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmExp
     *
     * $(GRAMMAR $(RULEDEF asmExp):
     *     $(RULE asmLogOrExp) ($(LITERAL '?') $(RULE asmExp) $(LITERAL ':') $(RULE asmExp))?
     *     ;)
     */
    AsmExp parseAsmExp()
    {
        auto node = new AsmExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmInstruction
     *
     * $(GRAMMAR $(RULEDEF asmInstruction):
     *     $(LITERAL Identifier)
     *     | $(LITERAL 'align') I$(RULE ntegerLiteral)
     *     | $(LITERAL 'align') $(LITERAL Identifier)
     *     | $(LITERAL Identifier) $(LITERAL ':') $(RULE asmInstruction)
     *     | $(LITERAL Identifier) $(RULE asmExp)
     *     | $(RULE opcode) $(RULE operands)
     *     ;)
     */
    AsmInstruction parseAsmInstruction()
    {
        auto node = new AsmInstruction;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmLogAndExp
     *
     * $(GRAMMAR $(RULEDEF asmLogAndExp):
     *     $(RULE asmOrExp) ('&&' $(RULE asmOrExp))?
     *     ;)
     */
    AsmLogAndExp parseAsmLogAndExp()
    {
        auto node = new AsmLogAndExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmLogOrExp
     *
     * $(GRAMMAR $(RULEDEF asmLogOrExp):
     *     $(RULE asmLogAndExp) ('||' $(RULE asmLogAndExp))?
     *     ;)
     */
    AsmLogOrExp parseAsmLogOrExp()
    {
        auto node = new AsmLogOrExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmMulExp
     *
     * $(GRAMMAR $(RULEDEF asmMulExp):
     *     $(RULE asmBrExp) (($(LITERAL '*') | $(LITERAL '/') | $(LITERAL '%')) $(RULE asmBrExp))?
     *     ;)
     */
    AsmMulExp parseAsmMulExp()
    {
        auto node = new AsmMulExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmOrExp
     *
     * $(GRAMMAR $(RULEDEF asmOrExp):
     *     $(RULE asmXorExp) ($(LITERAL '|') $(RULE asmXorExp))?
     *     ;)
     */
    AsmOrExp parseAsmOrExp()
    {
        auto node = new AsmOrExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmPrimaryExp
     *
     * $(GRAMMAR $(RULEDEF asmPrimaryExp):
     *       $(RULE IntegerLiteral)
     *     | $(RULE FloatLiteral)
     *     | $(RULE register)
     *     | $(RULE identifierChain)
     *     | $(LITERAL '$')
     *     ;)
     */
    AsmPrimaryExp parseAsmPrimaryExp()
    {
        auto node = new AsmPrimaryExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmRelExp
     *
     * $(GRAMMAR $(RULEDEF asmRelExp):
     *     $(RULE asmShiftExp) (($(LITERAL '<') | $(LITERAL '<=') | $(LITERAL '>') | $(LITERAL '>=')) $(RULE asmShiftExp))?
     *     ;)
     */
    AsmRelExp parseAsmRelExp()
    {
        auto node = new AsmRelExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmShiftExp
     *
     * $(GRAMMAR $(RULEDEF asmShiftExp):
     *     $(RULE asmAddExp) (($(LITERAL '<<') | $(LITERAL '>>') | $(LITERAL '>>>')) $(RULE asmAddExp))?
     *     ;)
     */
    AsmShiftExp parseAsmShiftExp()
    {
        auto node = new AsmShiftExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmStatement
     *
     * $(GRAMMAR $(RULEDEF asmStatement):
     *     $(LITERAL 'asm') $(LITERAL '{') $(RULE asmInstruction)+ $(LITERAL '}')
     *     ;)
     */
    AsmStatement parseAsmStatement()
    {
        // TODO asm
        auto node = new AsmStatement;
        advance();
        skipBraceContent();
        return node;
    }

    /**
     * Parses an AsmTypePrefix
     *
     * $(GRAMMAR $(RULEDEF asmTypePrefix):
     *     $(LITERAL Identifier) $(LITERAL Identifier)
     *     | $(LITERAL 'byte') $(LITERAL Identifier)
     *     | $(LITERAL 'short') $(LITERAL Identifier)
     *     | $(LITERAL 'int') $(LITERAL Identifier)
     *     | $(LITERAL 'float') $(LITERAL Identifier)
     *     | $(LITERAL 'double') $(LITERAL Identifier)
     *     | $(LITERAL 'real') $(LITERAL Identifier)
     *     ;)
     */
    AsmTypePrefix parseAsmTypePrefix()
    {
        auto node = new AsmTypePrefix;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmUnaExp
     *
     * $(GRAMMAR $(RULEDEF asmUnaExp):
     *     $(RULE asmTypePrefix) $(RULE asmExp)
     *     | $(LITERAL Identifier) $(RULE asmExp)
     *     | $(LITERAL '+') $(RULE asmUnaExp)
     *     | $(LITERAL '-') $(RULE asmUnaExp)
     *     | $(LITERAL '!') $(RULE asmUnaExp)
     *     | $(LITERAL '~') $(RULE asmUnaExp)
     *     | $(RULE asmPrimaryExp)
     *     ;)
     */
    AsmUnaExp parseAsmUnaExp()
    {
        auto node = new AsmUnaExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AsmXorExp
     *
     * $(GRAMMAR $(RULEDEF asmXorExp):
     *     $(RULE asmAndExp) ($(LITERAL '^') $(RULE asmAndExp))?
     *     ;)
     */
    AsmXorExp parseAsmXorExp()
    {
        auto node = new AsmXorExp;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an AssertExpression
     *
     * $(GRAMMAR $(RULEDEF assertExpression):
     *     $(LITERAL 'assert') $(LITERAL '$(LPAREN)') $(RULE assignExpression) ($(LITERAL ',') $(RULE assignExpression))? $(LITERAL '$(RPAREN)')
     *     ;)
     */
    AssertExpression parseAssertExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new AssertExpression;
        expect(TokenType.assert_);
        if (expect(TokenType.lParen) is null) return null;
        node.assertion = parseAssignExpression();
        if (currentIs(TokenType.comma))
        {
            advance();
            node.message = parseAssignExpression();
        }
        if (expect(TokenType.rParen) is null) return null;
        return node;
    }

    /**
     * Parses an AssignExpression
     *
     * $(GRAMMAR $(RULEDEF assignExpression):
     *     $(RULE ternaryExpression) ($(RULE assignOperator) $(RULE assignExpression))?
     *     ;
     *$(RULEDEF assignOperator):
     *       $(LITERAL '=')
     *     | $(LITERAL '>>>=')
     *     | $(LITERAL '>>=')
     *     | $(LITERAL '<<=')
     *     | $(LITERAL '+=')
     *     | $(LITERAL '-=')
     *     | $(LITERAL '*=')
     *     | $(LITERAL '%=')
     *     | $(LITERAL '&=')
     *     | $(LITERAL '/=')
     *     | $(LITERAL '|=')
     *     | $(LITERAL '^^=')
     *     | $(LITERAL '^=')
     *     | $(LITERAL '~=')
     *     ;)
     */
    AssignExpression parseAssignExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new AssignExpression;
        node.ternaryExpression = parseTernaryExpression();
        if (currentIsOneOf(TokenType.assign, TokenType.unsignedShiftRightEqual,
            TokenType.shiftRightEqual, TokenType.shiftLeftEqual,
            TokenType.plusEqual, TokenType.minusEqual, TokenType.mulEqual,
            TokenType.modEqual, TokenType.bitAndEqual, TokenType.divEqual,
            TokenType.bitOrEqual, TokenType.powEqual, TokenType.xorEqual,
            TokenType.catEqual))
        {
            node.operator = advance().type;
            node.assignExpression = parseAssignExpression();
        }
        return node;
    }

    /**
     * Parses an AssocArrayLiteral
     *
     * $(GRAMMAR $(RULEDEF assocArrayLiteral):
     *     $(LITERAL '[') $(RULE keyValuePairs) $(LITERAL ']')
     *     ;)
     */
    AssocArrayLiteral parseAssocArrayLiteral()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new AssocArrayLiteral;
        if (expect(TokenType.lBracket) is null) return null;
        node.keyValuePairs = parseKeyValuePairs();
        if (expect(TokenType.rBracket) is null) return null;
        return node;
    }

    /**
     * Parses an AtAttribute
     *
     * $(GRAMMAR $(RULEDEF atAttribute):
     *     $(LITERAL '@') ($(LITERAL Identifier) | $(LITERAL '$(LPAREN)') $(RULE argumentList) $(LITERAL '$(RPAREN)') | $(RULE functionCallExpression))
     *     ;)
     */
    AtAttribute parseAtAttribute()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new AtAttribute;
        if (expect(TokenType.at) is null) return null;
        with (TokenType) switch (current.type)
        {
        case identifier:
            if (peekIsOneOf(lParen, dot, not))
                node.functionCallExpression = parseFunctionCallExpression();
            else
                node.identifier = advance();
            break;
        case lParen:
            node.argumentList = parseArgumentList();
            expect(rParen);
            break;
        default:
            error(`"(", or identifier expected`);
            return null;
        }
        return node;
    }

    /**
     * Parses an Attribute
     *
     * $(GRAMMAR $(RULEDEF attribute):
     *       $(RULE linkageAttribute)
     *     | $(RULE alignAttribute)
     *     | $(RULE pragmaExpression)
     *     | $(RULE deprecated)
     *     | $(RULE atAttribute)
     *     | $(LITERAL 'private')
     *     | $(LITERAL 'package')
     *     | $(LITERAL 'protected')
     *     | $(LITERAL 'public')
     *     | $(LITERAL 'export')
     *     | $(LITERAL 'extern')
     *     | $(LITERAL 'final')
     *     | $(LITERAL 'synchronized')
     *     | $(LITERAL 'override')
     *     | $(LITERAL 'abstract')
     *     | $(LITERAL 'const')
     *     | $(LITERAL 'auto')
     *     | $(LITERAL 'scope')
     *     | $(LITERAL '___gshared')
     *     | $(LITERAL 'shared')
     *     | $(LITERAL 'immutable')
     *     | $(LITERAL 'inout')
     *     | $(LITERAL 'static')
     *     | $(LITERAL 'pure')
     *     | $(LITERAL 'nothrow')
     *     | $(LITERAL 'enum')
     *     ;)
     */
    Attribute parseAttribute()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Attribute;
        switch (current.type)
        {
        case TokenType.extern_:
            if (peekIs(TokenType.lParen))
                node.linkageAttribute = parseLinkageAttribute();
            else
                node.attribute = advance().type;
            break;
        case TokenType.align_:
            node.alignAttribute = parseAlignAttribute();
            break;
        case TokenType.pragma_:
            node.pragmaExpression = parsePragmaExpression();
            break;
        case TokenType.deprecated_:
            node.deprecated_ = parseDeprecated();
            break;
        case TokenType.private_:
        case TokenType.package_:
        case TokenType.protected_:
        case TokenType.public_:
        case TokenType.export_:
        case TokenType.final_:
        case TokenType.synchronized_:
        case TokenType.override_:
        case TokenType.abstract_:
        case TokenType.const_:
        case TokenType.auto_:
        case TokenType.scope_:
        case TokenType.gshared:
        case TokenType.shared_:
        case TokenType.immutable_:
        case TokenType.inout_:
        case TokenType.static_:
        case TokenType.pure_:
        case TokenType.nothrow_:
        case TokenType.enum_:
            node.attribute = advance().type;
            break;
        case TokenType.at:
            node.atAttribute = parseAtAttribute();
            break;
        default:
            error("Attribute expected");
            return null;
        }
        return node;
    }

    /**
     * Parses an AttributedDeclaration
     *
     * $(GRAMMAR $(RULEDEF attributedDeclaration):
     *     $(RULE attribute) ($(LITERAL ':') | $(RULE declaration) | $(LITERAL '{') $(RULE declaration)* $(LITERAL '}'))
     *     ;)
     */
    AttributedDeclaration parseAttributedDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new AttributedDeclaration;
        node.attribute = parseAttribute();
        switch (current.type)
        {
        case TokenType.colon:
            advance();
            break;
        default:
            node.declaration = parseDeclaration();
            break;
        }
        return node;
    }

    /**
     * Parses an AutoDeclaration
     *
     * $(GRAMMAR $(RULEDEF autoDeclaration):
     *     $(RULE storageClass) $(LITERAL Identifier) $(LITERAL '=') $(RULE initializer) ($(LITERAL ',') $(LITERAL Identifier) $(LITERAL '=') $(RULE initializer))* $(LITERAL ';')
     *     ;)
     */
    AutoDeclaration parseAutoDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new AutoDeclaration;
        node.storageClass = parseStorageClass();
        if (node.storageClass is null) return null;
        do
        {
            auto ident = expect(TokenType.identifier);
            if (ident is null) return null;
            node.identifiers ~= *ident;
            if (expect(TokenType.assign) is null) return null;
            auto init = parseInitializer();
            if (init is null) return null;
            node.initializers ~= init;
            if (currentIs(TokenType.comma))
                advance();
            else
                break;
        } while (true);
        if (expect(TokenType.semicolon) is null) return null;
        return node;
    }

    /**
     * Parses a BlockStatement
     *
     * $(GRAMMAR $(RULEDEF blockStatement):
     *     $(LITERAL '{') $(RULE declarationsAndStatements)? $(LITERAL '}')
     *     ;)
     */
    BlockStatement parseBlockStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new BlockStatement();
        if (expect(TokenType.lBrace) is null) return null;
        version (development) skipBraceContent();
        if (!currentIs(TokenType.rBrace))
            node.declarationsAndStatements = parseDeclarationsAndStatements();
        if (expect(TokenType.rBrace) is null) return null;
        return node;
    }

    /**
     * Parses a BodyStatement
     *
     * $(GRAMMAR $(RULEDEF bodyStatement):
     *     $(LITERAL 'body') $(RULE blockStatement)
     *     ;)
     */
    BodyStatement parseBodyStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new BodyStatement;
        expect(TokenType.body_);
        node.blockStatement = parseBlockStatement();
        return node;
    }

    /**
     * Parses a BreakStatement
     *
     * $(GRAMMAR $(RULEDEF breakStatement):
     *     $(LITERAL 'break') $(LITERAL Identifier)? $(LITERAL ';')
     *     ;)
     */
    BreakStatement parseBreakStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        expect(TokenType.break_);
        auto node = new BreakStatement;
        switch (current.type)
        {
        case TokenType.identifier:
            node.identifier = advance();
            if (expect(TokenType.semicolon) is null) return null;
            break;
        case TokenType.semicolon:
            advance();
            break;
        default:
            error("Identifier or semicolon expected following \"break\"");
            return null;
        }
        return node;
    }

    /**
     * Parses a BaseClass
     *
     * $(GRAMMAR $(RULEDEF baseClass):
     *     ($(RULE typeofExpression) $(LITERAL '.'))? $(RULE identifierOrTemplateChain)
     *     ;)
     */
    BaseClass parseBaseClass()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new BaseClass;
        if (currentIs(TokenType.typeof_))
        {
            node.typeofExpression = parseTypeofExpression();
            if (expect(TokenType.dot) is null) return null;
        }
        node.identifierOrTemplateChain = parseIdentifierOrTemplateChain();
        return node;
    }

    /**
     * Parses a BaseClassList
     *
     * $(GRAMMAR $(RULEDEF baseClassList):
     *     $(RULE baseClass) ($(LITERAL ',') $(RULE baseClass))*
     *     ;)
     */
    BaseClassList parseBaseClassList()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseCommaSeparatedRule!(BaseClassList, BaseClass)();
    }

    /**
     * Parses an BuiltinType
     *
     * $(GRAMMAR $(RULEDEF builtinType):
     *      $(LITERAL 'bool')
     *    | $(LITERAL 'byte')
     *    | $(LITERAL 'ubyte')
     *    | $(LITERAL 'short')
     *    | $(LITERAL 'ushort')
     *    | $(LITERAL 'int')
     *    | $(LITERAL 'uint')
     *    | $(LITERAL 'long')
     *    | $(LITERAL 'ulong')
     *    | $(LITERAL 'char')
     *    | $(LITERAL 'wchar')
     *    | $(LITERAL 'dchar')
     *    | $(LITERAL 'float')
     *    | $(LITERAL 'double')
     *    | $(LITERAL 'real')
     *    | $(LITERAL 'ifloat')
     *    | $(LITERAL 'idouble')
     *    | $(LITERAL 'ireal')
     *    | $(LITERAL 'cfloat')
     *    | $(LITERAL 'cdouble')
     *    | $(LITERAL 'creal')
     *    | $(LITERAL 'void')
     *    ;)
     */
    Token parseBasicType()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        if (isBasicType(current.type))
            return advance();
        error("Basic type expected");
        Token t;
        return t;
    }

    /**
     * Parses a CaseRangeStatement
     *
     * $(GRAMMAR $(RULEDEF caseRangeStatement):
     *     $(LITERAL 'case') $(RULE assignExpression) $(LITERAL ':') $(LITERAL '...') $(LITERAL 'case') $(RULE assignExpression) $(LITERAL ':') $(RULE declarationsAndStatements)
     *     ;)
     */
    CaseRangeStatement parseCaseRangeStatement(AssignExpression low = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new CaseRangeStatement;
        if (low is null)
        {
            expect(TokenType.case_);
            node.low = parseAssignExpression();
        }
        if (expect(TokenType.colon) is null) return null;
        if (expect(TokenType.vararg) is null) return null;
        expect(TokenType.case_);
        node.high = parseAssignExpression();
        if (expect(TokenType.colon) is null) return null;
        node.declarationsAndStatements = parseDeclarationsAndStatements();
        return node;
    }

    /**
     * Parses an CaseStatement
     *
     * $(GRAMMAR $(RULEDEF caseStatement):
     *     $(LITERAL 'case') $(RULE argumentList) $(LITERAL ':') $(RULE declarationsAndStatements)
     *     ;)
     */
    CaseStatement parseCaseStatement(ArgumentList argumentList = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new CaseStatement;
        if (argumentList !is null)
            expect(TokenType.case_);
        node.argumentList = argumentList is null ? parseArgumentList() : argumentList;
        if (expect(TokenType.colon) is null) return null;
        node.declarationsAndStatements = parseDeclarationsAndStatements();
        return node;
    }

    /**
     * Parses a CastExpression
     *
     * $(GRAMMAR $(RULEDEF castExpression):
     *     $(LITERAL 'cast') $(LITERAL '$(LPAREN)') ($(RULE type) | $(RULE castQualifier))? $(LITERAL '$(RPAREN)') $(RULE unaryExpression)
     *     ;)
     */
    CastExpression parseCastExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new CastExpression;
        expect(TokenType.cast_);
        if (expect(TokenType.lParen) is null) return null;
        if (isCastQualifier())
            node.castQualifier = parseCastQualifier();
        else
            node.type = parseType();
        if (expect(TokenType.rParen) is null) return null;
        node.unaryExpression = parseUnaryExpression();
        return node;
    }

    private bool isCastQualifier() const
    {
        switch (current.type)
        {
        case TokenType.const_:
            return peekIsOneOf(TokenType.shared_, TokenType.rParen);
        case TokenType.immutable_:
            return peekIs(TokenType.rParen);
        case TokenType.inout_:
            return peekIsOneOf(TokenType.shared_, TokenType.rParen);
        case TokenType.shared_:
            return peekIsOneOf(TokenType.const_, TokenType.inout_, TokenType.rParen);
        default:
            return false;
        }
    }

    /**
     * Parses a CastQualifier
     *
     * $(GRAMMAR $(RULEDEF castQualifier):
     *      $(LITERAL 'const')
     *    | $(LITERAL 'const') $(LITERAL 'shared')
     *    | $(LITERAL 'immutable')
     *    | $(LITERAL 'inout')
     *    | $(LITERAL 'inout') $(LITERAL 'shared')
     *    | $(LITERAL 'shared')
     *    | $(LITERAL 'shared') $(LITERAL 'const')
     *    | $(LITERAL 'shared') $(LITERAL 'inout')
     *    ;)
     */
    CastQualifier parseCastQualifier()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new CastQualifier;
        switch (current.type)
        {
        case TokenType.inout_:
        case TokenType.const_:
            node.first = advance().type;
            if (currentIs(TokenType.shared_))
            {
                node.hasSecond = true;
                node.second = advance().type;
            }
            break;
        case TokenType.shared_:
            node.first = advance().type;
            if (currentIsOneOf(TokenType.const_, TokenType.inout_))
            {
                node.hasSecond = true;
                node.second = advance().type;
            }
            break;
        case TokenType.immutable_:
            node.first = advance().type;
            break;
        default:
            error("const, immutable, inout, or shared expected");
            return null;
        }
        return node;
    }

    unittest
    {
        auto sourceCode = q{
const;
const shared;
immutable;
inout;
inout shared;
shared;
shared const;
shared inout;
incorrect;
        };

        Parser p = getParserForUnittest(sourceCode, "parseCastQualifier");

        CastQualifier one = p.parseCastQualifier();
        assert (one.first == TokenType.const_);
        assert (!one.hasSecond);
        p.expect(TokenType.semicolon);

        CastQualifier two = p.parseCastQualifier();
        assert (two.first == TokenType.const_);
        assert (two.hasSecond);
        assert (two.second == TokenType.shared_);
        p.expect(TokenType.semicolon);

        CastQualifier three = p.parseCastQualifier();
        assert (three.first == TokenType.immutable_);
        assert (!three.hasSecond);
        p.expect(TokenType.semicolon);

        CastQualifier four = p.parseCastQualifier();
        assert (four.first == TokenType.inout_);
        assert (!four.hasSecond);
        p.expect(TokenType.semicolon);

        CastQualifier five = p.parseCastQualifier();
        assert (five.first == TokenType.inout_);
        assert (five.hasSecond);
        assert (five.second == TokenType.shared_);
        p.expect(TokenType.semicolon);

        CastQualifier six = p.parseCastQualifier();
        assert (six.first == TokenType.shared_);
        assert (!six.hasSecond);
        p.expect(TokenType.semicolon);

        CastQualifier seven = p.parseCastQualifier();
        assert (seven.first == TokenType.shared_);
        assert (seven.hasSecond);
        assert (seven.second == TokenType.const_);
        p.expect(TokenType.semicolon);

        CastQualifier eight = p.parseCastQualifier();
        assert (eight.first == TokenType.shared_);
        assert (eight.hasSecond);
        assert (eight.second == TokenType.inout_);
        p.expect(TokenType.semicolon);

        CastQualifier nine = p.parseCastQualifier();
        assert (nine is null);
        assert (p.errorCount > 0);

        stderr.writeln("Unittest for parseCastQualifier() passed.");
    }

    /**
     * Parses a Catch
     *
     * $(GRAMMAR $(RULEDEF catch):
     *     $(LITERAL 'catch') $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL Identifier)? $(LITERAL '$(RPAREN)') $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    Catch parseCatch()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Catch;
        expect(TokenType.catch_);
        if (expect(TokenType.lParen) is null) return null;
        node.type = parseType();
        if (currentIs(TokenType.identifier))
            node.identifier = advance();
        if (expect(TokenType.rParen) is null) return null;
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
        return node;
    }

    /**
     * Parses a Catches
     *
     * $(GRAMMAR $(RULEDEF catches):
     *       $(RULE catch)+
     *     | $(RULE catch)* $(RULE lastCatch)
     *     ;)
     */
    Catches parseCatches()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Catches;
        while (true)
        {
            expect(TokenType.catch_);
            if (currentIs(TokenType.lParen))
            {
                node.catches ~= parseCatch();
                if (currentIs(TokenType.catch_))
                    continue;
                else
                    break;
            }
            else
            {
                node.lastCatch  = parseLastCatch();
                break;
            }
        }
        return node;
    }

    /**
     * Parses a ClassBody
     *
     * $(GRAMMAR $(RULEDEF classBody):
     *     $(LITERAL '{') $(RULE declarationOrInvariant)* $(LITERAL '}')
     *     ;)
     */
    ClassBody parseClassBody()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new ClassBody;
        if (expect(TokenType.lBrace) is null) return null;
        while (moreTokens() && !currentIs(TokenType.rBrace))
        {
            auto doi = parseDeclarationOrInvariant();
            if (doi !is null)
                node.declarationOrInvariants ~= doi;
        }
        if (expect(TokenType.rBrace) is null) return null;
        return node;
    }

    /**
     * Parses a ClassDeclaration
     *
     * $(GRAMMAR $(RULEDEF classDeclaration):
     *     $(LITERAL 'class') $(LITERAL Identifier) ($(RULE templateParameters) $(RULE constraint)?)? ($(LITERAL ':') $(RULE baseClassList))? $(RULE classBody)
     *     ;)
     */
    ClassDeclaration parseClassDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new ClassDeclaration;
        expect(TokenType.class_);
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.name = *ident;
        if (currentIs(TokenType.lParen))
        {
            node.templateParameters = parseTemplateParameters();
            if (currentIs(TokenType.if_))
            {
                node.constraint = parseConstraint();
            }
        }
        if (currentIs(TokenType.colon))
        {
            advance();
            node.baseClassList = parseBaseClassList();
        }
        node.classBody = parseClassBody();
        return node;
    }

    unittest
    {
        string sourceCode =
q{class ClassOne {}
class ClassTwo : Super {}
class ClassThree(A, B) : Super {}
class ClassFour(A, B) if (someTest()) : Super {}}c;

        Parser p = getParserForUnittest(sourceCode, "parseClassDeclaration");

        auto classOne = p.parseClassDeclaration();
        assert (classOne.name == "ClassOne");
        assert (classOne.classBody.declarationOrInvariants.length == 0);
        assert (classOne.baseClassList is null);
        assert (classOne.constraint is null);
        assert (classOne.templateParameters is null);

        auto classTwo = p.parseClassDeclaration();
        assert (classTwo.name == "ClassTwo", classTwo.name.value);
        assert (classTwo.baseClassList !is null);
        assert (classTwo.baseClassList.items.length == 1,
            to!string(classTwo.baseClassList.items.length));
        assert (classTwo.classBody.declarationOrInvariants.length == 0,
            to!string(classTwo.classBody.declarationOrInvariants.length));

        auto classThree = p.parseClassDeclaration();
        assert (classThree.name == "ClassThree", classThree.name.value);
        assert (classThree.templateParameters !is null);
        assert (classThree.templateParameters.templateParameterList.items.length == 2);
        assert (classThree.baseClassList !is null);
        assert (classThree.baseClassList.items.length == 1);
        assert (classThree.classBody.declarationOrInvariants.length == 0,
            to!string(classThree.classBody.declarationOrInvariants.length));

        //auto classFour = p.parseClassDeclaration();
        //assert (classFour.name == "ClassFour", classFour.name.value);
        //assert (classFour.templateParameters !is null);
        //assert (classFour.baseClassList !is null);
        //assert (classFour.constraint !is null);
        //assert (classFour.baseClassList.items.length == 1);
        //assert (classFour.classBody.declarationOrInvariants.length == 0,
        //    to!string(classFour.classBody.declarationOrInvariants.length));

        stderr.writeln("Unittest for parseClassDeclaration() passed.");
    }

    /**
     * Parses a CmpExpression
     *
     * $(GRAMMAR $(RULEDEF cmpExpression):
     *       $(RULE shiftExpression)
     *     | $(RULE equalExpression)
     *     | $(RULE identityExpression)
     *     | $(RULE relExpression)
     *     | $(RULE inExpression)
     *     ;)
     */
    CmpExpression parseCmpExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new CmpExpression;
        auto shift = parseShiftExpression();
        with (TokenType) switch (current.type)
        {
        case is_:
            node.identityExpression = parseIdentityExpression(shift);
            break;
        case in_:
            node.inExpression = parseInExpression(shift);
            break;
        case not:
            if (peekIs(is_))
                node.identityExpression = parseIdentityExpression(shift);
            else if (peekIs(in_))
                node.inExpression = parseInExpression(shift);
            break;
        case less:
        case lessEqual:
        case greater:
        case greaterEqual:
        case unordered:
        case notLessEqualGreater:
        case lessOrGreater:
        case lessEqualGreater:
        case notGreater:
        case notGreaterEqual:
        case notLess:
        case notLessEqual:
            node.relExpression = parseRelExpression(shift);
            break;
        case equal:
        case notEqual:
            node.equalExpression = parseEqualExpression(shift);
            break;
        default:
            node.shiftExpression = shift;
            break;
        }
        return node;
    }

    /**
     * Parses a CompileCondition
     *
     * $(GRAMMAR $(RULEDEF compileCondition):
     *       $(RULE versionCondition)
     *     | $(RULE debugCondition)
     *     | $(RULE staticIfCondition)
     *     ;)
     */
    CompileCondition parseCompileCondition()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new CompileCondition;
        switch (current.type)
        {
        case TokenType.version_:
            node.versionCondition = parseVersionCondition();
            break;
        case TokenType.debug_:
            node.debugCondition = parseDebugCondition();
            break;
        case TokenType.static_:
            node.staticIfCondition = parseStaticIfCondition();
            break;
        default:
            error(`"version", "debug", or "static" expected`);
            return null;
        }
        return node;
    }

    /**
     * Parses a ConditionalDeclaration
     *
     * $(GRAMMAR $(RULEDEF conditionalDeclaration):
     *     $(RULE compileCondition) ($(RULE declaration) | $(LITERAL '{') $(RULE declaration)* $(LITERAL '}')) ($(LITERAL 'else') ($(RULE declaration) | $(LITERAL '{') $(RULE declaration)* $(LITERAL '}')))?
     *     ;)
     */
    ConditionalDeclaration parseConditionalDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new ConditionalDeclaration;
        node.compileCondition = parseCompileCondition();
        if (currentIs(TokenType.lBrace))
        {
            advance();
            do
            {
                auto dec = parseDeclaration();
                if (dec is null) return null;
                node.trueDeclarations ~= dec;
                if (!moreTokens() || currentIs(TokenType.rBrace))
                    break;
            }
            while (true);
            if (expect(TokenType.rBrace) is null) return null;
        }
        else
        {
            auto dec = parseDeclaration();
            if (dec is null) return null;
            node.trueDeclarations ~= dec;
        }

        if(currentIs(TokenType.else_))
            advance();
        else
            return node;

        if (currentIs(TokenType.lBrace))
        {
            advance();
            do
            {
                auto dec = parseDeclaration();
                if (dec is null) return null;
                node.falseDeclarations ~= dec;
                if (!moreTokens() || currentIs(TokenType.rBrace))
                    break;
            }
            while (true);
            if (expect(TokenType.rBrace) is null) return null;
        }
        else
        {
            auto dec = parseDeclaration();
            if (dec is null) return null;
            node.falseDeclarations ~= dec;
        }
        return node;
    }

    /**
     * Parses an ConditionalStatement
     *
     * $(GRAMMAR $(RULEDEF conditionalStatement):
     *     $(RULE compileCondition) $(RULE statementNoCaseNoDefault) ($(LITERAL 'else') $(RULE statementNoCaseNoDefault))?
     *     ;)
     */
    ConditionalStatement parseConditionalStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new ConditionalStatement;
        node.compileCondition = parseCompileCondition();
        node.trueStatement = parseStatementNoCaseNoDefault();
        if (currentIs(TokenType.else_))
        {
            advance();
            node.falseStatement = parseStatementNoCaseNoDefault();
        }
        return node;
    }

    /**
     * Parses a Constraint
     *
     * $(GRAMMAR $(RULEDEF constraint):
     *     $(LITERAL 'if') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Constraint parseConstraint()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Constraint;
        if (expect(TokenType.if_) is null) return null;
        if (expect(TokenType.lParen) is null) return null;
        node.expression = parseExpression();
        if (expect(TokenType.rParen) is null) return null;
        return node;
    }

    /**
     * Parses a Constructor
     *
     * $(GRAMMAR $(RULEDEF constructor):
     *       $(LITERAL 'this') $(RULE templateParameters) $(RULE parameters) $(RULE memberFunctionAttribute)* $(RULE constraint)? $(RULE functionBody)
     *     | $(LITERAL 'this') $(RULE parameters) $(RULE memberFunctionAttribute)* ($(RULE functionBody) | $(LITERAL ';'))
     *     ;)
     */
    Constructor parseConstructor()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Constructor;
        if (expect(TokenType.this_) is null) return null;
        auto p = peekPastParens();
        bool isTemplate = false;
        if (p !is null && p.type == TokenType.lParen)
        {
            isTemplate = true;
            node.templateParameters = parseTemplateParameters();
        }
        node.parameters = parseParameters();

        while(moreTokens() && currentIsMemberFunctionAttribute())
            node.memberFunctionAttributes ~= parseMemberFunctionAttribute();

        if (isTemplate && currentIs(TokenType.if_))
            node.constraint = parseConstraint();

        if (isTemplate)
            node.functionBody = parseFunctionBody();
        else if (currentIs(TokenType.semicolon))
            advance();
        else
            node.functionBody = parseFunctionBody();

        return node;
    }

    /**
     * Parses an ContinueStatement
     *
     * $(GRAMMAR $(RULEDEF continueStatement):
     *     $(LITERAL 'continue') $(LITERAL Identifier)? $(LITERAL ';')
     *     ;)
     */
    ContinueStatement parseContinueStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        if (expect(TokenType.continue_) is null) return null;
        auto node = new ContinueStatement;
        switch (current.type)
        {
        case TokenType.identifier:
            node.identifier = advance();
            if (expect(TokenType.semicolon) is null) return null;
            break;
        case TokenType.semicolon:
            advance();
            break;
        default:
            error(`Identifier or semicolon expected following "continue"`);
            return null;
        }
        return node;
    }

    /**
     * Parses a DebugCondition
     *
     * $(GRAMMAR $(RULEDEF debugCondition):
     *     $(LITERAL 'debug') ($(LITERAL '$(LPAREN)') ($(LITERAL IntegerLiteral) | $(LITERAL Identifier)) $(LITERAL '$(RPAREN)'))?
     *     ;)
     */
    DebugCondition parseDebugCondition()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new DebugCondition;
        if (expect(TokenType.debug_) is null) return null;
        if (currentIs(TokenType.lParen))
        {
            advance();
            node.hasIdentifierOrInteger = true;
            if (currentIsOneOf(TokenType.intLiteral, TokenType.identifier))
                node.identifierOrInteger = advance();
            else
            {
                error(`Integer literal or identifier expected`);
                return null;
            }
            if (expect(TokenType.rParen) is null) return null;
        }
        else
            node.hasIdentifierOrInteger = false;
        return node;
    }

    /**
     * Parses a DebugSpecification
     *
     * $(GRAMMAR $(RULEDEF debugSpecification):
     *     $(LITERAL 'debug') $(LITERAL '=') ($(LITERAL Identifier) | $(LITERAL IntegerLiteral)) $(LITERAL ';')
     *     ;)
     */
    DebugSpecification parseDebugSpecification()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new DebugSpecification;
        if (expect(TokenType.debug_) is null) return null;
        if (expect(TokenType.assign) is null) return null;
        if (currentIsOneOf(TokenType.identifier, TokenType.intLiteral))
            node.identifierOrInteger = advance();
        else
        {
            error("Integer literal or identifier expected");
            return null;
        }
        if (expect(TokenType.semicolon) is null) return null;
        return node;
    }

    /**
     * Parses a Declaration
     *
     * $(GRAMMAR $(RULEDEF declaration):
     *       $(RULE aliasDeclaration)
     *     | $(RULE aliasThisDeclaration)
     *     | $(RULE attributedDeclaration)
     *     | $(RULE classDeclaration)
     *     | $(RULE conditionalDeclaration)
     *     | $(RULE constructor)
     *     | $(RULE destructor)
     *     | $(RULE enumDeclaration)
     *     | $(RULE functionDeclaration)
     *     | $(RULE importDeclaration)
     *     | $(RULE interfaceDeclaration)
     *     | $(RULE mixinDeclaration)
     *     | $(RULE pragmaDeclaration)
     *     | $(RULE sharedStaticConstructor)
     *     | $(RULE sharedStaticDestructor)
     *     | $(RULE staticAssertDeclaration)
     *     | $(RULE staticConstructor)
     *     | $(RULE staticDestructor)
     *     | $(RULE structDeclaration)
     *     | $(RULE templateDeclaration)
     *     | $(RULE unionDeclaration)
     *     | $(RULE unittest)
     *     | $(RULE variableDeclaration)
     *     | $(LITERAL '{') $(RULE declaration)+ $(LITERAL '}')
     *     ;)
     */
    Declaration parseDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Declaration;
        with (TokenType) switch (current.type)
        {
        case semicolon:
            // http://d.puremagic.com/issues/show_bug.cgi?id=4559
            warn("Empty declaration");
            advance();
            break;
        case lBrace:
            advance();
            while (moreTokens() && !currentIs(rBrace))
            {
                auto declaration = parseDeclaration();
                if (declaration !is null)
                    node.declarations ~= declaration;
            }
            expect(TokenType.rBrace);
            break;
        case alias_:
            if (startsWith(alias_, identifier, this_))
                node.aliasThisDeclaration = parseAliasThisDeclaration();
            else
                node.aliasDeclaration = parseAliasDeclaration();
            break;
        case class_:
            node.classDeclaration = parseClassDeclaration();
            break;
        case this_:
            node.constructor = parseConstructor();
            break;
        case tilde:
            node.destructor = parseDestructor();
            break;
        case enum_:
            if (peekIsOneOf(TokenType.lBrace, TokenType.colon))
                node.enumDeclaration = parseEnumDeclaration();
            else if (!peekIs(TokenType.identifier))
                goto storageClass;
            else
            {
                auto b = setBookmark();
                advance();
                assert (current.type == identifier);
                if (peekIs(TokenType.assign))
                {
                    trace("** 'enum identifier =' ");
                    goToBookmark(b);
                    node.variableDeclaration = parseVariableDeclaration();
                }
                else if (peekIsOneOf(TokenType.lBrace, TokenType.colon, TokenType.semicolon))
                {
                    trace("** 'enum identifier { : ;' ");
                    goToBookmark(b);
                    node.enumDeclaration = parseEnumDeclaration();
                }
                else
                {
                    trace("** something else");
                    goToBookmark(b);
                    goto storageClass;
                }
            }
            break;
        case import_:
            node.importDeclaration = parseImportDeclaration();
            break;
        case interface_:
            node.interfaceDeclaration = parseInterfaceDeclaration();
            break;
        case mixin_:
            node.mixinDeclaration = parseMixinDeclaration();
            break;
        case pragma_:
            node.pragmaDeclaration = parsePragmaDeclaration();
            break;
        case shared_:
            if (startsWith(shared_, static_, this_))
                node.sharedStaticConstructor = parseSharedStaticConstructor();
            else if (startsWith(shared_, static_, tilde))
                node.sharedStaticDestructor = parseSharedStaticDestructor();
            else if (peekIs(lParen))
                goto type;
            else
                node.attributedDeclaration = parseAttributedDeclaration();
            break;
        case static_:
            if (startsWith(static_, this_))
                node.staticConstructor = parseStaticConstructor();
            else if (startsWith(static_, tilde))
                node.staticDestructor = parseStaticDestructor();
            else if (startsWith(static_, if_))
                node.conditionalDeclaration = parseConditionalDeclaration();
            else if (startsWith(static_, assert_))
                node.staticAssertDeclaration = parseStaticAssertDeclaration();
            else
                node.attributedDeclaration = parseAttributedDeclaration();
            break;
        case struct_:
            node.structDeclaration = parseStructDeclaration();
            break;
        case template_:
            node.templateDeclaration = parseTemplateDeclaration();
            break;
        case union_:
            node.unionDeclaration = parseUnionDeclaration();
            break;
        case unittest_:
            node.unittest_ = parseUnittest();
            break;
        case typeof_:
        case bool_: .. case wchar_:
        case identifier:
        type:
            Type type = parseType();
            if (!currentIs(identifier))
            {
                error("Identifier expected");
                return null;
            }
            if (peekIs(lParen))
                node.functionDeclaration = parseFunctionDeclaration(type);
            else
                node.variableDeclaration = parseVariableDeclaration(type);
            break;
        case version_:
            if (peekIs(lParen))
                node.conditionalDeclaration = parseConditionalDeclaration();
            else if (peekIs(assign))
                node.versionSpecification = parseVersionSpecification();
            else
            {
                error(`"=" or "(" expected following "version"`);
                return null;
            }
            break;
        case debug_:
            node.conditionalDeclaration = parseConditionalDeclaration();
            break;
        case auto_:
            if (startsWith(auto_, ref_, identifier, lParen)
                    || startsWith(auto_, identifier, lParen))
                node.functionDeclaration = parseFunctionDeclaration();
            else
                goto storageClass;
            break;
        case ref_:
            node.functionDeclaration = parseFunctionDeclaration();
            break;
        case const_:
            if (startsWith(const_, identifier, assign))
                node.variableDeclaration = parseVariableDeclaration();
            else
                goto typeConstructor;
            break;
        case immutable_:
            if (startsWith(immutable_, identifier, assign))
                node.variableDeclaration = parseVariableDeclaration();
            else
                goto typeConstructor;
            break;
        case inout_:
        typeConstructor:
            if (peekIs(TokenType.lParen))
                goto type;
            else
                goto case;
        case at:
        case align_:
        case deprecated_:
        case private_:
        case package_:
        case protected_:
        case public_:
        case export_:
        case extern_:
        case final_:
        case synchronized_:
        case override_:
        case abstract_:
        case gshared:
        case pure_:
        case nothrow_:
        storageClass:
            node.attributedDeclaration = parseAttributedDeclaration();
            break;
        default:
            error("Declaration expected");
            advance();
            return null;
        }
        return node;
    }

    /**
     * Parses DeclarationsAndStatements
     *
     * $(GRAMMAR $(RULEDEF declarationsAndStatements):
     *     $(RULE declarationOrStatement)+
     *     ;
     * $(RULEDEF declarationOrStatement):
     *       $(RULE declaration)
     *     | $(RULE statementNoCaseNoDefault)
     *     ;)
     */
    DeclarationsAndStatements parseDeclarationsAndStatements()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new DeclarationsAndStatements;
        while (!currentIsOneOf(TokenType.rBrace) && moreTokens())
        {
            auto dos = parseDeclarationOrStatement();
            if (dos.statement !is null || dos.declaration !is null)
                node.declarationsAndStatements ~= dos;
            else
                return null;
        }
        return node;
    }

    /**
     * Parses a DeclarationOrInvariant
     *
     * $(GRAMMAR $(RULEDEF declarationOrInvariant):
     *       $(RULE declaration)
     *     | $(RULE invariant)
     *     ;)
     */
    DeclarationOrInvariant parseDeclarationOrInvariant()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new DeclarationOrInvariant;
        if (currentIs(TokenType.invariant_))
            node.invariant_ = parseInvariant();
        else
            node.declaration = parseDeclaration();
        return node;
    }

    /**
     * Parses a DeclarationOrStatement
     *
     * $(GRAMMAR $(RULEDEF declarationOrStatement):
     *       $(RULE declaration)
     *     | $(RULE statement)
     *     ;)
     */
    DeclarationOrStatement parseDeclarationOrStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new DeclarationOrStatement;
        // "Any ambiguities in the grammar between Statements and
        // Declarations are resolved by the declarations taking precedence."
        if (isDeclaration())
            node.declaration = parseDeclaration();
        else
            node.statement = parseStatementNoCaseNoDefault();
        return node;
    }

    /**
     * Parses a Declarator
     *
     * $(GRAMMAR $(RULEDEF declarator):
     *     $(LITERAL Identifier) ($(LITERAL '=') $(RULE initializer))?
     *     ;)
     */
    Declarator parseDeclarator()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Declarator;
        auto id = expect(TokenType.identifier);
        if (id is null) return null;
        node.identifier = *id;
        if (currentIsOneOf(TokenType.lBracket, TokenType.star))
        {
            error("C-style variable declarations are not supported.");
            return null;
        }
        if (currentIs(TokenType.assign))
        {
            advance();
            node.initializer = parseInitializer();
        }
        return node;
    }

    /**
     * Parses a DefaultStatement
     *
     * $(GRAMMAR $(RULEDEF defaultStatement):
     *     $(LITERAL 'default') $(LITERAL ':') $(RULE declarationsAndStatements)
     *     ;)
     */
    DefaultStatement parseDefaultStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new DefaultStatement;
        if (expect(TokenType.default_) is null) return null;
        if (expect(TokenType.colon) is null) return null;
        node.declarationsAndStatements = parseDeclarationsAndStatements();
        return node;
    }

    /**
     * Parses a DeleteExpression
     *
     * $(GRAMMAR $(RULEDEF deleteExpression):
     *     $(LITERAL 'delete') $(RULE unaryExpression)
     *     ;)
     */
    DeleteExpression parseDeleteExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new DeleteExpression;
        if (expect(TokenType.delete_) is null) return null;
        node.unaryExpression = parseUnaryExpression();
        return node;
    }

    /**
     * Parses a Deprecated attribute
     *
     * $(GRAMMAR $(RULEDEF deprecated):
     *     $(LITERAL 'deprecated') ($(LITERAL '$(LPAREN)') $(RULE assignExpression) $(LITERAL '$(RPAREN)'))?
     *     ;)
     */
    Deprecated parseDeprecated()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Deprecated;
        if (expect(TokenType.deprecated_) is null) return null;
        if (currentIs(TokenType.lParen))
        {
            advance();
            node.assignExpression = parseAssignExpression();
            if (expect(TokenType.rParen) is null) return null;
        }
        return node;
    }

    /**
     * Parses a Destructor
     *
     * $(GRAMMAR $(RULEDEF destructor):
     *     $(LITERAL '~') $(LITERAL 'this') $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)') $(RULE functionBody)
     *     ;)
     */
    Destructor parseDestructor()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Destructor;
        if (expect(TokenType.tilde) is null) return null;
        if (expect(TokenType.this_) is null) return null;
        if (expect(TokenType.lParen) is null) return null;
        if (expect(TokenType.rParen) is null) return null;
        node.functionBody = parseFunctionBody();
        return node;
    }

    unittest
    {
        auto sourceCode = q{~this(){}}c;
        Parser p = getParserForUnittest(sourceCode, "parseDestructor");
        Destructor d = p.parseDestructor();
        assert (d !is null);
        assert (d.functionBody !is null);
        assert (p.errorCount == 0);
        stderr.writeln("Unittest for parseDestructor() passed.");
    }

    /**
     * Parses a DoStatement
     *
     * $(GRAMMAR $(RULEDEF doStatement):
     *     $(LITERAL 'do') $(RULE nonEmptyStatementNoCaseNoDefault) $(LITERAL 'while') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)') $(LITERAL ';')
     *     ;)
     */
    DoStatement parseDoStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new DoStatement;
        if (expect(TokenType.do_) is null) return null;
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
        if (expect(TokenType.while_) is null) return null;
        if (expect(TokenType.lParen) is null) return null;
        node.expression = parseExpression();
        if (expect(TokenType.rParen) is null) return null;
        if (expect(TokenType.semicolon) is null) return null;
        return node;
    }

    /**
     * Parses an EnumBody
     *
     * $(GRAMMAR $(RULEDEF enumBody):
     *       $(LITERAL ';')
     *     | $(LITERAL '{') $(RULE enumMember) ($(LITERAL ',') $(RULE enumMember)?)* $(LITERAL '}')
     *     ;)
     */
    EnumBody parseEnumBody()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new EnumBody;
        if (!currentIs(TokenType.semicolon))
        {
            expect (TokenType.lBrace);
            while (moreTokens())
            {
                if (!currentIsOneOf(TokenType.comma, TokenType.rBrace))
                    node.enumMembers ~= parseEnumMember();
                else if (currentIs(TokenType.comma))
                {
                    advance();
                    continue;
                }
                else if (currentIs(TokenType.rBrace))
                    break;
                else
                {
                    error(`",", "}", or enum member expected`);
                    goto ret;
                }
            }
            expect (TokenType.rBrace);
        }
    ret:
        return node;
    }

    /**
     * Parses an EnumDeclaration
     *
     * $(GRAMMAR $(RULEDEF enumDeclaration):
     *     $(LITERAL 'enum') $(LITERAL Identifier)? ($(LITERAL ':') $(RULE type))? $(RULE enumBody)
     *     ;)
     */
    EnumDeclaration parseEnumDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new EnumDeclaration;
        if (expect(TokenType.enum_) is null) return null;
        if (currentIs(TokenType.identifier))
            node.identifier = advance();
        if (currentIs(TokenType.colon))
        {
            advance();
            node.type = parseType();
        }
        node.enumBody = parseEnumBody();
        return node;
    }

    /**
     * Parses an EnumMember
     *
     * $(GRAMMAR $(RULEDEF enumMember):
     *       $(LITERAL Identifier)
     *     | ($(LITERAL Identifier) | $(RULE type)) $(LITERAL '=') $(RULE assignExpression)
     *     ;)
     */
    EnumMember parseEnumMember()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new EnumMember;
        if (currentIs(TokenType.identifier))
        {
            if (peekIsOneOf(TokenType.comma, TokenType.rBrace))
                node.identifier = advance();
            else if (peekIs(TokenType.assign))
            {
                node.identifier = advance();
                goto assign;
            }
            else
                goto type;
        }
        else
        {
    type:
            node.type = parseType();
    assign:
            expect(TokenType.assign);
            node.assignExpression = parseAssignExpression();
        }
        return node;
    }

    /**
     * Parses an EqualExpression
     *
     * $(GRAMMAR $(RULEDEF equalExpression):
     *     $(RULE shiftExpression) ($(LITERAL '==') | $(LITERAL '!=')) $(RULE shiftExpression)
     *     ;)
     */
    EqualExpression parseEqualExpression(ShiftExpression shift = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new EqualExpression;
        node.left = shift is null ? parseShiftExpression() : shift;
        if (currentIsOneOf(TokenType.equal, TokenType.notEqual))
            node.operator = advance().type;
        node.right = parseShiftExpression();
        return node;
    }

    /**
     * Parses an Expression
     *
     * $(GRAMMAR $(RULEDEF expression):
     *     $(RULE assignExpression) ($(LITERAL ',') $(RULE assignExpression))*
     *     ;)
     */
    Expression parseExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseCommaSeparatedRule!(Expression, AssignExpression)();
    }

    /**
     * Parses an ExpressionStatement
     *
     * $(GRAMMAR $(RULEDEF expressionStatement):
     *     $(RULE expression) $(LITERAL ';')
     *     ;)
     */
    ExpressionStatement parseExpressionStatement(Expression expression = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new ExpressionStatement;
        node.expression = expression is null ? parseExpression() : expression;
        if (expect(TokenType.semicolon) is null) return null;
        return node;
    }

    /**
     * Parses a FinalSwitchStatement
     *
     * $(GRAMMAR $(RULEDEF finalSwitchStatement):
     *     $(LITERAL 'final') $(RULE switchStatement)
     *     ;)
     */
    FinalSwitchStatement parseFinalSwitchStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new FinalSwitchStatement;
        if (expect(TokenType.final_) is null) return null;
        node.switchStatement = parseSwitchStatement();
        if (node.switchStatement is null) return null;
        return node;
    }

    /**
     * Parses a Finally
     *
     * $(GRAMMAR $(RULEDEF finally):
     *     $(LITERAL 'finally') $(RULE statementNoCaseNoDefault)
     *     ;)
     */
    Finally parseFinally()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Finally;
        if (expect(TokenType.finally_) is null) return null;
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
        return node;
    }

    /**
     * Parses a ForStatement
     *
     * $(GRAMMAR $(RULEDEF forStatement):
     *     $(LITERAL 'for') $(LITERAL '$(LPAREN)') $(RULE declarationOrStatement) $(RULE expression)? $(LITERAL ';') $(RULE expression)? $(LITERAL '$(RPAREN)') $(RULE statementNoCaseNoDefault)
     *     ;)
     */
    ForStatement parseForStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new ForStatement;
        if (expect(TokenType.for_) is null) return null;
        if (expect(TokenType.lParen) is null) return null;
        node.declarationOrStatement = parseDeclarationOrStatement();
        if (currentIs(TokenType.semicolon))
            advance();
        else
            node.test = parseExpression();
        expect(TokenType.semicolon);
        if (!currentIs(TokenType.rParen))
            node.increment = parseExpression();
        if (expect(TokenType.rParen) is null) return null;
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
        if (node.statementNoCaseNoDefault is null) return null;
        return node;
    }

    /**
     * Parses a ForeachStatement
     *
     * $(GRAMMAR $(RULEDEF foreachStatement):
     *       ($(LITERAL 'foreach') | $(LITERAL 'foreach_reverse')) $(LITERAL '$(LPAREN)') $(RULE foreachTypeList) $(LITERAL ';') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE nonEmptyStatementNoCaseNoDefault)
     *     | ($(LITERAL 'foreach') | $(LITERAL 'foreach_reverse')) $(LITERAL '$(LPAREN)') $(RULE foreachType) $(LITERAL ';') $(RULE expression) $(LITERAL '..') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    ForeachStatement parseForeachStatement()
    {
        auto node = new ForeachStatement;

        if (currentIsOneOf(TokenType.foreach_, TokenType.foreach_reverse_))
            node.foreachType = advance().type;
        else
        {
            error(`"foreach" or "foreach_reverse" expected`);
            return null;
        }
        if (expect(TokenType.lParen) is null) return null;
        auto feType = parseForeachTypeList();
        bool canBeRange = feType.items.length == 1;
        expect(TokenType.semicolon);
        node.low = parseExpression();
        if (node.low is null) return null;
        if (currentIs(TokenType.slice))
        {
            if (!canBeRange)
            {
                error(`Cannot have more than one foreach varible for a foreach range statement`);
                return null;
            }
            advance();
            node.high = parseExpression();
            if (node.high is null) return null;
        }
        if (expect(TokenType.rParen) is null) return null;
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
        if (node.statementNoCaseNoDefault is null) return null;
        return node;
    }

    /**
     * Parses a ForeachType
     *
     * $(GRAMMAR $(RULEDEF foreachType):
     *     $(RULE typeConstructors)? $(RULE type)? $(LITERAL Identifier)
     *     ;)
     */
    ForeachType parseForeachType()
    {
        auto node = new ForeachType;
        if (currentIsOneOf(TokenType.ref_, TokenType.const_, TokenType.immutable_,
            TokenType.shared_, TokenType.inout_))
        {
            if ((node.typeConstructors = parseTypeConstructors()) is null)
                return null;
        }
        if (currentIs(TokenType.identifier) && peekIsOneOf(TokenType.comma, TokenType.semicolon))
        {
            node.identifier = advance();
            return node;
        }
        if ((node.type = parseType()) is null) return null;
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.identifier = *ident;
        return node;
    }

    /**
     * Parses a ForeachTypeList
     *
     * $(GRAMMAR $(RULEDEF foreachTypeList):
     *     $(RULE foreachType) ($(LITERAL ',') $(RULE foreachType))*
     *     ;)
     */
    ForeachTypeList parseForeachTypeList()
    {
        return parseCommaSeparatedRule!(ForeachTypeList, ForeachType)();
    }

    /**
     * Parses a FunctionAttribute
     *
     * $(GRAMMAR $(RULEDEF functionAttribute):
     *       $(RULE atAttribute)
     *     | $(LITERAL 'pure')
     *     | $(LITERAL 'nothrow')
     *     ;)
     */
    FunctionAttribute parseFunctionAttribute(bool validate = true)
    {
        auto node = new FunctionAttribute;
        with (TokenType) switch (current.type)
        {
        case at:
            node.atAttribute = parseAtAttribute();
            break;
        case pure_:
        case nothrow_:
            node.token = advance();
            break;
        default:
            if (validate)
                error(`@attribute, "pure", or "nothrow" expected`);
            return null;
        }
        return node;
    }

    /**
     * Parses a FunctionBody
     *
     * $(GRAMMAR $(RULEDEF functionBody):
     *       $(RULE blockStatement)
     *     | ($(RULE inStatement) | $(RULE outStatement) | $(RULE outStatement) $(RULE inStatement) | $(RULE inStatement) $(RULE outStatement))? $(RULE bodyStatement)
     *     ;)
     */
    FunctionBody parseFunctionBody()
    {
        auto node = new FunctionBody;
        if (currentIs(TokenType.lBrace))
            node.blockStatement = parseBlockStatement();
        else
        {
            if (currentIs(TokenType.in_))
            {
                node.inStatement = parseInStatement();
                if (currentIs(TokenType.out_))
                    node.outStatement = parseOutStatement();
            }
            else if (currentIs(TokenType.out_))
            {
                node.outStatement = parseOutStatement();
                if (currentIs(TokenType.in_))
                    node.inStatement = parseInStatement();
            }
            node.bodyStatement = parseBodyStatement();
        }
        return node;
    }

    unittest
    {
        auto sourceCode = q{
{} // one
in {} body{} // two
out {} body{} // three
in {} out {} body {} // four
out {} in {} body {} // five
body {} // six
        };

        Parser p = getParserForUnittest(sourceCode, "parseFunctionBody");

        FunctionBody functionBodyOne = p.parseFunctionBody();
        assert (functionBodyOne.blockStatement !is null);

        FunctionBody functionBodyTwo = p.parseFunctionBody();
        assert (functionBodyTwo.blockStatement is null);
        assert (functionBodyTwo.inStatement !is null);
        assert (functionBodyTwo.outStatement is null);
        assert (functionBodyTwo.bodyStatement !is null);

        FunctionBody functionBodyThree = p.parseFunctionBody();
        assert (functionBodyThree.blockStatement is null);
        assert (functionBodyThree.inStatement is null);
        assert (functionBodyThree.outStatement !is null);
        assert (functionBodyThree.bodyStatement !is null);

        FunctionBody functionBodyFour = p.parseFunctionBody();
        assert (functionBodyFour.blockStatement is null);
        assert (functionBodyFour.inStatement !is null);
        assert (functionBodyFour.outStatement !is null);
        assert (functionBodyFour.bodyStatement !is null);

        FunctionBody functionBodyFive = p.parseFunctionBody();
        assert (functionBodyFive.blockStatement is null);
        assert (functionBodyFive.inStatement !is null);
        assert (functionBodyFive.outStatement !is null);
        assert (functionBodyFive.bodyStatement !is null);

        FunctionBody functionBodySix = p.parseFunctionBody();
        assert (functionBodySix.blockStatement is null);
        assert (functionBodySix.inStatement is null);
        assert (functionBodySix.outStatement is null);
        assert (functionBodySix.bodyStatement !is null);

        stderr.writeln("Unittest for parseFunctionBody() passed.");
    }

    /**
     * Parses a FunctionCallExpression
     *
     * $(GRAMMAR $(RULEDEF functionCallExpression):
     *     $(RULE unaryExpression) $(RULE templateArguments)? $(RULE arguments)
     *     ;)
     */
    FunctionCallExpression parseFunctionCallExpression(UnaryExpression unary = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new FunctionCallExpression;
        node.unaryExpression = unary is null ? parseUnaryExpression() : unary;
        if (currentIs(TokenType.not))
            node.templateArguments = parseTemplateArguments();
        node.arguments = parseArguments();
        return node;
    }

    /**
     * Parses a FunctionCallStatement
     *
     * $(GRAMMAR $(RULEDEF functionCallStatement):
     *     $(RULE functionCallExpression) $(LITERAL ';')
     *     ;)
     */
    FunctionCallStatement parseFunctionCallStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new FunctionCallStatement;
        node.functionCallExpression = parseFunctionCallExpression();
        if (expect(TokenType.semicolon) is null) return null;
        return node;
    }

    /**
     * Parses a FunctionDeclaration
     *
     * $(GRAMMAR $(RULEDEF functionDeclaration):
     *       $(RULE memberFunctionAttribute)* ($(RULE type) | $(LITERAL 'auto') $(LITERAL 'ref')? | $(LITERAL 'ref') $(LITERAL 'auto')?) $(LITERAL Identifier) $(RULE templateParameters) $(RULE parameters) $(RULE memberFunctionAttribute)* $(RULE constraint)? $(RULE functionBody)
     *     | $(RULE memberFunctionAttribute)* ($(RULE type) | $(LITERAL 'auto') $(LITERAL 'ref')? | $(LITERAL 'ref') $(LITERAL 'auto')?) $(LITERAL Identifier) $(RULE parameters) $(RULE memberFunctionAttribute)* ($(RULE functionBody) | $(LITERAL ';'))
     *     ;)
     */
    FunctionDeclaration parseFunctionDeclaration(Type type = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new FunctionDeclaration;

        while(moreTokens() && currentIsMemberFunctionAttribute())
            node.memberFunctionAttributes ~= parseMemberFunctionAttribute();

        with (TokenType) switch (current.type)
        {
        case auto_:
            advance();
            node.hasAuto = true;
            if (currentIs(ref_))
            {
                node.hasRef = true;
                advance();
            }
            break;
        case ref_:
            advance();
            node.hasRef = true;
            if (currentIs(auto_))
            {
                node.hasAuto = true;
                advance();
                break;
            }
            else if (startsWith(identifier, lParen))
                break;
            else
                goto default;
        default:
            node.returnType = type is null ? parseType() : type;
            break;
        }

        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;

        node.name = *ident;

        if (!currentIs(TokenType.lParen))
        {
            error(`"(" expected`);
            return null;
        }

        auto p = peekPastParens();
        bool isTemplate = p !is null && p.type == TokenType.lParen;

        if (isTemplate)
            node.templateParameters = parseTemplateParameters();

        node.parameters = parseParameters();

        while(moreTokens() && currentIsMemberFunctionAttribute())
            node.memberFunctionAttributes ~= parseMemberFunctionAttribute();

        if (isTemplate && currentIs(TokenType.if_))
            node.constraint = parseConstraint();

        if (isTemplate)
            node.functionBody = parseFunctionBody();
        else if (currentIs(TokenType.semicolon))
            advance();
        else
            node.functionBody = parseFunctionBody();

        return node;
    }

    /**
     * Parses a FunctionLiteralExpression
     *
     * $(GRAMMAR $(RULEDEF functionLiteralExpression):
     *     (($(LITERAL 'function') | $(LITERAL 'delegate')) $(RULE type)?)? ($(RULE parameters) $(RULE functionAttribute)*)? $(RULE functionBody)
     *     ;)
     */
    FunctionLiteralExpression parseFunctionLiteralExpression()
    {
        auto node = new FunctionLiteralExpression;
        if (currentIsOneOf(TokenType.function_, TokenType.delegate_))
        {
            node.functionOrDelegate = advance().type;
            if (!currentIsOneOf(TokenType.lParen, TokenType.in_, TokenType.body_,
                TokenType.out_, TokenType.rBrace))
            {
                node.type = parseType();
                if (node.type is null) return null;
            }
        }
        if (currentIs(TokenType.lParen))
        {
            node.parameters = parseParameters();
            if (node.parameters is null) return null;
            do
            {
                auto attr = parseFunctionAttribute(false);
                if (attr is null)
                    break;
                else
                    node.functionAttributes ~= attr;
            } while (true);
        }
        node.functionBody = parseFunctionBody();
        if (node.functionBody is null) return null;
        return node;
    }

    /**
     * Parses a GotoStatement
     *
     * $(GRAMMAR $(RULEDEF gotoStatement):
     *     $(LITERAL 'goto') ($(LITERAL Identifier) | $(LITERAL 'default') | $(LITERAL 'case') $(RULE expression)?) $(LITERAL ';')
     *     ;)
     */
    GotoStatement parseGotoStatement()
    {
        auto node = new GotoStatement;
        if (expect(TokenType.goto_) is null) return null;
        with (TokenType) switch (current.type)
        {
        case identifier:
        case default_:
            node.token = advance();
            break;
        case case_:
            node.token = advance();
            node.expression = parseExpression();
            break;
        default:
            error(`Identifier, "default", or "case" expected`);
            return null;
        }
        if (expect(TokenType.semicolon) is null) return null;
        return node;
    }

    /**
     * Parses an IdentifierChain
     *
     * $(GRAMMAR $(RULEDEF identifierChain):
     *     $(LITERAL Identifier) ($(LITERAL '.') $(LITERAL Identifier))*
     *     ;)
     */
    IdentifierChain parseIdentifierChain()
    {
        auto node = new IdentifierChain;
        while (true)
        {
            auto ident = expect(TokenType.identifier);
            if (ident is null) return null;
            node.identifiers ~= *ident;
            if (currentIs(TokenType.dot))
            {
                advance();
                continue;
            }
            else
                break;
        }
        return node;
    }

    /**
     * Parses an IdentifierList
     *
     * $(GRAMMAR $(RULEDEF identifierList):
     *     $(LITERAL Identifier) ($(LITERAL ',') $(LITERAL Identifier))*
     *     ;)
     */
    IdentifierList parseIdentifierList()
    {
        auto node = new IdentifierList;
        do
        {
            auto ident = expect(TokenType.identifier);
            if (ident is null) return null;
            node.identifiers ~= *ident;
            if (currentIs(TokenType.comma))
            {
                advance();
                continue;
            }
            else
                break;
        } while (true);
        return node;
    }

    /**
     * Parses an IdentifierOrTemplateChain
     *
     * $(GRAMMAR $(RULEDEF identifierOrTemplateChain):
     *     $(RULE identifierOrTemplateInstance) ($(LITERAL '.') $(RULE identifierOrTemplateInstance))*
     *     ;)
     */
    IdentifierOrTemplateChain parseIdentifierOrTemplateChain()
    {
        auto node = new IdentifierOrTemplateChain;
        while (true)
        {
            node.identifierOrTemplateInstances ~= parseIdentifierOrTemplateInstance();
            if (!currentIs(TokenType.dot))
                break;
            else
                advance();
        }
        return node;
    }

    /**
     * Parses an IdentifierOrTemplateInstance
     *
     * $(GRAMMAR $(RULEDEF identifierOrTemplateInstance):
     *       $(LITERAL Identifier)
     *     | $(RULE templateInstance)
     *     ;)
     */
    IdentifierOrTemplateInstance parseIdentifierOrTemplateInstance()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new IdentifierOrTemplateInstance;
        if (peekIs(TokenType.not) && !startsWith(TokenType.identifier,
            TokenType.not, TokenType.is_))
        {
            node.templateInstance = parseTemplateInstance();
        }
        else
        {
            auto ident = expect(TokenType.identifier);
            if (ident is null) return null;
            node.identifier = *ident;
        }
        return node;
    }

    /**
     * Parses an IdentityExpression
     *
     * $(GRAMMAR $(RULEDEF identityExpression):
     *     $(RULE shiftExpression) ($(LITERAL 'is') | $(LITERAL '!') $(LITERAL 'is')) $(RULE shiftExpression)
     *     ;)
     */
    IdentityExpression parseIdentityExpression(ShiftExpression shift = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new IdentityExpression;
        node.left = shift is null ? parseShiftExpression() : shift;
        if (currentIs(TokenType.not))
        {
            advance();
            node.negated = true;
        }
        if (expect(TokenType.is_) is null) return null;
        node.right = parseShiftExpression();
        return node;
    }

    /**
     * Parses an IfStatement
     *
     * $(GRAMMAR $(RULEDEF ifStatement):
     *     $(LITERAL 'if') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE statementNoCaseNoDefault) ($(LITERAL 'else') $(RULE statementNoCaseNoDefault))?
     *     ;)
     */
    IfStatement parseIfStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new IfStatement;
        if (expect(TokenType.if_) is null) return null;
        if (expect(TokenType.lParen) is null) return null;
        node.expression = parseExpression();
        if (expect(TokenType.rParen) is null) return null;
        node.thenStatement = parseStatementNoCaseNoDefault();
        if (currentIs(TokenType.else_))
        {
            advance();
            node.elseStatement = parseStatementNoCaseNoDefault();
        }
        return node;
    }

    /**
     * Parses an ImportBind
     *
     * $(GRAMMAR $(RULEDEF importBind):
     *     $(LITERAL Identifier) ($(LITERAL '=') $(LITERAL Identifier))?
     *     ;)
     */
    ImportBind parseImportBind()
    {
        auto node = new ImportBind;
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.left = *ident;
        if (currentIs(TokenType.assign))
        {
            advance();
            node.hasRight = true;
            auto id = expect(TokenType.identifier);
            if (id is null) return null;
            node.right = *id;
        }
        return node;
    }

    /**
     * Parses ImportBindings
     *
     * $(GRAMMAR $(RULEDEF importBindings):
     *     $(RULE singleImport) $(LITERAL ':') $(RULE importBind) ($(LITERAL ',') $(RULE importBind))*
     *     ;)
     */
    ImportBindings parseImportBindings(SingleImport singleImport)
    {
        auto node = new ImportBindings;
        node.singleImport = singleImport is null ? parseSingleImport() : singleImport;
        if (expect(TokenType.colon) is null) return null;
        while (true)
        {
            node.importBinds ~= parseImportBind();
            if (currentIs(TokenType.comma))
                advance();
            else
                break;
        }
        return node;
    }

    /**
     * Parses an ImportDeclaration
     *
     * $(GRAMMAR $(RULEDEF importDeclaration):
     *       $(LITERAL 'import') $(RULE singleImport) ($(LITERAL ',') $(RULE singleImport))* ($(LITERAL ',') $(RULE importBindings))? $(LITERAL ';')
     *     | $(LITERAL 'import') $(RULE importBindings) $(LITERAL ';')
     *     ;)
     */
    ImportDeclaration parseImportDeclaration()
    {
        auto node = new ImportDeclaration;
        if (expect(TokenType.import_) is null) return null;
        SingleImport si = parseSingleImport();
        if (currentIs(TokenType.colon))
            node.importBindings = parseImportBindings(si);
        else
        {
            node.singleImports ~= si;
            if (currentIs(TokenType.comma))
            {
                advance();
                while (true)
                {
                    auto single = parseSingleImport();
                    if (currentIs(TokenType.colon))
                    {
                        node.importBindings = parseImportBindings(single);
                        break;
                    }
                    else
                    {
                        node.singleImports ~= single;
                        if (currentIs(TokenType.comma))
                            advance();
                        else
                            break;
                    }
                }
            }
        }
        if (expect(TokenType.semicolon) is null) return null;
        return node;
    }

    unittest
    {
        auto sourceCode =
q{import std.stdio;
import foo, bar;
import io = std.stdio;
import std.stdio: writefln, foo = writef;
import io = std.stdio : foo = writefln;
import foo, bar, baz;
import core.stdc.stdio, std.string : KeepTerminator;
}c;

        Parser p = getParserForUnittest(sourceCode, "parseImportDeclaration");

        ImportDeclaration one = p.parseImportDeclaration();
        assert (one !is null);
        assert (one.singleImports.length == 1);
        assert (p.errorCount == 0);

        ImportDeclaration two = p.parseImportDeclaration();
        assert (two !is null);
        assert (two.singleImports.length == 2);
        assert (p.errorCount == 0);

        ImportDeclaration three = p.parseImportDeclaration();
        assert (three !is null);
        assert (three.singleImports.length == 1);
        assert (p.errorCount == 0);

        ImportDeclaration four = p.parseImportDeclaration();
        assert (four !is null);
        assert (four.importBindings !is null);
        assert (four.importBindings.importBinds.length == 2);
        assert (p.errorCount == 0);

        ImportDeclaration five = p.parseImportDeclaration();
        assert (five !is null);
        assert (p.errorCount == 0);

        ImportDeclaration six = p.parseImportDeclaration();
        assert (six !is null);
        assert (six.singleImports.length == 3);
        assert (p.errorCount == 0);

        ImportDeclaration seven = p.parseImportDeclaration();
        assert (seven !is null);
        assert (seven.singleImports.length == 1);
        assert (seven.importBindings !is null);
        assert (p.errorCount == 0);

        stderr.writeln("Unittest for parseImportDeclaration() passed.");
    }

    /**
     * Parses an ImportExpression
     *
     * $(GRAMMAR $(RULEDEF importExpression):
     *     $(LITERAL 'import') $(LITERAL '$(LPAREN)') $(RULE assignExpression) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    ImportExpression parseImportExpression()
    {
        auto node = new ImportExpression;
        if (expect(TokenType.import_) is null) return null;
        if (expect(TokenType.lParen) is null) return null;
        node.assignExpression = parseAssignExpression();
        if (expect(TokenType.rParen) is null) return null;
        return node;
    }

    /**
     * Parses an IndexExpression
     *
     * $(GRAMMAR $(RULEDEF indexExpression):
     *     $(RULE unaryExpression) $(LITERAL '[') $(RULE argumentList) $(LITERAL ']')
     *     ;)
     */
    IndexExpression parseIndexExpression(UnaryExpression unaryExpression = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new IndexExpression;
        node.unaryExpression = unaryExpression is null ? parseUnaryExpression() : unaryExpression;
        if (expect(TokenType.lBracket) is null) return null;
        node.argumentList = parseArgumentList();
        if (expect(TokenType.rBracket) is null) return null;
        return node;
    }

    /**
     * Parses an InExpression
     *
     * $(GRAMMAR $(RULEDEF inExpression):
     *     $(RULE shiftExpression) ($(LITERAL 'in') | $(LITERAL '!') $(LITERAL 'in')) $(RULE shiftExpression)
     *     ;)
     */
    InExpression parseInExpression(ShiftExpression shift = null)
    {
        auto node = new InExpression;
        node.left = shift is null ? parseShiftExpression() : shift;
        if (currentIs(TokenType.not))
            advance();
        if (expect(TokenType.in_) is null) return null;
        node.right = parseShiftExpression();
        return node;
    }

    /**
     * Parses an InStatement
     *
     * $(GRAMMAR $(RULEDEF inStatement):
     *     $(LITERAL 'in') $(RULE blockStatement)
     *     ;)
     */
    InStatement parseInStatement()
    {
        auto node = new InStatement;
        if (expect(TokenType.in_) is null) return null;
        node.blockStatement = parseBlockStatement();
        return node;
    }

    /**
     * Parses an Initialize
     *
     * $(GRAMMAR $(RULEDEF initialize):
     *       $(LITERAL ';')
     *     | $(RULE statementNoCaseNoDefault)
     *     ;)
     */
    Initialize parseInitialize()
    {
        auto node = new Initialize;
        if (!currentIs(TokenType.semicolon))
        {
            node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
            if (node.statementNoCaseNoDefault is null) return null;
        }
        else if (expect(TokenType.semicolon) is null)
            return null;
        return node;
    }

    /**
     * Parses an Initializer
     *
     * $(GRAMMAR $(RULEDEF initializer):
     *       $(LITERAL 'void')
     *     | $(RULE nonVoidInitializer)
     *     ;)
     */
    Initializer parseInitializer()
    {
        auto node = new Initializer;
        if (currentIs(TokenType.void_))
            advance();
        else
            node.nonVoidInitializer = parseNonVoidInitializer();
        return node;
    }

    /**
     * Parses an InterfaceDeclaration
     *
     * $(GRAMMAR $(RULEDEF interfaceDeclaration):
     *     $(LITERAL 'interface') $(LITERAL Identifier) ($(RULE templateParameters) $(RULE constraint)?)? ($(LITERAL ':') $(RULE baseClassList))? $(RULE structBody)
     *     ;)
     */
    InterfaceDeclaration parseInterfaceDeclaration()
    {
        auto node = new InterfaceDeclaration;
        if (expect(TokenType.interface_) is null) return null;
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.identifier = *ident;
        if (currentIs(TokenType.lParen))
        {
            node.templateParameters = parseTemplateParameters();
            if (currentIs(TokenType.if_))
                node.constraint = parseConstraint();
        }
        if (currentIs(TokenType.colon))
        {
            advance();
            node.baseClassList = parseBaseClassList();
        }
        node.structBody = parseStructBody();
        return node;
    }

    unittest
    {
        auto sourceCode =
q{interface One {}
interface Two : Number {}
interface Three(T) if (someTest(T)) {}
interface "Four"
}c;

        Parser p = getParserForUnittest(sourceCode, "parseInterfaceDeclaration");

        InterfaceDeclaration one = p.parseInterfaceDeclaration();
        assert (one !is null);
        assert (one.identifier == "One");
        assert (one.constraint is null);
        assert (one.templateParameters is null);
        assert (one.structBody !is null);
        assert (one.baseClassList is null);
        assert (p.errorCount == 0);

        InterfaceDeclaration two = p.parseInterfaceDeclaration();
        assert (two !is null);
        assert (two.identifier == "Two");
        assert (two.constraint is null);
        assert (two.templateParameters is null);
        assert (two.structBody !is null);
        assert (two.baseClassList !is null);
        assert (p.errorCount == 0);

        InterfaceDeclaration three = p.parseInterfaceDeclaration();
        assert (three !is null);
        assert (three.identifier == "Three");
        assert (three.constraint !is null);
        assert (three.templateParameters !is null);
        assert (three.structBody !is null);
        assert (three.baseClassList is null);
        assert (p.errorCount == 0);

        InterfaceDeclaration four = p.parseInterfaceDeclaration();
        assert (four is null);
        assert (p.errorCount > 0);

        stderr.writeln("Unittest for parseInterfaceDeclaration() passed.");
    }

    /**
     * Parses an Invariant
     *
     * $(GRAMMAR $(RULEDEF invariant):
     *     $(LITERAL 'invariant') ($(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)'))? $(RULE blockStatement)
     *     ;)
     */
    Invariant parseInvariant()
    {
        auto node = new Invariant;
        if (expect(TokenType.invariant_) is null) return null;
        if (currentIs(TokenType.lParen))
        {
            advance();
            if (expect(TokenType.rParen) is null) return null;
        }
        if ((node.blockStatement = parseBlockStatement()) is null) return null;
        return node;
    }

    unittest
    {
        auto sourceCode =
q{invariant() {}
invariant{}
invariant() foo();
}c;

        Parser p = getParserForUnittest(sourceCode, "parseInvariant");

        auto inv1 = p.parseInvariant();
        assert (inv1 !is null);
        assert (inv1.blockStatement !is null);
        assert (p.errorCount == 0);

        auto inv2 = p.parseInvariant();
        assert (inv2 !is null);
        assert (inv2.blockStatement !is null);
        assert (p.errorCount == 0);

        auto inv3 = p.parseInvariant();
        assert (inv3 is null);
        assert (p.errorCount > 0);
    }

    /**
     * Parses an IsExpression
     *
     * $(GRAMMAR $(RULEDEF isExpression):
     *     $(LITERAL'is') $(LITERAL '$(LPAREN)') ($(RULE type) $(LITERAL Identifier)? (($(LITERAL ':') | $(LITERAL '==')) $(RULE typeSpecialization) ($(LITERAL ',') $(RULE templateParameterList))?)?)) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    IsExpression parseIsExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new IsExpression;
        if (expect(TokenType.is_) is null) return null;
        if (expect(TokenType.lParen) is null) return null;
        node.type = parseType();
        if (node.type is null) return null;
        if (currentIs(TokenType.identifier))
            node.identifier = advance();
        if (currentIsOneOf(TokenType.equal, TokenType.colon))
        {
            node.equalsOrColon = advance().type;
            node.typeSpecialization = parseTypeSpecialization();
            if (currentIs(TokenType.comma))
            {
                advance();
                node.templateParameterList = parseTemplateParameterList();
            }
        }
        if (expect(TokenType.rParen) is null) return null;
        return node;
    }

    /**
     * Parses a KeyValuePair
     *
     * $(GRAMMAR $(RULEDEF keyValuePair):
     *     $(RULE assignExpression) $(LITERAL ':') $(RULE assignExpression)
     *     ;)
     */
    KeyValuePair parseKeyValuePair()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new KeyValuePair;
        node.key = parseAssignExpression();
        if (expect(TokenType.colon) is null) return null;
        node.value = parseAssignExpression();
        return node;
    }

    /**
     * Parses KeyValuePairs
     *
     * $(GRAMMAR $(RULEDEF keyValuePairs):
     *     $(RULE keyValuePair) ($(LITERAL ',') $(RULE keyValuePair))*
     *     ;)
     */
    KeyValuePairs parseKeyValuePairs()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new KeyValuePairs;
        while (true)
        {
            auto kvPair = parseKeyValuePair();
            if (kvPair !is null)
                node.keyValuePairs ~= kvPair;
            if (currentIs(TokenType.comma))
                advance();
            else
                break;
        }
        return node;
    }

    /**
     * Parses a LabeledStatement
     *
     * $(GRAMMAR $(RULEDEF labeledStatement):
     *     $(LITERAL Identifier) $(LITERAL ':') $(RULE statement)
     *     ;)
     */
    LabeledStatement parseLabeledStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new LabeledStatement;
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.identifier = *ident;
        expect(TokenType.colon);
        node.statement = parseStatement();
        return node;
    }

    /**
     * Parses a LambdaExpression
     *
     * $(GRAMMAR $(RULEDEF lambdaExpression):
     *     ($(LITERAL Identifier) | $(RULE parameters) $(RULE functionAttribute)* ) $(LITERAL '=>') $(RULE assignExpression)
     *     ;)
     */
    LambdaExpression parseLambdaExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new LambdaExpression;
        if (currentIs(TokenType.identifier))
            node.identifier = advance();
        else if (currentIs(TokenType.lParen))
        {
            node.parameters = parseParameters();
            do
            {
                auto attribute = parseFunctionAttribute(false);
                if (attribute is null)
                    break;
                node.functionAttributes ~= attribute;
            }
            while (moreTokens());
        }
        else
        {
            error(`Identifier or argument list expected`);
            return null;
        }

        if (expect(TokenType.goesTo) is null) return null;

        if ((node.assignExpression = parseAssignExpression()) is null)
            return null;

        return node;
    }

    /**
     * Parses a LastCatch
     *
     * $(GRAMMAR $(RULEDEF lastCatch):
     *     $(LITERAL 'catch') $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    LastCatch parseLastCatch()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new LastCatch;
        if (expect(TokenType.catch_) is null) return null;
        if ((node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault()) is null)
            return null;
        return node;
    }

    /**
     * Parses a LinkageAttribute
     *
     * $(GRAMMAR $(RULEDEF linkageAttribute):
     *     $(LITERAL 'extern') $(LITERAL '$(LPAREN)') $(LITERAL Identifier) $(LITERAL '++')? $(LITERAL '$(RPAREN)')
     *     ;)
     */
    LinkageAttribute parseLinkageAttribute()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new LinkageAttribute;
        expect(TokenType.extern_);
        expect(TokenType.lParen);
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.identifier = *ident;
        if (currentIs(TokenType.increment))
        {
            advance();
            node.hasPlusPlus = true;
        }
        expect(TokenType.rParen);
        return node;
    }

    /**
     * Parses a MemberFunctionAttribute
     *
     * $(GRAMMAR $(RULEDEF memberFunctionAttribute):
     *       $(RULE functionAttribute)
     *     | $(LITERAL 'immutable')
     *     | $(LITERAL 'inout')
     *     | $(LITERAL 'shared')
     *     | $(LITERAL 'const')
     *     ;)
     */
    MemberFunctionAttribute parseMemberFunctionAttribute()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new MemberFunctionAttribute;
        with (TokenType) switch (current.type)
        {
        case at:
            node.atAttribute = parseAtAttribute();
            break;
        case immutable_:
        case inout_:
        case shared_:
        case const_:
        case pure_:
        case nothrow_:
            node.tokenType = advance().type;
            break;
        default:
            error(`Member funtion attribute expected`);
        }
        return node;
    }

    /**
     * Parses a MixinDeclaration
     *
     * $(GRAMMAR $(RULEDEF mixinDeclaration):
     *     $(RULE mixinExpression) $(LITERAL ';')
     *     ;)
     */
    MixinDeclaration parseMixinDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new MixinDeclaration;
        node.mixinExpression = parseMixinExpression();
        expect(TokenType.semicolon);
        return node;
    }

    /**
     * Parses a MixinExpression
     *
     * $(GRAMMAR $(RULEDEF mixinExpression):
     *     $(LITERAL 'mixin') $(LITERAL '$(LPAREN)') $(RULE assignExpression) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    MixinExpression parseMixinExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new MixinExpression;
        expect(TokenType.mixin_);
        expect(TokenType.lParen);
        node.assignExpression = parseAssignExpression();
        expect(TokenType.rParen);
        return node;
    }

    /**
     * Parses a MixinTemplateName
     *
     * $(GRAMMAR $(RULEDEF mixinTemplateName):
     *     ($(RULE typeofExpression)? $(LITERAL '.'))? $(RULE identifierOrTemplateChain)
     *     ;)
     */
    MixinTemplateName parseMixinTemplateName()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new MixinTemplateName;
        if (currentIs(TokenType.typeof_))
        {
            node.typeofExpression = parseTypeofExpression();
            expect(TokenType.dot);
        }
        node.identifierOrTemplateChain = parseIdentifierOrTemplateChain();
        return node;
    }

    /**
     * Parses a Module
     *
     * $(GRAMMAR $(RULEDEF module):
     *     $(RULE moduleDeclaration)? $(RULE declaration)*
     *     ;)
     */
    Module parseModule()
    {
        Module m = new Module;
        if (currentIs(TokenType.module_))
            m.moduleDeclaration = parseModuleDeclaration();
        while (moreTokens())
        {
            auto declaration = parseDeclaration();
            if (declaration !is null)
                m.declarations ~= declaration;
        }
        return m;
    }

    /**
     * Parses a ModuleDeclaration
     *
     * $(GRAMMAR $(RULEDEF moduleDeclaration):
     *     $(LITERAL 'module') $(RULE identifierChain) $(LITERAL ';')
     *     ;)
     */
    ModuleDeclaration parseModuleDeclaration()
    {
        auto node = new ModuleDeclaration;
        expect(TokenType.module_);
        node.moduleName = parseIdentifierChain();
        expect(TokenType.semicolon);
        return node;
    }

    /**
     * Parses a MulExpression
     * $(GRAMMAR $(RULEDEF mulExpression):
     *       $(RULE unaryExpression)
     *     | $(RULE mulExpression) ($(LITERAL '*') | $(LITERAL '/') | $(LITERAL '%')) $(RULE unaryExpression)
     *     ;)
     */
    MulExpression parseMulExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(MulExpression, UnaryExpression,
            TokenType.star, TokenType.div, TokenType.mod)();
    }

    /**
     * Parses a NewAnonClassExpression
     *
     * $(GRAMMAR $(RULEDEF newAnonClassExpression):
     *     $(LITERAL 'new') $(RULE arguments)? $(LITERAL 'class') $(RULE arguments)? $(RULE baseClassList)? $(RULE classBody)
     *     ;)
     */
    NewAnonClassExpression parseNewAnonClassExpression()
    {
        auto node = new NewAnonClassExpression;
        expect(TokenType.new_);
        if (currentIs(TokenType.lParen))
            node.allocatorArguments = parseArguments();
        expect(TokenType.class_);
        if (!currentIs(TokenType.lBrace))
            node.baseClassList = parseBaseClassList();
        node.classBody = parseClassBody();
        return node;
    }

    /**
     * Parses a NewExpression
     *
     * $(GRAMMAR $(RULEDEF newExpression):
     *       $(LITERAL 'new') $(RULE type) ($(LITERAL '[') $(RULE assignExpression) $(LITERAL ']') | $(RULE arguments))?
     *     | $(RULE newAnonClassExpression)
     *     ;)
     */
    NewExpression parseNewExpression()
    {
        auto node = new NewExpression;
        if (peekIsOneOf(TokenType.class_, TokenType.lParen))
            node.newAnonClassExpression = parseNewAnonClassExpression();
        else
        {
            expect(TokenType.new_);
            node.type = parseType();
            if (currentIs(TokenType.lBracket))
            {
                advance();
                node.assignExpression = parseAssignExpression();
                expect(TokenType.rBracket);
            }
            else if (currentIs(TokenType.lParen))
                node.arguments = parseArguments();
        }
        return node;
    }

    /**
     * Parses a StatementNoCaseNoDefault
     *
     * $(GRAMMAR $(RULEDEF statementNoCaseNoDefault):
     *       $(RULE labeledStatement)
     *     | $(RULE blockStatement)
     *     | $(RULE ifStatement)
     *     | $(RULE whileStatement)
     *     | $(RULE doStatement)
     *     | $(RULE forStatement)
     *     | $(RULE foreachStatement)
     *     | $(RULE switchStatement)
     *     | $(RULE finalSwitchStatement)
     *     | $(RULE continueStatement)
     *     | $(RULE breakStatement)
     *     | $(RULE returnStatement)
     *     | $(RULE gotoStatement)
     *     | $(RULE withStatement)
     *     | $(RULE synchronizedStatement)
     *     | $(RULE tryStatement)
     *     | $(RULE throwStatement)
     *     | $(RULE scopeGuardStatement)
     *     | $(RULE asmStatement)
     *     | $(RULE foreachRangeStatement)
     *     | $(RULE conditionalStatement)
     *     | $(RULE staticAssertStatement)
     *     | $(RULE versionSpecification)
     *     | $(RULE debugSpecification)
     *     | $(RULE expressionStatement)
     *     ;)
     */
    StatementNoCaseNoDefault parseStatementNoCaseNoDefault()
    {
        auto node = new StatementNoCaseNoDefault;
        with (TokenType) switch (current.type)
        {
        case lBrace:
            node.blockStatement = parseBlockStatement();
            break;
        case if_:
            node.ifStatement = parseIfStatement();
            break;
        case while_:
            node.whileStatement = parseWhileStatement();
            break;
        case do_:
            node.doStatement = parseDoStatement();
            break;
        case for_:
            node.forStatement = parseForStatement();
            break;
        case foreach_:
        case foreach_reverse_:
            node.foreachStatement = parseForeachStatement();
            break;
        case switch_:
            node.switchStatement = parseSwitchStatement();
            break;
        case continue_:
            node.continueStatement = parseContinueStatement();
            break;
        case break_:
            node.breakStatement = parseBreakStatement();
            break;
        case return_:
            node.returnStatement = parseReturnStatement();
            break;
        case goto_:
            node.gotoStatement = parseGotoStatement();
            break;
        case with_:
            node.withStatement = parseWithStatement();
            break;
        case synchronized_:
            node.synchronizedStatement = parseSynchronizedStatement();
            break;
        case try_:
            node.tryStatement = parseTryStatement();
            break;
        case throw_:
            node.throwStatement = parseThrowStatement();
            break;
        case scope_:
            node.scopeGuardStatement = parseScopeGuardStatement();
            break;
        case asm_:
            node.asmStatement = parseAsmStatement();
            break;
        case final_:
            if (peekIs(switch_))
            {
                node.finalSwitchStatement = parseFinalSwitchStatement();
                break;
            }
            else
            {
                error(`"switch" expected`);
                return null;
            }
        case debug_:
            if (peekIs(TokenType.assign))
                node.debugSpecification = parseDebugSpecification();
            else
                node.conditionalStatement = parseConditionalStatement();
            break;
        case version_:
            if (peekIs(TokenType.assign))
                node.versionSpecification = parseVersionSpecification();
            else
                node.conditionalStatement = parseConditionalStatement();
            break;
        case static_:
            if (peekIs(TokenType.if_))
                node.conditionalStatement = parseConditionalStatement();
            else if (peekIs(TokenType.assert_))
                node.staticAssertStatement = parseStaticAssertStatement();
            break;
        case delete_:
        case assert_:
        default:
            node.expressionStatement = parseExpressionStatement();
            break;
        }
        return node;
    }

    /**
     * Parses a NonVoidInitializer
     *
     * $(GRAMMAR $(RULEDEF nonVoidInitializer):
     *       $(RULE assignExpression)
     *     | $(RULE arrayInitializer)
     *     | $(RULE structInitializer)
     *     ;)
     */
    NonVoidInitializer parseNonVoidInitializer()
    {
        auto node = new NonVoidInitializer;
        if (currentIs(TokenType.lBrace))
            node.structInitializer = parseStructInitializer();
        else if (currentIs(TokenType.lBracket))
            node.arrayInitializer = parseArrayInitializer();
        else
            node.assignExpression = parseAssignExpression();
        return node;
    }

    /**
     * Parses Operands
     *
     * $(GRAMMAR $(RULEDEF operands):
     *     $(RULE asmExp)+
     *     ;)
     */
    Operands parseOperands()
    {
        auto node = new Operands;
        assert (false, "asm"); // TODO asm
        return node;
    }

    /**
     * Parses an OrExpression
     *
     * $(GRAMMAR $(RULEDEF orExpression):
     *       $(RULE xorExpression)
     *     | $(RULE orExpression) $(LITERAL '|') $(RULE xorExpression)
     *     ;)
     */
    OrExpression parseOrExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(OrExpression, XorExpression,
            TokenType.bitOr)();
    }

    /**
     * Parses an OrOrExpression
     *
     * $(GRAMMAR $(RULEDEF orOrExpression):
     *       $(RULE andAndExpression)
     *     | $(RULE orOrExpression) $(LITERAL '||') $(RULE andAndExpression)
     *     ;)
     */
    OrOrExpression parseOrOrExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(OrOrExpression, AndAndExpression,
            TokenType.logicOr)();
    }

    /**
     * Parses an OutStatement
     *
     * $(GRAMMAR $(RULEDEF outStatement):
     *     $(LITERAL 'out') ($(LITERAL '$(LPAREN)') $(LITERAL Identifier) $(LITERAL '$(RPAREN)'))? $(RULE blockStatement)
     *     ;)
     */
    OutStatement parseOutStatement()
    {
        auto node = new OutStatement;
        expect(TokenType.out_);
        if (currentIs(TokenType.lParen))
        {
            advance();
            auto ident = expect(TokenType.identifier);
            if (ident is null) return null;
            node.parameter = *ident;
            expect(TokenType.rParen);
        }
        node.blockStatement = parseBlockStatement();
        return node;
    }

    /**
     * Parses a Parameter
     *
     * $(GRAMMAR $(RULEDEF parameter):
     *     $(RULE parameterAttribute)* $(RULE type) ($(LITERAL Identifier)? $(LITERAL '...') | ($(LITERAL Identifier) ($(LITERAL '=') $(RULE assignExpression))?))?
     *     ;)
     */
    Parameter parseParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Parameter;
        while (moreTokens())
        {
            TokenType type = parseParameterAttribute(false);
            if (type == TokenType.invalid)
                break;
            else
                node.parameterAttributes ~= type;
        }
        node.type = parseType();
        if (node.type is null) return null;
        if (currentIs(TokenType.identifier))
        {
            node.name = advance();
            if (currentIs(TokenType.vararg))
            {
                advance();
                node.vararg = true;
            }
            else if (currentIs(TokenType.assign))
            {
                advance();
                node.default_ = parseAssignExpression();
            }
        }
        else if (currentIs(TokenType.vararg))
        {
            node.vararg = true;
            advance();
        }
        return node;
    }

    /**
     * Parses a ParameterAttribute
     *
     * $(GRAMMAR $(RULEDEF parameterAttribute):
     *       $(RULE typeConstructor)
     *     | $(LITERAL 'final')
     *     | $(LITERAL 'in')
     *     | $(LITERAL 'lazy')
     *     | $(LITERAL 'out')
     *     | $(LITERAL 'ref')
     *     | $(LITERAL 'scope')
     *     | $(LITERAL 'auto')
     *     ;)
     */
    TokenType parseParameterAttribute(bool validate = false)
    {
        with (TokenType) switch (current.type)
        {
        case immutable_:
        case shared_:
        case const_:
        case inout_:
            if (peekIs(TokenType.lParen))
                return invalid;
            else
                goto case auto_;
        case final_:
        case in_:
        case lazy_:
        case out_:
        case ref_:
        case scope_:
        case auto_:
            return advance().type;
        default:
            if (validate) error("Parameter attribute expected");
            return invalid;
        }
    }

    /**
     * Parses Parameters
     *
     * $(GRAMMAR $(RULEDEF parameters):
     *       $(LITERAL '$(LPAREN)') $(RULE parameter) ($(LITERAL ',') $(RULE parameter))* ($(LITERAL ',') $(LITERAL '...'))? $(LITERAL '$(RPAREN)')
     *     | $(LITERAL '$(LPAREN)') $(LITERAL '...') $(LITERAL '$(RPAREN)')
     *     | $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Parameters parseParameters()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Parameters;
        expect(TokenType.lParen);
        if (currentIs(TokenType.rParen))
            goto end;
        if (currentIs(TokenType.vararg))
        {
            node.hasVarargs = true;
            goto end;
        }
        while (moreTokens())
        {
            if (currentIs(TokenType.vararg))
            {
                advance();
                node.hasVarargs = true;
                break;
            }
            if (currentIs(TokenType.rParen))
                break;
            auto param = parseParameter();
            if (param is null)
                break;
            node.parameters ~= param;
            if (currentIs(TokenType.comma))
                advance();
            else
                break;
        }
    end:
        expect(TokenType.rParen);
        return node;
    }

    unittest
    {
        string sourceCode =
q{(int a, ...)
(double ...)
(Range r)}c;

        Parser p = getParserForUnittest(sourceCode, "parseParameters");

        Parameters params1 = p.parseParameters();
        assert (params1.hasVarargs);
        assert (params1.parameters.length == 1);
        assert (params1.parameters[0].name == "a");

        Parameters params2 = p.parseParameters();
        assert (params2.parameters.length == 1);
        assert (params2.parameters[0].vararg);
        assert (params2.parameters[0].type !is null);

        Parameters params3 = p.parseParameters();
        assert (params3.parameters.length == 1);
        assert (!params3.parameters[0].vararg);
        assert (params3.parameters[0].type !is null);

        stderr.writeln("Unittest for parseParameters() passed.");
    }

    /**
     * Parses a Postblit
     *
     * $(GRAMMAR $(RULEDEF parameters):
     *     $(LITERAL 'this') $(LITERAL '$(LPAREN)') $(LITERAL 'this') $(LITERAL '$(RPAREN)') $(RULE functionBody)
     *     ;)
     */
    Postblit parsePostblit()
    {
        auto node = new Postblit;
        expect(TokenType.this_);
        expect(TokenType.lParen);
        expect(TokenType.this_);
        expect(TokenType.rParen);
        node.functionBody = parseFunctionBody();
        return node;
    }

    /**
     * Parses a PostIncDecExpression
     *
     * $(GRAMMAR $(RULEDEF postIncDecExpression):
     *     $(RULE unaryExpression) ($(LITERAL '++') | $(LITERAL '--'))
     *     ;)
     */
    PostIncDecExpression parsePostIncDecExpression(UnaryExpression unary = null)
    {
        auto node = new PostIncDecExpression;
        node.unaryExpression = unary is null ? parseUnaryExpression() : unary;
        node.operator = advance();
        return node;
    }

    /**
     * Parses a PowExpression
     *
     * $(GRAMMAR $(RULEDEF powExpression):
     *       $(RULE unaryExpression)
     *     | $(RULE powExpression) $(LITERAL '^^') $(RULE unaryExpression)
     *     ;)
     */
    PowExpression parsePowExpression()
    {
        return parseLeftAssocBinaryExpression!(PowExpression, UnaryExpression,
            TokenType.pow)();
    }

    /**
     * Parses a PragmaDeclaration
     *
     * $(GRAMMAR $(RULEDEF pragmaDeclaration):
     *     $(RULE pragmaExpression) $(LITERAL ';')
     *     ;)
     */
    PragmaDeclaration parsePragmaDeclaration()
    {
        auto node = new PragmaDeclaration;
        node.pragmaExpression = parsePragmaExpression();
        expect(TokenType.semicolon);
        return node;
    }

    /**
     * Parses a PragmaExpression
     *
     * $(GRAMMAR $(RULEDEF pragmaExpression):
     *     $(RULE 'pragma') $(LITERAL '$(LPAREN)') $(LITERAL Identifier) ($(LITERAL ',') $(RULE argumentList))? $(LITERAL '$(RPAREN)')
     *     ;)
     */
    PragmaExpression parsePragmaExpression()
    {
        auto node = new PragmaExpression;
        expect(TokenType.pragma_);
        expect(TokenType.lParen);
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.identifier = *ident;
        if (currentIs(TokenType.comma))
        {
            advance();
            node.argumentList = parseArgumentList();
        }
        expect(TokenType.rParen);
        return node;
    }

    /**
     * Parses a PreIncDecExpression
     *
     * $(GRAMMAR $(RULEDEF preIncDecExpression):
     *     ($(LITERAL '++') | $(LITERAL '--')) $(RULE unaryExpression)
     *     ;)
     */
    PreIncDecExpression parsePreIncDecExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new PreIncDecExpression;
        if (currentIsOneOf(TokenType.increment, TokenType.decrement))
            advance();
        else
        {
            error(`"++" or "--" expected`);
            return null;
        }
        node.unaryExpression = parseUnaryExpression();
        return node;
    }

    /**
     * Parses a PrimaryExpression
     *
     * $(GRAMMAR $(RULEDEF primaryExpression):
     *       $(RULE identifierOrTemplateInstance)
     *     | $(RULE basicType) $(LITERAL '.') $(LITERAL Identifier)
     *     | $(RULE typeofExpression)
     *     | $(RULE typeidExpression)
     *     | $(RULE vector)
     *     | $(RULE arrayLiteral)
     *     | $(RULE assocArrayLiteral)
     *     | $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)')
     *     | $(RULE isExpression)
     *     | $(RULE lambdaExpression)
     *     | $(RULE functionLiteralExpression)
     *     | $(RULE traitsExpression)
     *     | $(RULE mixinExpression)
     *     | $(RULE importExpression)
     *     | $(LITERAL '$')
     *     | $(LITERAL 'this')
     *     | $(LITERAL 'super')
     *     | $(LITERAL '_null')
     *     | $(LITERAL '_true')
     *     | $(LITERAL '_false')
     *     | $(LITERAL '___DATE__')
     *     | $(LITERAL '___TIME__')
     *     | $(LITERAL '___TIMESTAMP__')
     *     | $(LITERAL '___VENDOR__')
     *     | $(LITERAL '___VERSION__')
     *     | $(LITERAL '___FILE__')
     *     | $(LITERAL '___LINE__')
     *     | $(LITERAL '___MODULE__')
     *     | $(LITERAL '___FUNCTION__')
     *     | $(LITERAL '___PRETTY_FUNCTION__')
     *     | $(LITERAL IntegerLiteral)
     *     | $(LITERAL FloatLiteral)
     *     | $(LITERAL StringLiteral)
     *     | $(LITERAL CharacterLiteral)
     *     ;)
     */
    PrimaryExpression parsePrimaryExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new PrimaryExpression;
        with (TokenType) switch (current.type)
        {
        case identifier:
            if (peekIs(TokenType.goesTo))
                node.lambdaExpression = parseLambdaExpression();
            else
                node.identifierOrTemplateInstance = parseIdentifierOrTemplateInstance();
            break;
        mixin (BASIC_TYPE_CASE_RANGE);
            node.basicType = advance().type;
            expect(dot);
            auto t = expect(identifier);
            if (t !is null)
                node.primary = *t;
            break;
        case function_:
        case delegate_:
        case lBrace:
        case in_:
        case out_:
        case body_:
            node.functionLiteralExpression = parseFunctionLiteralExpression();
            break;
        case typeof_:
            node.typeofExpression = parseTypeofExpression();
            break;
        case typeid_:
            node.typeidExpression = parseTypeidExpression();
            break;
        case vector:
            node.vector = parseVector();
            break;
        case lBracket:
            if (isAssociativeArrayLiteral())
                node.assocArrayLiteral = parseAssocArrayLiteral();
            else
                node.arrayLiteral = parseArrayLiteral();
            break;
        case lParen:
            auto p = peekPastParens();
            if (p !is null && p.type == TokenType.goesTo)
                node.lambdaExpression = parseLambdaExpression();
            else
            {
                advance();
                node.expression = parseExpression();
                expect(TokenType.rParen);
            }
            break;
        case is_:
            node.isExpression = parseIsExpression();
            break;
        case traits:
            node.traitsExpression = parseTraitsExpression();
            break;
        case mixin_:
            node.mixinExpression = parseMixinExpression();
            break;
        case import_:
            node.importExpression = parseImportExpression();
            break;
        case dollar:
        case this_:
        case null_:
        case true_:
        case false_:
        mixin (SPECIAL_CASE_RANGE);
        mixin (LITERAL_CASE_RANGE);
            node.primary = advance();
            break;
        default:
            error(`Primary expression expected`);
            return null;
        }
        return node;
    }

    /**
     * Parses a Register
     *
     * $(GRAMMAR $(RULEDEF register):
     *     $(LITERAL Identifier)
     *     | $(LITERAL Identifier) $(LITERAL '$(LPAREN)') $(RULE IntegerLiteral) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Register parseRegister()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Register;
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.identifier = *ident;
        if (currentIs(TokenType.lParen))
        {
            advance();
            auto intLit = expect(TokenType.intLiteral);
            if (intLit is null) return null;
            node.intLiteral = *intLit;
            expect(TokenType.rParen);
        }
        return node;
    }

    /**
     * Parses a RelExpression
     *
     * $(GRAMMAR $(RULEDEF relExpression):
     *     $(RULE shiftExpression)
     *     | $(RULE relExpression) $(RULE relOperator) $(RULE shiftExpression)
     *     ;
     *$(RULEDEF relOperator):
     *       $(LITERAL '<')
     *     | $(LITERAL '<=')
     *     | $(LITERAL '>')
     *     | $(LITERAL '>=')
     *     | $(LITERAL '!<>=')
     *     | $(LITERAL '!<>')
     *     | $(LITERAL '<>')
     *     | $(LITERAL '<>=')
     *     | $(LITERAL '!>')
     *     | $(LITERAL '!>=')
     *     | $(LITERAL '!<')
     *     | $(LITERAL '!<=')
     *     ;)
     */
    RelExpression parseRelExpression(ShiftExpression shift = null)
    {
        return parseLeftAssocBinaryExpression!(RelExpression, ShiftExpression,
            TokenType.less, TokenType.lessEqual, TokenType.greater,
            TokenType.greaterEqual, TokenType.unordered,
            TokenType.notLessEqualGreater, TokenType.lessOrGreater,
            TokenType.lessEqualGreater, TokenType.notGreater,
            TokenType.notGreaterEqual, TokenType.notLess,
            TokenType.notLessEqual)(shift);
    }

    /**
     * Parses a ReturnStatement
     *
     * $(GRAMMAR $(RULEDEF returnStatement):
     *     $(LITERAL 'return') $(RULE expression)? $(LITERAL ';')
     *     ;)
     */
    ReturnStatement parseReturnStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new ReturnStatement;
        expect(TokenType.return_);
        if (tokens[index] != TokenType.semicolon)
            node.expression = parseExpression();
        expect(TokenType.semicolon);
        return node;
    }

    /**
     * Parses a ScopeGuardStatement
     *
     * $(GRAMMAR $(RULEDEF scopeGuardStatement):
     *     $(LITERAL 'scope') $(LITERAL '$(LPAREN)') $(LITERAL Identifier) $(LITERAL '$(RPAREN)') $(RULE statementNoCaseNoDefault)
     *     ;)
     */
    ScopeGuardStatement parseScopeGuardStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new ScopeGuardStatement;
        expect(TokenType.scope_);
        expect(TokenType.lParen);
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.identifier = *ident;
        expect(TokenType.rParen);
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
        return node;
    }

    /**
     * Parses a SharedStaticConstructor
     *
     * $(GRAMMAR $(RULEDEF sharedStaticConstructor):
     *     $(LITERAL 'shared') $(LITERAL 'static') $(LITERAL 'this') $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)') $(RULE functionBody)
     *     ;)
     */
    SharedStaticConstructor parseSharedStaticConstructor()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new SharedStaticConstructor;
        expect(TokenType.shared_);
        expect(TokenType.static_);
        expect(TokenType.this_);
        expect(TokenType.lParen);
        expect(TokenType.rParen);
        node.functionBody = parseFunctionBody();
        return node;
    }

    /**
     * Parses a SharedStaticDestructor
     *
     * $(GRAMMAR $(RULEDEF sharedStaticDestructor):
     *     $(LITERAL 'shared') $(LITERAL 'static') $(LITERAL '~') $(LITERAL 'this') $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)') $(RULE functionBody)
     *     ;)
     */
    SharedStaticDestructor parseSharedStaticDestructor()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new SharedStaticDestructor;
        expect(TokenType.shared_);
        expect(TokenType.static_);
        expect(TokenType.tilde);
        expect(TokenType.this_);
        expect(TokenType.lParen);
        expect(TokenType.rParen);
        node.functionBody = parseFunctionBody();
        return node;
    }

    /**
     * Parses a ShiftExpression
     *
     * $(GRAMMAR $(RULEDEF shiftExpression):
     *       $(RULE addExpression)
     *     | $(RULE shiftExpression) ($(LITERAL '<<') | $(LITERAL '>>') | $(LITERAL '>>>')) $(RULE addExpression)
     *     ;)
     */
    ShiftExpression parseShiftExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(ShiftExpression, AddExpression,
            TokenType.shiftLeft, TokenType.shiftRight,
            TokenType.unsignedShiftRight)();
    }

    /**
     * Parses a SingleImport
     *
     * $(GRAMMAR $(RULEDEF singleImport):
     *     ($(LITERAL Identifier) $(LITERAL '='))? $(RULE identifierChain)
     *     ;)
     */
    SingleImport parseSingleImport()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new SingleImport;
        if (startsWith(TokenType.identifier, TokenType.assign))
        {
            node.identifier = advance();
            advance();
        }
        node.identifierChain = parseIdentifierChain();
        return node;
    }

    /**
     * Parses a SliceExpression
     *
     * $(GRAMMAR $(RULEDEF sliceExpression):
     *       $(RULE unaryExpression) $(LITERAL '[') $(RULE assignExpression) $(LITERAL '..') $(RULE assignExpression) $(LITERAL ']')
     *     | $(RULE unaryExpression) $(LITERAL '[') $(LITERAL ']')
     *     ;)
     */
    SliceExpression parseSliceExpression(UnaryExpression unary = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new SliceExpression;
        node.unaryExpression = unary is null ? parseUnaryExpression() : unary;
        if (expect(TokenType.lBracket) is null) return null;
        if (!currentIs(TokenType.rBracket))
        {
            node.lower = parseAssignExpression();
            expect(TokenType.slice);
            node.upper = parseAssignExpression();
        }
        if (expect(TokenType.rBracket) is null) return null;
        return node;
    }

    /**
     * Parses a Statement
     *
     * $(GRAMMAR $(RULEDEF statement):
     *       $(RULE statementNoCaseNoDefault)
     *     | $(RULE caseStatement)
     *     | $(RULE caseRangeStatement)
     *     | $(RULE defaultStatement)
     *     ;)
     */
    Statement parseStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Statement;
        switch (current.type)
        {
        case TokenType.case_:
            auto argumentList = parseArgumentList();
            if (argumentList.items.length == 1 && startsWith(TokenType.colon, TokenType.slice))
                node.caseRangeStatement = parseCaseRangeStatement(argumentList.items[0]);
            else
                node.caseStatement = parseCaseStatement();
            break;
        case TokenType.default_:
            node.defaultStatement = parseDefaultStatement();
            break;
        default:
            node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
            break;
        }
        return node;
    }

    /**
     * Parses a StaticAssertDeclaration
     *
     * $(GRAMMAR $(RULEDEF staticAssertDeclaration):
     *     $(RULE staticAssertStatement)
     *     ;)
     */
    StaticAssertDeclaration parseStaticAssertDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new StaticAssertDeclaration;
        node.staticAssertStatement = parseStaticAssertStatement();
        if (node.staticAssertStatement is null) return null;
        return node;
    }


    /**
     * Parses a StaticAssertStatement
     *
     * $(GRAMMAR $(RULEDEF staticAssertStatement):
     *     $(LITERAL 'static') $(RULE assertExpression) $(LITERAL ';')
     *     ;)
     */
    StaticAssertStatement parseStaticAssertStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new StaticAssertStatement;
        if (expect(TokenType.static_) is null) return null;
        node.assertExpression = parseAssertExpression();
        if (node.assertExpression is null) return null;
        if (expect(TokenType.semicolon) is null) return null;
        return node;
    }

    /**
     * Parses a StaticConstructor
     *
     * $(GRAMMAR $(RULEDEF staticConstructor):
     *     $(LITERAL 'static') $(LITERAL 'this') $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)') $(RULE functionBody)
     *     ;)
     */
    StaticConstructor parseStaticConstructor()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new StaticConstructor;
        expect(TokenType.static_);
        expect(TokenType.this_);
        expect(TokenType.lParen);
        expect(TokenType.rParen);
        node.functionBody = parseFunctionBody();
        return node;
    }

    /**
     * Parses a StaticDestructor
     *
     * $(GRAMMAR $(RULEDEF staticConstructor):
     *     $(LITERAL 'static') $(LITERAL '~') $(LITERAL 'this') $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)') $(RULE functionBody)
     *     ;)
     */
    StaticDestructor parseStaticDestructor()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new StaticDestructor;
        expect(TokenType.static_);
        expect(TokenType.tilde);
        expect(TokenType.this_);
        expect(TokenType.lParen);
        expect(TokenType.rParen);
        node.functionBody = parseFunctionBody();
        return node;
    }

    /**
     * Parses an StaticIfCondition
     *
     * $(GRAMMAR $(RULEDEF staticIfCondition):
     *     $(LITERAL 'static') $(LITERAL 'if') $(LITERAL '$(LPAREN)') $(RULE assignExpression) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    StaticIfCondition parseStaticIfCondition()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new StaticIfCondition;
        expect(TokenType.static_);
        expect(TokenType.if_);
        expect(TokenType.lParen);
        node.assignExpression = parseAssignExpression();
        expect(TokenType.rParen);
        return node;
    }

    /**
     * Parses an StorageClass
     *
     * $(GRAMMAR $(RULE storageClass):
     *       $(RULE atAttribute)
     *     | $(RULE typeConstructor)
     *     | $(LITERAL 'abstract')
     *     | $(LITERAL 'auto')
     *     | $(LITERAL 'deprecated')
     *     | $(LITERAL 'enum')
     *     | $(LITERAL 'extern')
     *     | $(LITERAL 'final')
     *     | $(LITERAL 'nothrow')
     *     | $(LITERAL 'override')
     *     | $(LITERAL 'pure')
     *     | $(LITERAL '___gshared')
     *     | $(LITERAL 'scope')
     *     | $(LITERAL 'static')
     *     | $(LITERAL 'synchronized')
     *     ;)
     */
    StorageClass parseStorageClass()
    {
        auto node = new StorageClass;
        with (TokenType) switch (current.type)
        {
        case at:
            node.atAttribute = parseAtAttribute();
            if (node.atAttribute is null) return null;
            break;
        case abstract_:
        case auto_:
        case deprecated_:
        case enum_:
        case extern_:
        case final_:
        case nothrow_:
        case override_:
        case pure_:
        case gshared:
        case scope_:
        case static_:
        case synchronized_:
        case const_:
        case immutable_:
        case inout_:
        case shared_:
            node.token = advance();
            break;
        default:
            error(`Storage class expected`);
            return null;
        }
        return node;
    }

    /**
     * Parses a StructBody
     *
     * $(GRAMMAR $(RULEDEF structBody):
     *     $(LITERAL '{') $(RULE structBodyItem)* $(LITERAL '}')
     *     ;)
     */
    StructBody parseStructBody()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new StructBody;
        expect(TokenType.lBrace);
        while (!currentIs(TokenType.rBrace) && moreTokens())
            node.structBodyItems ~= parseStructBodyItem();
        expect(TokenType.rBrace);
        return node;
    }

    /**
     * Parses a StructBodyItem
     *
     * $(GRAMMAR $(RULEDEF structBodyItem):
     *       $(RULE declaration)
     *     | $(RULE postBlit)
     *     | $(RULE invariant)
     *     ;)
     */
    StructBodyItem parseStructBodyItem()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new StructBodyItem;
        if (currentIs(TokenType.invariant_))
            node.invariant_ = parseInvariant();
        else if (startsWith(TokenType.this_, TokenType.lParen, TokenType.this_))
            node.postblit = parsePostblit();
        else
            node.declaration = parseDeclaration();
        return node;
    }

    /**
     * Parses a StructDeclaration
     *
     * $(GRAMMAR $(RULEDEF structDeclaration):
     *     $(LITERAL 'struct') $(LITERAL Identifier) ($(RULE templateParameters) $(RULE constraint)? $(RULE structBody) | ($(RULE structBody) | $(LITERAL ';')))
     *     ;)
     */
    StructDeclaration parseStructDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new StructDeclaration;
        expect(TokenType.struct_);
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.name = *ident;
        if (currentIs(TokenType.lParen))
        {
            node.templateParameters = parseTemplateParameters();
            if (tokens[index] == TokenType.if_)
                node.constraint = parseConstraint();
            node.structBody = parseStructBody();
        }
        else if (currentIs(TokenType.lBrace))
        {
            node.structBody = parseStructBody();
        }
        else if (currentIs(TokenType.semicolon))
            advance();
        else
        {
            error("Template Parameters, Struct Body, or Semicolon expected");
            return null;
        }
        return node;
    }

    /**
     * Parses an StructInitializer
     *
     * $(GRAMMAR $(RULEDEF structInitializer):
     *     $(LITERAL '{') $(RULE structMemberInitializers)? $(LITERAL '}')
     *     ;)
     */
    StructInitializer parseStructInitializer()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new StructInitializer;
        expect(TokenType.lBrace);
        node.structMemberInitializers = parseStructMemberInitializers();
        expect(TokenType.rBrace);
        return node;
    }

    /**
     * Parses a StructMemberInitializer
     *
     * $(GRAMMAR $(RULEDEF structMemberInitializer):
     *     ($(LITERAL Identifier) $(LITERAL ':'))? $(RULE nonVoidInitializer)
     *     ;)
     */
    StructMemberInitializer parseStructMemberInitializer()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new StructMemberInitializer;
        if (startsWith(TokenType.identifier, TokenType.colon))
        {
            node.identifier = tokens[index++];
            index++;
        }
        node.nonVoidInitializer = parseNonVoidInitializer();
        return node;
    }

    /**
     * Parses StructMemberInitializers
     *
     * $(GRAMMAR $(RULEDEF structMemberInitializers):
     *     $(RULE structMemberInitializer) ($(LITERAL ',') $(RULE structMemberInitializer)?)*
     *     ;)
     */
    StructMemberInitializers parseStructMemberInitializers()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new StructMemberInitializers;
        do
        {
            auto structMemberInitializer = parseStructMemberInitializer();
            if (currentIs(TokenType.comma))
            {
                advance();
                if (currentIs(TokenType.identifier))
                    continue;
                else
                    break;
            }
            else
                break;
        } while (true);
        return node;
    }

    /**
     * Parses a SwitchBody
     *
     * $(GRAMMAR $(RULEDEF switchBody):
     *     $(LITERAL '{') $(RULE statement)+ $(LITERAL '}')
     *     ;)
     */
    SwitchBody parseSwitchBody()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new SwitchBody;
        expect(TokenType.lBrace);
        while (moreTokens() && tokens[index] != TokenType.rBrace)
            node.statements ~= parseStatement();
        expect(TokenType.rBrace);
        return node;
    }

    /**
     * Parses a SwitchStatement
     *
     * $(GRAMMAR $(RULEDEF switchStatement):
     *     $(LITERAL 'switch') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE switchBody)
     *     ;)
     */
    SwitchStatement parseSwitchStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new SwitchStatement;
        expect(TokenType.switch_);
        expect(TokenType.lParen);
        node.expression = parseExpression();
        expect(TokenType.rParen);
        node.switchBody = parseSwitchBody();
        return node;
    }

    /**
     * Parses a Symbol
     *
     * $(GRAMMAR $(RULEDEF symbol):
     *     $(LITERAL '.')? $(RULE identifierOrTemplateChain)
     *     ;)
     */
    Symbol parseSymbol()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Symbol;
        if (currentIs(TokenType.dot))
        {
            node.hasDot = true;
            advance();
        }
        node.identifierOrTemplateChain = parseIdentifierOrTemplateChain();
        return node;
    }

    /**
     * Parses a SynchronizedStatement
     *
     * $(GRAMMAR $(RULEDEF synchronizedStatement):
     *     $(LITERAL 'synchronized') ($(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)'))? $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    SynchronizedStatement parseSynchronizedStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new SynchronizedStatement;
        expect(TokenType.synchronized_);
        if (tokens[index] == TokenType.lParen)
        {
            expect(TokenType.lParen);
            node.expression = parseExpression();
            expect(TokenType.rParen);
        }
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
        return node;
    }

    /**
     * Parses a TemplateAliasParameter
     *
     * $(GRAMMAR $(RULEDEF templateAliasParameter):
     *     $(LITERAL 'alias') $(RULE type)? $(LITERAL Identifier) ($(LITERAL ':') ($(RULE type) | $(RULE assignExpression)))? ($(LITERAL '=') ($(RULE type) | $(RULE assignExpression)))?
     *     ;)
     */
    TemplateAliasParameter parseTemplateAliasParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateAliasParameter;
        expect(TokenType.alias_);
        if (currentIs(TokenType.identifier))
        {
            if (peekIsOneOf(TokenType.comma, TokenType.rParen, TokenType.assign))
                node.identifier = advance();
        }
        else
        {
            if ((node.type = parseType()) is null) return null;
            auto ident = expect(TokenType.identifier);
            if (ident is null) return null;
            node.identifier = *ident;
        }

        if (currentIs(TokenType.colon))
        {
            advance();
            if (isType())
                node.colonType = parseType();
            else
                node.colonExpression = parseAssignExpression();
        }
        if (currentIs(TokenType.assign))
        {
            advance();
            if (isType())
                node.assignType = parseType();
            else
                node.assignExpression = parseAssignExpression();
        }
        return node;
    }

    /**
     * Parses a TemplateArgument
     *
     * $(GRAMMAR $(RULEDEF templateArgument):
     *       $(RULE type)
     *     | $(RULE assignExpression)
     *     ;)
     */
    TemplateArgument parseTemplateArgument()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateArgument;
        if (isType())
        {
            if ((node.type = parseType()) is null) return null;
        }
        else
        {
            if ((node.assignExpression = parseAssignExpression()) is null) return null;
        }
        return node;
    }

    /**
     * Parses a TemplateArgumentList
     *
     * $(GRAMMAR $(RULEDEF templateArgumentList):
     *     $(RULE templateArgument) ($(LITERAL ',') $(RULE templateArgument)?)*
     *     ;)
     */
    TemplateArgumentList parseTemplateArgumentList()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseCommaSeparatedRule!(TemplateArgumentList, TemplateArgument)();
    }

    /**
     * Parses TemplateArguments
     *
     * $(GRAMMAR $(RULEDEF templateArguments):
     *     $(LITERAL '!') ($(LITERAL '$(LPAREN)') $(RULE templateArgumentList)? $(LITERAL '$(RPAREN)') | $(RULE templateSingleArgument))
     *     ;)
     */
    TemplateArguments parseTemplateArguments()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateArguments;
        expect(TokenType.not);
        if (currentIs(TokenType.lParen))
        {
            advance();
            node.templateArgumentList = parseTemplateArgumentList();
            expect(TokenType.rParen);
        }
        else
            node.templateSingleArgument = parseTemplateSingleArgument();
        return node;
    }

    /**
     * Parses a TemplateDeclaration
     *
     * $(GRAMMAR $(RULEDEF templateDeclaration):
     *     $(LITERAL 'template') $(LITERAL Identifier) $(RULE templateParameters) $(RULE constraint)? $(LITERAL '{') $(RULE declaration)+ $(LITERAL '}')
     *     ;)
     */
    TemplateDeclaration parseTemplateDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateDeclaration;
        expect(TokenType.template_);
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.identifier = *ident;
        node.templateParameters = parseTemplateParameters();
        if (currentIs(TokenType.if_))
            node.constraint = parseConstraint();
        expect(TokenType.lBrace);
        do
            node.declarations ~= parseDeclaration();
        while (!currentIs(TokenType.rBrace) && moreTokens());
        expect(TokenType.rBrace);
        return node;
    }

    /**
     * Parses a TemplateInstance
     *
     * $(GRAMMAR $(RULEDEF templateInstance):
     *     $(LITERAL Identifier) $(RULE templateArguments)
     *     ;)
     */
    TemplateInstance parseTemplateInstance()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateInstance;
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.identifier = *ident;
        node.templateArguments = parseTemplateArguments();
        if (node.templateArguments is null)
            return null;
        return node;
    }

    /**
     * Parses a TemplateMixinStatement
     *
     * $(GRAMMAR $(RULEDEF templateMixinStatement):
     *     $(LITERAL 'mixin') $(RULE mixinTemplateName) $(RULE templateArguments)? $(LITERAL Identifier)? $(LITERAL ';')
     *     ;)
     */
    TemplateMixinStatement parseTemplateMixinStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateMixinStatement;
        expect(TokenType.mixin_);
        node.mixinTemplateName = parseMixinTemplateName();
        if (currentIs(TokenType.not))
            node.templateArguments = parseTemplateArguments();
        if (currentIs(TokenType.identifier))
            node.identifier = advance();
        expect(TokenType.semicolon);
        return node;
    }

    /**
     * Parses a TemplateParameter
     *
     * $(GRAMMAR $(RULEDEF templateParameter):
     *       $(RULE templateTypeParameter)
     *     | $(RULE templateValueParameter)
     *     | $(RULE templateAliasParameter)
     *     | $(RULE templateTupleParameter)
     *     | $(RULE templateThisParameter)
     *     ;)
     */
    TemplateParameter parseTemplateParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateParameter;
        with (TokenType) switch (current.type)
        {
        case alias_:
            node.templateAliasParameter = parseTemplateAliasParameter();
            break;
        case identifier:
            if(peekIs(vararg))
                node.templateTupleParameter = parseTemplateTupleParameter();
            else if (peekIsOneOf(colon, assign, comma, rParen))
                node.templateTypeParameter = parseTemplateTypeParameter();
            else
                node.templateValueParameter = parseTemplateValueParameter();
            break;
        case this_:
            node.templateThisParameter = parseTemplateThisParameter();
            break;
        default:
            node.templateValueParameter = parseTemplateValueParameter();
            break;
        }
        return node;
    }

    /**
     * Parses an TemplateParameterList
     *
     * $(GRAMMAR $(RULEDEF templateParameterList):
     *     $(RULE templateParameter) ($(LITERAL ',') $(RULE templateParameter)?)*
     *     ;)
     */
    TemplateParameterList parseTemplateParameterList()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseCommaSeparatedRule!(TemplateParameterList, TemplateParameter)();
    }

    /**
     * Parses TemplateParameters
     *
     * $(GRAMMAR $(RULEDEF templateParameters):
     *     $(LITERAL '$(LPAREN)') $(RULE templateParameterList)? $(LITERAL '$(RPAREN)')
     *     ;)
     */
    TemplateParameters parseTemplateParameters()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateParameters;
        if (expect(TokenType.lParen) is null) return null;
        if (!currentIs(TokenType.rParen))
            node.templateParameterList = parseTemplateParameterList();
        if (expect(TokenType.rParen) is null) return null;
        return node;
    }

    /**
     * Parses a TemplateSingleArgument
     *
     * $(GRAMMAR $(RULEDEF templateSingleArgument):
     *       $(RULE builtinType)
     *     | $(LITERAL Identifier)
     *     | $(LITERAL CharacterLiteral)
     *     | $(LITERAL StringLiteral)
     *     | $(LITERAL IntegerLiteral)
     *     | $(LITERAL FloatLiteral)
     *     | $(LITERAL '_true')
     *     | $(LITERAL '_false')
     *     | $(LITERAL '_null')
     *     | $(LITERAL 'this')
     *     | $(LITERAL '__DATE__')
     *     | $(LITERAL '__TIME__')
     *     | $(LITERAL '__TIMESTAMP__')
     *     | $(LITERAL '__VENDOR__')
     *     | $(LITERAL '__VERSION__')
     *     | $(LITERAL '__FILE__')
     *     | $(LITERAL '__LINE__')
     *     | $(LITERAL '__MODULE__')
     *     | $(LITERAL '__FUNCTION__')
     *     | $(LITERAL '__PRETTY_FUNCTION__')
     *     ;)
     */
    TemplateSingleArgument parseTemplateSingleArgument()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateSingleArgument;
        with (TokenType) switch (current.type)
        {
        case true_:
        case false_:
        case null_:
        case this_:
        case identifier:
        mixin (SPECIAL_CASE_RANGE);
        mixin (LITERAL_CASE_RANGE);
        mixin (BASIC_TYPE_CASE_RANGE);
            node.token = advance();
            break;
        default:
            error(`Invalid template argument. (Try enclosing in parenthesis?)`);
            return null;
        }
        return node;
    }

    /**
     * Parses a TemplateThisParameter
     *
     * $(GRAMMAR $(RULEDEF templateThisParameter):
     *     $(LITERAL 'this') $(RULE templateTypeParameter)
     *     ;)
     */
    TemplateThisParameter parseTemplateThisParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateThisParameter;
        expect(TokenType.this_);
        node.templateTypeParameter = parseTemplateTypeParameter();
        return node;
    }

    /**
     * Parses an TemplateTupleParameter
     *
     * $(GRAMMAR $(RULEDEF templateTupleParameter):
     *     $(LITERAL Identifier) $(LITERAL '...')
     *     ;)
     */
    TemplateTupleParameter parseTemplateTupleParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateTupleParameter;
        auto i = expect(TokenType.identifier);
        if (i is null)
            return null;
        node.identifier = *i;
        if (expect(TokenType.vararg) is null) return null;
        return node;
    }

    /**
     * Parses a TemplateTypeParameter
     *
     * $(GRAMMAR $(RULEDEF templateTypeParameter):
     *     $(LITERAL Identifier) ($(LITERAL ':') $(RULE type))? ($(LITERAL '=') $(RULE type))?
     *     ;)
     */
    TemplateTypeParameter parseTemplateTypeParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateTypeParameter;
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.identifier = *ident;
        if (currentIs(TokenType.colon))
        {
            advance();
            node.colonType = parseType();
        }
        else if (currentIs(TokenType.assign))
        {
            advance();
            node.assignType = parseType();
        }
        return node;
    }

    /**
     * Parses a TemplateValueParameter
     *
     * $(GRAMMAR $(RULEDEF templateValueParameter):
     *     $(RULE type) $(LITERAL Identifier) ($(LITERAL ':') $(RULE expression))? $(RULE templateValueParameterDefault)?
     *     ;)
     */
    TemplateValueParameter parseTemplateValueParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateValueParameter;
        if ((node.type = parseType()) is null) return null;
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.identifier = *ident;
        if (currentIs(TokenType.colon))
        {
            advance();
            if ((node.expression = parseExpression()) is null) return null;
        }
        if (currentIs(TokenType.assign))
        {
            if ((node.templateValueParameterDefault = parseTemplateValueParameterDefault()) is null)
                return null;
        }
        return node;
    }

    /**
     * Parses a TemplateValueParameterDefault
     *
     * $(GRAMMAR $(RULEDEF templateValueParameterDefault):
     *     $(LITERAL '=') ('__FILE__' | '__MODULE__' | '__LINE__' | '__FUNCTION__' | '__PRETTY_FUNCTION__' | $(RULE assignExpression))
     *     ;)
     */
    TemplateValueParameterDefault parseTemplateValueParameterDefault()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TemplateValueParameterDefault;
        expect(TokenType.assign);
        with (TokenType) switch (current.type)
        {
        case specialFile:
        case specialModule:
        case specialLine:
        case specialFunction:
        case specialPrettyFunction:
            node.token = advance();
            break;
        default:
            node.assignExpression = parseAssignExpression();
            break;
        }
        return node;
    }

    /**
     * Parses a TernaryExpression
     *
     * $(GRAMMAR $(RULEDEF ternaryExpression):
     *     $(RULE orOrExpression) ($(LITERAL '?') $(RULE expression) $(LITERAL ':') $(RULE ternaryExpression))?
     *     ;)
     */
    TernaryExpression parseTernaryExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TernaryExpression;
        node.orOrExpression = parseOrOrExpression();
        if (currentIs(TokenType.ternary))
        {
            advance();
            node.expression = parseExpression();
            expect(TokenType.colon);
            node.ternaryExpression = parseTernaryExpression();
        }
        return node;
    }

    /**
     * Parses a ThrowStatement
     *
     * $(GRAMMAR $(RULEDEF throwStatement):
     *     $(LITERAL 'throw') $(RULE expression) $(LITERAL ';')
     *     ;)
     */
    ThrowStatement parseThrowStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new ThrowStatement;
        expect(TokenType.throw_);
        node.expression = parseExpression();
        expect(TokenType.semicolon);
        return node;
    }

    /**
     * Parses an TraitsExpression
     *
     * $(GRAMMAR $(RULEDEF traitsExpression):
     *     $(LITERAL '__traits') $(LITERAL '$(LPAREN)') $(LITERAL Identifier) ($(LITERAL ',') $(RULE TemplateArgumentList)) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    TraitsExpression parseTraitsExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TraitsExpression;
        if (expect(TokenType.traits) is null) return null;
        if (expect(TokenType.lParen) is null) return null;
        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;
        node.identifier = *ident;
        if ((node.templateArgumentList = parseTemplateArgumentList()) is null) return null;
        if (expect(TokenType.rParen) is null) return null;
        return node;
    }

    /**
     * Parses a TryStatement
     *
     * $(GRAMMAR $(RULEDEF tryStatement):
     *     $(LITERAL 'try') $(RULE nonEmptyStatementNoCaseNoDefault) ($(RULE catches) | $(RULE catches) $(RULE finally) | $(RULE finally))
     *     ;)
     */
    TryStatement parseTryStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TryStatement;
        expect(TokenType.try_);
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
        if (currentIs(TokenType.catch_))
            node.catches = parseCatches();
        if (currentIs(TokenType.finally_))
            node.finally_ = parseFinally();
        return node;
    }

    /**
     * Parses a Type
     *
     * $(GRAMMAR $(RULEDEF type):
     *     $(RULE typeConstructors)? $(RULE type2) $(RULE typeSuffix)*
     *     ;)
     */
    Type parseType()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Type;
        switch(current.type)
        {
        case TokenType.const_:
        case TokenType.immutable_:
        case TokenType.inout_:
        case TokenType.shared_:
        case TokenType.scope_:
            if (!peekIs(TokenType.lParen))
                node.typeConstructors = parseTypeConstructors();
            break;
        default:
            break;
        }
        node.type2 = parseType2();
        if (node.type2 is null)
            return null;
        loop: while (true) with (TokenType) switch (current.type)
        {
        case TokenType.star:
        case TokenType.lBracket:
        case TokenType.delegate_:
        case TokenType.function_:
            auto suffix = parseTypeSuffix();
            if (suffix !is null)
                node.typeSuffixes ~= suffix;
            else
                return null;
            break;
        default:
            break loop;
        }
        return node;
    }

    /**
     * Parses a Type2
     *
     * $(GRAMMAR $(RULEDEF type2):
     *       $(RULE builtinType)
     *     | $(RULE symbol)
     *     | $(RULE typeofExpression) ($(LITERAL '.') $(RULE identifierOrTemplateChain))?
     *     | $(RULE typeConstructor) $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Type2 parseType2()
    {
        auto node = new Type2;
        with (TokenType) switch (current.type)
        {
            case identifier:
            case dot:
                if ((node.symbol = parseSymbol()) is null)
                    return null;
                break;
            case bool_: .. case wchar_:
                if ((node.basicType = parseBasicType()) == TokenType.invalid)
                    return null;
                break;
            case typeof_:
                if ((node.typeofExpression = parseTypeofExpression()) is null)
                    return null;
                if (currentIs(TokenType.dot))
                {
                    advance();
                    node.identifierOrTemplateChain = parseIdentifierOrTemplateChain();
                    if (node.identifierOrTemplateChain is null)
                        return null;
                }
                break;
            case const_:
            case immutable_:
            case inout_:
            case shared_:
                node.typeConstructor = parseTypeConstructor();
                if (expect(TokenType.lParen) is null) return null;
                if ((node.type = parseType()) is null) return null;
                if (expect(TokenType.rParen) is null) return null;
                break;
            default:
                error("Basic type, type constructor, symbol, or typeof expected");
                return null;
        }
        return node;
    }

    /**
     * Parses a TypeConstructor
     *
     * $(GRAMMAR $(RULEDEF typeConstructor):
     *       $(LITERAL 'const')
     *     | $(LITERAL 'immutable')
     *     | $(LITERAL 'inout')
     *     | $(LITERAL 'shared')
     *     | $(LITERAL 'scope')
     *     ;)
     */
    TokenType parseTypeConstructor(bool validate = true)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        with (TokenType) switch (current.type)
        {
        case const_:
        case immutable_:
        case inout_:
        case shared_:
        case scope_:
            return advance().type;
        default:
            if (validate)
                error(`"const", "immutable", "inout", "shared", or "scope" expected`);
            return invalid;
        }
    }

    /**
     * Parses TypeConstructors
     *
     * $(GRAMMAR $(RULEDEF typeConstructors):
     *     $(RULE typeConstructor)+
     *     ;)
     */
    TokenType[] parseTypeConstructors()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        TokenType[] r;
        while (moreTokens())
        {
            TokenType type = parseTypeConstructor(false);
            if (type == TokenType.invalid)
                break;
            else
                r ~= type;
        }
        return r;
    }

    /**
     * Parses a TypeSpecialization
     *
     * $(GRAMMAR $(RULEDEF typeSpecialization):
     *       $(RULE type)
     *     | $(LITERAL 'struct')
     *     | $(LITERAL 'union')
     *     | $(LITERAL 'class')
     *     | $(LITERAL 'interface')
     *     | $(LITERAL 'enum')
     *     | $(LITERAL 'function')
     *     | $(LITERAL 'delegate')
     *     | $(LITERAL 'super')
     *     | $(LITERAL 'const')
     *     | $(LITERAL 'immutable')
     *     | $(LITERAL 'inout')
     *     | $(LITERAL 'shared')
     *     | $(LITERAL 'return')
     *     | $(LITERAL 'typedef')
     *     | $(LITERAL '___parameters')
     *     ;)
     */
    TypeSpecialization parseTypeSpecialization()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TypeSpecialization;
        with (TokenType) switch (current.type)
        {
        case struct_:
        case union_:
        case class_:
        case interface_:
        case enum_:
        case function_:
        case delegate_:
        case super_:
        case return_:
        case typedef_:
        case parameters:
        case const_:
        case immutable_:
        case inout_:
        case shared_:
            if (peekIsOneOf(rParen, comma))
                node.token = advance();
            else
                goto default;
            break;
        default:
            node.type = parseType();
            break;
        }
        return node;
    }

    /**
     * Parses a TypeSuffix
     *
     * $(GRAMMAR $(RULEDEF typeSuffix):
     *       $(LITERAL '*')
     *     | $(LITERAL '[') ($(RULE type) | $(RULE assignExpression))? $(LITERAL ']')
     *     | ($(LITERAL 'delegate') | $(LITERAL 'function')) $(RULE parameters) $(RULE memberFunctionAttribute)*
     *     ;)
     */
    TypeSuffix parseTypeSuffix()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TypeSuffix;
        with (TokenType) switch(current.type)
        {
        case star:
            node.star = true;
            advance();
            return node;
        case lBracket:
            node.array = true;
            advance();
            if (currentIs(rBracket))
                goto end;
            auto bookmark = setBookmark();
            auto type = parseType();
            if (type is null)
            {
                goToBookmark(bookmark);
                node.assignExpression = parseAssignExpression();
                if (node.assignExpression is null) return null;
            }
            else
                node.type = type;
        end:
            if (expect(TokenType.rBracket) is null) return null;
            return node;
        case delegate_:
        case function_:
            advance();
            node.parameters = parseParameters();
            while (currentIsMemberFunctionAttribute())
                node.memberFunctionAttributes ~= parseMemberFunctionAttribute();
            return node;
        default:
            error(`"*", "[", "delegate", or "function" expected.`);
            return null;
        }
    }

    /**
     * Parses a TypeidExpression
     *
     * $(GRAMMAR $(RULEDEF typeidExpression):
     *     $(LITERAL 'typeid') $(LITERAL '$(LPAREN)')($(RULE type) | $(RULE expression)) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    TypeidExpression parseTypeidExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TypeidExpression;
        expect(TokenType.typeid_);
        expect(TokenType.lParen);
        if (isExpression())
        {
            node.expression = parseExpression();
            if (node.expression is null) return null;
        }
        else
        {
            node.type = parseType();
            if (node.type is null) return null;
        }
        expect(TokenType.rParen);
        return node;
    }

    /**
     * Parses a TypeofExpression
     *
     * $(GRAMMAR $(RULEDEF typeofExpression):
     *     $(LITERAL 'typeof') $(LITERAL '$(LPAREN)')($(RULE expression) | $(LITERAL 'return')) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    TypeofExpression parseTypeofExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new TypeofExpression;
        expect(TokenType.typeof_);
        expect(TokenType.lParen);
        if (tokens[index] == TokenType.return_)
            node.return_ = tokens[index];
        else
            node.expression = parseExpression();
        expect(TokenType.rParen);
        return node;
    }

    /**
     * Parses a UnaryExpression
     *
     * $(GRAMMAR $(RULEDEF unaryExpression):
     *       $(RULE primaryExpression)
     *     | $(LITERAL '&') $(RULE unaryExpression)
     *     | $(LITERAL '!') $(RULE unaryExpression)
     *     | $(LITERAL '*') $(RULE unaryExpression)
     *     | $(LITERAL '+') $(RULE unaryExpression)
     *     | $(LITERAL '-') $(RULE unaryExpression)
     *     | $(LITERAL '~') $(RULE unaryExpression)
     *     | $(LITERAL '++') $(RULE unaryExpression)
     *     | $(LITERAL '--') $(RULE unaryExpression)
     *     | $(RULE newExpression)
     *     | $(RULE deleteExpression)
     *     | $(RULE castExpression)
     *     | $(RULE assertExpression)
     *     | $(RULE functionCallExpression)
     *     | $(RULE sliceExpression)
     *     | $(RULE indexExpression)
     *     | $(RULE unaryExpression) $(LITERAL '.') $(RULE identifierOrTemplateInstance)
     *     | $(RULE unaryExpression) $(LITERAL '--')
     *     | $(RULE unaryExpression) $(LITERAL '++')
     *     ;)
     */
    UnaryExpression parseUnaryExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new UnaryExpression;
        with(TokenType) switch (current.type)
        {
        case bitAnd:
        case not:
        case star:
        case plus:
        case minus:
        case tilde:
        case increment:
        case decrement:
            node.prefix = advance();
            node.unaryExpression = parseUnaryExpression();
            return node;
        case new_:
            node.newExpression = parseNewExpression();
            return node;
        case delete_:
            node.deleteExpression = parseDeleteExpression();
            return node;
        case cast_:
            node.castExpression = parseCastExpression();
            return node;
        case assert_:
            node.assertExpression = parseAssertExpression();
            return node;
        default:
            node.primaryExpression = parsePrimaryExpression();
        }

        loop: while (moreTokens()) with (TokenType) switch (current.type)
        {
        case not:
            if (peekIs(is_))
                break loop;
            index++;
            bool jump =  (currentIs(TokenType.lParen) && peekPastParens().type == TokenType.lParen)
                || peekIs(TokenType.lParen);
            index--;
            if (jump)
                goto case lParen;
            else
                break loop;
        case lParen:
            auto n = new UnaryExpression();
            n.functionCallExpression = parseFunctionCallExpression(node);
            node = n;
            break;
        case increment:
        case decrement:
            auto n = new UnaryExpression();
            n.unaryExpression = node;
            n.suffix = advance();
            node = n;
            break;
        case lBracket:
            auto n = new UnaryExpression;
            if (isSliceExpression())
                n.sliceExpression = parseSliceExpression(node);
            else
                n.indexExpression = parseIndexExpression(node);
            node = n;
            break;
        case dot:
            advance();
            auto n = new UnaryExpression();
            n.unaryExpression = node;
            n.identifierOrTemplateInstance = parseIdentifierOrTemplateInstance();
            node = n;
            break;
        default:
            break loop;
        }
        return node;
    }

    unittest
    {
        auto sourceCode =
q{doStuff(5)}c;
        Parser p = getParserForUnittest(sourceCode, "parseUnaryExpression");
        auto unary = p.parseUnaryExpression();
        assert (unary !is null);
        assert (unary.functionCallExpression !is null);
        stderr.writeln("Unittest for parseUnaryExpression() passed.");
    }

    /**
     * Parses an UnionDeclaration
     *
     * $(GRAMMAR $(RULEDEF unionDeclaration):
     *       $(LITERAL 'union') $(LITERAL Identifier) (($(RULE templateParameters) $(RULE constraint)? $(RULE structBody))? | ($(RULE structBody) | $(LITERAL ';')))
     *     | $(LITERAL 'union') $(RULE structBody)
     *     ;)
     */
    UnionDeclaration parseUnionDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new UnionDeclaration;
        expect(TokenType.union_);
        bool templated = false;
        if (currentIs(TokenType.identifier))
        {
            node.identifier = advance();
            if (currentIs(TokenType.lParen))
            {
                templated = true;
                node.templateParameters = parseTemplateParameters();
                if (currentIs(TokenType.if_))
                    node.constraint = parseConstraint();
                node.structBody = parseStructBody();
            }
            else
                goto semiOrStructBody;
        }
        else
        {
    semiOrStructBody:
            if (currentIs(TokenType.semicolon))
                advance();
            else
                node.structBody = parseStructBody();
        }
        return node;
    }

    /**
     * Parses a Unittest
     *
     * $(GRAMMAR $(RULEDEF unittest):
     *     $(LITERAL 'unittest') $(RULE blockStatement)
     *     ;)
     */
    Unittest parseUnittest()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Unittest;
        expect(TokenType.unittest_);
        node.blockStatement = parseBlockStatement();
        return node;
    }

    /**
     * Parses a VariableDeclaration
     *
     * $(GRAMMAR $(RULEDEF variableDeclaration):
     *       $(RULE _type) $(RULE declarator) ($(LITERAL ',') $(RULE declarator))* $(LITERAL ';')
     *     | $(RULE autoDeclaration)
     *     ;)
     */
    VariableDeclaration parseVariableDeclaration(Type type = null)
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        auto node = new VariableDeclaration;
        with (TokenType) if (currentIsOneOf(const_, immutable_, inout_, shared_,
            abstract_, auto_, deprecated_, enum_, extern_, final_, nothrow_,
            override_, pure_, gshared, scope_, static_, synchronized_)
            && !(peekIs(lParen)))
        {
            node.autoDeclaration = parseAutoDeclaration();
            return node;
        }

        node.type = type is null ? parseType() : type;

        while(true)
        {
            auto declarator = parseDeclarator();
            if (declarator is null) return null;
            node.declarators ~= declarator;
            if (moreTokens() && currentIs(TokenType.comma))
                advance();
            else
                break;
        }
        expect(TokenType.semicolon);
        return node;
    }

    /**
     * Parses a Vector
     *
     * $(GRAMMAR $(RULEDEF vector):
     *     $(LITERAL '__vector') $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Vector parseVector()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new Vector;
        expect(TokenType.vector);
        expect(TokenType.lParen);
        node.type = parseType();
        expect(TokenType.rParen);
        return node;
    }

    /**
     * Parses a VersionCondition
     *
     * $(GRAMMAR $(RULEDEF versionCondition):
     *     $(LITERAL 'version') $(LITERAL '$(LPAREN)') ($(LITERAL IntegerLiteral) | $(LITERAL Identifier) | $(LITERAL 'unittest') | $(LITERAL 'assert')) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    VersionCondition parseVersionCondition()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new VersionCondition;
        expect(TokenType.version_);
        expect(TokenType.lParen);
        if (currentIsOneOf(TokenType.intLiteral, TokenType.identifier,
            TokenType.unittest_, TokenType.assert_))
        {
            node.token = advance();
        }
        else
        {
            error(`Expected an integer literal, an identifier, "assert", or "unittest"`);
            return null;
        }
        expect(TokenType.rParen);
        return node;
    }

    /**
     * Parses a VersionSpecification
     *
     * $(GRAMMAR $(RULEDEF versionSpecification):
     *     $(LITERAL 'version') $(LITERAL '=') ($(LITERAL Identifier) | $(LITERAL IntegerLiteral)) $(LITERAL ';')
     *     ;)
     */
    VersionSpecification parseVersionSpecification()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new VersionSpecification;
        expect(TokenType.version_);
        expect(TokenType.assign);
        if (!currentIsOneOf(TokenType.identifier, TokenType.intLiteral))
        {
            error("Identifier or integer literal expected");
            return null;
        }
        node.token = advance();
        expect(TokenType.semicolon);
        return node;
    }

    /**
     * Parses a WhileStatement
     *
     * $(GRAMMAR $(RULEDEF whileStatement):
     *     $(LITERAL 'while') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE statementNoCaseNoDefault)
     *     ;)
     */
    WhileStatement parseWhileStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new WhileStatement;
        expect(TokenType.while_);
        expect(TokenType.lParen);
        node.expression = parseExpression();
        expect(TokenType.rParen);
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
        return node;
    }

    /**
     * Parses a WithStatement
     *
     * $(GRAMMAR $(RULEDEF withStatement):
     *     $(LITERAL 'with') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    WithStatement parseWithStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = new WithStatement;
        if (expect(TokenType.with_) is null) return null;
        if (expect(TokenType.lParen) is null) return null;
        if ((node.expression = parseExpression()) is null) return null;
        if (expect(TokenType.rParen) is null) return null;
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
        if (node.statementNoCaseNoDefault is null) return null;
        return node;
    }

    /**
     * Parses an XorExpression
     *
     * $(GRAMMAR $(RULEDEF xorExpression):
     *       $(RULE andExpression)
     *     | $(RULE xorExpression) $(LITERAL '^') $(RULE andExpression)
     *     ;)
     */
    XorExpression parseXorExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(XorExpression, AndExpression,
            TokenType.xor)();
    }

private:

    bool isAssociativeArrayLiteral()
    {
        return hasMagicDelimiter!(TokenType.colon)();
    }

    bool isSliceExpression()
    {
        if (startsWith(TokenType.lBracket, TokenType.rBracket))
            return true;
        return hasMagicDelimiter!(TokenType.slice)();
    }

    bool hasMagicDelimiter(alias T)()
    {
        auto i = index;
        scope(exit) index = i;
        if (!currentIs(TokenType.lBracket))
            return false;
        advance();
        while (moreTokens()) with (TokenType) switch (current.type)
        {
        case lBrace: skipBraceContent(); break;
        case lParen: skipParenContent(); break;
        case lBracket: skipBracketContent(); break;
        case rBracket: return false;
        case T: return true;
        default: advance(); break;
        }
        return false;
    }

    bool isDeclaration()
    {
        with (TokenType) switch (current.type)
        {
        mixin(BASIC_TYPE_CASE_RANGE);
        case const_:
        case immutable_:
        case inout_:
        case shared_:
        case gshared:
        case alias_:
        case class_:
        case enum_:
        case interface_:
        case struct_:
        case union_:
        case unittest_:
        case auto_:
        case ref_:
        case at:
        case align_:
        case deprecated_:
        case private_:
        case package_:
        case protected_:
        case public_:
        case export_:
        case extern_:
        case final_:
        case synchronized_:
        case override_:
        case abstract_:
        case pure_:
        case nothrow_:
            return true;
        case case_:
        case default_:
        case return_:
        case if_:
        case while_:
        case do_:
        case for_:
        case foreach_:
        case switch_:
        case continue_:
        case break_:
        case goto_:
        case try_:
        case throw_:
        case scope_:
        case asm_:
        case foreach_reverse_:
            return false;
        default:
            break;
        }
        // h4x
        auto b = setBookmark();
        scope(exit) goToBookmark(b);
        return parseDeclaration() !is null;
    }

    bool isStatement()
    {
        auto b = setBookmark();
        scope (exit) goToBookmark(b);
        return parseStatement() !is null;
    }

    bool isExpression()
    {
        auto b = setBookmark();
        scope (exit) goToBookmark(b);
        return parseExpression() !is null;
    }

    bool isType()
    {
        auto b = setBookmark();
        scope (exit) goToBookmark(b);
        return parseType() !is null;
    }

    bool currentIsMemberFunctionAttribute() const
    {
        switch (current.type)
        {
        case TokenType.const_:
        case TokenType.immutable_:
        case TokenType.inout_:
        case TokenType.shared_:
        case TokenType.at:
        case TokenType.pure_:
        case TokenType.nothrow_:
            return true;
        default:
            return false;
        }
    }

    Left parseLeftAssocBinaryExpression(alias Left, alias Right, Operators ...)
        (Right r = null)
    {
        auto node = new Left;
        mixin ("node.right = r is null ? parse" ~ Right.stringof ~ "() : r;");
        while (currentIsOneOf(Operators))
        {
            static if (__traits(hasMember, Left, "operator"))
                node.operator = advance().type;
            else
                advance();
            auto newNode = new Left;
            newNode.left = node;
            mixin ("newNode.right = parse" ~ Right.stringof ~ "();");
            node = newNode;
        }
        return node;
    }

    ListType parseCommaSeparatedRule(alias ListType, alias ItemType)()
    {
        auto node = new ListType;
        while (true)
        {
            mixin ("node.items ~= parse" ~ ItemType.stringof ~ "();");
            if (currentIs(TokenType.comma))
                advance();
            else
                break;
        }
        return node;
    }

    void warn(lazy string message)
    {
        if (suppressMessages > 0)
            return;
        ++warningCount;
        auto column = index < tokens.length ? tokens[index].column : 0;
        auto line = index < tokens.length ? tokens[index].line : 0;
        if (messageFunction is null)
            writefln("^^ %s(%d:%d): %s", fileName, line, column, message);
        else
            messageFunction(fileName, line, column, message);
    }

    void error(lazy string message)
    {
        import std.stdio;
        if (suppressMessages <= 0)
        {
            ++errorCount;
            auto column = index < tokens.length ? tokens[index].column : 0;
            auto line = index < tokens.length ? tokens[index].line : 0;
            if (messageFunction is null)
                writefln("!! %s(%d:%d): %s", fileName, line, column, message);
            else
                messageFunction(fileName, line, column, message);
        }
        while (moreTokens())
        {
            if (currentIsOneOf(TokenType.semicolon, TokenType.rBrace))
            {
                advance();
                break;
            }
            else
                advance();
        }
    }

    void skipContent(alias O, alias C)()
    {
        int depth = 1;
        while (moreTokens())
        {
            switch (tokens[index].type)
            {
                case O:
                    depth++;
                    advance();
                    break;
                case C:
                    depth--;
                    if (depth <= 0)
                        return;
                    else
                        advance();
                    break;
                default:
                    advance();
                    break;
            }
        }
    }

    void skipBraceContent()
    {
        skipContent!(TokenType.lBrace, TokenType.rBrace)();
    }

    void skipParenContent()
    {
        skipContent!(TokenType.lParen, TokenType.rParen)();
    }

    void skipBracketContent()
    {
        skipContent!(TokenType.lBracket, TokenType.rBracket)();
    }

    const(Token)* peek()
    {
        return index + 1 < tokens.length ? &tokens[index + 1] : null;
    }

    const(Token)* peekPast(alias O, alias C)()
    in
    {
        assert (tokens[index].type == O);
    }
    body
    {
        int depth = 1;
        auto i = index;
        ++i;
        while (i < tokens.length)
        {
            if (i >= tokens.length)
                return null;
            if (tokens[i] == O)
                ++depth;
            else if (tokens[i] == C)
            {
                --depth;
                ++i;
                if (depth <= 0)
                    break;
            }
            ++i;
        }
        return depth == 0 ? &tokens[i] : null;
    }

    const(Token)* peekPastParens()
    {
        return peekPast!(TokenType.lParen, TokenType.rParen)();
    }

    const(Token)* peekPastBrackets()
    {
        return peekPast!(TokenType.lBracket, TokenType.rBracket)();
    }

    const(Token)* peekPastBraces()
    {
        return peekPast!(TokenType.lBrace, TokenType.rBrace)();
    }

    bool peekIs(TokenType t) const
    {
        return index + 1 < tokens.length && tokens[index + 1].type == t;
    }

    bool peekIsOneOf(TokenType[] types...) const
    {
        if (index + 1 >= tokens.length) return false;
        return canFind(types, tokens[index + 1].type);
    }

    /**
     * Returns a token of the specified type if it was the next token, otherwise
     * calls the error function and returns null.
     */
    const(Token)* expect(TokenType type, string loc = __PRETTY_FUNCTION__)
    {
        if (index < tokens.length && tokens[index].type == type)
            return &tokens[index++];
        else
        {
            if (tokenValues[type] is null)
                error("Expected " ~ to!string(type) ~ " instead of "
                    ~ (index < tokens.length ? tokens[index].value : "EOF") ~ " at " ~ loc);
            else
                error("Expected " ~ tokenValues[type] ~ " instead of "
                    ~ (index < tokens.length ? tokens[index].value : "EOF") ~ " at " ~ loc);
            return null;
        }
    }

    /**
     * Returns: the _current token
     */
    Token current() const @property
    {
        return tokens[index];
    }

    /**
     * Advances to the next token and returns the current token
     */
    Token advance()
    {
        return tokens[index++];
    }

    /**
     * Returns: true if the current token has the given type
     */
    bool currentIs(TokenType type) const
    {
        return index < tokens.length && tokens[index] == type;
    }

    /**
     * Returns: true if the current token is one of the given types
     */
    bool currentIsOneOf(TokenType[] types...) const
    {
        return canFind(types, current.type);
    }

    bool startsWith(TokenType[] types...) const
    {
        for (size_t i = 0; i != types.length; ++i)
        {
            if (tokens[index + i].type != types[i])
                return false;
        }
        return true;
    }

    /**
     * Returns: true if there are more tokens
     */
    bool moreTokens() const
    {
        return index + 1 < tokens.length;
    }

    size_t setBookmark()
    {
        ++suppressMessages;
        return index;
    }

    void goToBookmark(size_t i)
    {
        --suppressMessages;
        index = i;
    }

    version (unittest) static void doNothingErrorFunction(string fileName,
        int line, int column, string message) {}

    version (unittest) static Parser getParserForUnittest(string sourceCode,
        string testName)
    {
        LexerConfig config;
        auto r = byToken(cast(const(ubyte)[]) sourceCode, config);
        Parser p;
        //p.messageFunction = &doNothingErrorFunction;
        p.fileName = testName ~ ".d";
        p.tokens = r.array();
        return p;
    }

    template traceEnterAndExit(string fun)
    {
        enum traceEnterAndExit = `version (verbose) trace(">> ` ~ fun ~ ` ");`
            ~ `version (verbose) scope(exit) trace("<< ` ~ fun ~ ` ");`;
    }

    version (verbose)
    {
        void trace(lazy string message)
        {
            if (suppressMessages > 0)
                return;
            if (index < tokens.length)
                writeln(message, "(", current.line, ":", current.column, ")");
            else
                writeln(message, "(EOF:0)");
        }
    }
    else
    {
        void trace(lazy string message) {}
    }

    uint errorCount;
    uint warningCount;
    const(Token)[] tokens;
    size_t index;
    string fileName;
    void function(string, int, int, string) messageFunction;
    static immutable string BASIC_TYPE_CASE_RANGE = q{case bool_: .. case wchar_:};
    static immutable string LITERAL_CASE_RANGE = q{case doubleLiteral: .. case wstringLiteral:};
    static immutable string SPECIAL_CASE_RANGE = q{case specialDate: .. case specialPrettyFunction:};
    int suppressMessages;
}
