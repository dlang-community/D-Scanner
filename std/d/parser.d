// Written in the D programming language

/**
 * This module contains a _parser for D source code.
 *
 * Examples:
 * ---
 * // TODO
 * ---
 *
 * Copyright: Brian Schott 2013
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt Boost, License 1.0)
 * Authors: Brian Schott
 * Source: $(PHOBOSSRC std/d/_parser.d)
 * MACROS:
 *     GRAMMAR = <pre style="font-weight: bold">$0</pre>
 *     RULEDEF = <a name="$0"><span>$0</span></a>
 *     RULE = <a href="#$0"><span style="font-weight: bold">$0</span></a>
 *     LITERAL = <span style="color: green; font-weight: normal;">$0</span>
 */

module std.d.parser;

import std.d.lexer;
import std.d.ast;
import std.conv;
import std.algorithm;
import std.array;
version(unittest) import std.stdio;

/**
* Params:
*     tokens = the tokens parsed by std.d.lexer
* Returns: the parsed module
*/
Module parseModule(R)(R tokens) if (is (ElementType!R == Token))
{
    auto parser = new Parser();
    parser.tokens = tokens.array();
    return parser.parseModule();
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
        auto node = new AddExpression;
        node.right = parseMulExpression();
        while (currentIsOneOf(TokenType.plus, TokenType.minus, TokenType.tilde))
        {
            node.operator = advance().type;
            auto newNode = new AddExpression;
            newNode.left = node;
            newNode.right = parseMulExpression();
            node = newNode;
        }
        return node;
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
        auto node = new AliasDeclaration;
        expect(TokenType.alias_);

        // TODO

        expect(TokenType.semicolon);
        return node;
    }

    /**
     * Parses an AliasInitializer
     * $(GRAMMAR $(RULEDEF aliasInitializer):
     *     $(LITERAL Identifier) $(LITERAL '=') $(RULE type)
     *     ;)
     */
    AliasInitializer parseAliasInitializer()
    {
        auto node = new AliasInitializer;
        node.identifier = *expect(TokenType.identifier);
        expect(TokenType.assign);
        node.type = parseType();
        return node;
    }

    /**
     * Parses an AliasThisDeclaration
     * $(GRAMMAR $(RULEDEF aliasThisDeclaration): $(LITERAL 'alias') $(LITERAL Identifier) $(LITERAL 'this') $(LITERAL ';')
     *     ;)
     */
    AliasThisDeclaration parseAliasThisDeclaration()
    {
        auto node = new AliasThisDeclaration;
        expect(TokenType.alias_);
        node.identifier = *expect(TokenType.identifier);
        expect(TokenType.this_);
        expect(TokenType.semicolon);
        return node;
    }

    /**
     * Parses an AlignAttribute.
     * $(GRAMMAR $(RULEDEF alignAttribute): $(LITERAL 'align') ($(LITERAL '$(LPAREN)') $(LITERAL IntegerLiteral) $(LITERAL '$(RPAREN)'))?
     *     ;)
     */
    AlignAttribute parseAlignAttribute()
    {
        auto node = new AlignAttribute;
        expect(TokenType.align_);
        if (currentIs(TokenType.lParen))
        {
            expect(TokenType.lParen);
            node.intLiteral = *expect(TokenType.intLiteral);
            expect(TokenType.rParen);
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
        auto node = new AndAndExpression;
        node.right = parseOrExpression();
        while (currentIs(TokenType.logicAnd))
        {
            advance();
            auto node2 = new AndAndExpression;
            node2.left = node;
            node2.right = parseOrExpression();
            node = node2;
        }
        return node;
    }

    /**
     * Parses an AndExpression
     *
     * $(GRAMMAR $(RULEDEF andExpression):
     *     $(RULE cmpExpression)
     *     $(RULE andExpression) $(LITERAL '&') $(RULE cmpExpression)
     *     ;)
     */
    AndExpression parseAndExpression()
    {
        auto node = new AndExpression;
        // TODO
        return node;
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
        auto node = new ArgumentList;
        while (true)
        {
            node.arguments ~= parseAssignExpression();
            if (moreTokens() && currentIs(TokenType.comma))
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
     * Parses Arguments
     *
     * $(GRAMMAR $(RULEDEF arguments):
     *     $(LITERAL '$(LPAREN)') $(RULE argumentList)? $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Arguments parseArguments()
    {
        auto node = new Arguments;
        expect(TokenType.lParen);
        node.argumentList = parseArgumentList();
        expect(TokenType.rParen);
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
        auto node = new ArrayInitializer;
        expect(TokenType.lBracket);
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

        expect(TokenType.rBracket);
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
        auto node = new ArrayLiteral;
        expect(TokenType.lBracket);
        node.argumentList = parseArgumentList();
        expect(TokenType.rBracket);
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
        auto node = new ArrayMemberInitialization;
        // TODO
        return node;
    }

    /**
     * Parses an AsmAddExp
     *
     * $(GRAMMAR )
     */
    AsmAddExp parseAsmAddExp()
    {
        auto node = new AsmAddExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmAndExp
     *
     * $(GRAMMAR )
     */
    AsmAndExp parseAsmAndExp()
    {
        auto node = new AsmAndExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmBrExp
     *
     * $(GRAMMAR )
     */
    AsmBrExp parseAsmBrExp()
    {
        auto node = new AsmBrExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmEqualExp
     *
     * $(GRAMMAR )
     */
    AsmEqualExp parseAsmEqualExp()
    {
        auto node = new AsmEqualExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmExp
     *
     * $(GRAMMAR )
     */
    AsmExp parseAsmExp()
    {
        auto node = new AsmExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmInstruction
     *
     * $(GRAMMAR )
     */
    AsmInstruction parseAsmInstruction()
    {
        auto node = new AsmInstruction;
        // TODO
        return node;
    }

    /**
     * Parses an AsmLogAndExp
     *
     * $(GRAMMAR )
     */
    AsmLogAndExp parseAsmLogAndExp()
    {
        auto node = new AsmLogAndExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmLogOrExp
     *
     * $(GRAMMAR )
     */
    AsmLogOrExp parseAsmLogOrExp()
    {
        auto node = new AsmLogOrExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmMulExp
     *
     * $(GRAMMAR )
     */
    AsmMulExp parseAsmMulExp()
    {
        auto node = new AsmMulExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmOrExp
     *
     * $(GRAMMAR )
     */
    AsmOrExp parseAsmOrExp()
    {
        auto node = new AsmOrExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmPrimaryExp
     *
     * $(GRAMMAR )
     */
    AsmPrimaryExp parseAsmPrimaryExp()
    {
        auto node = new AsmPrimaryExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmRelExp
     *
     * $(GRAMMAR )
     */
    AsmRelExp parseAsmRelExp()
    {
        auto node = new AsmRelExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmShiftExp
     *
     * $(GRAMMAR )
     */
    AsmShiftExp parseAsmShiftExp()
    {
        auto node = new AsmShiftExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmStatement
     *
     * $(GRAMMAR )
     */
    AsmStatement parseAsmStatement()
    {
        auto node = new AsmStatement;
        // TODO
        return node;
    }

    /**
     * Parses an AsmTypePrefix
     *
     * $(GRAMMAR )
     */
    AsmTypePrefix parseAsmTypePrefix()
    {
        auto node = new AsmTypePrefix;
        // TODO
        return node;
    }

    /**
     * Parses an AsmUnaExp
     *
     * $(GRAMMAR )
     */
    AsmUnaExp parseAsmUnaExp()
    {
        auto node = new AsmUnaExp;
        // TODO
        return node;
    }

    /**
     * Parses an AsmXorExp
     *
     * $(GRAMMAR )
     */
    AsmXorExp parseAsmXorExp()
    {
        auto node = new AsmXorExp;
        // TODO
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
        auto node = new AssertExpression;
        expect(TokenType.assert_);
        expect(TokenType.lParen);
        node.assertion = parseAssignExpression();
        if (currentIs(TokenType.comma))
        {
            advance();
            node.message = parseAssignExpression();
        }
        expect(TokenType.rParen);
        return node;
    }

    /**
     * Parses an AssertStatement
     *
     * $(GRAMMAR $(RULEDEF assertStatement):
     *     $(RULE assertExpression) $(LITERAL ';')
     *     ;)
     */
    AssertStatement parseAssertStatement()
    {
        auto node = new AssertStatement;
        node.assertExpression = parseAssertExpression();
        expect(TokenType.semicolon);
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
     * Parses an AssignStatement
     *
     * $(GRAMMAR $(RULEDEF assignStatement):
     *       $(RULE unaryExpression) $(RULE assignOperator) $(RULE assignExpression) ($(LITERAL ',') $(RULE unaryExpression) $(RULE assignOperator) $(RULE assignExpression))* $(LITERAL ';')
     *     | $(RULE preIncDecExpression) $(LITERAL ';')
     *     | $(RULE postIncDecExpression) $(LITERAL ';')
     *     ;)
     */
    AssignStatement parseAssignStatement()
    {
        auto node = new AssignStatement;
        // TODO
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
        auto node = new AssocArrayLiteral;
        expect(TokenType.lBracket);
        node.keyValuePairs = parseKeyValuePairs();
        expect(TokenType.rBracket);
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
        auto node = new AtAttribute;
        // TODO
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
     *     | $(LITERAL 'gshared')
     *     | $(LITERAL 'shared')
     *     | $(LITERAL 'immutable')
     *     | $(LITERAL 'inout')
     *     | $(LITERAL 'static')
     *     | $(LITERAL 'pure')
     *     | $(LITERAL 'nothrow')
     *     ;)
     */
    Attribute parseAttribute()
    {
        auto node = new Attribute;
        switch (current().type)
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
            node.attribute = advance().type;
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
        auto node = new AttributedDeclaration;
        node.attribute = parseAttribute();
        switch (current().type)
        {
        case TokenType.colon:
            break;
        case TokenType.lBrace:
            while (moreTokens() && !currentIs(TokenType.rBrace))
                node.declarations ~= parseDeclaration();
            expect(TokenType.rBrace);
            break;
        default:
            node.declarations ~= parseDeclaration();
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
        auto node = new AutoDeclaration;
        // TODO
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
        auto node = new BlockStatement();
        expect(TokenType.lBrace);
        if (!currentIs(TokenType.rBrace))
            node.declarationsAndStatements = parseDeclarationsAndStatements();
        expect(TokenType.rBrace);
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
        expect(TokenType.break_);
        auto node = new BreakStatement;
        switch (current().type)
        {
        case TokenType.identifier:
            node.identifier = advance();
            expect(TokenType.semicolon);
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
	 *       $(RULE typeofExpression) ($(LITERAL '.') $(RULE identifierOrTemplateChain))?
     *     | $(RULE identifierOrTemplateChain)
	 *     ;)
     */
    BaseClass parseBaseClass()
    {
        auto node = new BaseClass;
        if (currentIs(TokenType.typeof_))
        {
            node.typeofExpression = parseTypeofExpression();
            expect(TokenType.dot);
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
        auto node = new BaseClassList;
        do
            node.baseClasses ~= parseBaseClass();
        while(currentIs(TokenType.comma));
        return node;
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
    BasicType parseBasicType()
    {
        auto node = new BasicType;
        if (isBasicType(current().type))
            node.type = advance().type;
        else
        {
            error("Basic type expected");
            return null;
        }
        return node;
    }

    /**
     * Parses a CaseRangeStatement
     *
     * $(GRAMMAR $(RULEDEF caseRangeStatement):
     *     $(LITERAL 'case') $(RULE assignExpression) $(LITERAL ':') $(LITERAL '...') $(LITERAL 'case') $(RULE assignExpression) $(LITERAL ':') $(RULE declarationsAndStatements)
     *     ;)
     */
    CaseRangeStatement parseCaseRangeStatement()
    {
        auto node = new CaseRangeStatement;
        expect(TokenType.case_);
        node.low = parseAssignExpression();
        expect(TokenType.colon);
        expect(TokenType.vararg);
        expect(TokenType.case_);
        node.high = parseAssignExpression();
        expect(TokenType.colon);
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
    CaseStatement parseCaseStatement()
    {
        auto node = new CaseStatement;
        expect(TokenType.case_);
        node.argumentList = parseArgumentList();
        expect(TokenType.colon);
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
        auto node = new CastExpression;
        expect(TokenType.cast_);
        expect(TokenType.lParen);
        if (isCastQualifier())
            node.castQualifier = parseCastQualifier();
        else
            node.type = parseType();
        expect(TokenType.rParen);
        node.unaryExpression = parseUnaryExpression();
        return node;
    }

    private bool isCastQualifier() const
    {
        switch (current().type)
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
        auto node = new CastQualifier;
        switch (current().type)
        {
        case TokenType.inout_:
        case TokenType.const_:
            node.first = advance().type;
            if (currentIs(TokenType.shared_))
                node.second = advance().type;
            break;
        case TokenType.shared_:
            node.first = advance().type;
            if (currentIsOneOf(TokenType.const_, TokenType.inout_))
                node.second = advance().type;
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

    /**
     * Parses a Catch
     *
     * $(GRAMMAR $(RULEDEF catch):
     *     $(LITERAL 'catch') $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL Identifier)? $(LITERAL '$(RPAREN)') $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    Catch parseCatch()
    {
        auto node = new Catch;
        expect(TokenType.catch_);
        expect(TokenType.lParen);
        node.type = parseType();
        if (currentIs(TokenType.identifier))
            node.identifier = advance();
        expect(TokenType.rParen);
        node.nonEmptyStatementNoCaseNoDefault = parseNonEmptyStatementNoCaseNoDefault();
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
     * Parses an ClassBody
     *
     * $(GRAMMAR $(RULEDEF classBody):
     *     $(LITERAL '{') $(RULE declarationOrInvariant)* $(LITERAL '}')
     *     ;)
     */
    ClassBody parseClassBody()
    {
        auto node = new ClassBody;
        expect(TokenType.lBrace);
        while (!currentIs(TokenType.rBrace))
            node.declarationOrInvariants ~= parseDeclarationOrInvariant();
        expect(TokenType.rBrace);
        return node;
    }

    /**
     * Parses an ClassDeclaration
     *
     * $(GRAMMAR $(RULEDEF classDeclaration):
     *     $(LITERAL 'class') $(LITERAL Identifier) ($(RULE templateParameters) $(RULE constraint)?)? ($(LITERAL ':') $(RULE baseClassList))? $(RULE classBody)
     *     ;)
     */
    ClassDeclaration parseClassDeclaration()
    {
        auto node = new ClassDeclaration;
        expect(TokenType.class_);
        node.name = *expect(TokenType.identifier);
        if (currentIs(TokenType.lParen))
        {
            node.templateParameters = parseTemplateParameters();
            if (currentIs(TokenType.if_))
                node.constraint = parseConstraint();
        }
        if (currentIs(TokenType.colon))
            node.superClasses = parseIdentifierList();
        node.classBody = parseClassBody();
        return node;
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
        auto node = new CmpExpression;
        // TODO
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
        auto node = new CompileCondition;
        switch (current().type)
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
        auto node = new ConditionalDeclaration;
        // TODO
        return node;
    }

    /**
     * Parses an ConditionalStatement
     *
     * $(GRAMMAR $(RULEDEF conditionalStatement):
     *     $(RULE compileCondition) $(RULE nonEmptyStatementNoCaseNoDefault) ($(LITERAL 'else') $(RULE nonEmptyStatementNoCaseNoDefault))?
     *     ;)
     */
    ConditionalStatement parseConditionalStatement()
    {
        auto node = new ConditionalStatement;
        // TODO
        return node;
    }

    /**
     * Parses an Constraint
     *
     * $(GRAMMAR $(RULEDEF constraint):
     *     $(LITERAL 'if') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Constraint parseConstraint()
    {
        auto node = new Constraint;
        expect(TokenType.if_);
        expect(TokenType.lParen);
        node.expression = parseExpression();
        expect(TokenType.rParen);
        return node;
    }

    /**
     * Parses a Constructor
     *
     * $(GRAMMAR $(RULEDEF constructor):
     *     $(LITERAL 'this') $(RULE parameters) $(RULE functionBody)
     *     ;)
     */
    Constructor parseConstructor()
    {
        auto node = new Constructor;
        expect(TokenType.this_);
        node.parameters = parseParameters();
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
        expect(TokenType.continue_);
        auto node = new ContinueStatement;
        switch (current().type)
        {
        case TokenType.identifier:
            node.identifier = advance();
            expect(TokenType.semicolon);
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
        auto node = new DebugCondition;
        expect(TokenType.debug_);
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
            expect(TokenType.rParen);
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
        auto node = new DebugSpecification;
        expect(TokenType.debug_);
        expect(TokenType.assign);
        if (currentIsOneOf(TokenType.identifier, TokenType.intLiteral))
            node.identifierOrInteger = advance();
        else
        {
            error("Integer literal or identifier expected");
            return null;
        }
        expect(TokenType.semicolon);
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
     *     ;)
     */
    Declaration parseDeclaration()
    {
        auto node = new Declaration;
        switch (current().type)
        {
        case TokenType.alias_:
            if (startsWith(TokenType.alias_, TokenType.identifier, TokenType.this_))
                node.aliasThisDeclaration = parseAliasThisDeclaration();
            else
                node.aliasDeclaration = parseAliasDeclaration();
            break;
        case TokenType.class_:
            node.classDeclaration = parseClassDeclaration();
            break;
        case TokenType.this_:
            node.constructor = parseConstructor();
            break;
        case TokenType.tilde:
            node.destructor = parseDestructor();
            break;
        case TokenType.enum_:
            node.enumDeclaration = parseEnumDeclaration();
            break;
        case TokenType.import_:
            node.importDeclaration = parseImportDeclaration();
            break;
        case TokenType.interface_:
            node.interfaceDeclaration = parseInterfaceDeclaration();
            break;
        case TokenType.mixin_:
            node.mixinDeclaration = parseMixinDeclaration();
            break;
        case TokenType.pragma_:
            node.pragmaDeclaration = parsePragmaDeclaration();
            break;
        case TokenType.shared_:
            // TODO:
            // sharedStaticConstructor shared static this
            // sharedStaticDestructor shared static ~ this
            // attributedDeclaration shared anything else
            break;
        case TokenType.static_:
            // TODO:
            // staticConstructor static this
            // staticDestructor static ~
            // attributedDeclaration static anything else
            // conditionalDeclaration static if
            break;
        case TokenType.struct_:
            node.structDeclaration = parseStructDeclaration();
            break;
        case TokenType.template_:
            node.templateDeclaration = parseTemplateDeclaration();
            break;
        case TokenType.union_:
            node.unionDeclaration = parseUnionDeclaration();
            break;
        case TokenType.unittest_:
            node.unittest_ = parseUnittest();
            break;
        case TokenType.identifier:
            // TODO:
            // variableDeclaration
            // functionDeclaration
            break;
        case TokenType.version_:
        case TokenType.debug_:
            node.conditionalDeclaration = parseConditionalDeclaration();
            break;
        default:
            error("Declaration expected");
            return null;
        }
        return node;
    }

    /**
     * Parses an DeclarationsAndStatements
     *
    * $(GRAMMAR $(RULEDEF declarationsAndStatements):
    *     ($(RULE declaration) | $(RULE statementNoCaseNoDefault))+
    *     ;)
     */
    DeclarationsAndStatements parseDeclarationsAndStatements()
    {
        auto node = new DeclarationsAndStatements;
        // TODO
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
        auto node = new DeclarationOrInvariant;
        if (currentIs(TokenType.invariant_))
            node.invariant_ = parseInvariant();
        else
            node.declaration = parseDeclaration();
        return node;
    }

    /**
     * Parses a Declarator
     *
     * $(GRAMMAR $(RULEDEF declarator):
     *     $(LITERAL Identifier) $(RULE declaratorSuffix)? ($(LITERAL '=') $(RULE initializer))?
     *     ;)
     */
    Declarator parseDeclarator()
    {
        auto node = new Declarator;
        node.identifier = *expect(TokenType.identifier);
        if (currentIs(TokenType.lBracket))
            node.declaratorSuffix = parseDeclaratorSuffix();
        if (currentIs(TokenType.assign))
        {
            advance();
            node.initializer = parseInitializer();
        }
        return node;
    }

    /**
     * Parses a DeclaratorSuffix
     *
     * $(GRAMMAR $(RULEDEF declaratorSuffix):
     *     $(LITERAL '[') ($(RULE type) | $(RULE assignExpression))? $(LITERAL ']')
     *     ;)
     */
    DeclaratorSuffix parseDeclaratorSuffix()
    {
        auto node = new DeclaratorSuffix;
        // TODO
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
        auto node = new DefaultStatement;
        expect(TokenType.default_);
        expect(TokenType.colon);
        node.declarationsAndStatements = parseDeclarationsAndStatements();
        return node;
    }

    /**
     * Parses an DeleteExpression
     *
     * $(GRAMMAR $(RULEDEF deleteExpression):
     *     $(LITERAL 'delete') $(RULE unaryExpression)
     *     ;)
     */
    DeleteExpression parseDeleteExpression()
    {
        auto node = new DeleteExpression;
        // TODO
        return node;
    }

    /**
     * Parses a DeleteStatement
     *
     * $(GRAMMAR $(RULEDEF deleteStatement):
     *     $(RULE deleteExpression) $(LITERAL ';')
     *     ;)
     */
    DeleteStatement parseDeleteStatement()
    {
        auto node = new DeleteStatement;
        node.deleteExpression = parseDeleteExpression();
        expect(TokenType.semicolon);
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
        auto node = new Deprecated;
        expect(TokenType.deprecated_);
        if (currentIs(TokenType.lParen))
        {
            advance();
            node.assignExpression = parseAssignExpression();
            expect(TokenType.rParen);
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
        auto node = new Destructor;
        expect(TokenType.tilde);
        expect(TokenType.this_);
        expect(TokenType.lParen);
        expect(TokenType.rParen);
        node.functionBody = parseFunctionBody();
        return node;
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
        auto node = new DoStatement;
        expect(TokenType.do_);
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
        expect(TokenType.while_);
        expect(TokenType.lParen);
        node.expression = parseExpression();
        expect(TokenType.rParen);
        expect(TokenType.semicolon);
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
        auto node = new EnumBody;
        if (!currentIs(TokenType.semicolon))
        {
            expect (TokenType.lBrace);
            while (moreTokens())
            {
                if (!currentIs(TokenType.comma))
                    node.enumMembers ~= parseEnumMember();
                if (currentIs(TokenType.comma))
                    continue;
                else if (currentIs(TokenType.rBrace))
                    break;
                else
                {
                    error(`",", "}" or enum member expected`);
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
        auto node = new EnumDeclaration;
        expect(TokenType.enum_);
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
        auto node = new EnumMember;
        // TODO
        return node;
    }

    /**
     * Parses an EqualExpression
     *
     * $(GRAMMAR $(RULEDEF equalExpression):
     *     $(RULE shiftExpression) ($(LITERAL '==') | $(LITERAL '!=')) $(RULE shiftExpression)
     *     ;)
     */
    EqualExpression parseEqualExpression()
    {
        auto node = new EqualExpression;
        // TODO
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
        auto node = new Expression;
        // TODO
        return node;
    }

    /**
     * Parses an FinalSwitchStatement
     *
     * $(GRAMMAR $(RULEDEF finalSwitchStatement):
     *     $(LITERAL 'final') $(RULE switchStatement)
     *     ;)
     */
    FinalSwitchStatement parseFinalSwitchStatement()
    {
        auto node = new FinalSwitchStatement;
        // TODO
        return node;
    }

    /**
     * Parses a Finally
     *
     * $(GRAMMAR $(RULEDEF finally_):
     *     $(LITERAL 'finally') $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    Finally parseFinally()
    {
        auto node = new Finally;
        // TODO
        return node;
    }

    /**
     * Parses a ForStatement
     *
     * $(GRAMMAR $(RULEDEF forStatement):
     *     $(LITERAL 'for') $(LITERAL '$(LPAREN)') $(RULE declarationOrStatement) $(RULE expression)? $(LITERAL ';') $(RULE expression)? $(LITERAL '$(RPAREN)') $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    ForStatement parseForStatement()
    {
        auto node = new ForStatement;
        // TODO
        return node;
    }

    /**
     * Parses a ForeachRangeStatement
     *
     * $(GRAMMAR $(RULEDEF foreachRangeStatement):
     *     $(LITERAL 'foreach') $(LITERAL '$(LPAREN)') $(RULE foreachType) $(LITERAL ';') $(RULE expression) $(LITERAL '..') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    ForeachRangeStatement parseForeachRangeStatement()
    {
        auto node = new ForeachRangeStatement;
        // TODO
        return node;
    }

    /**
     * Parses a ForeachStatement
     *
     * $(GRAMMAR $(RULEDEF foreachStatement):
     *     ($(LITERAL 'foreach') | $(LITERAL 'foreach_reverse')) $(LITERAL '$(LPAREN)') $(RULE foreachTypeList) $(LITERAL ';') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    ForeachStatement parseForeachStatement()
    {
        auto node = new ForeachStatement;
        // TODO
        return node;
    }

    /**
     * Parses a ForeachType
     *
     * $(GRAMMAR $(RULEDEF foreachType):
     *     $(LITERAL 'ref')? $(RULE type)? $(LITERAL Identifier)
     *     ;)
     */
    ForeachType parseForeachType()
    {
        auto node = new ForeachType;
        // TODO
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
        auto node = new ForeachTypeList;
        // TODO
        return node;
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
    FunctionAttribute parseFunctionAttribute()
    {
        auto node = new FunctionAttribute;
        // TODO
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
        // TODO
        return node;
    }

    /**
     * Parses a FunctionCallExpression
     *
     * $(GRAMMAR $(RULEDEF functionCallExpression):
     *     $(RULE unaryExpression) $(RULE templateArguments)? $(RULE arguments)
     *     ;)
     */
    FunctionCallExpression parseFunctionCallExpression()
    {
        auto node = new FunctionCallExpression;
        // TODO
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
        auto node = new FunctionCallStatement;
        // TODO
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
    FunctionDeclaration parseFunctionDeclaration()
    {
        auto node = new FunctionDeclaration;
        // TODO
        return node;
    }

    /**
     * Parses an FunctionLiteralExpression
     *
     * $(GRAMMAR $(RULEDEF functionLiteralExpression):
     *     (($(LITERAL 'function') | $(LITERAL 'delegate')) $(RULE type)?)? ($(RULE parameters) $(RULE functionAttribute)*)? $(RULE functionBody)
     *     ;)
     */
    FunctionLiteralExpression parseFunctionLiteralExpression()
    {
        auto node = new FunctionLiteralExpression;
        // TODO
        return node;
    }

    /**
     * Parses an GotoStatement
     *
     * $(GRAMMAR $(RULEDEF gotoStatement):
     *     $(LITERAL 'goto') ($(LITERAL Identifier) | $(LITERAL 'default') | $(LITERAL 'case') $(RULE expression)?) $(LITERAL ';')
     *     ;)
     */
    GotoStatement parseGotoStatement()
    {
        auto node = new GotoStatement;
		// TODO
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
            node.identifiers ~= *expect(TokenType.identifier);
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

    unittest
    {
        auto input = cast(ubyte[]) "abcde.frocegu.creou.faowe"c;
        LexerConfig config;
        auto tokens = byToken(input, config);
        Parser p;
        p.fileName = "test";
        p.tokens = tokens.array();
        auto chain = p.parseIdentifierChain();
        assert (chain.identifiers == ["abcde", "frocegu", "creou", "faowe"]);
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
        while (true)
        {
            node.identifiers ~= *expect(TokenType.identifier);
            if (currentIs(TokenType.comma))
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
     * Parses an IdentifierOrTemplateChain
     *
     * $(GRAMMAR $(RULEDEF identifierOrTemplateChain):
     *     $(RULE identifierOrTemplateInstance) ($(LITERAL '.') $(RULE identifierOrTemplateInstance))*
     *     ;)
     */
    IdentifierOrTemplateChain parseIdentifierOrTemplateChain()
    {
        auto node = new IdentifierOrTemplateChain;
        // TODO
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
        auto node = new IdentifierOrTemplateInstance;
        // TODO
        return node;
    }

    /**
     * Parses an IdentityExpression
     *
     * $(GRAMMAR $(RULEDEF identityExpression):
     *     $(RULE shiftExpression) ($(LITERAL 'is') | $(LITERAL '!') $(LITERAL 'is')) $(RULE shiftExpression)
     *     ;)
     */
    IdentityExpression parseIdentityExpression()
    {
        auto node = new IdentityExpression;
        // TODO
        return node;
    }

    /**
     * Parses an IfStatement
     *
     * $(GRAMMAR $(RULEDEF ifStatement):
     *     $(LITERAL 'if') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE nonEmptyStatementNoCaseNoDefault) ($(LITERAL 'else') $(RULE nonEmptyStatementNoCaseNoDefault))?
     *     ;)
     */
    IfStatement parseIfStatement()
    {
        auto node = new IfStatement;
        // TODO
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
        // TODO
        return node;
    }

    /**
     * Parses an ImportBindings
     *
     * $(GRAMMAR $(RULEDEF importBindings):
     *     $(RULE singleImport) $(LITERAL ':') $(RULE importBind) ($(LITERAL ',') $(RULE importBind))*
     *     ;)
     */
    ImportBindings parseImportBindings()
    {
        auto node = new ImportBindings;
        // TODO
        return node;
    }

    /**
     * Parses an ImportDeclaration
     *
     * $(GRAMMAR $(RULEDEF importDeclaration):
     *     $(LITERAL 'static')? $(LITERAL 'import') $(RULE importList) $(LITERAL ';')
     *     ;)
     */
    ImportDeclaration parseImportDeclaration()
    {
        auto node = new ImportDeclaration;
        // TODO
        return node;
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
        // TODO
        return node;
    }

    /**
     * Parses an ImportList
     *
     * $(GRAMMAR $(RULEDEF importList): $(RULE singleImport) ($(LITERAL ',') $(RULE importList))?
     *     | $(RULE importBindings)
     *     ;)
     */
    ImportList parseImportList()
    {
        auto node = new ImportList;
        // TODO
        return node;
    }

    /**
     * Parses an IndexExpression
     *
     * $(GRAMMAR $(RULEDEF indexExpression):
     *     $(RULE unaryExpression) $(LITERAL '[') $(RULE argumentList) $(LITERAL ']')
	 *     ;)
     */
    IndexExpression parseImportList()
    {
        auto node = new IndexExpression;
        // TODO
        return node;
    }

    /**
     * Parses an InExpression
     *
     * $(GRAMMAR $(RULEDEF inExpression):
     *     $(RULE shiftExpression) ($(LITERAL 'in') | $(LITERAL '!') $(LITERAL 'in')) $(RULE shiftExpression)
     *     ;)
     */
    InExpression parseInExpression()
    {
        auto node = new InExpression;
        // TODO
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
        // TODO
        return node;
    }

    /**
     * Parses an Initialize
     *
     * $(GRAMMAR $(RULEDEF initialize):
     *       $(LITERAL ';')
     *     | $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    Initialize parseInitialize()
    {
        auto node = new Initialize;
        // TODO
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
        // TODO
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
        return node;
    }

    /**
     * Parses an Invariant
     *
     * $(GRAMMAR $(RULEDEF invariant):
     *     $(LITERAL 'invariant') $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)') $(RULE blockStatement)
     *     ;)
     */
    Invariant parseInvariant()
    {
        auto node = new Invariant;
        // TODO
        return node;
    }

    /**
     * Parses an IsExpression
     *
     * $(GRAMMAR $(RULEDEF isExpression):
     *     $(LITERAL'is') $(LITERAL '$(LPAREN)') ($(RULE assignExpression) | ($(RULE type) $(LITERAL Identifier)? (($(LITERAL ':') | '==') $(RULE typeSpecialization) ($(LITERAL ',') $(RULE templateParameterList))?)?)) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    IsExpression parseIsExpression()
    {
        auto node = new IsExpression;
        // TODO
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
        auto node = new KeyValuePair;
        // TODO
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
        auto node = new KeyValuePairs;
        // TODO
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
        auto node = new LabeledStatement;
        node.identifier = *expect(TokenType.identifier);
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
        auto node = new LambdaExpression;
        // TODO
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
        auto node = new LastCatch;
        // TODO
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
        auto node = new LinkageAttribute;
        expect(TokenType.extern_);
        expect(TokenType.lParen);
        node.identifier = *expect(TokenType.identifier);
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
        auto node = new MemberFunctionAttribute;
        // TODO
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
        auto node = new MixinExpression;
        // TODO
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
        auto node = new MixinTemplateName;
        // TODO
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
        while (index < tokens.length)
        {
            switch (tokens[index].type)
            {
            case TokenType.module_:
                if (m.moduleDeclaration is null)
                    m.moduleDeclaration = parseModuleDeclaration();
                else
                    error("Only one module declaration is allowed per module");
                break;
            default:
                m.declarations ~= parseDeclaration();
            }
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
        auto node = new MulExpression;
        auto left = parseUnaryExpression();
        if (tokens[index] == TokenType.star || tokens[index] == TokenType.div)
        {
            node.operator = tokens[index++];
            node.right = parseUnaryExpression();
        }
        return node;
    }

    /**
     * Parses an NewAnonClassExpression
     *
     * $(GRAMMAR $(RULEDEF newAnonClassExpression):
     *     $(LITERAL 'new') $(RULE arguments)? $(LITERAL 'class') $(RULE arguments)? $(LITERAL Identifier) $(RULE identifierList)? $(RULE classBody)
     *     ;)
     */
    NewAnonClassExpression parseNewAnonClassExpression()
    {
        auto node = new NewAnonClassExpression;
        // TODO
        return node;
    }

    /**
     * Parses an NewExpression
     *
     * $(GRAMMAR $(RULEDEF newExpression):
     *       $(LITERAL 'new') $(RULE type) ($(LITERAL '[') $(RULE assignExpression) $(LITERAL ']') | $(RULE arguments))?
     *     | $(RULE newAnonClassExpression)
     *     ;)
     */
    NewExpression parseNewExpression()
    {
        auto node = new NewExpression;
        // TODO
        return node;
    }

    /**
     * Parses an NonEmptyStatement
     *
     * $(GRAMMAR $(RULEDEF nonEmptyStatement):
     *       $(RULE nonEmptyStatementNoCaseNoDefault)
     *     | $(RULE caseStatement)
     *     | $(RULE caseRangeStatement)
     *     | $(RULE defaultStatement)
     *     ;)
     */
    NonEmptyStatement parseNonEmptyStatement()
    {
        auto node = new NonEmptyStatement;
        return node;
    }

    /**
     * Parses an NonEmptyStatementNoCaseNoDefault
     *
     * $(GRAMMAR $(RULEDEF nonEmptyStatementNoCaseNoDefault):
     *       $(RULE labeledStatement)
     *     | $(RULE blockStatement)
     *     | $(RULE assignStatement)
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
     *     | $(RULE assertStatement)
     *     | $(RULE templateMixinStatement)
     *     | $(RULE versionSpecification)
     *     | $(RULE debugSpecification)
     *     | $(RULE functionCallStatement)
     *     | $(RULE deleteStatement)
     *     ;)
     */
    NonEmptyStatementNoCaseNoDefault parseNonEmptyStatementNoCaseNoDefault()
    {
        auto node = new NonEmptyStatementNoCaseNoDefault;
        // TODO
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
        // TODO
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
        // TODO
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
        auto node = new OrExpression;
        // TODO
        return node;
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
        auto node = new OrOrExpression;
        // TODO
        return node;
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
        // TODO
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
        auto node = new Parameter;
        // TODO
        return node;
    }

    /**
     * Parses a ParameterAttribute
     *
     * $(GRAMMAR $(RULEDEF parameterAttribute):
     *       $(LITERAL 'auto')
     *     | $(LITERAL 'final')
     *     | $(LITERAL 'in')
     *     | $(LITERAL 'lazy')
     *     | $(LITERAL 'out')
     *     | $(LITERAL 'ref')
     *     | $(LITERAL 'scope')
     *     | $(RULE typeConstructor)
     *     ;)
     */
    ParameterAttribute parseParameterAttribute()
    {
        auto node = new ParameterAttribute;
        // TODO
        return node;
    }

    /**
     * Parses Parameters
     *
     * $(GRAMMAR $(RULEDEF parameters):
     *     $(LITERAL '(') (($(RULE parameter) ($(LITERAL ',') $(RULE parameter))*)? ($(LITERAL ',') '...')? | '...') $(LITERAL ')')
     *     ;)
     */
    Parameters parseParameters()
    {
        auto node = new Parameters;
        // TODO
        return node;
    }

    /**
     * Parses a PostIncDecExpression
     *
     * $(GRAMMAR $(RULEDEF postIncDecExpression):
     *     $(RULE unaryExpression) ($(LITERAL '++') | $(LITERAL '--'))
     *     ;)
     */
    PostIncDecExpression parsePostIncDecExpression()
    {
        auto node = new PostIncDecExpression;
        // TODO
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
        auto node = new PowExpression;
        // TODO
        return node;
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
        // TODO
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
        // TODO
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
        auto node = new PreIncDecExpression;
        // TODO
        return node;
    }

    /**
     * Parses a PrimaryExpression
     *
     * $(GRAMMAR $(RULEDEF primaryExpression):
     *       $(RULE identifierOrTemplateInstance)
     *     | $(LITERAL '.') $(RULE identifierOrTemplateInstance)
     *     | $(RULE type) $(LITERAL '.') $(LITERAL Identifier)
     *     | $(RULE typeofExpression)
     *     | $(RULE typeidExpression)
     *     | $(LITERAL '$')
     *     | $(LITERAL 'this')
     *     | $(LITERAL 'super')
     *     | $(LITERAL 'null')
     *     | $(LITERAL 'true')
     *     | $(LITERAL 'false')
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
     *     | $(RULE IntegerLiteral)
     *     | $(RULE FloatLiteral)
     *     | $(RULE StringLiteral)
     *     | $(RULE CharacterLiteral)
     *     | $(RULE arrayLiteral)
     *     | $(RULE assocArrayLiteral)
     *     | $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)')
     *     | $(RULE isExpression)
     *     | $(RULE lambdaExpression)
     *     | $(RULE functionLiteralExpression)
     *     | $(RULE traitsExpression)
     *     | $(RULE mixinExpression)
     *     | $(RULE importExpression)
     *     ;)
     */
    PrimaryExpression parsePrimaryExpression()
    {
        auto node = new PrimaryExpression;
        // TODO
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
        auto node = new Register;
        // TODO
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
    RelExpression parseRelExpression()
    {
        auto node = new RelExpression;
        // TODO
        return node;
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
     *     $(LITERAL 'scope') $(LITERAL '$(LPAREN)') $(LITERAL Identifier) $(LITERAL '$(RPAREN)') $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    ScopeGuardStatement parseScopeGuardStatement()
    {
        auto node = new ScopeGuardStatement;
        // TODO
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
        auto node = new SharedStaticConstructor;
        // TODO
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
        auto node = new SharedStaticDestructor;
        // TODO
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
        auto node = new ShiftExpression;
        // TODO
        return node;
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
        auto node = new SingleImport;
        // TODO
        return node;
    }

    /**
     * Parses a Statement
     *
     * $(GRAMMAR $(RULEDEF statement):
     *       $(LITERAL ';')
     *     | $(RULE nonEmptyStatement)
     *     ;)
     */
    Statement parseStatement()
    {
        auto node = new Statement;
        if (currentIs(TokenType.semicolon))
            advance();
        else
            node.nonEmptyStatement = parseNonEmptyStatement();
        return node;
    }

    /**
     * Parses a StatementNoCaseNoDefault
     *
     * $(GRAMMAR $(RULEDEF statementNoCaseNoDefault):
     *       $(LITERAL ';')
     *     | $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    StatementNoCaseNoDefault parseStatementNoCaseNoDefault()
    {
        auto node = new StatementNoCaseNoDefault;
        if (tokens[index] != TokenType.semicolon)
            node.nonEmptyStatementNoCaseNoDefault = parseNonEmptyStatementNoCaseNoDefault();
        else
            expect(TokenType.semicolon);
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
        auto node = new StaticAssertDeclaration;
        node.staticAssertStatement = parseStaticAssertStatement();
        return node;
    }

    /**
     * Parses a StaticAssertStatement
     *
	 * $(GRAMMAR $(RULEDEF staticAssertStatement):
     *     $(LITERAL 'static') $(RULE assertStatement)
     *     ;)
     */
    StaticAssertStatement parseStaticAssertStatement()
    {
        auto node = new StaticAssertStatement;
        expect(TokenType.static_);
        node.assertStatement = parseAssertStatement();
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
     *     $(RULE 'static') $(LITERAL 'if') $(LITERAL '$(LPAREN)') $(RULE assignExpression) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    StaticIfCondition parseStaticIfCondition()
    {
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
        // TODO
        return node;
    }

    /**
     * Parses a StructBody
     *
     * $(GRAMMAR $(RULEDEF structBody):
     *     $(LITERAL '{') $(RULE declaration)* $(LITERAL '}')
     *     ;)
     */
    StructBody parseStructBody()
    {
        auto node = new StructBody;
        expect(TokenType.lBrace);
        while (tokens[index] != TokenType.rBrace && moreTokens())
            node.declarations ~= parseDeclaration();
        expect(TokenType.rBrace);
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
        auto node = new StructDeclaration;
        expect(TokenType.struct_);
        node.identifier = *expect(TokenType.identifier);
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
        auto node = new StructMemberInitializers;
        // TODO
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
        auto node = new SwitchBody;
        expect(TokenType.lBrace);
        while (moreTokens() && tokens[index] != TokenType.rBrace)
            node.statements ~= parseStatement();
        expect(TokenType.rBrace);
        return node;
    }

    /**
     * Parses an SwitchStatement
     *
     * $(GRAMMAR $(RULEDEF switchStatement):
     *     $(LITERAL 'switch') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE switchBody)
     *     ;)
     */
    SwitchStatement parseSwitchStatement()
    {
        auto node = new SwitchStatement;
        expect(TokenType.switch_);
        expect(TokenType.lParen);
        node.expression = parseExpression();
        expect(TokenType.rParen);
        node.switchBody = parseSwitchBody();
        return node;
    }

    /**
     * Parses an Symbol
     *
     * $(GRAMMAR )
     */
    Symbol parseSymbol()
    {
        auto node = new Symbol;
        if (tokens[index] == TokenType.dot)
        {
            node.hasDot = true;
            ++index;
        }
        node.identifierOrTemplateChain = parseIdentifierOrTemplateChain();
        return node;
    }

    /**
     * Parses an SynchronizedStatement
     *
     * $(GRAMMAR )
     */
    SynchronizedStatement parseSynchronizedStatement()
    {
        auto node = new SynchronizedStatement;
        expect(TokenType.synchronized_);
        if (tokens[index] == TokenType.lParen)
        {
            expect(TokenType.lParen);
            node.expression = parseExpression();
            expect(TokenType.rParen);
        }
        node.nonEmptyStatementNoCaseNoDefault = parseNonEmptyStatementNoCaseNoDefault();
        return node;
    }

    /**
     * Parses an TemplateAliasParameter
     *
     * $(GRAMMAR )
     */
    TemplateAliasParameter parseTemplateAliasParameter()
    {
        auto node = new TemplateAliasParameter;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateArgument
     *
     * $(GRAMMAR )
     */
    TemplateArgument parseTemplateArgument()
    {
        auto node = new TemplateArgument;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateArgumentList
     *
     * $(GRAMMAR )
     */
    TemplateArgumentList parseTemplateArgumentList()
    {
        auto node = new TemplateArgumentList;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateArguments
     *
     * $(GRAMMAR )
     */
    TemplateArguments parseTemplateArguments()
    {
        auto node = new TemplateArguments;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateDeclaration
     *
     * $(GRAMMAR )
     */
    TemplateDeclaration parseTemplateDeclaration()
    {
        auto node = new TemplateDeclaration;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateInstance
     *
     * $(GRAMMAR )
     */
    TemplateInstance parseTemplateInstance()
    {
        auto node = new TemplateInstance;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateMixinStatement
     *
     * $(GRAMMAR )
     */
    TemplateMixinStatement parseTemplateMixinStatement()
    {
        auto node = new TemplateMixinStatement;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateParameter
     *
     * $(GRAMMAR )
     */
    TemplateParameter parseTemplateParameter()
    {
        auto node = new TemplateParameter;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateParameterList
     *
     * $(GRAMMAR )
     */
    TemplateParameterList parseTemplateParameterList()
    {
        auto node = new TemplateParameterList;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateParameters
     *
     * $(GRAMMAR )
     */
    TemplateParameters parseTemplateParameters()
    {
        auto node = new TemplateParameters;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateSingleArgument
     *
     * $(GRAMMAR )
     */
    TemplateSingleArgument parseTemplateSingleArgument()
    {
        auto node = new TemplateSingleArgument;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateThisParameter
     *
     * $(GRAMMAR )
     */
    TemplateThisParameter parseTemplateThisParameter()
    {
        auto node = new TemplateThisParameter;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateTupleParameter
     *
     * $(GRAMMAR )
     */
    TemplateTupleParameter parseTemplateTupleParameter()
    {
        auto node = new TemplateTupleParameter;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateTypeParameter
     *
     * $(GRAMMAR )
     */
    TemplateTypeParameter parseTemplateTypeParameter()
    {
        auto node = new TemplateTypeParameter;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateValueParameter
     *
     * $(GRAMMAR )
     */
    TemplateValueParameter parseTemplateValueParameter()
    {
        auto node = new TemplateValueParameter;
        // TODO
        return node;
    }

    /**
     * Parses an TemplateValueParameterDefault
     *
     * $(GRAMMAR )
     */
    TemplateValueParameterDefault parseTemplateValueParameterDefault()
    {
        auto node = new TemplateValueParameterDefault;
        // TODO
        return node;
    }

    /**
     * Parses an TernaryExpression
     *
     * $(GRAMMAR )
     */
    TernaryExpression parseTernaryExpression()
    {
        auto node = new TernaryExpression;
        node.orOrExpression = parseOrOrExpression();
        if (tokens[index] == TokenType.ternary)
        {
            ++index;
            node.expression = parseExpression();
            expect(TokenType.colon);
            node.ternaryExpression = parseTernaryExpression();
        }
        return node;
    }

    /**
     * Parses an ThrowStatement
     *
     * $(GRAMMAR )
     */
    ThrowStatement parseThrowStatement()
    {
        auto node = new ThrowStatement;
        expect(TokenType.throw_);
        node.expression = parseExpression();
        expect(TokenType.semicolon);
        return node;
    }

    /**
     * Parses an TraitsArgument
     *
     * $(GRAMMAR )
     */
    TraitsArgument parseTraitsArgument()
    {
        auto node = new TraitsArgument;
        // TODO
        return node;
    }

    /**
     * Parses an TraitsExpression
     *
     * $(GRAMMAR )
     */
    TraitsExpression parseTraitsExpression()
    {
        auto node = new TraitsExpression;
        // TODO
        return node;
    }

    /**
     * Parses an TryStatement
     *
     * $(GRAMMAR )
     */
    TryStatement parseTryStatement()
    {
        auto node = new TryStatement;
        // TODO
        return node;
    }

    /**
     * Parses an Type
     *
     * $(GRAMMAR )
     */
    Type parseType()
    {
        auto node = new Type;
        // TODO
        return node;
    }

    /**
     * Parses an Type2
     *
     * $(GRAMMAR )
     */
    Type2 parseType2()
    {
        auto node = new Type2;
        // TODO
        return node;
    }

    /**
     * Parses an Type3
     *
     * $(GRAMMAR )
     */
    Type3 parseType3()
    {
        auto node = new Type3;
        // TODO
        return node;
    }

    /**
     * Parses an TypeConstructor
     *
     * $(GRAMMAR )
     */
    TypeConstructor parseTypeConstructor()
    {
        auto node = new TypeConstructor;
        // TODO
        return node;
    }

    /**
     * Parses an TypeConstructors
     *
     * $(GRAMMAR )
     */
    TypeConstructors parseTypeConstructors()
    {
        auto node = new TypeConstructors;
        // TODO
        return node;
    }

    /**
     * Parses an TypeSpecialization
     *
     * $(GRAMMAR )
     */
    TypeSpecialization parseTypeSpecialization()
    {
        auto node = new TypeSpecialization;
        // TODO
        return node;
    }

    /**
     * Parses an TypeSuffix
     *
     * $(GRAMMAR )
     */
    TypeSuffix parseTypeSuffix()
    {
        auto node = new TypeSuffix;
        // TODO
        return node;
    }

    /**
     * Parses an TypeidExpression
     *
     * $(GRAMMAR )
     */
    TypeidExpression parseTypeidExpression()
    {
        auto node = new TypeidExpression;
        // TODO
        return node;
    }

    /**
     * Parses an TypeofExpression
     *
     * $(GRAMMAR )
     */
    TypeofExpression parseTypeofExpression()
    {
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
     *     | $(RULE newExpression)
     *     | $(RULE deleteExpression)
     *     | $(RULE castExpression)
     *     | $(RULE functionCallExpression)
     *     | $(RULE preIncDecExpression)
     *     | $(RULE postIncDecExpression)
     *     | $(RULE sliceExpression)
     *     | $(RULE indexExpression)
     *     | $(RULE unaryExpression) $(LITERAL '.') $(RULE identifierOrTemplateInstance)
     *     | $(RULE assertExpression)
     *     ;)
     */
    UnaryExpression parseUnaryExpression()
    {
        auto node = new UnaryExpression;
        // TODO
        return node;
    }

    /**
     * Parses an UnionDeclaration
     *
     * $(GRAMMAR )
     */
    UnionDeclaration parseUnionDeclaration()
    {
        auto node = new UnionDeclaration;
        // TODO
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
        auto node = new Unittest;
        expect(TokenType.unittest_);
        node.blockStatement = parseBlockStatement();
        return node;
    }

    /**
     * Parses a VariableDeclaration
     *
     * $(GRAMMAR )
     */
    VariableDeclaration parseVariableDeclaration()
    {
        auto node = new VariableDeclaration;
        // TODO
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
     *     $(LITERAL 'with') $(LITERAL '$(LPAREN)') ($(RULE expression) | $(RULE symbol) | $(RULE templateInstance)) $(LITERAL '$(RPAREN)') $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
     */
    WithStatement parseWithStatement()
    {
        auto node = new WithStatement;
        expect(TokenType.with_);
        expect(TokenType.lParen);
        // TODO: magic here
        expect(TokenType.rParen);
        parseNonEmptyStatementNoCaseNoDefault();
        return node;
    }

    /**
     * Parses an XorExpression
     *
     * $(GRAMMAR )
     */
    XorExpression parseXorExpression()
    {
        auto node = new XorExpression;
        // TODO
        return node;
    }

    void error(string message)
    {
        ++errorCount;
        import std.stdio;
        stderr.writefln("%s(%d:%d): %s", fileName, tokens[index].line,
            tokens[index].column, message);
        while (index < tokens.length)
        {
            if (tokens[++index].type == TokenType.semicolon)
                break;
        }
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
        while (index < tokens.length)
        {
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
    const(Token)* expect(TokenType type)
    {
        if (tokens[index].type == type)
            return &tokens[index++];
        else
        {
            error("Expected " ~ to!string(type));
            return null;
        }
    }

    /**
     * Returns: the current token
     */
    Token current() const
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
        return canFind(types, current().type);
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
        return index < tokens.length;
    }

    uint errorCount;
    const(Token)[] tokens;
    size_t index;
    string fileName;
}
