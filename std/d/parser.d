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
 * The grammar for D starts with the $(LINK2 #.module, module) rule.
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
version(verbose) import std.stdio;

// TODO: any place that says *expect(...) needs to be fixed

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
		version(verbose) writeln("parseAddExpression");
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
     * $(GRAMMAR $(RULEDEF aliasThisDeclaration):
     *     $(LITERAL 'alias') $(LITERAL Identifier) $(LITERAL 'this') $(LITERAL ';')
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
     * $(GRAMMAR $(RULEDEF alignAttribute):
     *     $(LITERAL 'align') ($(LITERAL '$(LPAREN)') $(LITERAL IntegerLiteral) $(LITERAL '$(RPAREN)'))?
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
		version(verbose) writeln("parseAndAndExpression");
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
		version(verbose) writeln("parseAndExpression");
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
        // TODO
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
        // TODO
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
        // TODO
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
        // TODO
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
        // TODO
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
        // TODO
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
        // TODO
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
        // TODO
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
        // TODO
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
        // TODO
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
        // TODO
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
        // TODO
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
        auto node = new AsmStatement;
        // TODO
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
        // TODO
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
        // TODO
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
		version(verbose) writeln("parseAssignExpression");
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
     *     ;)
     */
    AssignStatement parseAssignStatement()
    {
        auto node = new AssignStatement;
        // TODO
        expect(TokenType.semicolon);
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
        expect(TokenType.at);
        with (TokenType) switch (current().type)
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
        version (development) skipBraceContent();
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
     *     ($(RULE typeofExpression) $(LITERAL '.'))? $(RULE identifierOrTemplateChain)
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
        if (isBasicType(current().type))
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
        auto node = new Catch;
        expect(TokenType.catch_);
        expect(TokenType.lParen);
        node.type = parseType();
        if (currentIs(TokenType.identifier))
            node.identifier = advance();
        expect(TokenType.rParen);
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
        auto node = new ClassBody;
        expect(TokenType.lBrace);
        version (development) skipBraceContent();
        expect(TokenType.rBrace);
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
        auto node = new ClassDeclaration;
        expect(TokenType.class_);
        node.name = *expect(TokenType.identifier);
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
class ClassFour(A, B) if (someTest()) : Super {}};

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
        assert (classTwo.baseClassList.baseClasses.length == 1,
            to!string(classTwo.baseClassList.baseClasses.length));
        assert (classTwo.classBody.declarationOrInvariants.length == 0,
            to!string(classTwo.classBody.declarationOrInvariants.length));

        auto classThree = p.parseClassDeclaration();
        assert (classThree.name == "ClassThree", classThree.name.value);
        assert (classThree.templateParameters !is null);
        assert (classThree.baseClassList !is null);
        assert (classThree.baseClassList.baseClasses.length == 1);
        assert (classThree.classBody.declarationOrInvariants.length == 0,
            to!string(classThree.classBody.declarationOrInvariants.length));

        auto classFour = p.parseClassDeclaration();
        assert (classFour.name == "ClassFour", classFour.name.value);
        assert (classFour.templateParameters !is null);
        assert (classFour.baseClassList !is null);
        assert (classFour.constraint !is null);
        assert (classFour.baseClassList.baseClasses.length == 1);
        assert (classFour.classBody.declarationOrInvariants.length == 0,
            to!string(classFour.classBody.declarationOrInvariants.length));

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
		version(verbose) writeln("parseCmpExpression");
        auto node = new CmpExpression;
        auto shift = parseShiftExpression();
        with (TokenType) switch (current().type)
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
        node.compileCondition = parseCompileCondition();
        return node;
    }

    /**
     * Parses an ConditionalStatement
     *
     * $(GRAMMAR $(RULEDEF conditionalStatement):
     *     $(RULE compileCondition) $(RULE statementNoCaseNoDefault) ($(LITERAL 'else') $(RULE nonEmptyStatementNoCaseNoDefault))?
     *     ;)
     */
    ConditionalStatement parseConditionalStatement()
    {
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
        auto node = new Constraint;
        expect(TokenType.if_);
        expect(TokenType.lParen);
        version (development)
            skipParenContent();
        else
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
        import std.stdio;
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
            if (startsWith(TokenType.shared_, TokenType.static_, TokenType.this_))
                node.sharedStaticConstructor = parseSharedStaticConstructor();
            else if (startsWith(TokenType.shared_, TokenType.static_, TokenType.tilde))
                node.sharedStaticDestructor = parseSharedStaticDestructor();
            else
                node.attributedDeclaration = parseAttributedDeclaration();
            break;
        case TokenType.static_:
            if (startsWith(TokenType.static_, TokenType.this_))
                node.staticConstructor = parseStaticConstructor();
            else if (startsWith(TokenType.static_, TokenType.tilde))
                node.staticDestructor = parseStaticDestructor();
            else if (startsWith(TokenType.static_, TokenType.if_))
                node.conditionalDeclaration = parseConditionalDeclaration();
            else
                node.attributedDeclaration = parseAttributedDeclaration();
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
        case TokenType.bool_: .. case TokenType.wchar_:
        case TokenType.identifier:
            Type type = parseType();
            if (!currentIs(TokenType.identifier))
            {
                error("Identifier expected");
                return null;
            }
            if (peekIs(TokenType.lParen))
                node.functionDeclaration = parseFunctionDeclaration(type);
            else
                node.variableDeclaration = parseVariableDeclaration(type);
            break;
        case TokenType.version_:
			if (peekIs(TokenType.lParen))
				node.conditionalDeclaration = parseConditionalDeclaration();
			else if (peekIs(TokenType.assign))
				node.versionSpecification = parseVersionSpecification();
			else
			{
				error(`"=" or "(" expected following "version"`);
				return null;
			}
			break;
        case TokenType.debug_:
            node.conditionalDeclaration = parseConditionalDeclaration();
            break;
		case TokenType.at:
		case TokenType.extern_:
		case TokenType.align_:
		//case TokenType.pragma_:
		case TokenType.deprecated_:
		case TokenType.private_:
		case TokenType.package_:
		case TokenType.protected_:
		case TokenType.public_:
		case TokenType.export_:
		//case TokenType.extern_:
		case TokenType.final_:
		case TokenType.synchronized_:
		case TokenType.override_:
		case TokenType.abstract_:
		case TokenType.const_:
		case TokenType.auto_:
		case TokenType.scope_:
		case TokenType.gshared:
		//case TokenType.shared_:
		case TokenType.immutable_:
		case TokenType.inout_:
		//case TokenType.static_:
		case TokenType.pure_:
		case TokenType.nothrow_:
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
     *     $(LITERAL Identifier) ($(LITERAL '=') $(RULE initializer))?
     *     ;)
     */
    Declarator parseDeclarator()
    {
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
        auto node = new DefaultStatement;
        expect(TokenType.default_);
        expect(TokenType.colon);
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
        auto node = new DeleteExpression;
        // TODO
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
    EqualExpression parseEqualExpression(ShiftExpression shift = null)
    {
		version(verbose) writeln("parseEqualExpression");
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
		auto node = new ExpressionStatement;
		node.expression = expression is null ? parseExpression() : expression;
		expect(TokenType.semicolon);
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
        auto node = new FinalSwitchStatement;
        expect(TokenType.final_);
        node.switchStatement = parseSwitchStatement();
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
        auto node = new Finally;
        expect(TokenType.finally_);
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
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
    FunctionAttribute parseFunctionAttribute()
    {
        auto node = new FunctionAttribute;
        with (TokenType) switch (current().type)
        {
        case at:
            node.atAttribute = parseAtAttribute();
            break;
        case pure_:
        case nothrow_:
            node.token = advance();
            break;
        default:
            break;
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
        auto node = new FunctionCallStatement;
        node.functionCallExpression = parseFunctionCallExpression();
        expect(TokenType.semicolon);
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
        auto node = new FunctionDeclaration;

        while(moreTokens() && currentIsMemberFunctionAttribute())
            node.memberFunctionAttributes ~= parseMemberFunctionAttribute();

        switch (current.type)
        {
        case TokenType.auto_:
            break;
        case TokenType.ref_:
            break;
        default:
            break;
        }

        node.returnType = type is null ? parseType() : type;

        auto ident = expect(TokenType.identifier);
        if (ident is null) return null;

        node.name = *ident;

        if (!currentIs(TokenType.lParen))
        {
            error(`"(" expected`);
            return null;
        }

        bool isTemplate = peekPastParens().type == TokenType.lParen;

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
        // TODO
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
        expect(TokenType.goto_);
        with (TokenType) switch (current().type)
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
        expect(TokenType.semicolon);
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

    unittest // TODO
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
        while (true)
        {
            node.identifierOrTemplateInstances ~= parseIdentifierOrTemplateInstance();
            if (!currentIs(TokenType.dot))
                break;
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
        auto node = new IdentifierOrTemplateInstance;
        if (peekIs(TokenType.not))
            node.templateInstance = parseTemplateInstance();
        else
            node.identifier = *expect(TokenType.identifier);
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
        auto node = new IdentityExpression;
        node.left = shift is null ? parseShiftExpression() : shift;
        if (currentIs(TokenType.not))
            node.negated = true;
        expect(TokenType.is_);
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
        auto node = new IfStatement;
        expect(TokenType.if_);
        expect(TokenType.lParen);
        node.expression = parseExpression();
        expect(TokenType.rParen);
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
        if (!currentIs(TokenType.identifier))
        {
            error("Identifier expected.");
            return null;
        }
        if (peekIs(TokenType.assign))
        {
            node.left = advance();
            advance();
            node.hasRight = true;
            node.right = *expect(TokenType.identifier);
        }
        else
            node.left = advance();
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
        expect(TokenType.colon);
        while (true)
        {
            node.importBinds ~= parseImportBind();
            if (moreTokens() && currentIs(TokenType.comma))
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
     *     $(LITERAL 'import') ($(RULE singleImport) ($(LITERAL ',') $(RULE singleImport))* | $(RULE importBindings)) $(LITERAL ';')
     *     ;)
     */
    ImportDeclaration parseImportDeclaration()
    {
        auto node = new ImportDeclaration;
        expect(TokenType.import_);
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
                    node.singleImports ~= parseSingleImport();
                    if (moreTokens() && currentIs(TokenType.comma))
                        advance();
                    else
                        break;
                }
            }
        }
        expect(TokenType.semicolon);
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
        expect(TokenType.import_);
        expect(TokenType.lParen);
        node.assignExpression = parseAssignExpression();
        expect(TokenType.rParen);
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
        expect(TokenType.in_);
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
        expect(TokenType.in_);
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
        expect(TokenType.interface_);
        if (!currentIs(TokenType.identifier))
        {
            error("Identifier expected");
            return null;
        }
        node.identifier = advance();
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
     *     $(LITERAL'is') $(LITERAL '$(LPAREN)') ($(RULE assignExpression) | ($(RULE type) $(LITERAL Identifier)? (($(LITERAL ':') | '==') $(RULE typeSpecialization) ($(LITERAL ',') $(RULE templateParameterList))?)?)) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    IsExpression parseIsExpression()
    {
        auto node = new IsExpression;
        if (expect(TokenType.is_) is null) return null;
        if (expect(TokenType.lParen) is null) return null;
        // TODO
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
		version(verbose) writeln("parseMulExpression");
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
            node.baseClassList = parseBaseClassList;
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
     * Parses a NonEmptyStatement
     *
     * $(GRAMMAR $(RULEDEF nonEmptyStatement):
     *       $(RULE statementNoCaseNoDefault)
     *     | $(RULE caseStatement)
     *     | $(RULE caseRangeStatement)
     *     | $(RULE defaultStatement)
     *     ;)
     */
    NonEmptyStatement parseNonEmptyStatement()
    {
        auto node = new NonEmptyStatement;
        with (TokenType) switch (current().type)
        {
        case default_:
            node.defaultStatement = parseDefaultStatement();
            break;
        case case_:
            // TODO
            break;
        default:
            node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
            break;
        }
        return node;
    }

    /**
     * Parses a StatementNoCaseNoDefault
     *
     * $(GRAMMAR $(RULEDEF statementNoCaseNoDefault):
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
     *     | $(RULE templateMixinStatement)
     *     | $(RULE versionSpecification)
     *     | $(RULE debugSpecification)
     *     | $(RULE expressionStatement)
     *     ;)
     */
    StatementNoCaseNoDefault parseStatementNoCaseNoDefault()
    {
        auto node = new StatementNoCaseNoDefault;
        with (TokenType) switch (current().type)
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
        // TODO
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
        case delete_:
		case assert_:
        default:
			node.expressionStatement = parseExpressionStatement();
            break;
        // TODO: assignStatement
        // TODO: finalSwitchStatement
        // TODO: foreachRangeStatement
        // TODO: conditionalStatement
        // TODO: staticAssertStatement
        // TODO: templateMixinStatement
        // TODO: versionSpecification
        // TODO: debugSpecification
        // TODO: functionCallStatement
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
		version(verbose) writeln("parseOrExpression");
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
		version(verbose) writeln("parseOrOrExpression");
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
            node.parameter = *expect(TokenType.identifier);
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
        auto node = new Parameter;
        version (verbose) writeln("parseParameter");
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
				node.vararg = true;
			else if (currentIs(TokenType.assign))
			{
				advance();
				node.default_ = parseAssignExpression();
			}
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
        with (TokenType) switch (current().type)
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
        version (verbose) writeln("parseParameters");
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
(double ...)}c;

        Parser p = getParserForUnittest(sourceCode, "parseParameters");

		Parameters params1 = p.parseParameters();
		assert (params1.hasVarargs);
		assert (params1.paramaters.length == 1);
		assert (params1.paramaters[0].name == "a");

		Parameters params2 = p.parseParameters();
		assert (params2.paramaters.length == 1);
		assert (params2.paramaters[0].vararg);
		assert (params2.paramaters[0].type !is null);
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
     *       $(LITERAL Identifier)
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
		version(verbose) writeln("parsePrimaryExpression");
        auto node = new PrimaryExpression;
        with (TokenType) switch (current().type)
        {
        case identifier:
            if (peekIs(TokenType.goesTo))
                node.lambdaExpression = parseLambdaExpression();
            else
                node.identifier = advance();
            break;
        mixin (BASIC_TYPE_CASE_RANGE);
            node.basicType = advance().type;
            expect(dot);
            auto t = expect(identifier);
            if (t !is null)
                node.identifier = *t;
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
            if (peekPastParens().type == TokenType.goesTo)
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
			version(verbose) writeln("special or literal ", index, " ", current());
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
        auto node = new Register;
        node.identifier = *expect(TokenType.identifier);
        if (currentIs(TokenType.lParen))
        {
            advance();
            node.intLiteral = *expect(TokenType.intLiteral);
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
        auto node = new ScopeGuardStatement;
        expect(TokenType.scope_);
        expect(TokenType.lParen);
        node.identifier = *expect(TokenType.identifier);
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
		version(verbose) writeln("parseShiftExpression");
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
        auto node = new SingleImport;
        if (!currentIs(TokenType.identifier))
        {
            error("Identifier expected");
            return null;
        }
        if (peekIs(TokenType.assign))
        {
            node.identifier = advance();
            advance();
        }
        node.identifierChain = parseIdentifierChain();
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
        auto node = new Statement;
        switch (current().type)
        {
        case TokenType.case_:
            // TODO
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
        auto node = new StaticAssertDeclaration;
        // TODO
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
     *     $(LITERAL 'static') $(LITERAL 'if') $(LITERAL '$(LPAREN)') $(RULE assignExpression) $(LITERAL '$(RPAREN)')
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
     *     $(LITERAL '{') $(RULE structBodyItem)* $(LITERAL '}')
     *     ;)
     */
    StructBody parseStructBody()
    {
        auto node = new StructBody;
        expect(TokenType.lBrace);
		version (development)
			skipBraceContent();
		else
			static assert (0);
        //while (tokens[index] != TokenType.rBrace && moreTokens())
        //    node.structBodyItems ~= parseStructBodyItem();
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
        auto node = new StructDeclaration;
        expect(TokenType.struct_);
        node.name = *expect(TokenType.identifier);
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
     * Parses a SwitchStatement
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
     * Parses a Symbol
     *
     * $(GRAMMAR $(RULEDEF symbol):
     *     $(LITERAL '.')? $(RULE identifierOrTemplateChain)
     *     ;)
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
     * Parses a SynchronizedStatement
     *
     * $(GRAMMAR $(RULEDEF synchronizedStatement):
     *     $(LITERAL 'synchronized') ($(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)'))? $(RULE nonEmptyStatementNoCaseNoDefault)
     *     ;)
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
        node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault();
        return node;
    }

    /**
     * Parses a TemplateAliasParameter
     *
     * $(GRAMMAR $(RULEDEF templateAliasParameter):
     *     $(LITERAL 'alias') $(RULE type)? $(LITERAL Identifier) ($(LITERAL ':') ($(RULE type) | $(RULE expression)))? ($(LITERAL '=') ($(RULE type) | $(RULE expression)))?
     *     ;)
     */
    TemplateAliasParameter parseTemplateAliasParameter()
    {
        auto node = new TemplateAliasParameter;
        expect(TokenType.alias_);
        // TODO
        return node;
    }

    /**
     * Parses a TemplateArgument
     *
     * $(GRAMMAR $(RULEDEF templateArgument):
     *       $(RULE type)
     *     | $(RULE assignExpression)
     *     | $(RULE symbol)
     *     ;)
     */
    TemplateArgument parseTemplateArgument()
    {
        auto node = new TemplateArgument;
        // TODO
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
        auto node = new TemplateDeclaration;
        expect(TokenType.template_);
        node.identifier = *expect(TokenType.identifier);
        node.templateParameters = parseTemplateParameters();
        if (currentIs(TokenType.if_))
            node.constraint = parseConstraint();
        expect(TokenType.lBrace);
        do
            node.declarations ~= parseDeclaration();
        while (!currentIs(TokenType.rBrace));
        expect(TokenType.rBrace);
        return node;
    }

    /**
     * Parses a TemplateInstance
     *
     * $(GRAMMAR $(RULEDEF templateInstance):
     *     $(LITERAL symbol) $(RULE templateArguments)
     *     ;)
     */
    TemplateInstance parseTemplateInstance()
    {
        auto node = new TemplateInstance;
        node.symbol = parseSymbol();
        node.templateArguments = parseTemplateArguments();
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
        auto node = new TemplateParameter;
        // TODO
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
        auto node = new TemplateParameters;
        expect(TokenType.lParen);
        version (development)
			skipParenContent();
		else
			node.templateParameterList = parseTemplateParameterList();
        expect(TokenType.rParen);
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
        auto node = new TemplateSingleArgument;
        with (TokenType) switch (current().type)
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
        auto node = new TemplateTupleParameter;
        auto i = expect(TokenType.identifier);
        if (i is null)
            return null;
        node.identifier = *i;
        expect(TokenType.vararg);
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
        auto node = new TemplateTypeParameter;
        // TODO
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
        auto node = new TemplateValueParameter;
        // TODO
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
        auto node = new TemplateValueParameterDefault;
        expect(TokenType.assign);
        with (TokenType) switch (current().type)
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
		version(verbose) writeln("parseTernaryExpression");
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
        auto node = new ThrowStatement;
        expect(TokenType.throw_);
        node.expression = parseExpression();
        expect(TokenType.semicolon);
        return node;
    }

    /**
     * Parses an TraitsArgument
     *
     * $(GRAMMAR $(RULEDEF traitsArgument):
     *       $(RULE assignExpression)
     *     | $(RULE type)
     *     ;)
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
     * $(GRAMMAR $(RULEDEF traitsExpression):
     *     $(LITERAL '__traits') $(LITERAL '$(LPAREN)') $(LITERAL Identifier) ($(LITERAL ',') $(RULE traitsArgument))+ $(LITERAL '$(RPAREN)')
     *     ;)
     */
    TraitsExpression parseTraitsExpression()
    {
        auto node = new TraitsExpression;
        expect(TokenType.traits);
        expect(TokenType.lParen);
        node.identifier = *expect(TokenType.identifier);
        do
        {
            expect(TokenType.comma);
            node.traitsArguments ~= parseTraitsArgument();
        }
        while (moreTokens() && currentIs(TokenType.comma));
        expect(TokenType.rParen);
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
        auto node = new Type;
        switch(current.type)
        {
        case TokenType.const_:
        case TokenType.immutable_:
        case TokenType.inout_:
        case TokenType.shared_:
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
     *     ;)
     */
    TokenType parseTypeConstructor(bool validate = true)
    {
        with (TokenType) switch (current().type)
		{
		case const_:
		case immutable_:
		case inout_:
		case shared_:
			return advance().type;
		default:
			if (validate)
				error(`"const", "immutable", "inout", or "shared" expected`);
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
     *     | $(LITERAL '___parameters')
     *     ;)
     */
    TypeSpecialization parseTypeSpecialization()
    {
        auto node = new TypeSpecialization;
        with (TokenType) switch (current().type)
		{
		case struct_:
		case union_:
		case interface_:
		case enum_:
		case function_:
		case delegate_:
		case super_:
		case const_:
		case immutable_:
		case inout_:
		case shared_:
		case return_:
		case parameters:
			node.token = advance();
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
        auto node = new TypeSuffix;
        version (verbose) writeln("parseTypeSuffix");
        with (TokenType) switch(current().type)
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
            }
            else
                node.type = type;
        end:
            expect(TokenType.rBracket);
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
        auto node = new TypeidExpression;
        expect(TokenType.typeid_);
        expect(TokenType.lParen);
        // TODO
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
	 *     | $(RULE unaryExpression) ($(LITERAL '--')
	 *     | $(RULE unaryExpression) ($(LITERAL '++')
     *     ;)
     */
    UnaryExpression parseUnaryExpression()
    {
		version(verbose) writeln("parseUnaryExpression");
        auto node = new UnaryExpression;
        with(TokenType) switch (current().type)
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
            node.castExpression = parseCastExpression;
            return node;
        case assert_:
            node.assertExpression = parseAssertExpression();
            return node;
		default:
			node.primaryExpression = parsePrimaryExpression();
        }

		loop: while (true) with (TokenType) switch (current().type)
		{
		case not:
			index++;
			bool jump = peekPastParens().type == TokenType.lParen;
			index--;
			if (jump)
				goto case lParen;
			else
			{
				error(`Function call expected`);
				return null;
			}
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
			assert (false);
		case dot:
			advance();
			auto n = new UnaryExpression();
			n.unaryExpression = node;
			n.identifierOrTemplateInstance = parseIdentifierOrTemplateInstance();
			break;
		default:
			break loop;
		}
		return node;
    }

    /**
     * Parses an UnionDeclaration
     *
     * $(GRAMMAR $(RULEDEF unionDeclaration):
     *     $(LITERAL 'union') $(LITERAL Identifier) (($(RULE templateParameters) $(RULE constraint)? $(RULE structBody))? | ($(RULE structBody) | $(LITERAL ';')))
     *     ;)
     */
    UnionDeclaration parseUnionDeclaration()
    {
        auto node = new UnionDeclaration;
        expect(TokenType.union_);
        node.identifier = *expect(TokenType.identifier);
        if (currentIs(TokenType.lParen))
        {
            node.templateParameters = parseTemplateParameters();
            if (currentIs(TokenType.if_))
                node.constraint = parseConstraint();
            node.structBody = parseStructBody;
        }
        else
        {
            if (currentIs(TokenType.semicolon))
                advance;
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
        auto node = new Unittest;
        expect(TokenType.unittest_);
        node.blockStatement = parseBlockStatement();
        return node;
    }

    /**
     * Parses a VariableDeclaration
     *
     * $(GRAMMAR $(RULEDEF variableDeclaration):
     *       $(RULE storageClass)? $(RULE type) $(RULE declarator) ($(LITERAL ',') $(RULE declarator))* $(LITERAL ';')
     *     | $(RULE autoDeclaration)
     *     ;)
     */
    VariableDeclaration parseVariableDeclaration(Type type = null)
    {
        auto node = new VariableDeclaration;

		if (currentIs(TokenType.auto_))
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
        parseStatementNoCaseNoDefault();
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
		version(verbose) writeln("parseXorExpression");
        return parseLeftAssocBinaryExpression!(XorExpression, AndExpression,
            TokenType.xor)();
    }

private:

    bool isAssociativeArrayLiteral()
    {
        auto i = index;
        scope(exit) index = i;
        if (!currentIs(TokenType.lBracket))
            return false;
        advance();
        while (moreTokens()) with (TokenType) switch (current().type)
        {
        case lBrace: skipBraceContent(); break;
        case lParen: skipParenContent(); break;
        case lBracket: skipBracketContent(); break;
        case rBracket: return false;
        case colon: return true;
        default: break;
        }
        return false;
    }

    bool isStatement() const
    {
        // TODO: Not complete
        with (TokenType) switch (current().type)
        {
        case lBrace:
        case if_:
        case while_:
        case do_:
        case for_:
        case foreach_:
        case foreach_reverse_:
        case switch_:
        case continue_:
        case break_:
        case return_:
        case goto_:
        case with_:
        case try_:
        case throw_:
        case asm_:
        case default_:
        case case_:
        case new_:
        mixin (SPECIAL_CASE_RANGE);
        mixin (LITERAL_CASE_RANGE);
        case true_:
        case false_:
            return true;
        case extern_:
        case union_:
        case class_:
        case interface_:
        case function_:
        case delegate_:
        case typedef_:
        case typeof_:
        case invariant_:
        case alias_:
        mixin (BASIC_TYPE_CASE_RANGE);
            return false;
        default:
            return false;
        }
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

    void error(lazy string message)
    {
        import std.stdio;
        ++errorCount;
        if (!suppressMessages)
        {
            if (errorFunction is null)
                stderr.writefln("%s(%d:%d): %s", fileName, tokens[index].line,
                    tokens[index].column, message);
            else
                errorFunction(fileName, tokens[index].line, tokens[index].column,
                    message);
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
            error("Expected " ~ tokenValues[type] ~ " instead of "
                ~ tokens[index].value);
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
        return index + 1 < tokens.length;
    }

    size_t setBookmark()
    {
        suppressMessages = true;
        return index;
    }

    void goToBookmark(size_t i)
    {
        suppressMessages = false;
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
        p.errorFunction = &doNothingErrorFunction;
        p.fileName = testName ~ ".d";
        p.tokens = r.array();
        return p;
    }

    uint errorCount;
    const(Token)[] tokens;
    size_t index;
    string fileName;
    void function(string, int, int, string) errorFunction;
    immutable string BASIC_TYPE_CASE_RANGE = q{case bool_: .. case wchar_:};
    immutable string LITERAL_CASE_RANGE = q{case doubleLiteral: .. case wstringLiteral:};
    immutable string SPECIAL_CASE_RANGE = q{case specialDate: .. case specialPrettyFunction:};
    bool suppressMessages;
}
