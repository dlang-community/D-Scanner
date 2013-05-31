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
 *     GRAMMAR = <pre>$0</pre>
 */

module std.d.parser;

import std.d.lexer;
import std.d.ast;
import std.conv;
import std.algorithm;
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
	 * $(GRAMMAR addExpression: mulExpression
	 *     | addExpression ('+' | '-' | '~') mulExpression
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
	 * Parses an AliasDeclaration
	 * $(GRAMMAR aliasDeclaration: 'alias' (aliasInitializer (',' aliasInitializer)* | type declarator) ';'
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
	 * $(GRAMMAR aliasInitializer: Identifier '=' type
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
	 * $(GRAMMAR aliasThisDeclaration: 'alias' Identifier 'this' ';'
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
	 * $(GRAMMAR alignAttribute: 'align' ('(' IntegerLiteral ')')?
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
	 * $(GRAMMAR andAndExpression: orExpression
	 *     | andAndExpression '&&' orExpression
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
	 * $(GRAMMAR )
	 */
	AndExpression parseAndExpression()
	{
		auto node = new AndExpression;

		return node;
	}

	/**
	 * Parses an ArgumentList
	 *
	 * $(GRAMMAR )
	 */
	ArgumentList parseArgumentList()
	{
		auto node = new ArgumentList;

		return node;
	}

	/**
	 * Parses an Arguments
	 *
	 * $(GRAMMAR )
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
	 * $(GRAMMAR )
	 */
	ArrayInitializer parseArrayInitializer()
	{
		auto node = new ArrayInitializer;

		return node;
	}

	/**
	 * Parses an ArrayLiteral
	 *
	 * $(GRAMMAR )
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
	 * $(GRAMMAR )
	 */
	ArrayMemberInitialization parseArrayMemberInitialization()
	{
		auto node = new ArrayMemberInitialization;

		return node;
	}

	/**
	 * Parses an ArrayMemberInitializations
	 *
	 * $(GRAMMAR )
	 */
	ArrayMemberInitializations parseArrayMemberInitializations()
	{
		auto node = new ArrayMemberInitializations;

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

		return node;
	}

	/**
	 * Parses an AssertExpression
	 *
	 * $(GRAMMAR )
	 */
	AssertExpression parseAssertExpression()
	{
		auto node = new AssertExpression;

		return node;
	}

	/**
	 * Parses an AssertStatement
	 *
	 * $(GRAMMAR )
	 */
	AssertStatement parseAssertStatement()
	{
		auto node = new AssertStatement;

		return node;
	}

	/**
	 * Parses an AssignExpression
	 *
	 * $(GRAMMAR )
	 */
	AssignExpression parseAssignExpression()
	{
		auto node = new AssignExpression;

		return node;
	}

	/**
	 * Parses an AssignStatement
	 *
	 * $(GRAMMAR )
	 */
	AssignStatement parseAssignStatement()
	{
		auto node = new AssignStatement;

		return node;
	}

	/**
	 * Parses an AssocArrayLiteral
	 *
	 * $(GRAMMAR )
	 */
	AssocArrayLiteral parseAssocArrayLiteral()
	{
		auto node = new AssocArrayLiteral;

		return node;
	}

	/**
	 * Parses an AtAttribute
	 *
	 * $(GRAMMAR )
	 */
	AtAttribute parseAtAttribute()
	{
		auto node = new AtAttribute;

		return node;
	}

	/**
	 * Parses an Attribute
	 *
	 * $(GRAMMAR )
	 */
	Attribute parseAttribute()
	{
		auto node = new Attribute;

		return node;
	}

	/**
	 * Parses an AttributedDeclaration
	 *
	 * $(GRAMMAR )
	 */
	AttributedDeclaration parseAttributedDeclaration()
	{
		auto node = new AttributedDeclaration;

		return node;
	}

	/**
	 * Parses an AutoDeclaration
	 *
	 * $(GRAMMAR )
	 */
	AutoDeclaration parseAutoDeclaration()
	{
		auto node = new AutoDeclaration;

		return node;
	}

	/**
	 * Parses an BlockStatement
	 *
	 * $(GRAMMAR )
	 */
	BlockStatement parseBlockStatement()
	{
		auto node = new BlockStatement();
		expect(TokenType.lBrace);
		switch (tokens[index].type)
		{
		case TokenType.rBrace:
			break;
		default:
			node.declarationsAndStatements = parseDeclarationsAndStatements();
		}
		return node;
	}

	/**
	 * Parses a BodyStatement
	 *
	 * $(GRAMMAR bodyStatement: 'body' blockStatement
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
	 * $(GRAMMAR breakStatement: 'break' Identifier? ';'
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
	 * Parses an BuiltinType
	 *
	 * $(GRAMMAR builtinType: 'bool'
     *    | 'byte'
     *    | 'ubyte'
     *    | 'short'
     *    | 'ushort'
     *    | 'int'
     *    | 'uint'
     *    | 'long'
     *    | 'ulong'
     *    | 'char'
     *    | 'wchar'
     *    | 'dchar'
     *    | 'float'
     *    | 'double'
     *    | 'real'
     *    | 'ifloat'
     *    | 'idouble'
     *    | 'ireal'
     *    | 'cfloat'
     *    | 'cdouble'
     *    | 'creal'
     *    | 'void'
     *    ;)
	 */
	BuiltinType parseBuiltinType()
	{
		auto node = new BuiltinType;
        if (isBasicType(current().type)
            node.type = advance().type;
        else
        {
            error("Basic type expected");
            return null;
        }
		return node;
	}

	/**
	 * Parses an CaseRangeStatement
	 *
	 * $(GRAMMAR )
	 */
	CaseRangeStatement parseCaseRangeStatement()
	{
		auto node = new CaseRangeStatement;

		return node;
	}

	/**
	 * Parses an CaseStatement
	 *
	 * $(GRAMMAR )
	 */
	CaseStatement parseCaseStatement()
	{
		auto node = new CaseStatement;

		return node;
	}

	/**
	 * Parses an CastExpression
	 *
	 * $(GRAMMAR )
	 */
	CastExpression parseCastExpression()
	{
		auto node = new CastExpression;

		return node;
	}

	/**
	 * Parses a CastQualifier
	 *
	 * $(GRAMMAR castQualifier: 'const'
     *    | 'const' 'shared'
     *    | 'immutable'
     *    | 'inout'
     *    | 'inout' 'shared'
     *    | 'shared'
     *    | 'shared' 'const'
     *    | 'shared' 'inout'
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
                node.second == advance().type;
            break;
        case TokenType.shared_:
            node.first = advance().type;
            if (currentIsOneOf(TokenType.const_, TokenType.inout_))
                node.second == advance().type;
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
	 * $(GRAMMAR catch_: 'catch' '(' type Identifier? ')' nonEmptyStatementNoCaseNoDefault
     *    ;)
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
	 * $(GRAMMAR )
	 */
	Catches parseCatches()
	{
		auto node = new Catches;

		return node;
	}

	/**
	 * Parses an ClassBody
	 *
	 * $(GRAMMAR )
	 */
	ClassBody parseClassBody()
	{
		auto node = new ClassBody;

		return node;
	}

	/**
	 * Parses an ClassDeclaration
	 *
	 * $(GRAMMAR )
	 */
	ClassDeclaration parseClassDeclaration()
	{
		auto node = new ClassDeclaration;

		return node;
	}

	/**
	 * Parses an CmpExpression
	 *
	 * $(GRAMMAR )
	 */
	CmpExpression parseCmpExpression()
	{
		auto node = new CmpExpression;

		return node;
	}

	/**
	 * Parses an CompileCondition
	 *
	 * $(GRAMMAR )
	 */
	CompileCondition parseCompileCondition()
	{
		auto node = new CompileCondition;

		return node;
	}

	/**
	 * Parses an ConditionalDeclaration
	 *
	 * $(GRAMMAR )
	 */
	ConditionalDeclaration parseConditionalDeclaration()
	{
		auto node = new ConditionalDeclaration;

		return node;
	}

	/**
	 * Parses an ConditionalStatement
	 *
	 * $(GRAMMAR )
	 */
	ConditionalStatement parseConditionalStatement()
	{
		auto node = new ConditionalStatement;

		return node;
	}

	/**
	 * Parses an Constraint
	 *
	 * $(GRAMMAR )
	 */
	Constraint parseConstraint()
	{
		auto node = new Constraint;

		return node;
	}

	/**
	 * Parses a Constructor
	 *
	 * $(GRAMMAR constructor: 'this' parameters functionBody
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
	 * $(GRAMMAR continueStatement: 'continue' Identifier? ';'
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
			error("Identifier or semicolon expected following \"continue\"");
			return null;
		}
		return node;
	}

	/**
	 * Parses an DebugCondition
	 *
	 * $(GRAMMAR )
	 */
	DebugCondition parseDebugCondition()
	{
		auto node = new DebugCondition;

		return node;
	}

	/**
	 * Parses an DebugSpecification
	 *
	 * $(GRAMMAR )
	 */
	DebugSpecification parseDebugSpecification()
	{
		auto node = new DebugSpecification;

		return node;
	}

	/**
	 * Parses an Declaration
	 *
	 * $(GRAMMAR )
	 */
	Declaration parseDeclaration()
	{
		auto node = new Declaration;

		return node;
	}

	/**
	 * Parses an DeclarationsAndStatements
	 *
	 * $(GRAMMAR )
	 */
	DeclarationsAndStatements parseDeclarationsAndStatements()
	{
		auto node = new DeclarationsAndStatements;

		return node;
	}

	/**
	 * Parses an Declarator
	 *
	 * $(GRAMMAR )
	 */
	Declarator parseDeclarator()
	{
		auto node = new Declarator;

		return node;
	}

	/**
	 * Parses an DeclaratorSuffix
	 *
	 * $(GRAMMAR )
	 */
	DeclaratorSuffix parseDeclaratorSuffix()
	{
		auto node = new DeclaratorSuffix;

		return node;
	}

	/**
	 * Parses an DefaultStatement
	 *
	 * $(GRAMMAR )
	 */
	DefaultStatement parseDefaultStatement()
	{
		auto node = new DefaultStatement;

		return node;
	}

	/**
	 * Parses an DeleteExpression
	 *
	 * $(GRAMMAR )
	 */
	DeleteExpression parseDeleteExpression()
	{
		auto node = new DeleteExpression;

		return node;
	}

	/**
	 * Parses an DeleteStatement
	 *
	 * $(GRAMMAR )
	 */
	DeleteStatement parseDeleteStatement()
	{
		auto node = new DeleteStatement;

		return node;
	}

	/**
	 * Parses an Deprecated
	 *
	 * $(GRAMMAR )
	 */
	Deprecated parseDeprecated()
	{
		auto node = new Deprecated;

		return node;
	}

	/**
	 * Parses a Destructor
	 *
	 * $(GRAMMAR destructor: '~' 'this' '(' ')' functionBody
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
	 * Parses an DoStatement
	 *
	 * $(GRAMMAR )
	 */
	DoStatement parseDoStatement()
	{
		auto node = new DoStatement;

		return node;
	}

	/**
	 * Parses an EnumBody
	 *
	 * $(GRAMMAR )
	 */
	EnumBody parseEnumBody()
	{
		auto node = new EnumBody;

		return node;
	}

	/**
	 * Parses an EnumDeclaration
	 *
	 * $(GRAMMAR )
	 */
	EnumDeclaration parseEnumDeclaration()
	{
		auto node = new EnumDeclaration;

		return node;
	}

	/**
	 * Parses an EnumMember
	 *
	 * $(GRAMMAR )
	 */
	EnumMember parseEnumMember()
	{
		auto node = new EnumMember;

		return node;
	}

	/**
	 * Parses an EqualExpression
	 *
	 * $(GRAMMAR )
	 */
	EqualExpression parseEqualExpression()
	{
		auto node = new EqualExpression;

		return node;
	}

	/**
	 * Parses an Expression
	 *
	 * $(GRAMMAR )
	 */
	Expression parseExpression()
	{
		auto node = new Expression;

		return node;
	}

	/**
	 * Parses an FinalSwitchStatement
	 *
	 * $(GRAMMAR )
	 */
	FinalSwitchStatement parseFinalSwitchStatement()
	{
		auto node = new FinalSwitchStatement;

		return node;
	}

	/**
	 * Parses an Finally
	 *
	 * $(GRAMMAR )
	 */
	Finally parseFinally()
	{
		auto node = new Finally;

		return node;
	}

	/**
	 * Parses an ForStatement
	 *
	 * $(GRAMMAR )
	 */
	ForStatement parseForStatement()
	{
		auto node = new ForStatement;

		return node;
	}

	/**
	 * Parses an ForeachRangeStatement
	 *
	 * $(GRAMMAR )
	 */
	ForeachRangeStatement parseForeachRangeStatement()
	{
		auto node = new ForeachRangeStatement;

		return node;
	}

	/**
	 * Parses an ForeachStatement
	 *
	 * $(GRAMMAR )
	 */
	ForeachStatement parseForeachStatement()
	{
		auto node = new ForeachStatement;

		return node;
	}

	/**
	 * Parses an ForeachType
	 *
	 * $(GRAMMAR )
	 */
	ForeachType parseForeachType()
	{
		auto node = new ForeachType;

		return node;
	}

	/**
	 * Parses an ForeachTypeList
	 *
	 * $(GRAMMAR )
	 */
	ForeachTypeList parseForeachTypeList()
	{
		auto node = new ForeachTypeList;

		return node;
	}

	/**
	 * Parses an FunctionAttribute
	 *
	 * $(GRAMMAR )
	 */
	FunctionAttribute parseFunctionAttribute()
	{
		auto node = new FunctionAttribute;

		return node;
	}

	/**
	 * Parses an FunctionBody
	 *
	 * $(GRAMMAR )
	 */
	FunctionBody parseFunctionBody()
	{
		auto node = new FunctionBody;

		return node;
	}

	/**
	 * Parses an FunctionCallExpression
	 *
	 * $(GRAMMAR )
	 */
	FunctionCallExpression parseFunctionCallExpression()
	{
		auto node = new FunctionCallExpression;

		return node;
	}

	/**
	 * Parses an FunctionCallStatement
	 *
	 * $(GRAMMAR )
	 */
	FunctionCallStatement parseFunctionCallStatement()
	{
		auto node = new FunctionCallStatement;

		return node;
	}

	/**
	 * Parses an FunctionDeclaration
	 *
	 * $(GRAMMAR )
	 */
	FunctionDeclaration parseFunctionDeclaration()
	{
		auto node = new FunctionDeclaration;

		return node;
	}

	/**
	 * Parses an FunctionLiteralExpression
	 *
	 * $(GRAMMAR )
	 */
	FunctionLiteralExpression parseFunctionLiteralExpression()
	{
		auto node = new FunctionLiteralExpression;

		return node;
	}

	/**
	 * Parses an GotoStatement
	 *
	 * $(GRAMMAR )
	 */
	GotoStatement parseGotoStatement()
	{
		auto node = new GotoStatement;
		return node;
	}

	/**
	 * Parses an IdentifierChain
	 *
	 * $(GRAMMAR )
	 */
	IdentifierChain parseIdentifierChain()
	{
		auto node = new IdentifierChain;

		return node;
	}

	/**
	 * Parses an IdentifierList
	 *
	 * $(GRAMMAR )
	 */
	IdentifierList parseIdentifierList()
	{
		auto node = new IdentifierList;

		return node;
	}

	/**
	 * Parses an IdentifierOrTemplateChain
	 *
	 * $(GRAMMAR )
	 */
	IdentifierOrTemplateChain parseIdentifierOrTemplateChain()
	{
		auto node = new IdentifierOrTemplateChain;

		return node;
	}

	/**
	 * Parses an IdentifierOrTemplateInstance
	 *
	 * $(GRAMMAR )
	 */
	IdentifierOrTemplateInstance parseIdentifierOrTemplateInstance()
	{
		auto node = new IdentifierOrTemplateInstance;

		return node;
	}

	/**
	 * Parses an IdentityExpression
	 *
	 * $(GRAMMAR )
	 */
	IdentityExpression parseIdentityExpression()
	{
		auto node = new IdentityExpression;

		return node;
	}

	/**
	 * Parses an IfStatement
	 *
	 * $(GRAMMAR )
	 */
	IfStatement parseIfStatement()
	{
		auto node = new IfStatement;

		return node;
	}

	/**
	 * Parses an ImportBind
	 *
	 * $(GRAMMAR )
	 */
	ImportBind parseImportBind()
	{
		auto node = new ImportBind;

		return node;
	}

	/**
	 * Parses an ImportBindings
	 *
	 * $(GRAMMAR )
	 */
	ImportBindings parseImportBindings()
	{
		auto node = new ImportBindings;

		return node;
	}

	/**
	 * Parses an ImportDeclaration
	 *
	 * $(GRAMMAR )
	 */
	ImportDeclaration parseImportDeclaration()()
	{
		auto node = new ImportDeclaration;

		return node;
	}

	/**
	 * Parses an ImportExpression
	 *
	 * $(GRAMMAR )
	 */
	ImportExpression parseImportExpression()
	{
		auto node = new ImportExpression;

		return node;
	}

	/**
	 * Parses an ImportList
	 *
	 * $(GRAMMAR )
	 */
	ImportList parseImportList()
	{
		auto node = new ImportList;

		return node;
	}

	/**
	 * Parses an InExpression
	 *
	 * $(GRAMMAR )
	 */
	InExpression parseInExpression()
	{
		auto node = new InExpression;

		return node;
	}

	/**
	 * Parses an InStatement
	 *
	 * $(GRAMMAR )
	 */
	InStatement parseInStatement()
	{
		auto node = new InStatement;

		return node;
	}

	/**
	 * Parses an Initialize
	 *
	 * $(GRAMMAR )
	 */
	Initialize parseInitialize()
	{
		auto node = new Initialize;

		return node;
	}

	/**
	 * Parses an Initializer
	 *
	 * $(GRAMMAR )
	 */
	Initializer parseInitializer()
	{
		auto node = new Initializer;

		return node;
	}

	/**
	 * Parses an InterfaceDeclaration
	 *
	 * $(GRAMMAR )
	 */
	InterfaceDeclaration parseInterfaceDeclaration()
	{
		auto node = new InterfaceDeclaration;
		return node;
	}

	/**
	 * Parses an Invariant
	 *
	 * $(GRAMMAR )
	 */
	Invariant parseInvariant()
	{
		auto node = new Invariant;

		return node;
	}

	/**
	 * Parses an IsExpression
	 *
	 * $(GRAMMAR )
	 */
	IsExpression parseIsExpression()
	{
		auto node = new IsExpression;

		return node;
	}

	/**
	 * Parses an KeyValuePair
	 *
	 * $(GRAMMAR )
	 */
	KeyValuePair parseKeyValuePair()
	{
		auto node = new KeyValuePair;

		return node;
	}

	/**
	 * Parses an KeyValuePairs
	 *
	 * $(GRAMMAR )
	 */
	KeyValuePairs parseKeyValuePairs()
	{
		auto node = new KeyValuePairs;

		return node;
	}

	/**
	 * Parses an LabeledStatement
	 *
	 * $(GRAMMAR )
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
	 * Parses an LambdaExpression
	 *
	 * $(GRAMMAR )
	 */
	LambdaExpression parseLambdaExpression()
	{
		auto node = new LambdaExpression;

		return node;
	}

	/**
	 * Parses an LastCatch
	 *
	 * $(GRAMMAR )
	 */
	LastCatch parseLastCatch()
	{
		auto node = new LastCatch;

		return node;
	}

	/**
	 * Parses an LinkageAttribute
	 *
	 * $(GRAMMAR )
	 */
	LinkageAttribute parseLinkageAttribute()
	{
		auto node = new LinkageAttribute;

		return node;
	}

	/**
	 * Parses an MemberFunctionAttribute
	 *
	 * $(GRAMMAR )
	 */
	MemberFunctionAttribute parseMemberFunctionAttribute()
	{
		auto node = new MemberFunctionAttribute;

		return node;
	}

	/**
	 * Parses an MixinDeclaration
	 *
	 * $(GRAMMAR )
	 */
	MixinDeclaration parseMixinDeclaration()
	{
		auto node = new MixinDeclaration;

		return node;
	}

	/**
	 * Parses an MixinExpression
	 *
	 * $(GRAMMAR )
	 */
	MixinExpression parseMixinExpression()
	{
		auto node = new MixinExpression;

		return node;
	}

	/**
	 * Parses an MixinTemplateName
	 *
	 * $(GRAMMAR )
	 */
	MixinTemplateName parseMixinTemplateName()
	{
		auto node = new MixinTemplateName;

		return node;
	}

	/**
	 * Parses a Module
	 *
	 * $(GRAMMAR module: moduleDeclaration? declaration*
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
	 * $(GRAMMAR moduleDeclaration: 'module' identifierChain ';'
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
	 * $(GRAMMAR mulExpression: unaryExpression
	 *     | mulExpression ('*' | '/' | '%') unaryExpression
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
	 * $(GRAMMAR )
	 */
	NewAnonClassExpression parseNewAnonClassExpression()
	{
		auto node = new NewAnonClassExpression;

		return node;
	}

	/**
	 * Parses an NewExpression
	 *
	 * $(GRAMMAR )
	 */
	NewExpression parseNewExpression()
	{
		auto node = new NewExpression;

		return node;
	}

	/**
	 * Parses an NonEmptyStatement
	 *
	 * $(GRAMMAR )
	 */
	NonEmptyStatement parseNonEmptyStatement()
	{
		auto node = new NonEmptyStatement;
		return node;
	}

	/**
	 * Parses an NonEmptyStatementNoCaseNoDefault
	 *
	 * $(GRAMMAR )
	 */
	NonEmptyStatementNoCaseNoDefault parseNonEmptyStatementNoCaseNoDefault()
	{
		auto node = new NonEmptyStatementNoCaseNoDefault;

		return node;
	}

	/**
	 * Parses an NonVoidInitializer
	 *
	 * $(GRAMMAR )
	 */
	NonVoidInitializer parseNonVoidInitializer()
	{
		auto node = new NonVoidInitializer;

		return node;
	}

	/**
	 * Parses an Opcode
	 *
	 * $(GRAMMAR )
	 */
	Opcode parseOpcode()
	{
		auto node = new Opcode;

		return node;
	}

	/**
	 * Parses an Operand
	 *
	 * $(GRAMMAR )
	 */
	Operand parseOperand()
	{
		auto node = new Operand;

		return node;
	}

	/**
	 * Parses an Operands
	 *
	 * $(GRAMMAR )
	 */
	Operands parseOperands()
	{
		auto node = new Operands;

		return node;
	}

	/**
	 * Parses an OrExpression
	 *
	 * $(GRAMMAR )
	 */
	OrExpression parseOrExpression()
	{
		auto node = new OrExpression;

		return node;
	}

	/**
	 * Parses an OrOrExpression
	 *
	 * $(GRAMMAR )
	 */
	OrOrExpression parseOrOrExpression()
	{
		auto node = new OrOrExpression;

		return node;
	}

	/**
	 * Parses an OutStatement
	 *
	 * $(GRAMMAR )
	 */
	OutStatement parseOutStatement()
	{
		auto node = new OutStatement;

		return node;
	}

	/**
	 * Parses an Parameter
	 *
	 * $(GRAMMAR )
	 */
	Parameter parseParameter()
	{
		auto node = new Parameter;

		return node;
	}

	/**
	 * Parses an ParameterAttribute
	 *
	 * $(GRAMMAR )
	 */
	ParameterAttribute parseParameterAttribute()
	{
		auto node = new ParameterAttribute;

		return node;
	}

	/**
	 * Parses an Parameters
	 *
	 * $(GRAMMAR )
	 */
	Parameters parseParameters()
	{
		auto node = new Parameters;

		return node;
	}

	/**
	 * Parses an PostIncDecExpression
	 *
	 * $(GRAMMAR )
	 */
	PostIncDecExpression parsePostIncDecExpression()
	{
		auto node = new PostIncDecExpression;

		return node;
	}

	/**
	 * Parses an PowExpression
	 *
	 * $(GRAMMAR )
	 */
	PowExpression parsePowExpression()
	{
		auto node = new PowExpression;

		return node;
	}

	/**
	 * Parses an PragmaDeclaration
	 *
	 * $(GRAMMAR )
	 */
	PragmaDeclaration parsePragmaDeclaration()
	{
		auto node = new PragmaDeclaration;

		return node;
	}

	/**
	 * Parses an PragmaExpression
	 *
	 * $(GRAMMAR )
	 */
	PragmaExpression parsePragmaExpression()
	{
		auto node = new PragmaExpression;

		return node;
	}

	/**
	 * Parses an PreIncDecExpression
	 *
	 * $(GRAMMAR )
	 */
	PreIncDecExpression parsePreIncDecExpression()
	{
		auto node = new PreIncDecExpression;

		return node;
	}

	/**
	 * Parses an PrimaryExpression
	 *
	 * $(GRAMMAR )
	 */
	PrimaryExpression parsePrimaryExpression()
	{
		auto node = new PrimaryExpression;

		return node;
	}

	/**
	 * Parses an ProtectionAttribute
	 *
	 * $(GRAMMAR )
	 */
	ProtectionAttribute parseProtectionAttribute()
	{
		auto node = new ProtectionAttribute;

		return node;
	}

	/**
	 * Parses an Register
	 *
	 * $(GRAMMAR )
	 */
	Register parseRegister()
	{
		auto node = new Register;

		return node;
	}

	/**
	 * Parses an RelExpression
	 *
	 * $(GRAMMAR )
	 */
	RelExpression parseRelExpression()
	{
		auto node = new RelExpression;

		return node;
	}

	/**
	 * Parses an ReturnStatement
	 *
	 * $(GRAMMAR )
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
	 * Parses an ScopeGuardStatement
	 *
	 * $(GRAMMAR )
	 */
	ScopeGuardStatement parseScopeGuardStatement()
	{
		auto node = new ScopeGuardStatement;

		return node;
	}

	/**
	 * Parses an SharedStaticConstructor
	 *
	 * $(GRAMMAR )
	 */
	SharedStaticConstructor parseSharedStaticConstructor()
	{
		auto node = new SharedStaticConstructor;

		return node;
	}

	/**
	 * Parses an SharedStaticDestructor
	 *
	 * $(GRAMMAR )
	 */
	SharedStaticDestructor parseSharedStaticDestructor()
	{
		auto node = new SharedStaticDestructor;

		return node;
	}

	/**
	 * Parses an ShiftExpression
	 *
	 * $(GRAMMAR )
	 */
	ShiftExpression parseShiftExpression()
	{
		auto node = new ShiftExpression;

		return node;
	}

	/**
	 * Parses an SingleImport
	 *
	 * $(GRAMMAR )
	 */
	SingleImport parseSingleImport()
	{
		auto node = new SingleImport;

		return node;
	}

	/**
	 * Parses a Statement
	 *
	 * $(GRAMMAR statement: ';'
     *     | nonEmptyStatement
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
	 * Parses an StatementNoCaseNoDefault
	 *
	 * $(GRAMMAR )
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
	 * Parses an StaticAssertDeclaration
	 *
	 * $(GRAMMAR )
	 */
	StaticAssertDeclaration parseStaticAssertDeclaration()
	{
		auto node = new StaticAssertDeclaration;
		node.staticAssertStatement = parseStaticAssertStatement();
		return node;
	}

	/**
	 * Parses an StaticAssertStatement
	 *
	 * $(GRAMMAR )
	 */
	StaticAssertStatement parseStaticAssertStatement()
	{
		auto node = new StaticAssertStatement;
		expect(TokenType.static_);
		node.assertStatement = parseAssertStatement();
		return node;
	}

	/**
	 * Parses an StaticConstructor
	 *
	 * $(GRAMMAR )
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
	 * Parses an StaticDestructor
	 *
	 * $(GRAMMAR )
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
	 * $(GRAMMAR )
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
	 * $(GRAMMAR )
	 */
	StorageClass parseStorageClass()
	{
		auto node = new StorageClass;

		return node;
	}

	/**
	 * Parses an StructBody
	 *
	 * $(GRAMMAR )
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
	 * Parses an StructDeclaration
	 *
	 * $(GRAMMAR structDeclaration: 'struct' Identifier (templateParameters constraint? structBody | (structBody | ';'))
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
	 * $(GRAMMAR )
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
	 * Parses an StructMemberInitializer
	 *
	 * $(GRAMMAR )
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
	 * Parses an StructMemberInitializers
	 *
	 * $(GRAMMAR )
	 */
	StructMemberInitializers parseStructMemberInitializers()
	{
		auto node = new StructMemberInitializers;

		return node;
	}

	/**
	 * Parses an SwitchBody
	 *
	 * $(GRAMMAR )
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
	 * $(GRAMMAR )
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
	 * Parses an UnaryExpression
	 *
	 * $(GRAMMAR )
	 */
	UnaryExpression parseUnaryExpression()
	{
		auto node = new UnaryExpression;

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

		return node;
	}

	/**
	 * Parses an Unittest
	 *
	 * $(GRAMMAR )
	 */
	Unittest parseUnittest()
	{
		auto node = new Unittest;
		expect(TokenType.unittest_);
		node.blockStatement = parseBlockStatement();
		return node;
	}

	/**
	 * Parses an VariableDeclaration
	 *
	 * $(GRAMMAR )
	 */
	VariableDeclaration parseVariableDeclaration()
	{
		auto node = new VariableDeclaration;

		return node;
	}

	/**
	 * Parses an VersionCondition
	 *
	 * $(GRAMMAR )
	 */
	VersionCondition parseVersionCondition()
	{
		auto node = new VersionCondition;
		expect(TokenType.version_);
		expect(TokenType.lParen);
		node.token = tokens[index];
		expect(TokenType.rParen);
		return node;
	}

	/**
	 * Parses an VersionSpecification
	 *
	 * $(GRAMMAR )
	 */
	VersionSpecification parseVersionSpecification()
	{
		auto node = new VersionSpecification;
		expect(TokenType.version_);
		expect(TokenType.assign);
		node.token = tokens[index];
		expect(TokenType.semicolon);
		return node;
	}

	/**
	 * Parses an WhileStatement
	 *
	 * $(GRAMMAR )
	 */
	WhileStatement parseWhileStatement()
	{
		auto node = new WhileStatement;

		return node;
	}

	/**
	 * Parses an WithStatement
	 *
	 * $(GRAMMAR )
	 */
	WithStatement parseWithStatement()
	{
		auto node = new WithStatement;
		expect(TokenType.with_);
		expect(TokenType.lParen);
		// magic here
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

		return node;
	}

	void error(string message)
	{
		import std.stdio;
		stderr.writefln("%s(%d:%d): %s", fileName, tokens[index].line,
			tokens[index].column, message);
		while (index < tokens.length)
		{
			if (tokens[++index].type == TokenType.semicolon)
				break;
		}
	}

	Token* peekPast(alias O, alias C)()
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

	Token* peekPastParens()
	{
		return peekPast!(TokenType.lParen, TokenType.rParen)();
	}

	Token* peekPastBrackets()
	{
		return peekPast!(TokenType.lBracket, TokenType.rBracket)();
	}

	Token* peekPastBraces()
	{
		return peekPast!(TokenType.lBrace, TokenType.rBrace)();
	}

	/**
	 * Returns a token of the specified type if it was the next token, otherwise
	 * calls the error function and returns null.
	 */
	Token* expect(TokenType type)
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
	Token current()
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
	bool currentIs(TokenType type)
	{
		return tokens[index] == type;
	}

	/**
	 * Returns: true if the current token is one of the given types
	 */
	bool currentIsOneOf(TokenType[] types...)
	{
		return canFind(types, current().type);
	}

	bool startsWith(TokenType[] types...)
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
	bool moreTokens()
	{
		return index < tokens.length;
	}

	Token[] tokens;
	size_t index;
	string fileName;
}
