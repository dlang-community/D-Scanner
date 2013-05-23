// Written in the D programming language

/**
 * This module contains a parser for D source code.
 */

module std.d.parser;

import std.d.lexer;
import std.d.ast;
version(unittest) import std.stdio;

Module parseModule(R)(R tokens) if (is (ElementType!R == Token))
{
	auto parser = new Parser();
	parser.tokens = tokens.array();
	return parser.parseModule();
}

private:

struct Parser
{
	AddExpression parseAddExpression()
    {
        auto node = new AddExpression;
        return node;
    }

	AliasDeclaration parseAliasDeclaration()
    {
        auto node = new AliasDeclaration;

        return node;
    }

    AliasInitializer parseAliasInitializer()
    {
        auto node = new AliasInitializer;

        return node;
    }

	AliasThisDeclaration parseAliasThisDeclaration()
    {
        auto node = new AliasThisDeclaration;

        return node;
    }

	AlignAttribute parseAlignAttribute()
    {
        auto node = new AlignAttribute;

        return node;
    }

	AndAndExpression parseAndAndExpression()
    {
        auto node = new AndAndExpression;

        return node;
    }

	AndExpression parseAndExpression()
    {
        auto node = new AndExpression;

        return node;
    }

	ArgumentList parseArgumentList()
    {
        auto node = new ArgumentList;

        return node;
    }

	Arguments parseArguments()
    {
        auto node = new Arguments;
        expect(TokenType.lParen);
        node.argumentList = parseArgumentList();
        expect(TokenType.rParen);
        return node;
    }

	ArrayInitializer parseArrayInitializer()
    {
        auto node = new ArrayInitializer;

        return node;
    }

    ArrayLiteral parseArrayLiteral()
    {
        auto node = new ArrayLiteral;
        expect(TokenType.lBracket);
        node.argumentList = parseArgumentList();
        expect(TokenType.rBracket);
        return node;
    }

	ArrayMemberInitialization parseArrayMemberInitialization()
    {
        auto node = new ArrayMemberInitialization;

        return node;
    }

	ArrayMemberInitializations parseArrayMemberInitializations()
    {
        auto node = new ArrayMemberInitializations;

        return node;
    }

	AsmAddExp parseAsmAddExp()
    {
        auto node = new AsmAddExp;

        return node;
    }

	AsmAndExp parseAsmAndExp()
    {
        auto node = new AsmAndExp;

        return node;
    }

	AsmBrExp parseAsmBrExp()
    {
        auto node = new AsmBrExp;

        return node;
    }

	AsmEqualExp parseAsmEqualExp()
    {
        auto node = new AsmEqualExp;

        return node;
    }

	AsmExp parseAsmExp()
    {
        auto node = new AsmExp;

        return node;
    }

	AsmInstruction parseAsmInstruction()
    {
        auto node = new AsmInstruction;

        return node;
    }

	AsmLogAndExp parseAsmLogAndExp()
    {
        auto node = new AsmLogAndExp;

        return node;
    }

	AsmLogOrExp parseAsmLogOrExp()
    {
        auto node = new AsmLogOrExp;

        return node;
    }

	AsmMulExp parseAsmMulExp()
    {
        auto node = new AsmMulExp;

        return node;
    }

	AsmOrExp parseAsmOrExp()
    {
        auto node = new AsmOrExp;

        return node;
    }

	AsmPrimaryExp parseAsmPrimaryExp()
    {
        auto node = new AsmPrimaryExp;

        return node;
    }

	AsmRelExp parseAsmRelExp()
    {
        auto node = new AsmRelExp;

        return node;
    }

	AsmShiftExp parseAsmShiftExp()
    {
        auto node = new AsmShiftExp;

        return node;
    }

	AsmStatement parseAsmStatement()
    {
        auto node = new AsmStatement;

        return node;
    }

	AsmTypePrefix parseAsmTypePrefix()
    {
        auto node = new AsmTypePrefix;

        return node;
    }

	AsmUnaExp parseAsmUnaExp()
    {
        auto node = new AsmUnaExp;

        return node;
    }

	AsmXorExp parseAsmXorExp()
    {
        auto node = new AsmXorExp;

        return node;
    }

	AssertExpression parseAssertExpression()
    {
        auto node = new AssertExpression;

        return node;
    }

	AssertStatement parseAssertStatement()
    {
        auto node = new AssertStatement;

        return node;
    }

	AssignExpression parseAssignExpression()
    {
        auto node = new AssignExpression;

        return node;
    }

	AssignStatement parseAssignStatement()
    {
        auto node = new AssignStatement;

        return node;
    }

	AssocArrayLiteral parseAssocArrayLiteral()
    {
        auto node = new AssocArrayLiteral;

        return node;
    }

	AtAttribute parseAtAttribute()
    {
        auto node = new AtAttribute;

        return node;
    }

	Attribute parseAttribute()
    {
        auto node = new Attribute;

        return node;
    }

	AttributedDeclaration parseAttributedDeclaration()
    {
        auto node = new AttributedDeclaration;

        return node;
    }

	AutoDeclaration parseAutoDeclaration()
    {
        auto node = new AutoDeclaration;

        return node;
    }

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

	BodyStatement parseBodyStatement()
    {
        auto node = new BodyStatement;

        return node;
    }

	BreakStatement parseBreakStatement()
	{
		expect(TokenType.break_);
		return parseContinueBreakStatement!(BreakStatement)();
	}

	BuiltinType parseBuiltinType()
    {
        auto node = new BuiltinType;

        return node;
    }

	CaseRangeStatement parseCaseRangeStatement()
    {
        auto node = new CaseRangeStatement;

        return node;
    }

	CaseStatement parseCaseStatement()
    {
        auto node = new CaseStatement;

        return node;
    }

	CastExpression parseCastExpression()
    {
        auto node = new CastExpression;

        return node;
    }

	CastQualifier parseCastQualifier()
    {
        auto node = new CastQualifier;

        return node;
    }

	Catch parseCatch()
    {
        auto node = new Catch;

        return node;
    }

	Catches parseCatches()
    {
        auto node = new Catches;

        return node;
    }

	ClassBody parseClassBody()
    {
        auto node = new ClassBody;

        return node;
    }

	ClassDeclaration parseClassDeclaration()
    {
        auto node = new ClassDeclaration;

        return node;
    }

	CmpExpression parseCmpExpression()
    {
        auto node = new CmpExpression;

        return node;
    }

	CompileCondition parseCompileCondition()
    {
        auto node = new CompileCondition;

        return node;
    }

	ConditionalDeclaration parseConditionalDeclaration()
    {
        auto node = new ConditionalDeclaration;

        return node;
    }

	ConditionalStatement parseConditionalStatement()
    {
        auto node = new ConditionalStatement;

        return node;
    }

	Constraint parseConstraint()
    {
        auto node = new Constraint;

        return node;
    }

	Constructor parseConstructor()
    {
        auto node = new Constructor;

        return node;
    }

	ContinueStatement parseContinueStatement()
	{
		expect(TokenType.continue_);
		return parseContinueBreakStatement!(ContinueStatement)();
	}

	DebugCondition parseDebugCondition()
    {
        auto node = new DebugCondition;

        return node;
    }

	DebugSpecification parseDebugSpecification()
    {
        auto node = new DebugSpecification;

        return node;
    }

	Declaration parseDeclaration()
    {
        auto node = new Declaration;

        return node;
    }

	DeclarationsAndStatements parseDeclarationsAndStatements()
    {
        auto node = new DeclarationsAndStatements;

        return node;
    }

	Declarator parseDeclarator()
    {
        auto node = new Declarator;

        return node;
    }

	DeclaratorSuffix parseDeclaratorSuffix()
    {
        auto node = new DeclaratorSuffix;

        return node;
    }

	DefaultStatement parseDefaultStatement()
    {
        auto node = new DefaultStatement;

        return node;
    }

	DeleteExpression parseDeleteExpression()
    {
        auto node = new DeleteExpression;

        return node;
    }

	DeleteStatement parseDeleteStatement()
    {
        auto node = new DeleteStatement;

        return node;
    }

	Deprecated parseDeprecated()
    {
        auto node = new Deprecated;

        return node;
    }

	Destructor parseDestructor()
    {
        auto node = new Destructor;

        return node;
    }

	DoStatement parseDoStatement()
    {
        auto node = new DoStatement;

        return node;
    }

	EnumBody parseEnumBody()
    {
        auto node = new EnumBody;

        return node;
    }

	EnumDeclaration parseEnumDeclaration()
    {
        auto node = new EnumDeclaration;

        return node;
    }

	EnumMember parseEnumMember()
    {
        auto node = new EnumMember;

        return node;
    }

	EqualExpression parseEqualExpression()
    {
        auto node = new EqualExpression;

        return node;
    }

	Expression parseExpression()
    {
        auto node = new Expression;

        return node;
    }

	FinalSwitchStatement parseFinalSwitchStatement()
    {
        auto node = new FinalSwitchStatement;

        return node;
    }

	Finally parseFinally()
    {
        auto node = new Finally;

        return node;
    }

	ForStatement parseForStatement()
    {
        auto node = new ForStatement;

        return node;
    }

	ForeachRangeStatement parseForeachRangeStatement()
    {
        auto node = new ForeachRangeStatement;

        return node;
    }

	ForeachStatement parseForeachStatement()
    {
        auto node = new ForeachStatement;

        return node;
    }

	ForeachType parseForeachType()
    {
        auto node = new ForeachType;

        return node;
    }

	ForeachTypeList parseForeachTypeList()
    {
        auto node = new ForeachTypeList;

        return node;
    }

	FunctionAttribute parseFunctionAttribute()
    {
        auto node = new FunctionAttribute;

        return node;
    }

	FunctionBody parseFunctionBody()
    {
        auto node = new FunctionBody;

        return node;
    }

	FunctionCallExpression parseFunctionCallExpression()
    {
        auto node = new FunctionCallExpression;

        return node;
    }

	FunctionCallStatement parseFunctionCallStatement()
    {
        auto node = new FunctionCallStatement;

        return node;
    }

	FunctionDeclaration parseFunctionDeclaration()
    {
        auto node = new FunctionDeclaration;

        return node;
    }

	FunctionLiteralExpression parseFunctionLiteralExpression()
    {
        auto node = new FunctionLiteralExpression;

        return node;
    }

	GotoStatement parseGotoStatement()
	{
		auto node = new GotoStatement;
		return node;
	}

	IdentifierChain parseIdentifierChain()
    {
        auto node = new IdentifierChain;

        return node;
    }

	IdentifierList parseIdentifierList()
    {
        auto node = new IdentifierList;

        return node;
    }

	IdentifierOrTemplateChain parseIdentifierOrTemplateChain()
    {
        auto node = new IdentifierOrTemplateChain;

        return node;
    }

	IdentifierOrTemplateInstance parseIdentifierOrTemplateInstance()
    {
        auto node = new IdentifierOrTemplateInstance;

        return node;
    }

	IdentityExpression parseIdentityExpression()
    {
        auto node = new IdentityExpression;

        return node;
    }

	IfStatement parseIfStatement()
    {
        auto node = new IfStatement;

        return node;
    }

	ImportBind parseImportBind()
    {
        auto node = new ImportBind;

        return node;
    }

	ImportBindings parseImportBindings()
    {
        auto node = new ImportBindings;

        return node;
    }

	ImportDeclaration parseImportDeclaration(Tokens)(ref Tokens tokens)
	in
	{
		assert(tokens[i] == TokenType.import_);
	}
	body
	{
		auto declaration = new ImportDeclaration;
		tokens.popFront();
		Import im;

		if (tokens[i].type != TokenType.identifier)
		{
			tokens.skipPastSemicolon();
			return declaration;
		}

		void completeImport()
		{
			im.moduleName = tokens.moveFront().value;
			tokens.popFront();
			declaration.imports ~= im;
		}

		void parseImportBindings()
		{
			loop: while (!tokens.empty)
			{
				if (tokens[i].type != TokenType.identifier)
					break;
				switch (tokens.peek().type)
				{
				case TokenType.assign:
					Import.ImportSymbol s;
					s.alias_ = tokens.moveFront().value;
					tokens.popFront();
					if (tokens.empty || tokens[i].type != TokenType.identifier)
						break loop;
					s.symbolName = tokens.moveFront().value;
					im.symbols ~= s;
					if (!tokens.empty())
					{
						if (tokens[i].type == TokenType.comma)
							tokens.popFront();
						if (tokens[i].type == TokenType.semicolon)
						{
							tokens.popFront();
							declaration.imports ~= im;
							break loop;
						}
					}
					break;
				case TokenType.comma:
					Import.ImportSymbol s;
					s.symbolName = tokens.moveFront().value;
					tokens.popFront();
					im.symbols ~= s;
					break;
				case TokenType.semicolon:
					Import.ImportSymbol s;
					s.symbolName = tokens.moveFront().value;
					tokens.popFront();
					im.symbols ~= s;
					declaration.imports ~= im;
					break loop;
				default:
					break loop;
				}
			}
		}

		loop: while (!tokens.empty)
		{
			switch  (tokens.peek().type)
			{
			case TokenType.dot:
				im.packageParts ~= tokens.moveFront().value;
				tokens.popFront();
				break;
			case TokenType.comma:
				completeImport();
				im = Import.init;
				break;
			case TokenType.semicolon:
				completeImport();
				break loop;
			case TokenType.colon:
				im.moduleName = tokens.moveFront().value;
				tokens.popFront();
				parseImportBindings();
				break loop;
			case TokenType.assign:
				im.alias_ = tokens.moveFront().value;
				tokens.popFront();
				break;
			default:
				tokens.popFront();
				break;
			}
		}

		return declaration;
	}

	ImportExpression parseImportExpression()
    {
        auto node = new ImportExpression;

        return node;
    }

	ImportList parseImportList()
    {
        auto node = new ImportList;

        return node;
    }

	InExpression parseInExpression()
    {
        auto node = new InExpression;

        return node;
    }

	InStatement parseInStatement()
    {
        auto node = new InStatement;

        return node;
    }

	Initialize parseInitialize()
    {
        auto node = new Initialize;

        return node;
    }

	Initializer parseInitializer()
    {
        auto node = new Initializer;

        return node;
    }

	InterfaceDeclaration parseInterfaceDeclaration()
    {
        auto node = new InterfaceDeclaration;
        return node;
    }

	Invariant parseInvariant()
    {
        auto node = new Invariant;

        return node;
    }

	IsExpression parseIsExpression()
    {
        auto node = new IsExpression;

        return node;
    }

	KeyValuePair parseKeyValuePair()
    {
        auto node = new KeyValuePair;

        return node;
    }

	KeyValuePairs parseKeyValuePairs()
    {
        auto node = new KeyValuePairs;

        return node;
    }

	LabeledStatement parseLabeledStatement()
	{
		auto node = new LabeledStatement;
		node.identifier = *expect(TokenType.identifier);
		expect(TokenType.colon);
		node.statement = parseStatement();
		return node;
	}

	LambdaExpression parseLambdaExpression()
    {
        auto node = new LambdaExpression;

        return node;
    }

	LastCatch parseLastCatch()
    {
        auto node = new LastCatch;

        return node;
    }

	LinkageAttribute parseLinkageAttribute()
    {
        auto node = new LinkageAttribute;

        return node;
    }

	MemberFunctionAttribute parseMemberFunctionAttribute()
    {
        auto node = new MemberFunctionAttribute;

        return node;
    }

	MixinDeclaration parseMixinDeclaration()
    {
        auto node = new MixinDeclaration;

        return node;
    }

	MixinExpression parseMixinExpression()
    {
        auto node = new MixinExpression;

        return node;
    }

	MixinTemplateName parseMixinTemplateName()
    {
        auto node = new MixinTemplateName;

        return node;
    }

	Module parseModule()
    {
        Module m = new Module;
        while (index < tokens.length)
        {
            switch (tokens[index].type)
            {
            case TokenType.module_:
                if (m.moduleDeclaration !is null)
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

	ModuleDeclaration parseModuleDeclaration()
    {
        auto node = new ModuleDeclaration;
        expect(TokenType.module_);
        node.moduleName = parseIdentifierChain();
        expect(TokenType.semicolon);
        return node;
    }

	MulExpression parseMulExpression()
    {
        auto node = new MulExpression;

        return node;
    }

	NewAnonClassExpression parseNewAnonClassExpression()
    {
        auto node = new NewAnonClassExpression;

        return node;
    }

	NewExpression parseNewExpression()
    {
        auto node = new NewExpression;

        return node;
    }

	NonEmptyStatement parseNonEmptyStatement()
	{
		auto node = new NonEmptyStatement;
        return node;
	}

	NonEmptyStatementNoCaseNoDefault parseNonEmptyStatementNoCaseNoDefault()
    {
        auto node = new NonEmptyStatementNoCaseNoDefault;

        return node;
    }

	NonVoidInitializer parseNonVoidInitializer()
    {
        auto node = new NonVoidInitializer;

        return node;
    }

	Opcode parseOpcode()
    {
        auto node = new Opcode;

        return node;
    }

	Operand parseOperand()
    {
        auto node = new Operand;

        return node;
    }

	Operands parseOperands()
    {
        auto node = new Operands;

        return node;
    }

	OrExpression parseOrExpression()
    {
        auto node = new OrExpression;

        return node;
    }

	OrOrExpression parseOrOrExpression()
    {
        auto node = new OrOrExpression;

        return node;
    }

	OutStatement parseOutStatement()
    {
        auto node = new OutStatement;

        return node;
    }

	Parameter parseParameter()
    {
        auto node = new Parameter;

        return node;
    }

	ParameterAttribute parseParameterAttribute()
    {
        auto node = new ParameterAttribute;

        return node;
    }

	Parameters parseParameters()
    {
        auto node = new Parameters;

        return node;
    }

	PostIncDecExpression parsePostIncDecExpression()
    {
        auto node = new PostIncDecExpression;

        return node;
    }

	PowExpression parsePowExpression()
    {
        auto node = new PowExpression;

        return node;
    }

	PragmaDeclaration parsePragmaDeclaration()
    {
        auto node = new PragmaDeclaration;

        return node;
    }

	PragmaExpression parsePragmaExpression()
    {
        auto node = new PragmaExpression;

        return node;
    }

	PreIncDecExpression parsePreIncDecExpression()
    {
        auto node = new PreIncDecExpression;

        return node;
    }

	PrimaryExpression parsePrimaryExpression()
    {
        auto node = new PrimaryExpression;

        return node;
    }

	ProtectionAttribute parseProtectionAttribute()
    {
        auto node = new ProtectionAttribute;

        return node;
    }

	Register parseRegister()
    {
        auto node = new Register;

        return node;
    }

	RelExpression parseRelExpression()
    {
        auto node = new RelExpression;

        return node;
    }

	ReturnStatement parseReturnStatement()
    {
        auto node = new ReturnStatement;
        expect(TokenType.return_);
        if (tokens[index] != TokenType.semicolon)
            node.expression = parseExpression();
        expect(TokenType.semicolon);
        return node;
    }

	ScopeGuardStatement parseScopeGuardStatement()
    {
        auto node = new ScopeGuardStatement;

        return node;
    }

	SharedStaticConstructor parseSharedStaticConstructor()
    {
        auto node = new SharedStaticConstructor;

        return node;
    }

	SharedStaticDestructor parseSharedStaticDestructor()
    {
        auto node = new SharedStaticDestructor;

        return node;
    }

	ShiftExpression parseShiftExpression()
    {
        auto node = new ShiftExpression;

        return node;
    }

	SingleImport parseSingleImport()
    {
        auto node = new SingleImport;

        return node;
    }

	Statement parseStatement()
    {
        auto node = new Statement;
        if (tokens[index] != TokenType.semicolon)
            node.nonEmptyStatement = parseNonEmptyStatement();
        else
            expect(TokenType.semicolon);
        return node;
    }

	StatementNoCaseNoDefault parseStatementNoCaseNoDefault()
    {
        auto node = new StatementNoCaseNoDefault;
        if (tokens[index] != TokenType.semicolon)
            node.nonEmptyStatementNoCaseNoDefault = parseNonEmptyStatementNoCaseNoDefault();
        else
            expect(TokenType.semicolon);
        return node;
    }

	StaticAssertDeclaration parseStaticAssertDeclaration()
    {
        auto node = new StaticAssertDeclaration;
        node.staticAssertStatement = parseStaticAssertStatement();
        return node;
    }

	StaticAssertStatement parseStaticAssertStatement()
    {
        auto node = new StaticAssertStatement;
        expect(TokenType.static_);
        node.assertStatement = parseAssertStatement();
        return node;
    }

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

	StorageClass parseStorageClass()
    {
        auto node = new StorageClass;

        return node;
    }

	StructBody parseStructBody()
    {
        auto node = new StructBody;
        expect(TokenType.lBrace);
        while (tokens[index] != TokenType.rBrace && moreTokens())
            node.declarations ~= parseDeclaration();
        expect(TokenType.rBrace);
        return node;
    }

	StructDeclaration parseStructDeclaration()
    {
        auto node = new StructDeclaration;
        expect(TokenType.struct_);
        node.identifier = *expect(TokenType.identifier);
        if (tokens[index] == TokenType.lParen)
        {
            node.templateParameters = parseTemplateParameters();
            if (tokens[index] == TokenType.if_)
                node.constraint = parseConstraint();
            node.structBody = parseStructBody();
        }
        else if (tokens[index] == TokenType.lBrace)
        {
            node.structBody = parseStructBody();
        }
        else if (tokens[index] != TokenType.semicolon)
            error("Template Parameters, Struct Body, or Semicolon expected");
        return node;
    }

	StructInitializer parseStructInitializer()
    {
        auto node = new StructInitializer;
        expect(TokenType.lBrace);
        node.structMemberInitializers = parseStructMemberInitializers();
        expect(TokenType.rBrace);
        return node;
    }

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

	StructMemberInitializers parseStructMemberInitializers()
    {
        auto node = new StructMemberInitializers;

        return node;
    }

	SwitchBody parseSwitchBody()
    {
        auto node = new SwitchBody;
        expect(TokenType.lBrace);
        while (moreTokens() && tokens[index] != TokenType.rBrace)
            node.statements ~= parseStatement();
        expect(TokenType.rBrace);
        return node;
    }

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

	TemplateAliasParameter parseTemplateAliasParameter()
    {
        auto node = new TemplateAliasParameter;

        return node;
    }

	TemplateArgument parseTemplateArgument()
    {
        auto node = new TemplateArgument;

        return node;
    }

	TemplateArgumentList parseTemplateArgumentList()
    {
        auto node = new TemplateArgumentList;

        return node;
    }

	TemplateArguments parseTemplateArguments()
    {
        auto node = new TemplateArguments;

        return node;
    }

	TemplateDeclaration parseTemplateDeclaration()
    {
        auto node = new TemplateDeclaration;

        return node;
    }

	TemplateInstance parseTemplateInstance()
    {
        auto node = new TemplateInstance;

        return node;
    }

	TemplateMixinStatement parseTemplateMixinStatement()
    {
        auto node = new TemplateMixinStatement;

        return node;
    }

	TemplateParameter parseTemplateParameter()
    {
        auto node = new TemplateParameter;

        return node;
    }

	TemplateParameterList parseTemplateParameterList()
    {
        auto node = new TemplateParameterList;

        return node;
    }

	TemplateParameters parseTemplateParameters()
    {
        auto node = new TemplateParameters;

        return node;
    }

	TemplateSingleArgument parseTemplateSingleArgument()
    {
        auto node = new TemplateSingleArgument;

        return node;
    }

	TemplateThisParameter parseTemplateThisParameter()
    {
        auto node = new TemplateThisParameter;

        return node;
    }

	TemplateTupleParameter parseTemplateTupleParameter()
    {
        auto node = new TemplateTupleParameter;

        return node;
    }

	TemplateTypeParameter parseTemplateTypeParameter()
    {
        auto node = new TemplateTypeParameter;

        return node;
    }

	TemplateValueParameter parseTemplateValueParameter()
    {
        auto node = new TemplateValueParameter;

        return node;
    }

	TemplateValueParameterDefault parseTemplateValueParameterDefault()
    {
        auto node = new TemplateValueParameterDefault;

        return node;
    }

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

	ThrowStatement parseThrowStatement()
    {
        auto node = new ThrowStatement;
        expect(TokenType.throw_);
        node.expression = parseExpression();
        expect(TokenType.semicolon);
        return node;
    }

	TraitsArgument parseTraitsArgument()
    {
        auto node = new TraitsArgument;

        return node;
    }

	TraitsExpression parseTraitsExpression()
    {
        auto node = new TraitsExpression;

        return node;
    }

	TryStatement parseTryStatement()
    {
        auto node = new TryStatement;

        return node;
    }

	Type parseType()
    {
        auto node = new Type;

        return node;
    }

	Type2 parseType2()
    {
        auto node = new Type2;

        return node;
    }

	Type3 parseType3()
    {
        auto node = new Type3;

        return node;
    }

	TypeConstructor parseTypeConstructor()
    {
        auto node = new TypeConstructor;

        return node;
    }

	TypeConstructors parseTypeConstructors()
    {
        auto node = new TypeConstructors;

        return node;
    }

	TypeSpecialization parseTypeSpecialization()
    {
        auto node = new TypeSpecialization;

        return node;
    }

	TypeSuffix parseTypeSuffix()
    {
        auto node = new TypeSuffix;

        return node;
    }

	TypeidExpression parseTypeidExpression()
    {
        auto node = new TypeidExpression;

        return node;
    }

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

	UnaryExpression parseUnaryExpression()
    {
        auto node = new UnaryExpression;

        return node;
    }

	UnionDeclaration parseUnionDeclaration()
    {
        auto node = new UnionDeclaration;

        return node;
    }

	Unittest parseUnittest()
    {
        auto node = new Unittest;
        expect(TokenType.unittest_);
        node.blockStatement = parseBlockStatement();
        return node;
    }

	VariableDeclaration parseVariableDeclaration()
    {
        auto node = new VariableDeclaration;

        return node;
    }

	VersionCondition parseVersionCondition()
    {
        auto node = new VersionCondition;
        expect(TokenType.version_);
        expect(TokenType.lParen);
        node.token = tokens[index];
        expect(TokenType.rParen);
        return node;
    }

	VersionSpecification parseVersionSpecification()
    {
        auto node = new VersionSpecification;
        expect(TokenType.version_);
        expect(TokenType.assign);
        node.token = tokens[index];
        expect(TokenType.semicolon);
        return node;
    }

	WhileStatement parseWhileStatement()
    {
        auto node = new WhileStatement;

        return node;
    }

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

	XorExpression parseXorExpression()
    {
        auto node = new XorExpression;

        return node;
    }

	///////////////////////////////////////////////////////////////

	statementType parseContinueBreakStatement(alias statementType)()
	{
		index++;
		auto c = new statementType;
		switch (tokens[index].type)
		{
			case TokenType.identifier:
				c.identifier = tokens[index++];
				goto case;
			case TokenType.semicolon:
				return c;
			default:
				error("Identifier or semicolon expected");
				return null;
		}

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

    Token* expect(TokenType type)
    {
        if (tokens[index].type == type)
            return &tokens[index++];
        else
            return null;
    }

    Token* peek()
    {
        return index + 1 < tokens.length ? &tokens[index + 1] : null;
    }

    bool nextIs(TokenType t)
    {
        return peek() && peek().type == t;
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

    bool moreTokens()
    {
        return index < tokens.length;
    }

    Token[] tokens;
    size_t index;
    string fileName;
}

unittest
{
	auto source = cast(ubyte[]) q{import std.stdio;
		import std.ascii: hexDigits;
		import r = std.range;
		import foo, bar;
		import std.stdio : writefln, foo = writef;}c;

	LexerConfig config;
	auto tokens = source.byToken(config).circularBuffer(4);
	assert (tokens[i] == "import");

	auto decl = parseImportDeclaration(tokens);
	assert (decl.imports.length == 1);
	assert (decl.imports[0].packageParts == ["std"]);
	assert (decl.imports[0].moduleName == "stdio");
	assert (tokens[i].value == "import", tokens.front.value);
	assert (tokens.peek(3).value == "ascii", tokens.front.value);

	decl = parseImportDeclaration(tokens);
	assert (decl.imports.length == 1, "%d".format(decl.imports.length));
	assert (decl.imports[0].packageParts == ["std"]);
	assert (decl.imports[0].moduleName == "ascii", decl.imports[0].moduleName);
	assert (decl.imports[0].symbols[0].symbolName == "hexDigits", decl.imports[0].symbols[0].symbolName);
	assert (decl.imports[0].symbols[0].alias_.length == 0);

	decl = parseImportDeclaration(tokens);
	assert (decl.imports.length == 1, "%s".format(decl.imports.length));
	assert (decl.imports[0].moduleName == "range");
	assert (decl.imports[0].packageParts == ["std"]);
	assert (decl.imports[0].alias_ == "r");

	decl = parseImportDeclaration(tokens);
	assert (decl.imports.length == 2);
	assert (decl.imports[0].packageParts.length == 0);
	assert (decl.imports[0].moduleName == "foo");
	assert (decl.imports[1].packageParts.length == 0);
	assert (decl.imports[1].moduleName == "bar");

	decl = parseImportDeclaration(tokens);
	assert (decl.imports.length == 1, "%s".format(decl.imports.length));
	assert (decl.imports[0].packageParts == ["std"]);
	assert (decl.imports[0].moduleName == "stdio");
	assert (decl.imports[0].symbols.length == 2);
	assert (decl.imports[0].symbols[0].symbolName == "writefln");
	assert (decl.imports[0].symbols[1].symbolName == "writef");
	assert (decl.imports[0].symbols[1].alias_ == "foo");
}
