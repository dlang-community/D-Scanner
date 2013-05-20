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
	AddExpression parseAddExpression() { return null; }
	AliasDeclaration parseAliasDeclaration() { return null; }
	AliasInitializer parseAliasInitializer() { return null; }
	AliasThisDeclaration parseAliasThisDeclaration() { return null; }
	AlignAttribute parseAlignAttribute() { return null; }
	AndAndExpression parseAndAndExpression() { return null; }
	AndExpression parseAndExpression() { return null; }
	ArgumentList parseArgumentList() { return null; }
	Arguments parseArguments() { return null; }
	ArrayInitializer parseArrayInitializer() { return null; }
	ArrayLiteral parseArrayLiteral() { return null; }
	ArrayMemberInitialization parseArrayMemberInitialization() { return null; }
	ArrayMemberInitializations parseArrayMemberInitializations() { return null; }
	AsmAddExp parseAsmAddExp() { return null; }
	AsmAndExp parseAsmAndExp() { return null; }
	AsmBrExp parseAsmBrExp() { return null; }
	AsmEqualExp parseAsmEqualExp() { return null; }
	AsmExp parseAsmExp() { return null; }
	AsmInstruction parseAsmInstruction() { return null; }
	AsmLogAndExp parseAsmLogAndExp() { return null; }
	AsmLogOrExp parseAsmLogOrExp() { return null; }
	AsmMulExp parseAsmMulExp() { return null; }
	AsmOrExp parseAsmOrExp() { return null; }
	AsmPrimaryExp parseAsmPrimaryExp() { return null; }
	AsmRelExp parseAsmRelExp() { return null; }
	AsmShiftExp parseAsmShiftExp() { return null; }
	AsmStatement parseAsmStatement() { return null; }
	AsmTypePrefix parseAsmTypePrefix() { return null; }
	AsmUnaExp parseAsmUnaExp() { return null; }
	AsmXorExp parseAsmXorExp() { return null; }
	AssertExpression parseAssertExpression() { return null; }
	AssertStatement parseAssertStatement() { return null; }
	AssignExpression parseAssignExpression() { return null; }
	AssignStatement parseAssignStatement() { return null; }
	AssocArrayLiteral parseAssocArrayLiteral() { return null; }
	AtAttribute parseAtAttribute() { return null; }
	Attribute parseAttribute() { return null; }
	AttributedDeclaration parseAttributedDeclaration() { return null; }
	AutoDeclaration parseAutoDeclaration() { return null; }

	BlockStatement parseBlockStatement()
	{
		auto statement = new BlockStatement();
		expect(TokenType.lBrace);
		switch (tokens[index].type)
		{
		case TokenType.rBrace:
			break;
		default:
			statement.statements ~= parseStatement();
		}
		return statement;
	}

	BodyStatement parseBodyStatement() { return null; }

	BreakStatement parseBreakStatement()
	{
		expect(TokenType.break_);
		return parseContinueBreakStatement!(BreakStatement)();
	}

	BuiltinType parseBuiltinType() { return null; }
	CaseRangeStatement parseCaseRangeStatement() { return null; }
	CaseStatement parseCaseStatement() { return null; }
	CastExpression parseCastExpression() { return null; }
	CastQualifier parseCastQualifier() { return null; }
	Catch parseCatch() { return null; }
	Catches parseCatches() { return null; }
	ClassBody parseClassBody() { return null; }
	ClassDeclaration parseClassDeclaration() { return null; }
	CmpExpression parseCmpExpression() { return null; }
	CompileCondition parseCompileCondition() { return null; }
	ConditionalDeclaration parseConditionalDeclaration() { return null; }
	ConditionalStatement parseConditionalStatement() { return null; }
	Constraint parseConstraint() { return null; }
	Constructor parseConstructor() { return null; }

	ContinueStatement parseContinueStatement()
	{
		expect(TokenType.continue_);
		return parseContinueBreakStatement!(ContinueStatement)();
	}

	DebugCondition parseDebugCondition() { return null; }
	DebugSpecification parseDebugSpecification() { return null; }
	Declaration parseDeclaration() { return null; }
	DeclarationsAndStatements parseDeclarationsAndStatements() { return null; }
	Declarator parseDeclarator() { return null; }
	DeclaratorSuffix parseDeclaratorSuffix() { return null; }
	DefaultStatement parseDefaultStatement() { return null; }
	DeleteExpression parseDeleteExpression() { return null; }
	DeleteStatement parseDeleteStatement() { return null; }
	Deprecated parseDeprecated() { return null; }
	Destructor parseDestructor() { return null; }
	DoStatement parseDoStatement() { return null; }
	EnumBody parseEnumBody() { return null; }
	EnumDeclaration parseEnumDeclaration() { return null; }
	EnumMember parseEnumMember() { return null; }
	EqualExpression parseEqualExpression() { return null; }
	Expression parseExpression() { return null; }
	FinalSwitchStatement parseFinalSwitchStatement() { return null; }
	Finally parseFinally() { return null; }
	ForStatement parseForStatement() { return null; }
	ForeachRangeStatement parseForeachRangeStatement() { return null; }
	ForeachStatement parseForeachStatement() { return null; }
	ForeachType parseForeachType() { return null; }
	ForeachTypeList parseForeachTypeList() { return null; }
	FunctionAttribute parseFunctionAttribute() { return null; }
	FunctionBody parseFunctionBody() { return null; }
	FunctionCallExpression parseFunctionCallExpression() { return null; }
	FunctionCallStatement parseFunctionCallStatement() { return null; }
	FunctionDeclaration parseFunctionDeclaration() { return null; }
	FunctionLiteralExpression parseFunctionLiteralExpression() { return null; }

	GotoStatement parseGotoStatement()
	{
		expect(TokenType.goto_);
		auto g = new GotoStatement;
		switch (tokens[index].type)
		{
		case TokenType.identifier:
			g.type = GotoStatement.GotoType.identifier;
			g.identifier = tokens[index++].value;
			break;
		case TokenType.default_:
			index++;
			g.type = GotoStatement.GotoType.default_;
			break;
		case TokenType.case_:
			g.type = GotoStatement.GotoType.case_;
			index++;
			break;
		default:
			error("Expected an identifier, \"default\", or \"case\" following \"goto\"");
			return null;
		}
		return g;
	}

	IdentifierChain parseIdentifierChain() { return null; }
	IdentifierList parseIdentifierList() { return null; }
	IdentifierOrTemplateChain parseIdentifierOrTemplateChain() { return null; }
	IdentifierOrTemplateInstance parseIdentifierOrTemplateInstance() { return null; }
	IdentityExpression parseIdentityExpression() { return null; }
	IfStatement parseIfStatement() { return null; }
	ImportBind parseImportBind() { return null; }
	ImportBindings parseImportBindings() { return null; }

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

	ImportExpression parseImportExpression() { return null; }
	ImportList parseImportList() { return null; }
	InExpression parseInExpression() { return null; }
	InStatement parseInStatement() { return null; }
	Initialize parseInitialize() { return null; }
	Initializer parseInitializer() { return null; }
	InterfaceDeclaration parseInterfaceDeclaration() { return null; }
	Invariant parseInvariant() { return null; }
	IsExpression parseIsExpression() { return null; }
	KeyValuePair parseKeyValuePair() { return null; }
	KeyValuePairs parseKeyValuePairs() { return null; }

	LabeledStatement parseLabeledStatement()
	in
	{
		assert (tokens[index].type == TokenType.identifier);
	}
	body
	{
		auto ls = new LabeledStatement;
		ls.label = tokens[index++].value;
		ls.statement = parseNoScopeStatement();
		return ls;
	}

	LambdaExpression parseLambdaExpression() { return null; }
	LastCatch parseLastCatch() { return null; }
	LinkageAttribute parseLinkageAttribute() { return null; }
	MemberFunctionAttribute parseMemberFunctionAttribute() { return null; }
	MixinDeclaration parseMixinDeclaration() { return null; }
	MixinExpression parseMixinExpression() { return null; }
	MixinTemplateName parseMixinTemplateName() { return null; }

	Module parseModule()
    {
        Module m = new Module;
        while (index < tokens.length)
        {
            switch (tokens[index].type)
            {
            case TokenType.module_:
                if (m.declaration !is null)
                    m.declaration = parseModuleDeclaration();
                else
                    error("Only one module declaration is allowed per module");
                break;
            default:
                m.declDefs.insert(parseDeclDef());
            }
        }
        return m;
    }

	ModuleDeclaration parseModuleDeclaration()
    in
    {
        assert (expect(TokenType.module_));
    }
    body
    {
        ModuleDeclaration declaration = new ModuleDeclaration;
        string recent;
        loop: while (index < tokens.length)
        {
            if (tokens[index].type == TokenType.identifier)
            {
                recent = tokens[index++].value;
                switch (tokens[index].type)
                {
                case TokenType.dot:
                    declaration.packageName ~= recent;
                    index++;
                    break;
                case TokenType.semicolon:
                    declaration.moduleName = recent;
                    index++;
                    break loop;
                default:
                    break;
                }
            }
            else
                error("Identifier expected");
        }
        return declaration;
    }

	MulExpression parseMulExpression() { return null; }
	NewAnonClassExpression parseNewAnonClassExpression() { return null; }
	NewExpression parseNewExpression() { return null; }

	NonEmptyStatement parseNonEmptyStatement()
	{
		switch (tokens[index].type)
		{
		case TokenType.case_:
			return parseCaseStatement();
		case TokenType.default_:
			return parseDefaultStatement();
		default:
			return null;
		}
	}

	NonEmptyStatementNoCaseNoDefault parseNonEmptyStatementNoCaseNoDefault() { return null; }
	NonVoidInitializer parseNonVoidInitializer() { return null; }
	Opcode parseOpcode() { return null; }
	Operand parseOperand() { return null; }
	Operands parseOperands() { return null; }
	OrExpression parseOrExpression() { return null; }
	OrOrExpression parseOrOrExpression() { return null; }
	OutStatement parseOutStatement() { return null; }
	Parameter parseParameter() { return null; }
	ParameterAttribute parseParameterAttribute() { return null; }
	Parameters parseParameters() { return null; }
	PostIncDecExpression parsePostIncDecExpression() { return null; }
	PowExpression parsePowExpression() { return null; }
	PragmaDeclaration parsePragmaDeclaration() { return null; }
	PragmaExpression parsePragmaExpression() { return null; }
	PreIncDecExpression parsePreIncDecExpression() { return null; }
	PrimaryExpression parsePrimaryExpression() { return null; }
	ProtectionAttribute parseProtectionAttribute() { return null; }
	Register parseRegister() { return null; }
	RelExpression parseRelExpression() { return null; }
	ReturnStatement parseReturnStatement() { return null; }
	ScopeGuardStatement parseScopeGuardStatement() { return null; }
	SharedStaticConstructor parseSharedStaticConstructor() { return null; }
	SharedStaticDestructor parseSharedStaticDestructor() { return null; }
	ShiftExpression parseShiftExpression() { return null; }
	SingleImport parseSingleImport() { return null; }
	Statement parseStatement() { return null; }
	StatementNoCaseNoDefault parseStatementNoCaseNoDefault() { return null; }
	StaticAssertDeclaration parseStaticAssertDeclaration() { return null; }
	StaticAssertStatement parseStaticAssertStatement() { return null; }
	StaticConstructor parseStaticConstructor() { return null; }
	StaticDestructor parseStaticDestructor() { return null; }
	StaticIfCondition parseStaticIfCondition() { return null; }
	StorageClass parseStorageClass() { return null; }
	StructBody parseStructBody() { return null; }
	StructDeclaration parseStructDeclaration() { return null; }
	StructInitializer parseStructInitializer() { return null; }
	StructMemberInitializer parseStructMemberInitializer() { return null; }
	StructMemberInitializers parseStructMemberInitializers() { return null; }
	SwitchBody parseSwitchBody() { return null; }
	SwitchStatement parseSwitchStatement() { return null; }
	Symbol parseSymbol() { return null; }
	SynchronizedStatement parseSynchronizedStatement() { return null; }
	TemplateAliasParameter parseTemplateAliasParameter() { return null; }
	TemplateArgument parseTemplateArgument() { return null; }
	TemplateArgumentList parseTemplateArgumentList() { return null; }
	TemplateArguments parseTemplateArguments() { return null; }
	TemplateDeclaration parseTemplateDeclaration() { return null; }
	TemplateInstance parseTemplateInstance() { return null; }
	TemplateMixinStatement parseTemplateMixinStatement() { return null; }
	TemplateParameter parseTemplateParameter() { return null; }
	TemplateParameterList parseTemplateParameterList() { return null; }
	TemplateParameters parseTemplateParameters() { return null; }
	TemplateSingleArgument parseTemplateSingleArgument() { return null; }
	TemplateThisParameter parseTemplateThisParameter() { return null; }
	TemplateTupleParameter parseTemplateTupleParameter() { return null; }
	TemplateTypeParameter parseTemplateTypeParameter() { return null; }
	TemplateValueParameter parseTemplateValueParameter() { return null; }
	TemplateValueParameterDefault parseTemplateValueParameterDefault() { return null; }
	TernaryExpression parseTernaryExpression() { return null; }
	ThrowStatement parseThrowStatement() { return null; }
	TraitsArgument parseTraitsArgument() { return null; }
	TraitsExpression parseTraitsExpression() { return null; }
	TryStatement parseTryStatement() { return null; }
	Type parseType() { return null; }
	Type2 parseType2() { return null; }
	Type3 parseType3() { return null; }
	TypeConstructor parseTypeConstructor() { return null; }
	TypeConstructors parseTypeConstructors() { return null; }
	TypeSpecialization parseTypeSpecialization() { return null; }
	TypeSuffix parseTypeSuffix() { return null; }
	TypeidExpression parseTypeidExpression() { return null; }
	TypeofExpression parseTypeofExpression() { return null; }
	UnaryExpression parseUnaryExpression() { return null; }
	UnionDeclaration parseUnionDeclaration() { return null; }
	Unittest parseUnittest() { return null; }
	VariableDeclaration parseVariableDeclaration() { return null; }
	VersionCondition parseVersionCondition() { return null; }
	VersionSpecification parseVersionSpecification() { return null; }
	WhileStatement parseWhileStatement() { return null; }
	WithStatement parseWithStatement() { return null; }
	XorExpression parseXorExpression() { return null; }

	///////////////////////////////////////////////////////////////

	statementType parseContinueBreakStatement(alias statementType)()
	{
		index++;
		auto c = new statementType;
		switch (tokens[index].type)
		{
			case TokenType.identifier:
				c.identifier = tokens[index++].value;
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

    bool startsWith(TokenType types...)
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
