// Written in the D programming language

/**
 * This module contains a parser for D source code.
 */

module std.d.parser;

import std.d.lexer;
import std.d.ast;
version(unittest) import std.stdio;

struct Parser
{
public:

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

private:

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

	DeclDef parseDeclDef()
	{
		switch (tokens[index].type)
		{
//			case TokenType.identifier:
//				if (nextIs(TokenType.colon))
//					return parseLabeledStatement();
//				break;
//			case TokenType.this_:
//				return parseConstructor();
//			case TokenType.tilde:
//				if (nextIs(TokenType.this_))
//					return parseDestructor();
//				break;
			default:
				return null;
		}
	}

//	LabeledStatement parseLabeledStatement()
//	in
//	{
//		assert (tokens[index].type == TokenType.identifier);
//	}
//	body
//	{
//		auto ls = new LabeledStatement;
//		ls.label = tokens[index++].value;
//		ls.statement = parseNoScopeStatement();
//		return ls;
//	}

//	NoScopeStatement parseNoScopeStatement()
//	{
//		switch (tokens[index].type)
//		{
//		case TokenType.semicolon:
//			return new EmptyStatement;
//		case TokenType.lBrace:
//			return parseBlockStatement();
//		default:
//			return parseNonEmptyStatement();
//		}
//	}

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

	bool moreTokens()
	{
		return index < tokens.length;
	}

	Token[] tokens;
	size_t index;
	string fileName;
}

//
//unittest
//{
//	auto a = cast(ubyte[]) q{/** */ module a.b.c;};
//	LexerConfig config;
//	auto ta = byToken(a, config);
//	auto moda = parseModuleDeclaration(ta);
//	assert (moda.packageName == ["a", "b"]);
//	assert (moda.moduleName == "c");
//
//	auto b = cast(ubyte[]) q{module a;};
//	auto tb = byToken(b, config);
//	auto modb = parseModuleDeclaration(tb);
//	assert (modb.packageName.length == 0);
//	assert (modb.moduleName == "a");
//}
//
//NonEmptyStatement parseNonEmptyStatement(Token[] tokens)
//{
//	switch (tokens[i].type)
//	{
//		case TokenType.case_:
//			return null;
//		case TokenType.default_:
//			return parseDefaultStatement(tokens);
//		default:
//			return parseNonEmptyStatementNoCaseNoDefault(tokens);
//	}
//}
//
//NonEmptyStatementNoCaseNoDefault parseNonEmptyStatementNoCaseNoDefault(Token[] tokens)
//{
//	switch (tokens[i].type)
//	{
//	case TokenType.identifier:
//	case TokenType.if_:
//		return parseIfStatement(tokens);
//	case TokenType.while_:
//		return parseWhileStatement(tokens);
//	case TokenType.do_:
//		return parseDoStatement(tokens);
//	case TokenType.for_:
//		return parseForStatement(tokens);
//	case TokenType.foreach_:
//		return parseForeachStatement(tokens);
//	case TokenType.switch_:
//		return parseSwitchStatement(tokens);
//	case TokenType.final_:
//		if (tokens.peek(1).type == TokenType.switch_)
//			return parseFinalSwitchStatement(tokens);
//		else
//			goto default;
//	case TokenType.continue_:
//		return parseContinueStatement(tokens);
//	case TokenType.break_:
//		return parseBreakStatement(tokens);
//	case TokenType.return_:
//		return parseReturnStatement(tokens);
//	case TokenType.goto_:
//		return parseGotoStatement(tokens);
//	case TokenType.with_:
//		return parseWithStatement(tokens);
//	case TokenType.synchronized_:
//		return parseSynchronizedStatement(tokens);
//	case TokenType.try_:
//		return parseTryStatement(tokens);
//	case TokenType.scope_:
//		return parseScopeGuardStatement(tokens);
//	case TokenType.throw_:
//		return parseThrowStatement(tokens);
//	case TokenType.asm_:
//		return parseAsmStatement(tokens);
//	case TokenType.pragma_:
//		return parsePragmaStatement(tokens);
//	case TokenType.mixin_:
//		if (tokens.peek(1).type == TokenType.lParen)
//			return parseMixinStatement(tokens);
//		else if (tokens.peek(1).type == TokenType.identifier)
//			return parseTemplateMixinStatement(tokens);
//		else
//		{
//			error(tokens, "Expected identifier or ( following \"mixin\"");
//			return null;
//		}
//	case TokenType.version_:
//		if (tokens.peek(1).type == TokenType.lParen)
//			return parseConditionalStatement(tokens);
//		else
//		{
//			error(tokens, "Expected ( following \"version\"");
//			return null;
//		}
//	case TokenType.debug_:
//		return parseConditionalStatement(tokens);
//	case TokenType.static_:
//		if (tokens.peek(1).type == TokenType.if_)
//			return parseConditionalStatement(tokens);
//		else if (tokens.peek(1).type == TokenType.assert_)
//			return parseStaticAssert(tokens);
//		else
//		{
//			error(tokens, "Expected \"if\" or \"assert\" following \"static\"");
//			return null;
//		}
//	case TokenType.import_:
//		return parseImportDeclaration(tokens);
//	default:
//		auto d = parseDeclarationStatement(tokens);
//		if (d is null)
//		{
//			auto e = parseExpressionStatement(tokens);
//			if (e is null)
//			{
//				error(tokens, "OMGWTF");
//				return null;
//			}
//			else
//				return e;
//		}
//		else
//			return d;
//	}
//}
//
//GotoStatement parseGotoStatement(Token[] tokens)
//in
//{
//	assert (tokens[i] == TokenType.goto_);
//}
//body
//{
//	tokens.popFront();
//	auto g = new GotoExpression;
//	switch (tokens[i].type)
//	{
//	case TokenType.identifier:
//		g.type = GotoStatement.GotoType.identifier;
//		g.identifier = tokens.moveFront().value;
//		break;
//	case TokenType.default_:
//		tokens.popFront();
//		g.type = GotoStatement.GotoType.break_;
//	case TokenType.case_:
//		g.type = GotoStatement.GotoType.case_;
//		tokens.popFront();
//	default:
//		error(tokens, "Expected an identifier, \"default\", or \"case\" following \"goto\"");
//		return null;
//	}
//}
//
//ContinueStatement parseContinueStatement(Token[] tokens)
//in
//{
//	assert (tokens[i] == TokenType.continue_);
//}
//body
//{
//	return parseContinueBreakStatement!(R, ContinueStatement)(tokens);
//}
//
//BreakStatement parseBreakStatement(Token[] tokens)
//in
//{
//	assert (tokens[i] == TokenType.break_);
//}
//body
//{
//	return parseBreakStatement!(R, BreakStatement)(tokens);
//}
//
//statementType parseContinueBreakStatement(R, alias statementType)(ref R tokens)
//{
//	tokens.popFront();
//	auto c = new statementType;
//	switch (tokens[i].type)
//	{
//		case TokenType.identifier:
//			c.identifier = tokens.moveFront().value;
//			goto case;
//		case TokenType.semicolon:
//			return c;
//		default:
//			error(tokens, "Identifier or semicolon expected");
//			return null;
//	}
//
//}
//
//
//T parseSingleTokenExpression(TokType, AstType, R)(ref R range)
//{
//	auto node = new AstType;
//	node.token = range.moveFront();
//	return node;
//}
//
//AssignExpression parseAssignExpression(Tokens)(ref Tokens tokens)
//{
//	auto expr = new AssignExpression;
//	expr.left = parseConditionalExpression(tokens);
//	switch (tokens[i].type)
//	{
//		case TokenType.assign:
//		case TokenType.plusEqual:
//		case TokenType.minusEqual:
//		case TokenType.mulEqual:
//		case TokenType.divEqual:
//		case TokenType.modEqual:
//		case TokenType.bitAndEqual:
//		case TokenType.bitOrEqual:
//		case TokenType.xorEqual:
//		case TokenType.catEqual:
//		case TokenType.shiftLeftEqual:
//		case TokenType.shiftRightEqual:
//		case TokenType.unsignedShiftRightEqual:
//		case TokenType.powEqual:
//			expr.operator = tokens.moveFront().type;
//			expr.right = parseAssignExpression();
//		default:
//			break;
//	}
//	return expr;
//}

//void main(string[] args) {}

/+
//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module parser;

import std.d.lexer;
import std.range;
import basicast;
version (unittest) import std.stdio;
version (unittest) import std.string;

void skipPastSemicolon(Tokens)(ref Tokens tokens)
{
	while (!tokens.empty)
	{
		if (tokens[i].type == TokenType.semicolon)
		{
			tokens.popFront();
			return;
		}
		else
			tokens.popFront();
	}
}

void skipDelimited(Tokens, alias O, alias C)(ref Tokens tokens)
in
{
	assert (tokens[i].type == O);
}
body
{
	tokens.popFront();
	int depth = 1;
	while (!tokens.empty)
	{
		switch (tokens[i].type)
		{
		case C:
			--depth;
			if (depth > 0)
				goto default;
			tokens.popFront();
			return;
		case O:
			++depth;
			goto default;
		default:
			tokens.popFront();
			break;
		}
	}
}

void skipBraces(Tokens)(ref Tokens tokens)
{
	return skipDelimited!(Tokens, TokenType.lBrace, TokenType.rBrace)(tokens);
}

void skipParens(Tokens)(ref Tokens tokens)
{
	return skipDelimited!(Tokens, TokenType.lParen, TokenType.rParen)(tokens);
}

void skipBrackets(Tokens)(ref Tokens tokens)
{
	return skipDelimited!(Tokens, TokenType.lBracket, TokenType.rBracket)(tokens);
}

string delimiterContent(Tokens, alias O, alias C)(ref Tokens tokens)
{
	tokens.popFront();
	int depth = 1;
	auto app = appender!(char[])();
	loop: while (!tokens.empty)
	{
		switch (tokens[i].type)
		{
		case C:
			--depth;
			if (depth > 0)
				goto default;
			tokens.popFront();
			break loop;
		case O:
			++depth;
			goto default;
		default:
			app.put(tokens.moveFront().value);
			break;
		}
	}
	return cast(string) app.data;
}

string bracketContent(Tokens)(ref Tokens tokens)
{
	return "[" ~ delimiterContent!(Tokens, TokenType.lBracket, TokenType.rBracket)(tokens) ~ "]";
}

string parenContent(Tokens)(ref Tokens tokens)
{
	return "(" ~ delimiterContent!(Tokens, TokenType.lParen, TokenType.rParen)(tokens) ~ ")";
}

string braceContent(Tokens)(ref Tokens tokens)
{
	return "{" ~ delimiterContent!(Tokens, TokenType.lBrace, TokenType.rBrace)(tokens) ~ "}";
}

bool isIdentifierOrBasicType(const TokenType type)
{
	return isType(type) || type == TokenType.identifier;
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

string parseType(Tokens)(ref Tokens tokens)
{
	if (tokens.front != TokenType.identifier && !isType(tokens.front.type))
		return null;
	auto app = appender!(ubyte[])();
	switch (tokens.front.type)
	{
	case TokenType.const_:
	case TokenType.immutable_:
	case TokenType.shared_:
	case TokenType.inout_:
	case TokenType.typeof_:
		app.put(cast(ubyte[]) tokens.moveFront().value);
		if (tokens.empty) goto ret;
		if (tokens.front.type == TokenType.lParen)
			app.put(cast(ubyte[]) parenContent(tokens));
		break;
	case TokenType.bool_: .. case TokenType.wchar_:
	case TokenType.identifier:
		app.put(cast(ubyte[]) tokens.moveFront().value);
		break;
	default:
		return null;
	}

	if (tokens.empty) goto ret;

	if (tokens.front.type == TokenType.not)
	{
		app.put('!');
		tokens.popFront();
		if (tokens.empty) goto ret;
		if (tokens.front.type == TokenType.lParen)
			app.put(cast(ubyte[]) parenContent(tokens));
		else if (isIdentifierOrBasicType(tokens.front.type))
			app.put(cast(ubyte[]) tokens.moveFront().value);
		else
			goto ret;
	}
	else if (tokens.front.type == TokenType.function_ || tokens.front.type == TokenType.delegate_)
	{
		app.put(' ');
		app.put(cast(ubyte[]) tokens.moveFront().value);
		if (tokens.empty) goto ret;
		if (tokens.front.type == TokenType.lParen)
		{
			app.put(cast(ubyte[]) parenContent(tokens));
			goto ret;
		}
	}

	loop: while (!tokens.empty)
	{
		switch (tokens.front.type)
		{
		case TokenType.star:
			app.put('*');
			tokens.popFront();
			break;
		case TokenType.lBracket:
			app.put(cast(ubyte[]) bracketContent(tokens));
			break;
		default:
			break loop;
		}
	}
ret:
	return cast(string) app.data;
}

unittest
{
	auto sources = [
		q{int}c,
		q{int function(int,int)}c,
		q{void}c,
		q{char*}c,
		q{char*[]*[]}c,
		q{Stuff!(int,double)*}c,
		q{Template!a[]}c,
		q{Template!(a)[]}c,
	];
	LexerConfig config;
	foreach (source; sources)
	{
		auto tokens = (cast(ubyte[]) source).byToken(config).circularBuffer(4);
		auto t = parseType(tokens);
		assert (t == source, t);
	}
}

Parameter parseParameter(Tokens)(ref Tokens tokens)
{
	Parameter p;
	p.type = parseType(tokens);
	if (!tokens.empty && (tokens.front.type == TokenType.delegate_ || tokens.front.type == TokenType.function_))
	{
		p.type ~= " " ~ tokens.moveFront().value;
		if (tokens.front.type == TokenType.lParen)
			p.type ~= "(" ~ parenContent(tokens) ~")";
	}
	if (tokens.empty || tokens.front.type == TokenType.comma || tokens.front.type == TokenType.rParen)
	{
		p.name = p.type;
		p.type = "";
	}
	else
		p.name = tokens.moveFront().value;
	return p;
}

unittest
{
	auto source = cast(ubyte[]) q{int[] a}c;
	LexerConfig config;
	auto tokens = source.byToken(config).circularBuffer(4);
	auto p = parseParameter(tokens);
	assert (p.name == "a", p.name);
	assert (p.type == "int[]", p.type);
}

Parameter[] parseParameters(Tokens)(ref Tokens tokens)
in
{
	assert (tokens.front == TokenType.lParen);
}
body
{
	tokens.popFront();
	if (tokens.front.type == TokenType.rParen)
	{
		tokens.popFront();
		return [];
	}
	auto app = appender!(Parameter[])();
	while (!tokens.empty)
	{
		app.put(parseParameter(tokens));
		if (tokens.empty)
			break;
		else if (tokens.front.type == TokenType.rParen)
		{
			tokens.popFront();
			break;
		}
		else if (tokens.front == TokenType.comma)
			tokens.popFront();
		else
		{
			tokens.popFront();
			break;
		}
	}
	return app.data;
}

unittest
{
	auto source = cast(ubyte[]) q{(int[] a, double d, void*[] voids,
		void function(int**, double*[][string]) vf, R);}c;
	LexerConfig config;
	auto tokens = source.byToken(config).circularBuffer(4);
	auto p = parseParameters(tokens);
	assert (p.length == 5, "%d".format(p.length));
	assert (p[0].name == "a", p[0].name);
	assert (p[0].type == "int[]", p[0].type);
	assert (p[1].name == "d", p[1].name);
	assert (p[1].type == "double", p[1].type);
	assert (p[2].name == "voids", p[2].name);
	assert (p[2].type == "void*[]", p[2].type);
	assert (p[3].name == "vf", p[3].name);
	assert (p[3].type == "void function(int**,double*[][string])", p[3].type);
	assert (p[4].name == "R", p[4].name);
	assert (p[4].type == "", p[4].type);
	assert (tokens.front.type == TokenType.semicolon, tokens.front.value);
}

unittest
{
	auto source = cast(ubyte[]) q{()}c;
	LexerConfig config;
	auto tokens = source.byToken(config).circularBuffer(2);
	auto p = parseParameters(tokens);
	assert (p.length == 0, "%s".format(p.length));
}

FunctionDeclaration parseFunctionDeclaration(Tokens)(ref Tokens tokens, string type)
{
	FunctionDeclaration fun;
	fun.returnType = type;
	fun.name = tokens.moveFront().value;
	fun.rtParameters = parseParameters(tokens);
	if (tokens.front.type == TokenType.lParen)
	{
		fun.ctParameters = fun.rtParameters;
		fun.rtParameters = parseParameters(tokens);
	}
	while (!tokens.empty && isAttribute(tokens.front.type))
		fun.attributes.set(tokens.moveFront().type);
	if (tokens.front.type == TokenType.lBrace)
		skipBraces(tokens);
	else if (tokens.front.type == TokenType.semicolon)
		tokens.popFront();
	return fun;
}

unittest
{
	auto source = cast(ubyte[]) q{
		void doStuff();
		T calcSomething(T)(T input) { return input * 2; }
		const(string)[] getStrings() const {}
	}c;
	LexerConfig config;
	auto tokens = source.byToken(config).circularBuffer(4);

	auto decl = parseFunctionDeclaration(tokens, parseType(tokens));
	assert (decl.name == "doStuff");
	assert (decl.returnType == "void");
	assert (decl.ctParameters.length == 0);
	assert (decl.rtParameters.length == 0);

	decl = parseFunctionDeclaration(tokens, parseType(tokens));
	assert (decl.name == "calcSomething");
	assert (decl.returnType == "T");
	assert (decl.ctParameters[0].name == "T");
	assert (decl.rtParameters[0].type == "T");
	assert (decl.rtParameters[0].name == "input");

	decl = parseFunctionDeclaration(tokens, parseType(tokens));
	assert (decl.returnType == "const(string)[]", decl.returnType);
	assert (decl.name == "getStrings", decl.name);
	assert (decl.ctParameters.length == 0);
	assert (decl.rtParameters.length == 0);
	assert (decl.attributes.get() == [TokenType.const_]);
	assert (tokens.empty);
}

VariableDeclaration parseVariableDeclaration(Tokens)(ref Tokens tokens, string type)
{
	VariableDeclaration v;
	v.line = tokens.front.line;
	v.type = type;
	v.name = tokens.front.value;
	tokens.popFront();
	if (tokens.front.type == TokenType.semicolon)
		tokens.popFront();
	else
		skipPastSemicolon(tokens);
	return v;
}

unittest
{
	auto source = cast(ubyte[]) q{int c;}c;
	LexerConfig config;
	auto tokens = source.byToken(config).circularBuffer(2);
	auto decl = parseVariableDeclaration(tokens, parseType(tokens));
	assert (decl.name == "c");
	assert (decl.type == "int", decl.type);
	assert (tokens.empty);
}

ModuleDeclaration parseModuleDeclaration(Tokens)(ref Tokens tokens)
in
{
	assert (tokens.front.type == TokenType.module_);
}
body
{
	tokens.popFront();
	ModuleDeclaration declaration;
	string recent;
	loop: while (!tokens.empty)
	{
		if (tokens.front.type == TokenType.identifier)
		{
			recent = tokens.moveFront().value;
			switch (tokens.front.type)
			{
			case TokenType.dot:
				declaration.package_ ~= recent;
				tokens.popFront();
				break;
			case TokenType.semicolon:
				declaration.name = recent;
				tokens.popFront();
				break loop;
			default:
				break;
			}
		}
		else
			skipPastSemicolon(tokens);
	}
	return declaration;
}

unittest
{
	auto a = cast(ubyte[]) q{/** */ module a.b.c;};
	LexerConfig config;
	auto ta = byToken(a, config).circularBuffer(2);
	auto moda = parseModuleDeclaration(ta);
	assert (moda.package_ == ["a", "b"]);
	assert (moda.name == "c");

	auto b = cast(ubyte[]) q{module a;};
	auto tb = byToken(b, config).circularBuffer(2);
	auto modb = parseModuleDeclaration(tb);
	assert (modb.package_.length == 0);
	assert (modb.name == "a");
}

Module parseMod(Tokens)(ref Tokens tokens)
{
	Module mod;
	while (!tokens.empty)
	{
		switch (tokens.front.type)
		{
//			case TokenType.const_:
//			case TokenType.immutable_:
//			case TokenType.shared_:
//			case TokenType.inout_:
//				if (tokens.peek.type == TokenType.lParen)
//				{
//					auto
//				}
//				break;
			case TokenType.rBrace:
				return mod;
			case TokenType.identifier:
			case TokenType.bool_: .. case TokenType.wchar_:
				auto type = parseType(tokens);
				if (tokens.front.type == TokenType.identifier)
				{
					if (tokens.peek.type == TokenType.lParen)
						mod.functions ~= parseFunctionDeclaration(tokens, type);
					else
						mod.variables ~= parseVariableDeclaration(tokens, type);
				}
				else
					skipPastSemicolon(tokens);
				break;
			case TokenType.module_:
				mod.moduleDeclaration = parseModuleDeclaration(tokens);
				break;
			case TokenType.class_:
				mod.classes ~= parseClassDeclaration(tokens);
				break;
//			case TokenType.align_:
//			case TokenType.deprecated_:
//			case TokenType.extern_:
			default:
				tokens.popFront();
		}
	}
	return mod;
}

Module parseModule(ref const(ubyte)[] source)
{
	LexerConfig config;
	auto tokens = source.byToken(config).circularBuffer(2);
	AttributeList attributes;
	return parseMod(tokens);
}

unittest
{
	auto source = cast(const(ubyte)[]) q{
		module a.b.c;

		int x;
		int doStuff();
		int doOtherStuff() {}

		class Point { int x; int y; }
	}c;
	auto mod = parseModule(source);
	assert (mod.moduleDeclaration.name == "c");
	assert (mod.moduleDeclaration.package_ == ["a", "b"]);
	assert (mod.functions.length == 2);
	assert (mod.variables.length == 1);
	assert (mod.classes.length == 1);
}

ClassDeclaration parseClassDeclaration(Tokens)(ref Tokens tokens)
in
{
	assert (tokens.front.type == TokenType.class_);
}
body
{
	tokens.popFront();
	ClassDeclaration decl;
	if (tokens.front.type != TokenType.identifier)


	return decl;
}

void main(string[] args)
{
}

+/
