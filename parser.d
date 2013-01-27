
//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module parser;

import std.stream;
import std.array;
import std.stdio;
import std.algorithm;
import std.range;
import std.d.lexer;

import types;
import langutils;
import circularbuffer;

alias CircularBuffer!Token TokenBuffer;

class Balanced : TokenBuffer
{
public:

	this(InputRange!Token tokens, TokenType open, TokenType close)
	{
		super(0, tokens);
        this.range = tokens;
		this.open = open;
		this.close = close;
	}

	override bool empty() @property
	{
		return _empty;
	}

	override Token front() @property
	{
		return range.front;
	}

	override void popFront()
	{
		range.popFront();
		if (range.front == open)
			++depth;
		else if (range.front == close)
			--depth;
		_empty = depth == 0 || range.empty;
	}

    invariant()
    {
        assert (range);
        assert (depth >= 0);
    }

private:
	int depth;
	TokenType open;
	TokenType close;
	InputRange!(Token) range;
	bool _empty;
}

/**
 * Params:
 *     tokens = the array of tokens
 *     index = an index into tokens such that tokens.front.type == open
 *     open = the opening delimiter
 *     close = the closing delimiter
 * Returns: all tokens that are between the balanced delimiters that start at
 *     tokens.front, not including the delimiters. If the delimiters in tokens
 *     are not balanced, this function will return tokens[index + 1 .. $];
 */
Balanced betweenBalanced(TokenBuffer tokens,
	TokenType open, TokenType close)
in
{
	assert (tokens.front == open);
}
body
{
	return new Balanced(tokens, open, close);
}


/**
 * See_also: betweenBalanced
 */
Balanced betweenBalancedBraces(TokenBuffer tokens)
{
	return betweenBalanced(tokens, TokenType.LBrace, TokenType.RBrace);
}


/**
 * See_also: betweenBalanced
 */
Balanced betweenBalancedParens(TokenBuffer tokens)
{
	return betweenBalanced(tokens, TokenType.LParen, TokenType.RParen);
}


/**
 * See_also: betweenBalanced
 */
Balanced betweenBalancedBrackets(TokenBuffer tokens)
{
	return betweenBalanced(tokens, TokenType.LBracket, TokenType.RBracket);
}

void skipBalanced(alias openToken, alias closeToken)(TokenBuffer tokens)
in
{
	assert (tokens.front == openToken);
}
body
{
	int depth = 1;
	tokens.popFront();
	while (!tokens.empty && depth != 0)
	{
		switch (tokens.front.type)
		{
		case openToken: ++depth; break;
		case closeToken: --depth; break;
		default: break;
		}
		tokens.popFront();
	}
}

void skipParens(TokenBuffer tokens)
{
	skipBalanced!(TokenType.LParen, TokenType.RParen)(tokens);
}

void skipBrackets(TokenBuffer tokens)
{
	skipBalanced!(TokenType.LBracket, TokenType.RBracket)(tokens);
}

void skipBraces(TokenBuffer tokens)
{
	skipBalanced!(TokenType.LBrace, TokenType.RBrace)(tokens);
}

/**
 * Params:
 *     tokens = the token array to examine
 *     index = an indext into tokens such that tokens.front.type == open
 *     open = the opening delimiter
 *     close = the closing delimiter
 * Returns: a string representing the contents of the two delimiters. This will
 *     not preserve whitespace, but it will place a single space character after
 *     a comma and between identifiers.
 */
string content(TokenBuffer tokens, TokenType open, TokenType close)
in
{
	assert (tokens.front == open);
}
body
{
	auto app = appender!string();
	int depth = 1;
	foreach (t; betweenBalanced(tokens, open, close))
	{
		if (t == TokenType.Comma)
			app.put(", ");
		else
			app.put(t.value);
	}
	return app.data;
}


/**
 * See_also: content
 */
string parenContent(TokenBuffer tokens)
{
	return "(" ~ content(tokens, TokenType.LParen, TokenType.RParen) ~ ")";
}


/**
 * See_also: content
 */
string bracketContent(TokenBuffer tokens)
{
	return "[" ~ content(tokens, TokenType.LBracket, TokenType.RBracket) ~ "]";
}


/**
 * Advances index until it indexes a character in tokens after a right brace if
 * index initially indexed a right brace, or advances index until it indexes a
 * character after a simicolon otherwise.
 */
void skipBlockStatement(TokenBuffer tokens)
{
	if (tokens.front == TokenType.LBrace)
		skipBraces(tokens);
	else
		skipPastNext(tokens, TokenType.Semicolon);
}


/**
 * Advances index until it indexes a character in tokens directly after a token
 * of type type. This function handles nesting of braces, brackets, and
 * parenthesis
 */
void skipPastNext(TokenBuffer tokens, TokenType type)
{
	while (!tokens.empty)
	{
		if (tokens.front.type == TokenType.LBrace)
			skipBraces(tokens);
		else if (tokens.front.type == TokenType.LParen)
			skipParens(tokens);
		else if (tokens.front.type == TokenType.LBracket)
			skipBrackets(tokens);
		else if (tokens.front.type == type)
		{
			tokens.popFront();
			return;
		}
		else
			tokens.popFront();
	}
}

string parseTypeDeclaration(TokenBuffer tokens)
{
	auto type = tokens.front.value;
	tokens.popFront();
	buildingType: while (!tokens.empty)
	{
		switch (tokens.front.type)
		{
		case TokenType.LBracket:
			type ~= bracketContent(tokens);
			break;
		case TokenType.Not:
			type ~= tokens.front.value;
			tokens.popFront();
			if (tokens.front == TokenType.LParen)
				type ~= parenContent(tokens);
			else
			{
				type ~= tokens.front.value;
				tokens.popFront();
			}
			break;
		case TokenType.Star:
		case TokenType.BitAnd:
			type ~= tokens.front.value;
			tokens.popFront();
			break;
		case TokenType.Function:
			type ~= " " ~ tokens.front.value;
			tokens.popFront();
			type ~= parenContent(tokens);
			break;
		default:
			break buildingType;
		}
	}
	return type;
}

/**
 * Parses a module from a token array.
 * Params:
 *     protection = the default protection level for a block statement
 *     attributes = the default attributes for a block statement
 * Returns: the parsed module
 */
Module parseModule(TokenBuffer tokens, string protection = "public", string[] attributes = [])
{
	string type;
	string name;
	string localProtection = "";
	string[] localAttributes = [];

	void resetLocals()
	{
		type = "";
		name = "";
		localProtection = "";
		localAttributes = [];
	}

	Module mod = new Module;
	size_t index = 0;
	while(!tokens.empty)
	{
		switch(tokens.front.type)
		{
		case TokenType.Pragma:
			tokens.popFront();
			if (tokens.front == TokenType.LParen)
			skipParens(tokens);
			break;
		case TokenType.Mixin:
		case TokenType.Assert:
			tokens.popFront();
			tokens.skipBlockStatement();
			break;
		case TokenType.Alias:
			Alias a = parseAlias(tokens,
				localProtection.empty() ? protection : localProtection,
				attributes);
			mod.aliases ~= a;
			break;
		case TokenType.Import:
			mod.imports ~= parseImports(tokens);
			resetLocals();
			break;
		case TokenType.Version:
			tokens.popFront();
			if (tokens.front == TokenType.LParen)
			{
				tokens.betweenBalancedParens();
				if (tokens.front == TokenType.LBrace)
				{
					auto braceContent = tokens.betweenBalancedBraces();
					mod.merge(parseModule(braceContent,
						localProtection.empty() ? protection : localProtection,
						attributes));
				}
			}
			else if (tokens.front == TokenType.Assign)
				tokens.skipBlockStatement();
			break;
		case TokenType.Deprecated:
		case TokenType.Nothrow:
		case TokenType.Override:
		case TokenType.Synchronized:
		case TokenType.Abstract:
		case TokenType.Final:
		case TokenType.Gshared:
		case TokenType.Static:
			localAttributes ~= tokens.front.value;
			tokens.popFront();
			break;
		case TokenType.Const:
		case TokenType.Immutable:
		case TokenType.Inout:
		case TokenType.Pure:
		case TokenType.Scope:
		case TokenType.Shared:
			auto tmp = tokens.front.value;
			tokens.popFront();
			if (tokens.front == TokenType.LParen)
				type = tmp ~ tokens.parenContent();
			else if (tokens.front == TokenType.Colon)
			{
				index++;
				attributes ~= tmp;
			}
			localAttributes ~= tmp;
			break;
		case TokenType.Align:
		case TokenType.Extern:
			string attribute = tokens.front.value;
			tokens.popFront();
			if (tokens.front == TokenType.LParen)
				attribute ~= parenContent(tokens);
			if (tokens.front == TokenType.LBrace)
				mod.merge(parseModule(tokens.betweenBalancedBraces(),
					localProtection.empty() ? protection : localProtection,
					attributes ~ attribute));
			else if (tokens.front == TokenType.Colon)
			{
				tokens.popFront();
				attributes ~= attribute;
			}
			else
				localAttributes ~= attribute;
			break;
		case TokenType.PROTECTION_BEGIN: .. case TokenType.PROTECTION_END:
			string p = tokens.front.value;
			tokens.popFront();
			if (tokens.front == TokenType.Colon)
			{
				protection = p;
				tokens.popFront();
			}
			else if (tokens.front == TokenType.LBrace)
				mod.merge(parseModule(tokens.betweenBalancedBraces(),
					p, attributes ~ localAttributes));
			else
				localProtection = p;
			break;
		case TokenType.Module:
			tokens.popFront();
			while (!tokens.empty && tokens.front != TokenType.Semicolon)
			{
				mod.name ~= tokens.front.value;
				tokens.popFront();
			}
			tokens.popFront();
			resetLocals();
			break;
		case TokenType.Union:
			mod.unions ~= parseUnion(tokens,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.Class:
			mod.classes ~= parseClass(tokens,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.Interface:
			mod.interfaces ~= parseInterface(tokens,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.Struct:
			mod.structs ~= parseStruct(tokens,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.Enum:
			mod.enums ~= parseEnum(tokens,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.Template:
			tokens.popFront(); // template
			tokens.popFront(); // name
			if (tokens.front == TokenType.LParen)
				tokens.betweenBalancedParens(); // params
			if (tokens.front == TokenType.LBrace)
				tokens.betweenBalancedBraces(); // body
			resetLocals();
			break;
		case TokenType.TYPES_BEGIN: .. case TokenType.TYPES_END:
		case TokenType.Auto:
		case TokenType.Identifier:
			if (type.empty())
			{
				type = tokens.parseTypeDeclaration();
			}
			else
			{
				name = tokens.front.value;
				tokens.popFront();
				if (tokens.empty) break;
				if (tokens.front == TokenType.LParen)
				{
					mod.functions ~= parseFunction(tokens, type, name,
						tokens.front.lineNumber,
						localProtection.empty() ? protection : localProtection,
						attributes ~ localAttributes);
				}
				else
				{
					Variable v = new Variable;
					v.name = name;
					v.type = type;
					v.attributes = localAttributes ~ attributes;
					v.protection = localProtection.empty() ? protection : localProtection;
					v.line = tokens.front.lineNumber;
					mod.variables ~= v;
				}
				resetLocals();
			}
			break;
		case TokenType.Unittest:
			tokens.popFront();
			if (!tokens.empty() && tokens.front == TokenType.LBrace)
				tokens.skipBlockStatement();
			resetLocals();
			break;
		case TokenType.Tilde:
			tokens.popFront();
			if (tokens.front == TokenType.This)
			{
				name = "~";
				goto case;
			}
			break;
		case TokenType.This:
			name ~= tokens.front.value;
			tokens.popFront();
			if (!tokens.empty && tokens.front == TokenType.LParen)
			{
				mod.functions ~= parseFunction(tokens, "", name,
					tokens.peek(-1).lineNumber,
					localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			}
			resetLocals();
			break;
		default:
			tokens.popFront();
			break;
		}
	}
	return mod;
}


/**
 * Parses an import statement
 * Returns: only the module names that were imported, not which symbols were
 * selectively improted.
 */
string[] parseImports(TokenBuffer tokens)
{
	assert(tokens.front == TokenType.Import);
	tokens.popFront();
	auto app = appender!(string[])();
	string im;
	while (!tokens.empty)
	{
		switch(tokens.front.type)
		{
		case TokenType.Comma:
			tokens.popFront();
			app.put(im);
			im = "";
			break;
		case TokenType.Assign:
		case TokenType.Semicolon:
			app.put(im);
			tokens.popFront();
			return app.data;
		case TokenType.Colon:
			app.put(im);
			tokens.skipBlockStatement();
			return app.data;
		default:
			im ~= tokens.front.value;
			tokens.popFront();
			break;
		}
	}
	return app.data;
}


/**
 * Parses an enum declaration
 */
Enum parseEnum(TokenBuffer tokens, string protection, string[] attributes)
in
{
	assert (tokens.front == TokenType.Enum);
}
body
{
	Enum e = new Enum;
	e.line = tokens.front.lineNumber;
	tokens.popFront();
	string enumType;
	e.protection = protection;

	if (tokens.front == TokenType.LBrace)
		goto enumBody;

	if (isIdentifierOrType(tokens.front))
	{
		if (tokens.canPeek() && tokens.peek() == TokenType.Identifier)
		{
			// enum long l = 4;
			EnumMember m;
			m.type = tokens.front.value;
			tokens.popFront();
			m.line = tokens.front.lineNumber;
			e.name = m.name = tokens.front.value;
			e.members ~= m;
			tokens.skipBlockStatement();
			return e;
		}
		else if (tokens.canPeek() && tokens.peek() == TokenType.Assign)
		{
			// enum m = "abcd";
			e.name = tokens.front.value;
			EnumMember m;
			m.name = e.name;
			m.line = tokens.front.lineNumber;
			m.type = getTypeFromToken(tokens.peek(2));
			e.members ~= m;
			tokens.skipBlockStatement();
			return e;
		}
	}

	if (isIdentifierOrType(tokens.front))
	{
		e.name = tokens.front.value;
		tokens.popFront();
	}

	if (tokens.front == TokenType.Colon)
	{
		tokens.popFront();
		if (!isIdentifierOrType(tokens.front))
			tokens.skipBlockStatement();
		else
		{
			enumType = tokens.front.value;
			tokens.popFront();
		}
	}

enumBody:
//
//	auto r = tokens.betweenBalancedBraces();
//	while (!r.empty)
//	{
//		EnumMember m;
//		if (isIdentifierOrType(r.front) && i + 1 < r.length && isIdentifierOrType(r[i + 1]))
//		{
//			m.line = r[i + 1].lineNumber;
//			m.name = r[i + 1].value;
//			m.type = r.front.value;
//		}
//		else if (isIdentifierOrType(r.front) && i + 1 < r.length && r[i + 1] == TokenType.Assign)
//		{
//			if (enumType == null && i + 2 < r.length)
//				m.type = getTypeFromToken(r[i + 2]);
//			else
//				m.type = enumType;
//			m.line = r.front.lineNumber;
//			m.name = r.front.value;
//		}
//		else
//		{
//			m.line = r.front.lineNumber;
//			m.name = r.front.value;
//			m.type = enumType == null ? "int" : enumType;
//		}
//		e.members ~= m;
//		skipPastNext(r, TokenType.Comma, i);
//	}
	return e;
}


/**
 * Parses a function declaration
 */
Function parseFunction(TokenBuffer tokens, string type,
	string name, uint line, string protection, string[] attributes)
in
{
	assert (tokens.front == TokenType.LParen);
}
body
{
	Function f = new Function;
	f.name = name;
	f.returnType = type;
	f.line = line;
	f.attributes.insertInPlace(f.attributes.length, attributes);

	Variable[] vars1 = parseParameters(tokens);
	if (!tokens.empty && tokens.front == TokenType.LParen)
	{
		f.templateParameters.insertInPlace(f.templateParameters.length,
			map!("a.type")(vars1));
		f.parameters.insertInPlace(f.parameters.length,
			parseParameters(tokens));
	}
	else
		f.parameters.insertInPlace(f.parameters.length, vars1);

	attributeLoop: while(!tokens.empty)
	{
		switch (tokens.front.type)
		{
		case TokenType.Immutable:
		case TokenType.Const:
		case TokenType.Pure:
		case TokenType.Nothrow:
		case TokenType.Final:
		case TokenType.Override:
			f.attributes ~= tokens.front.value;
			tokens.popFront();
			break;
		default:
			break attributeLoop;
		}
	}

	if (!tokens.empty && tokens.front == TokenType.If)
		f.constraint = parseConstraint(tokens);

	while (!tokens.empty &&
		(tokens.front == TokenType.In || tokens.front == TokenType.Out
		|| tokens.front == TokenType.Body))
	{
		tokens.popFront();
		if (!tokens.empty && tokens.front == TokenType.LParen
			&& tokens.peek(-1) == TokenType.Out)
		{
			tokens.skipParens();
		}

		if (!tokens.empty && tokens.front == TokenType.LBrace)
			tokens.skipBlockStatement();
	}
	if (!tokens.empty)
		return f;
	if (tokens.front == TokenType.LBrace)
		tokens.skipBlockStatement();
	else if (tokens.front == TokenType.Semicolon)
		tokens.popFront();
	return f;
}

string parseConstraint(TokenBuffer tokens)
{
	auto appender = appender!(string)();
	assert(tokens.front == TokenType.If);
	appender.put(tokens.front.value);
	tokens.popFront();
	assert(tokens.front == TokenType.LParen);
	return "if " ~ tokens.parenContent();
}

Variable[] parseParameters(TokenBuffer tokens)
in
{
	assert (tokens.front == TokenType.LParen);
}
body
{
	auto appender = appender!(Variable[])();
	Variable v = new Variable;
	auto r = betweenBalancedParens(tokens);
	size_t i = 0;
	while (!r.empty)
	{
		switch(r.front.type)
		{
		case TokenType.Alias:
		case TokenType.In:
		case TokenType.Out:
		case TokenType.Ref:
		case TokenType.Scope:
		case TokenType.Lazy:
		case TokenType.Const:
		case TokenType.Immutable:
		case TokenType.Shared:
		case TokenType.Inout:
			auto tmp = r.front.value;
			r.popFront();
			if (r.front == TokenType.LParen)
				v.type ~= tmp ~ parenContent(r);
			else
				v.attributes ~= tmp;
			break;
		case TokenType.Colon:
			i++;
			r.skipPastNext(TokenType.Comma);
			appender.put(v);
			v = new Variable;
			break;
		case TokenType.Comma:
			++i;
			appender.put(v);
			v = new Variable;
			break;
		default:
			if (v.type.empty())
			{
				v.type = r.parseTypeDeclaration();
				if (!r.empty)
					appender.put(v);
			}
			else
			{
				v.line = r.front.lineNumber;
				v.name = r.front.value;
				r.popFront();
				appender.put(v);
				if (!r.empty && r.front == TokenType.Vararg)
				{
					v.type ~= " ...";
				}
				v = new Variable;
				r.skipPastNext(TokenType.Comma);
			}
			break;
		}
	}
	return appender.data;
}

string[] parseBaseClassList(TokenBuffer tokens)
in
{
	assert(tokens.front == TokenType.Colon);
}
body
{
	auto appender = appender!(string[])();
	tokens.popFront();
	while (!tokens.empty)
	{
		if (tokens.front == TokenType.Identifier)
		{
			string base = parseTypeDeclaration(tokens);
			appender.put(base);
			if (tokens.front == TokenType.Comma)
				tokens.popFront();
			else
				break;
		}
		else
			break;
	}
	return appender.data;
}

void parseStructBody(TokenBuffer tokens, Struct st)
{
	st.bodyStart = tokens.front.startIndex;
	Module m = parseModule(tokens.betweenBalancedBraces());
	st.bodyEnd = tokens.peek(-1).startIndex;
	st.functions.insertInPlace(0, m.functions);
	st.variables.insertInPlace(0, m.variables);
	st.aliases.insertInPlace(0, m.aliases);
}


Struct parseStructOrUnion(TokenBuffer tokens, string protection,
	string[] attributes)
{
	Struct s = new Struct;
	s.line = tokens.front.lineNumber;
	s.attributes = attributes;
	s.protection = protection;
	s.name = tokens.front.value;
	tokens.popFront();
	if (tokens.front == TokenType.LParen)
		s.templateParameters.insertInPlace(s.templateParameters.length,
			map!("a.type")(parseParameters(tokens)));

	if (tokens.empty) return s;

	if (tokens.front == TokenType.If)
		s.constraint = parseConstraint(tokens);

	if (tokens.empty) return s;

	if (tokens.front == TokenType.LBrace)
		parseStructBody(tokens, s);
	else
		tokens.skipBlockStatement();
	return s;
}

Struct parseStruct(TokenBuffer tokens, string protection,
	string[] attributes)
in
{
	assert(tokens.front == TokenType.Struct);
}
body
{
	return parseStructOrUnion(tokens, protection, attributes);
}

Struct parseUnion(TokenBuffer tokens, string protection, string[] attributes)
in
{
	assert(tokens.front == TokenType.Union);
}
body
{
	tokens.popFront();
	return parseStructOrUnion(tokens, protection, attributes);
}

Inherits parseInherits(TokenBuffer tokens, string protection, string[] attributes)
{
	auto i = new Inherits;
	i.line = tokens.front.lineNumber;
	i.name = tokens.front.value;
	tokens.popFront();
	i.protection = protection;
	i.attributes.insertInPlace(i.attributes.length, attributes);
	if (tokens.front == TokenType.LParen)
		i.templateParameters.insertInPlace(i.templateParameters.length,
			map!("a.type")(parseParameters(tokens)));

	if (tokens.empty) return i;

	if (tokens.front == TokenType.If)
		i.constraint = parseConstraint(tokens);

	if (tokens.empty) return i;

	if (tokens.front == TokenType.Colon)
		i.baseClasses = parseBaseClassList(tokens);

	if (tokens.empty) return i;

	if (tokens.front == TokenType.LBrace)
		parseStructBody(tokens, i);
	else
		tokens.skipBlockStatement();
	return i;
}

Inherits parseInterface(TokenBuffer tokens, string protection, string[] attributes)
in
{
	assert (tokens.front == TokenType.Interface);
}
body
{
	tokens.popFront();
	return parseInherits(tokens, protection, attributes);
}


Inherits parseClass(TokenBuffer tokens, string protection, string[] attributes)
in
{
	assert(tokens.front == TokenType.Class);
}
body
{
	tokens.popFront();
	return parseInherits(tokens, protection, attributes);
}


/**
 * Parse an alias declaration.
 * Note that the language spec mentions a "AliasInitializerList" in the grammar,
 * but there seems to be no example of this being used, nor has the compiler
 * accepted any of my attempts to create one. Therefore, it's not supported here
 */
Alias parseAlias(TokenBuffer tokens, string protection, string[] attributes)
in
{
	assert(tokens.front == TokenType.Alias);
}
body
{
	tokens.popFront();
	Alias a = new Alias;
	a.aliasedType = parseTypeDeclaration(tokens);
	a.attributes = attributes;
	a.protection = protection;
	if (tokens.front == TokenType.Identifier)
	{
		a.name = tokens.front.value;
		a.line = tokens.front.lineNumber;
		skipBlockStatement(tokens);
	}
	else
		return null;
	return a;
}
