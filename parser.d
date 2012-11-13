
//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module parser;

import std.stream;
import std.array;
import std.stdio;
import std.algorithm;

import types, tokenizer;
import langutils;


/**
 * Params:
 *     tokens = the array of tokens
 *     index = an index into tokens such that tokens[index].type == open
 *     open = the opening delimiter
 *     close = the closing delimiter
 * Returns: all tokens that are between the balanced delimiters that start at
 *     tokens[index], not including the delimiters. If the delimiters in tokens
 *     are not balanced, this function will return tokens[index + 1 .. $];
 */
const(Token)[] betweenBalanced(const Token[] tokens, ref size_t index, TokenType open,
	TokenType close)
in
{
	assert (tokens[index] == open);
}
body
{
	++index;
	size_t start = index;
	int depth = 1;
	while (depth > 0 && index < tokens.length)
	{
		if (tokens[index] == open) ++depth;
		else if (tokens[index] == close) --depth;
		++index;
	}
	return tokens[start .. index - 1];
}


/**
 * See_also: betweenBalanced
 */
const(Token)[] betweenBalancedBraces(const Token[] tokens, ref size_t index)
{
	return betweenBalanced(tokens, index, TokenType.LBrace, TokenType.RBrace);
}


/**
 * See_also: betweenBalanced
 */
const(Token)[] betweenBalancedParens(const Token[] tokens, ref size_t index)
{
	return betweenBalanced(tokens, index, TokenType.LParen, TokenType.RParen);
}


/**
 * See_also: betweenBalanced
 */
const(Token)[] betweenBalancedBrackets(const Token[] tokens, ref size_t index)
{
	return betweenBalanced(tokens, index, TokenType.LBracket, TokenType.RBracket);
}


/**
 * If tokens[index] is currently openToken, advances index until it refers to a
 * location in tokens directly after the balanced occurance of closeToken. If
 * tokens[index] is closeToken, decrements index
 *
 */
void skipBalanced(alias openToken, alias closeToken)(const Token[] tokens, ref size_t index)
{
	int depth = tokens[index] == openToken ? 1 : -1;
	int deltaIndex = depth;
	index += deltaIndex;
	for (; index < tokens.length && index > 0 && depth != 0; index += deltaIndex)
	{
		switch (tokens[index].type)
		{
		case openToken: ++depth; break;
		case closeToken: --depth; break;
		default: break;
		}
	}
}

void skipParens(const Token[] tokens, ref size_t index)
{
	skipBalanced!(TokenType.LParen, TokenType.RParen)(tokens, index);
}

void skipBrackets(const Token[] tokens, ref size_t index)
{
	skipBalanced!(TokenType.LBracket, TokenType.RBracket)(tokens, index);
}

void skipBraces(const Token[] tokens, ref size_t index)
{
	skipBalanced!(TokenType.LBrace, TokenType.RBrace)(tokens, index);
}

/**
 * Params:
 *     tokens = the token array to examine
 *     index = an indext into tokens such that tokens[index].type == open
 *     open = the opening delimiter
 *     close = the closing delimiter
 * Returns: a string representing the contents of the two delimiters. This will
 *     not preserve whitespace, but it will place a single space character after
 *     a comma and between identifiers.
 */
string content(const Token[] tokens, ref size_t index, TokenType open, TokenType close)
in
{
	assert (tokens[index] == open);
}
body
{
	index++;
	auto app = appender!string();
	int depth = 1;
	while (depth > 0 && index < tokens.length)
	{
		if (tokens[index] == open) ++depth;
		else if (tokens[index] == close) --depth;
		else if (tokens[index] == TokenType.Comma)
		{
			app.put(", ");
		}
		else
			app.put(tokens[index].value);
		++index;
	}
	return app.data;
}


/**
 * See_also: content
 */
string parenContent(const Token[]tokens, ref size_t index)
{
	return "(" ~ content(tokens, index, TokenType.LParen, TokenType.RParen) ~ ")";
}


/**
 * See_also: content
 */
string bracketContent(const Token[]tokens, ref size_t index)
{
	return "[" ~ content(tokens, index, TokenType.LBracket, TokenType.RBracket) ~ "]";
}


/**
 * Advances index until it indexes a character in tokens after a right brace if
 * index initially indexed a right brace, or advances index until it indexes a
 * character after a simicolon otherwise.
 */
void skipBlockStatement(const Token[] tokens, ref size_t index)
{
	if (tokens[index] == TokenType.LBrace)
		betweenBalancedBraces(tokens, index);
	else
	{
		skipPastNext(tokens, TokenType.Semicolon, index);
	}
}


/**
 * Advances index until it indexes a character in tokens directly after a token
 * of type type. This function handles nesting of braces, brackets, and
 * parenthesis
 */
void skipPastNext(const Token[] tokens, TokenType type, ref size_t index)
{
	while (index < tokens.length)
	{
		if (tokens[index].type == TokenType.LBrace)
			betweenBalancedBraces(tokens, index);
		else if (tokens[index].type == TokenType.LParen)
			betweenBalancedParens(tokens, index);
		else if (tokens[index].type == TokenType.LBracket)
			betweenBalancedBrackets(tokens, index);
		else if (tokens[index].type == type)
		{
			++index;
			return;
		}
		else
			++index;
	}
}

string parseTypeDeclaration(const Token[] tokens, ref size_t index)
{
	auto type = tokens[index++].value.idup;
	buildingType: while (index < tokens.length)
	{
		switch (tokens[index].type)
		{
		case TokenType.LBracket:
			type ~= bracketContent(tokens, index);
			break;
		case TokenType.Not:
			type ~= tokens[index++].value;
			if (tokens[index] == TokenType.LParen)
				type ~= parenContent(tokens, index);
			else
				type ~= tokens[index++].value;
			break;
		case TokenType.Star:
		case TokenType.BitAnd:
			type ~= tokens[index++].value;
			break;
		case TokenType.Function:
			type ~= " " ~ tokens[index++].value;
			type ~= parenContent(tokens, index);
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
Module parseModule(const Token[] tokens, string protection = "public", string[] attributes = [])
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
	while(index < tokens.length)
	{
		switch(tokens[index].type)
		{
		case TokenType.Else:
		case TokenType.Mixin:
		case TokenType.Assert:
			++index;
			tokens.skipBlockStatement(index);
			break;
		case TokenType.Alias:
			Alias a = parseAlias(tokens, index,
				localProtection.empty() ? protection : localProtection,
				attributes);
			mod.aliases ~= a;
			break;
		case TokenType.Import:
			mod.imports ~= parseImports(tokens, index);
			resetLocals();
			break;
		case TokenType.Version:
			++index;
			if (tokens[index] == TokenType.LParen)
			{
				tokens.betweenBalancedParens(index);
				if (tokens[index] == TokenType.LBrace)
					mod.merge(parseModule(betweenBalancedBraces(tokens, index),
						localProtection.empty() ? protection : localProtection,
						attributes));
			}
			else if (tokens[index] == TokenType.Assign)
				tokens.skipBlockStatement(index);
			break;
		case TokenType.Deprecated:
		case TokenType.Nothrow:
		case TokenType.Override:
		case TokenType.Synchronized:
		case TokenType.AtDisable:
		case TokenType.AtProperty:
		case TokenType.AtSafe:
		case TokenType.AtSystem:
		case TokenType.Abstract:
		case TokenType.Final:
		case TokenType.Gshared:
		case TokenType.Static:
			localAttributes ~= tokens[index++].value;
			break;
		case TokenType.Const:
		case TokenType.Immutable:
		case TokenType.Inout:
		case TokenType.Pure:
		case TokenType.Scope:
		case TokenType.Shared:
			auto tmp = tokens[index++].value;
			if (tokens[index] == TokenType.LParen)
				type = tmp ~ parenContent(tokens, index);
			else if (tokens[index] == TokenType.Colon)
			{
				index++;
				attributes ~= tmp;
			}
			localAttributes ~= tmp;
			break;
		case TokenType.Align:
		case TokenType.Extern:
			string attribute = tokens[index++].value;
			if (tokens[index] == TokenType.LParen)
				attribute ~= parenContent(tokens, index);
			if (tokens[index] == TokenType.LBrace)
				mod.merge(parseModule(betweenBalancedBraces(tokens, index),
					localProtection.empty() ? protection : localProtection,
					attributes ~ attribute));
			else if (tokens[index] == TokenType.Colon)
			{
				++index;
				attributes ~= attribute;
			}
			else
				localAttributes ~= attribute;
			break;
		case TokenType.PROTECTION_BEGIN: .. case TokenType.PROTECTION_END:
			string p = tokens[index++].value;
			if (tokens[index] == TokenType.Colon)
			{
				protection = p;
				++index;
			}
			else if (tokens[index] == TokenType.LBrace)
				mod.merge(parseModule(betweenBalancedBraces(tokens, index),
					p, attributes ~ localAttributes));
			else
				localProtection = p;
			break;
		case TokenType.Module:
			++index;
			while (index < tokens.length && tokens[index] != TokenType.Semicolon)
				mod.name ~= tokens[index++].value;
			++index;
			resetLocals();
			break;
		case TokenType.Union:
			mod.unions ~= parseUnion(tokens, index,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.Class:
			mod.classes ~= parseClass(tokens, index,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.Interface:
			mod.interfaces ~= parseInterface(tokens, index,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.Struct:
			mod.structs ~= parseStruct(tokens, index,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.Enum:
			mod.enums ~= parseEnum(tokens, index,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.Template:
			++index; // template
			++index; // name
			if (tokens[index] == TokenType.LParen)
				tokens.betweenBalancedParens(index); // params
			if (tokens[index] == TokenType.LBrace)
				tokens.betweenBalancedBraces(index); // body
			resetLocals();
			break;
		case TokenType.TYPES_BEGIN: .. case TokenType.TYPES_END:
		case TokenType.Auto:
		case TokenType.Identifier:
			if (type.empty())
			{
				type = tokens.parseTypeDeclaration(index);
			}
			else
			{
				name = tokens[index++].value;
				if (index >= tokens.length) break;
				if (tokens[index] == TokenType.LParen)
				{
					mod.functions ~= parseFunction(tokens, index, type, name,
						tokens[index].lineNumber,
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
					v.line = tokens[index].lineNumber;
					mod.variables ~= v;
				}
				resetLocals();
			}
			break;
		case TokenType.Unittest:
			++index;
			if (!tokens.empty() && tokens[index] == TokenType.LBrace)
				tokens.skipBlockStatement(index);
			resetLocals();
			break;
		case TokenType.Tilde:
			++index;
			if (tokens[index] == TokenType.This)
			{
				name = "~";
				goto case;
			}
			break;
		case TokenType.This:
			name ~= tokens[index++].value;
			if (index < tokens.length && tokens[index] == TokenType.LParen)
			{
				mod.functions ~= parseFunction(tokens, index, "", name,
					tokens[index - 1].lineNumber,
					localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			}
			resetLocals();
			break;
		default:
			++index;
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
string[] parseImports(const Token[] tokens, ref size_t index)
{
	assert(tokens[index] == TokenType.Import);
	++index;
	auto app = appender!(string[])();
	string im;
	while (index < tokens.length)
	{
		switch(tokens[index].type)
		{
		case TokenType.Comma:
			++index;
			app.put(im);
			im = "";
			break;
		case TokenType.Assign:
		case TokenType.Semicolon:
			app.put(im);
			++index;
			return app.data;
		case TokenType.Colon:
			app.put(im);
			tokens.skipBlockStatement(index);
			return app.data;
		default:
			im ~= tokens[index++].value;
			break;
		}
	}
	return app.data;
}


/**
 * Parses an enum declaration
 */
Enum parseEnum(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
in
{
	assert (tokens[index] == TokenType.Enum);
}
body
{
	Enum e = new Enum;
	e.line = tokens[index].lineNumber;
	++index;
	string enumType;

	if (tokens[index] == TokenType.LBrace)
		goto enumBody;

	if (isIdentifierOrType(tokens[index]))
	{
		if (index + 1 < tokens.length && tokens[index + 1] == TokenType.Identifier)
		{
			// enum long l = 4;
			EnumMember m;
			m.type = tokens[index++].value;
			m.line = tokens[index].lineNumber;
			e.name = m.name = tokens[index].value;
			e.members ~= m;
			skipBlockStatement(tokens, index);
			return e;
		}
		else if (index + 1 < tokens.length && tokens[index + 1] == TokenType.Assign)
		{
			// enum m = "abcd";
			e.name = tokens[index].value;
			EnumMember m;
			m.name = e.name;
			m.line = tokens[index].lineNumber;
			m.type = getTypeFromToken(tokens[index + 2]);
			e.members ~= m;
			skipBlockStatement(tokens, index);
			return e;
		}
	}

	e.name = tokens[index++].value;

	if (tokens[index] == TokenType.Colon)
	{
		index++;
		if (!isIdentifierOrType(tokens[index]))
			skipBlockStatement(tokens, index);
		else
			enumType = tokens[index++].value;
	}

enumBody:

	auto r = betweenBalancedBraces(tokens, index);
	for (size_t i = 0; i < r.length;)
	{
		EnumMember m;
		if (isIdentifierOrType(r[i]) && i + 1 < r.length && isIdentifierOrType(r[i + 1]))
		{
			m.line = r[i + 1].lineNumber;
			m.name = r[i + 1].value;
			m.type = r[i].value;
		}
		else if (isIdentifierOrType(r[i]) && i + 1 < r.length && r[i + 1] == TokenType.Assign)
		{
			if (enumType == null && i + 2 < r.length)
				m.type = getTypeFromToken(r[i + 2]);
			else
				m.type = enumType;
			m.line = r[i].lineNumber;
			m.name = r[i].value;
		}
		else
		{
			m.line = r[i].lineNumber;
			m.name = r[i].value;
			m.type = enumType == null ? "int" : enumType;
		}
		e.members ~= m;
		skipPastNext(r, TokenType.Comma, i);
	}
	return e;
}


/**
 * Parses a function declaration
 */
Function parseFunction(const Token[] tokens, ref size_t index, string type,
	string name, uint line, string protection, string[] attributes)
in
{
	assert (tokens[index] == TokenType.LParen);
}
body
{
	Function f = new Function;
	f.name = name;
	f.returnType = type;
	f.line = line;
	f.attributes.insertInPlace(f.attributes.length, attributes);

	Variable[] vars1 = parseParameters(tokens, index);
	if (index < tokens.length && tokens[index] == TokenType.LParen)
	{
		f.templateParameters.insertInPlace(f.templateParameters.length,
			map!("a.type")(vars1));
		f.parameters.insertInPlace(f.parameters.length,
			parseParameters(tokens, index));
	}
	else
		f.parameters.insertInPlace(f.parameters.length, vars1);

	attributeLoop: while(index < tokens.length)
	{
		switch (tokens[index].type)
		{
		case TokenType.Immutable:
		case TokenType.Const:
		case TokenType.Pure:
		case TokenType.AtTrusted:
		case TokenType.AtProperty:
		case TokenType.Nothrow:
		case TokenType.Final:
		case TokenType.Override:
			f.attributes ~= tokens[index++].value;
			break;
		default:
			break attributeLoop;
		}
	}

	if (index < tokens.length && tokens[index] == TokenType.If)
		f.constraint = parseConstraint(tokens, index);

	while (index < tokens.length &&
		(tokens[index] == TokenType.In || tokens[index] == TokenType.Out
		|| tokens[index] == TokenType.Body))
	{
		++index;
		if (index < tokens.length && tokens[index] == TokenType.LParen
			&& tokens[index - 1] == TokenType.Out)
		{
			tokens.skipParens(index);
		}

		if (index < tokens.length && tokens[index] == TokenType.LBrace)
			tokens.skipBlockStatement(index);
	}
	if (index >= tokens.length)
		return f;
	if (tokens[index] == TokenType.LBrace)
		tokens.skipBlockStatement(index);
	else if (tokens[index] == TokenType.Semicolon)
		++index;
	return f;
}

string parseConstraint(const Token[] tokens, ref size_t index)
{
	auto appender = appender!(string)();
	assert(tokens[index] == TokenType.If);
	appender.put(tokens[index++].value);
	assert(tokens[index] == TokenType.LParen);
	return "if " ~ parenContent(tokens, index);
}

Variable[] parseParameters(const Token[] tokens, ref size_t index)
in
{
	assert (tokens[index] == TokenType.LParen);
}
body
{
	auto appender = appender!(Variable[])();
	Variable v = new Variable;
	auto r = betweenBalancedParens(tokens, index);
	size_t i = 0;
	while (i < r.length)
	{
		switch(r[i].type)
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
			auto tmp = r[i++].value;
			if (r[i] == TokenType.LParen)
				v.type ~= tmp ~ parenContent(r, i);
			else
				v.attributes ~= tmp;
			break;
		case TokenType.Colon:
			i++;
			r.skipPastNext(TokenType.Comma, i);
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
				v.type = r.parseTypeDeclaration(i);
				if (i >= r.length)
					appender.put(v);
			}
			else
			{
				v.line = r[i].lineNumber;
				v.name = r[i++].value;
				appender.put(v);
				if (i < r.length && r[i] == TokenType.Vararg)
				{
					v.type ~= " ...";
				}
				v = new Variable;
				r.skipPastNext(TokenType.Comma, i);
			}
			break;
		}
	}
	return appender.data;
}

string[] parseBaseClassList(const Token[] tokens, ref size_t index)
in
{
	assert(tokens[index] == TokenType.Colon);
}
body
{
	auto appender = appender!(string[])();
	++index;
	while (index < tokens.length)
	{
		if (tokens[index] == TokenType.Identifier)
		{
			string base = parseTypeDeclaration(tokens, index);
			appender.put(base);
			if (tokens[index] == TokenType.Comma)
				++index;
			else
				break;
		}
		else
			break;
	}
	return appender.data;
}

void parseStructBody(const Token[] tokens, ref size_t index, Struct st)
{
	st.bodyStart = tokens[index].startIndex;
	Module m = parseModule(betweenBalancedBraces(tokens, index));
	st.bodyEnd = tokens[index - 1].startIndex;
	st.functions.insertInPlace(0, m.functions);
	st.variables.insertInPlace(0, m.variables);
	st.aliases.insertInPlace(0, m.aliases);
}


Struct parseStructOrUnion(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
{
	Struct s = new Struct;
	s.line = tokens[index].lineNumber;
	s.attributes = attributes;
	s.protection = protection;
	s.name = tokens[index++].value;
	if (tokens[index] == TokenType.LParen)
		s.templateParameters.insertInPlace(s.templateParameters.length,
			map!("a.type")(parseParameters(tokens, index)));

	if (index >= tokens.length) return s;

	if (tokens[index] == TokenType.If)
		s.constraint = parseConstraint(tokens, index);

	if (index >= tokens.length) return s;

	if (tokens[index] == TokenType.LBrace)
		parseStructBody(tokens, index, s);
	else
		tokens.skipBlockStatement(index);
	return s;
}

Struct parseStruct(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
in
{
	assert(tokens[index] == TokenType.Struct);
}
body
{
	return parseStructOrUnion(tokens, ++index, protection, attributes);
}

Struct parseUnion(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
in
{
	assert(tokens[index] == TokenType.Union);
}
body
{
	return parseStructOrUnion(tokens, ++index, protection, attributes);
}

Inherits parseInherits(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
{
	auto i = new Inherits;
	i.line = tokens[index].lineNumber;
	i.name = tokens[index++].value;
	i.protection = protection;
	i.attributes.insertInPlace(i.attributes.length, attributes);
	if (tokens[index] == TokenType.LParen)
		i.templateParameters.insertInPlace(i.templateParameters.length,
			map!("a.type")(parseParameters(tokens, index)));

	if (index >= tokens.length) return i;

	if (tokens[index] == TokenType.If)
		i.constraint = parseConstraint(tokens, index);

	if (index >= tokens.length) return i;

	if (tokens[index] == TokenType.Colon)
		i.baseClasses = parseBaseClassList(tokens, index);

	if (index >= tokens.length) return i;

	if (tokens[index] == TokenType.LBrace)
		parseStructBody(tokens, index, i);
	else
		tokens.skipBlockStatement(index);
	return i;
}

Inherits parseInterface(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
in
{
	assert (tokens[index] == TokenType.Interface);
}
body
{
	return parseInherits(tokens, ++index, protection, attributes);
}


Inherits parseClass(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
in
{
	assert(tokens[index] == TokenType.Class);
}
body
{
	return parseInherits(tokens, ++index, protection, attributes);
}


/**
 * Parse an alias declaration.
 * Note that the language spec mentions a "AliasInitializerList" in the grammar,
 * but there seems to be no example of this being used, nor has the compiler
 * accepted any of my attempts to create one. Therefore, it's not supported here
 */
Alias parseAlias(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
in
{
	assert(tokens[index] == TokenType.Alias);
}
body
{
	index++;
	Alias a = new Alias;
	a.aliasedType = parseTypeDeclaration(tokens, index);
	a.attributes = attributes;
	a.protection = protection;
	if (tokens[index] == TokenType.Identifier)
	{
		a.name = tokens[index].value;
		a.line = tokens[index].lineNumber;
		skipBlockStatement(tokens, index);
	}
	else
		return null;
	return a;
}
