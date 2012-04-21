/*******************************************************************************
 * The MIT License
 *
 * Copyright (c) 2012 Brian Schott (Sir Alaran)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/


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
	return betweenBalanced(tokens, index, TokenType.lBrace, TokenType.rBrace);
}


/**
 * See_also: betweenBalanced
 */
const(Token)[] betweenBalancedParens(const Token[] tokens, ref size_t index)
{
	return betweenBalanced(tokens, index, TokenType.lParen, TokenType.rParen);
}


/**
 * See_also: betweenBalanced
 */
const(Token)[] betweenBalancedBrackets(const Token[] tokens, ref size_t index)
{
	return betweenBalanced(tokens, index, TokenType.lBracket, TokenType.rBracket);
}

void skipBalanced(alias Op, alias Cl)(const Token[] tokens, ref size_t index)
{
	int depth = tokens[index] == Op ? 1 : -1;
	int deltaIndex = depth;
	index += deltaIndex;
	for (; index < tokens.length && index > 0 && depth != 0; index += deltaIndex)
	{
		switch (tokens[index].type)
		{
		case Op: ++depth; break;
		case Cl: --depth; break;
		default: break;
		}
	}
}

void skipParens(const Token[] tokens, ref size_t index)
{
	skipBalanced!(TokenType.lParen, TokenType.rParen)(tokens, index);
}

void skipBrackets(const Token[] tokens, ref size_t index)
{
	skipBalanced!(TokenType.lBracket, TokenType.rBracket)(tokens, index);
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
		else if (tokens[index] == TokenType.comma)
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
	return "(" ~ content(tokens, index, TokenType.lParen, TokenType.rParen) ~ ")";
}


/**
 * See_also: content
 */
string bracketContent(const Token[]tokens, ref size_t index)
{
	return "[" ~ content(tokens, index, TokenType.lBracket, TokenType.rBracket) ~ "]";
}


/**
 * Advances index until it indexes a character in tokens after a right brace if
 * index initially indexed a right brace, or advances index until it indexes a
 * character after a simicolon otherwise.
 */
void skipBlockStatement(const Token[] tokens, ref size_t index)
{
	if (tokens[index] == TokenType.lBrace)
		betweenBalancedBraces(tokens, index);
	else
	{
		skipPastNext(tokens, TokenType.semicolon, index);
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
		if (tokens[index].type == TokenType.lBrace)
			betweenBalancedBraces(tokens, index);
		else if (tokens[index].type == TokenType.lParen)
			betweenBalancedParens(tokens, index);
		else if (tokens[index].type == TokenType.lBracket)
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
		case TokenType.lBracket:
			type ~= bracketContent(tokens, index);
			break;
		case TokenType.not:
			type ~= tokens[index++].value;
			if (tokens[index] == TokenType.lParen)
				type ~= parenContent(tokens, index);
			else
				type ~= tokens[index++].value;
			break;
		case TokenType.star:
		case TokenType.bitAnd:
			type ~= tokens[index++].value;
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
		case TokenType.tElse:
		case TokenType.tMixin:
		case TokenType.tAssert:
			++index;
			tokens.skipBlockStatement(index);
			break;
		case TokenType.tAlias:
			tokens.skipBlockStatement(index);
			break;
		case TokenType.tImport:
			mod.imports ~= parseImports(tokens, index);
			resetLocals();
			break;
		case TokenType.tVersion:
			++index;
			if (tokens[index] == TokenType.lParen)
			{
				tokens.betweenBalancedParens(index);
				if (tokens[index] == TokenType.lBrace)
					mod.merge(parseModule(betweenBalancedBraces(tokens, index),
						localProtection.empty() ? protection : localProtection,
						attributes));
			}
			else if (tokens[index] == TokenType.assign)
				tokens.skipBlockStatement(index);
			break;
		case TokenType.atDisable:
		case TokenType.atProperty:
		case TokenType.atSafe:
		case TokenType.atSystem:
		case TokenType.tAbstract:
		case TokenType.tConst:
		case TokenType.tDeprecated:
		case TokenType.tExtern:
		case TokenType.tFinal:
		case TokenType.t__gshared:
		case TokenType.tImmutable:
		case TokenType.tInout:
		case TokenType.tNothrow:
		case TokenType.tOverride:
		case TokenType.tPure:
		case TokenType.tScope:
		case TokenType.tShared:
		case TokenType.tStatic:
		case TokenType.tSynchronized:
			auto tmp = tokens[index++].value;
			if (tokens[index] == TokenType.lParen)
				type = tmp ~ parenContent(tokens, index);
			else if (tokens[index] == TokenType.colon)
			{
				index++;
				attributes ~= tmp;
			}
			else
				localAttributes ~= tmp;
			break;
		case TokenType.tAlign:
			string attribute = tokens[index++].value;
			if (tokens[index] == TokenType.lParen)
				attribute ~= parenContent(tokens, index);
			if (tokens[index] == TokenType.lBrace)
				mod.merge(parseModule(betweenBalancedBraces(tokens, index),
					localProtection.empty() ? protection : localProtection,
					attributes ~ attribute));
			else if (tokens[index] == TokenType.colon)
			{
				++index;
				attributes ~= attribute;
			}
			else
				localAttributes ~= attribute;
			break;
		case TokenType.PROTECTION_BEGIN: .. case TokenType.PROTECTION_END:
			string p = tokens[index++].value;
			if (tokens[index] == TokenType.colon)
			{
				protection = p;
				++index;
			}
			else if (tokens[index] == TokenType.lBrace)
				mod.merge(parseModule(betweenBalancedBraces(tokens, index),
					p, attributes ~ localAttributes));
			else
				localProtection = p;
			break;
		case TokenType.tModule:
			++index;
			while (index < tokens.length && tokens[index] != TokenType.semicolon)
				mod.name ~= tokens[index++].value;
			++index;
			resetLocals();
			break;
		case TokenType.tUnion:
			mod.unions ~= parseUnion(tokens, index,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.tClass:
			mod.classes ~= parseClass(tokens, index,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.tInterface:
			mod.interfaces ~= parseInterface(tokens, index,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.tStruct:
			mod.structs ~= parseStruct(tokens, index,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.tEnum:
			mod.enums ~= parseEnum(tokens, index,
				localProtection.empty() ? protection : localProtection,
					localAttributes ~ attributes);
			resetLocals();
			break;
		case TokenType.tTemplate:
			++index; // template
			++index; // name
			if (tokens[index] == TokenType.lParen)
				tokens.betweenBalancedParens(index); // params
			if (tokens[index] == TokenType.lBrace)
				tokens.betweenBalancedBraces(index); // body
			resetLocals();
			break;
		case TokenType.TYPES_BEGIN: .. case TokenType.TYPES_END:
		case TokenType.tAuto:
		case TokenType.identifier:
			if (type.empty())
			{
				type = tokens.parseTypeDeclaration(index);
			}
			else
			{
				name = tokens[index++].value;
				if (index >= tokens.length) break;
				if (tokens[index] == TokenType.lParen)
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
		case TokenType.tUnittest:
			++index;
			if (!tokens.empty() && tokens[index] == TokenType.lBrace)
				tokens.skipBlockStatement(index);
			resetLocals();
			break;
		case TokenType.tilde:
			++index;
			if (tokens[index] == TokenType.tThis)
			{
				name = "~";
				goto case;
			}
			break;
		case TokenType.tThis:
			name ~= tokens[index++].value;
			if (tokens[index] == TokenType.lParen)
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
	assert(tokens[index] == TokenType.tImport);
	++index;
	auto app = appender!(string[])();
	string im;
	while (index < tokens.length)
	{
		switch(tokens[index].type)
		{
		case TokenType.comma:
			++index;
			app.put(im);
			im = "";
			break;
		case TokenType.assign:
		case TokenType.semicolon:
			app.put(im);
			++index;
			return app.data;
		case TokenType.colon:
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
	assert (tokens[index] == TokenType.tEnum);
}
body
{
	++index;
	Enum e = new Enum;
	e.line = tokens[index].lineNumber;
	e.name = tokens[index++].value;

	if (tokens[index] == TokenType.colon)
	{
		++index;
		e.type = tokens[index++].value;
	}
	else
		e.type = "uint";

	if (tokens[index] != TokenType.lBrace)
	{
		tokens.skipBlockStatement(index);
		return e;
	}

	auto r = betweenBalancedBraces(tokens, index);
	for (size_t i = 0; i < r.length;)
	{
		if (r[i].type == TokenType.identifier)
		{
			EnumMember member;
			member.line = r[i].lineNumber;
			member.name = r[i].value;
			e.members ~= member;
			r.skipPastNext(TokenType.comma, i);
		}
		else
			++i;
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
	assert (tokens[index] == TokenType.lParen);
}
body
{
	Function f = new Function;
	f.name = name;
	f.returnType = type;
	f.line = line;
	f.attributes.insertInPlace(f.attributes.length, attributes);

	Variable[] vars1 = parseParameters(tokens, index);
	if (tokens[index] == TokenType.lParen)
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
		case TokenType.tImmutable:
		case TokenType.tConst:
		case TokenType.tPure:
		case TokenType.atTrusted:
		case TokenType.atProperty:
		case TokenType.tNothrow:
		case TokenType.tFinal:
		case TokenType.tOverride:
			f.attributes ~= tokens[index++].value;
			break;
		default:
			break attributeLoop;
		}
	}

	if (tokens[index] == TokenType.tIf)
		f.constraint = parseConstraint(tokens, index);
	while (index < tokens.length &&
		(tokens[index] == TokenType.tIn || tokens[index] == TokenType.tOut
		|| tokens[index] == TokenType.tBody))
	{
		++index;
		if (index < tokens.length && tokens[index] == TokenType.lBrace)
			tokens.skipBlockStatement(index);
	}
	if (index >= tokens.length)
		return f;
	if (tokens[index] == TokenType.lBrace)
		tokens.skipBlockStatement(index);
	else if (tokens[index] == TokenType.semicolon)
		++index;
	return f;
}

string parseConstraint(const Token[] tokens, ref size_t index)
{
	auto appender = appender!(string)();
	assert(tokens[index] == TokenType.tIf);
	appender.put(tokens[index++].value);
	assert(tokens[index] == TokenType.lParen);
	return "if " ~ parenContent(tokens, index);
}

Variable[] parseParameters(const Token[] tokens, ref size_t index)
in
{
	assert (tokens[index] == TokenType.lParen);
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
		case TokenType.tIn:
		case TokenType.tOut:
		case TokenType.tRef:
		case TokenType.tScope:
		case TokenType.tLazy:
		case TokenType.tConst:
		case TokenType.tImmutable:
		case TokenType.tShared:
		case TokenType.tInout:
			auto tmp = r[i++].value;
			if (r[i] == TokenType.lParen)
				v.type ~= tmp ~ parenContent(r, i);
			else
				v.attributes ~= tmp;
			break;
		case TokenType.colon:
			i++;
			r.skipPastNext(TokenType.comma, i);
			appender.put(v);
			v = new Variable;
			break;
		case TokenType.comma:
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
				if (i < r.length && r[i] == TokenType.vararg)
				{
					v.type ~= " ...";
				}
				v = new Variable;
				r.skipPastNext(TokenType.comma, i);
			}
			break;
		}
	}
	return appender.data;
}

string[] parseBaseClassList(const Token[] tokens, ref size_t index)
in
{
	assert(tokens[index] == TokenType.colon);
}
body
{
	auto appender = appender!(string[])();
	++index;
	while (index < tokens.length)
	{
		if (tokens[index] == TokenType.identifier)
		{
			string base = parseTypeDeclaration(tokens, index);
			appender.put(base);
			if (tokens[index] == TokenType.comma)
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
}


Struct parseStructOrUnion(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
{
	Struct s = new Struct;
	s.line = tokens[index].lineNumber;
	s.attributes = attributes;
	s.protection = protection;
	s.name = tokens[index++].value;
	if (tokens[index] == TokenType.lParen)
		s.templateParameters.insertInPlace(s.templateParameters.length,
			map!("a.type")(parseParameters(tokens, index)));

	if (index >= tokens.length) return s;

	if (tokens[index] == TokenType.tIf)
		s.constraint = parseConstraint(tokens, index);

	if (index >= tokens.length) return s;

	if (tokens[index] == TokenType.lBrace)
		parseStructBody(tokens, index, s);
	else
		tokens.skipBlockStatement(index);
	return s;
}

Struct parseStruct(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
in
{
	assert(tokens[index] == TokenType.tStruct);
}
body
{
	return parseStructOrUnion(tokens, ++index, protection, attributes);
}

Struct parseUnion(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
in
{
	assert(tokens[index] == TokenType.tUnion);
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
	if (tokens[index] == TokenType.lParen)
		i.templateParameters.insertInPlace(i.templateParameters.length,
			map!("a.type")(parseParameters(tokens, index)));

	if (index >= tokens.length) return i;

	if (tokens[index] == TokenType.tIf)
		i.constraint = parseConstraint(tokens, index);

	if (index >= tokens.length) return i;

	if (tokens[index] == TokenType.colon)
		i.baseClasses = parseBaseClassList(tokens, index);

	if (index >= tokens.length) return i;

	if (tokens[index] == TokenType.lBrace)
		parseStructBody(tokens, index, i);
	else
		tokens.skipBlockStatement(index);
	return i;
}

Inherits parseInterface(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
in
{
	assert (tokens[index] == TokenType.tInterface);
}
body
{
	return parseInherits(tokens, ++index, protection, attributes);
}


Inherits parseClass(const Token[] tokens, ref size_t index, string protection,
	string[] attributes)
in
{
	assert(tokens[index] == TokenType.tClass);
}
body
{
	return parseInherits(tokens, ++index, protection, attributes);
}
