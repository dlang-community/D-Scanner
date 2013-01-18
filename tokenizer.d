//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module tokenizer;

import std.range;
import std.file;
import std.traits;
import std.algorithm;
import std.conv;
import std.uni;
import std.stdio;
import std.ascii;
import std.format;

import langutils;
import codegen;
import entities;

pure bool isNewline(R)(R range)
{
	return range.front == '\n' || range.front == '\r';
}

pure bool isEoF(R)(R range)
{
	return range.empty || range.front == 0 || range.front == 0x1a;
}

char[] popNewline(R)(ref R range, ref uint index)
{
	char[] chars;
	if (range.front == '\r')
	{
		chars ~= range.front;
		range.popFront();
		++index;
	}
	if (range.front == '\n')
	{
		chars ~= range.front;
		range.popFront();
		++index;
	}
	return chars;
}

unittest
{
	uint i;
	auto s = "\r\ntest";
	assert (popNewline(s, i) == "\r\n");
	assert (s == "test");
}

/**
 * Returns:
 */
Token lexWhitespace(R)(ref R range, ref uint index, ref uint lineNumber)
{
	Token t;
	t.type = TokenType.Whitespace;
	t.lineNumber = lineNumber;
	t.startIndex = index;
	auto app = appender!(char[])();
	while (!isEoF(range) && std.uni.isWhite(range.front))
	{
		if (isNewline(range))
		{
			++lineNumber;
			app.put(popNewline(range, index));
		}
		else
		{
			app.put(range.front);
			range.popFront();
			++index;
		}
	}
	t.value = to!string(app.data);
	return t;
}

unittest
{
	import std.stdio;
	uint lineNum = 1;
	uint index;
	auto chars = " \n \r\n \tabcde";
	auto r = lexWhitespace(chars, index, lineNum);
	assert (r.value == " \n \r\n \t");
	assert (chars == "abcde");
	assert (lineNum == 3);
}

/**
 * Increments endIndex until it indexes a character directly after a comment
 * Params:
 *     inputString = the source code to examine
 *     endIndex = an index into inputString at the second character of a
 *     comment, i.e. points at the second slash in a // comment.
 *     lineNumber = the line number that corresponds to endIndex
 * Returns: The comment
 */
Token lexComment(R)(ref R input, ref uint index, ref uint lineNumber)
in
{
	assert (input.front == '/');
}
body
{
	Token t;
	t.lineNumber = lineNumber;
	t.type = TokenType.Comment;
	t.startIndex = index;
	auto app = appender!(char[])();
	app.put(input.front);
	input.popFront();
	switch(input.front)
	{
	case '/':
		while (!isEoF(input) && !isNewline(input))
		{
			app.put(input.front);
			input.popFront();
			++index;
		}
		break;
	case '*':
		while (!isEoF(input))
		{
			if (isNewline(input))
			{
				app.put(popNewline(input, index));
				++lineNumber;
			}
			else if (input.front == '*')
			{
				app.put(input.front);
				input.popFront();
				++index;
				if (input.front == '/')
				{
					app.put(input.front);
					input.popFront();
					++index;
					break;
				}
			}
			else
			{
				app.put(input.front);
				input.popFront();
				++index;
			}
		}
		break;
	case '+':
		int depth = 1;
		while (depth > 0 && !isEoF(input))
		{
			if (isNewline(input))
			{
				app.put(popNewline(input, index));
				lineNumber++;
			}
			else if (input.front == '+')
			{
				app.put(input.front);
				input.popFront();
				++index;
				if (input.front == '/')
				{
					app.put(input.front);
					input.popFront();
					++index;
					--depth;
				}
			}
			else if (input.front == '/')
			{
				app.put(input.front);
				input.popFront();
				++index;
				if (input.front == '+')
				{
					app.put(input.front);
					input.popFront();
					++index;
					++depth;
				}
			}
			else
			{
				app.put(input.front);
				input.popFront();
				++index;
			}
		}
		break;
	default:
		Token errorToken;
		return errorToken;
	}
	t.value = to!string(app.data);
	return t;
}

unittest
{
	uint index;
	uint lineNumber = 1;
	auto chars = "//this is a comment\r\nthis is not";
	auto comment = lexComment(chars, index, lineNumber);
	assert (chars == "\r\nthis is not");
	assert (comment.value == "//this is a comment");
}

unittest
{
	uint index = 0;
	uint lineNumber = 1;
	auto chars = "/* this is a\n\tcomment\r\n */this is not";
	auto comment = lexComment(chars, index, lineNumber);
	assert (chars == "this is not");
	assert (comment.value == "/* this is a\n\tcomment\r\n */");
	assert (lineNumber == 3);
}

unittest
{
	uint index;
	uint lineNumber = 1;
	auto chars = "/+this is a /+c/+omm+/ent+/ \r\nthis+/ is not";
	auto comment = lexComment(chars, index, lineNumber);
	assert (chars == " is not");
	assert (comment.value == "/+this is a /+c/+omm+/ent+/ \r\nthis+/");
	assert (lineNumber == 2);
}

unittest
{
	uint i;
	uint l;
	auto chars = "/(";
	auto comment = lexComment(chars, i, l);
	assert (comment == "");
}

/**
 * Pops up to upTo hex chars from the input range and returns them as a string
 */
string popDigitChars(R, alias isInterestingDigit)(ref R input, ref uint index,
	uint upTo)
{
	auto app = appender!(char[])();
	for (uint i = 0; i != upTo; ++i)
	{
		if (isInterestingDigit(input.front))
		{
			app.put(input.front);
			input.popFront();
		}
		else
			break;
	}
	return to!string(app.data);
}

string popHexChars(R)(ref R input, ref uint index, uint upTo)
{
	return popDigitChars!(R, isHexDigit)(input, index, upTo);
}

string popOctalChars(R)(ref R input, ref uint index, uint upTo)
{
	return popDigitChars!(R, isOctalDigit)(input, index, upTo);
}

unittest
{
	uint i;
	auto a = "124ac82d3fqwerty";
	auto ra = popHexChars(a, i, uint.max);
	assert (a == "qwerty");
	assert (ra == "124ac82d3f");
	auto b = "08a7c2e3";
	auto rb = popHexChars(b, i, 4);
	assert (rb.length == 4);
	assert (rb == "08a7");
	assert (b == "c2e3");
	auto c = "00123832";
	auto rc = popOctalChars(c, i, uint.max);
	assert (c == "832");
	assert (rc == "00123");
}

string interpretEscapeSequence(R)(ref R input, ref uint index)
in
{
	assert(input.front == '\\');
}
body
{
	input.popFront();
	switch (input.front)
	{
	case '\'':
	case '\"':
	case '?':
	case '\\':
	case 0:
	case 0x1a:
		auto f = input.front;
		input.popFront();
		++index;
		return to!string(f);
	case 'a': input.popFront(); ++index; return "\a";
	case 'b': input.popFront(); ++index; return "\b";
	case 'f': input.popFront(); ++index; return "\f";
	case 'n': input.popFront(); ++index; return "\n";
	case 'r': input.popFront(); ++index; return "\r";
	case 't': input.popFront(); ++index; return "\t";
	case 'v': input.popFront(); ++index; return "\v";
	case 'x':
		input.popFront();
		auto hexChars = popHexChars(input, index, 2);
		return to!string(cast(dchar) parse!uint(hexChars, 16));
	case '0': .. case '7':
		auto octalChars = popOctalChars(input, index, 3);
		return to!string(cast(dchar) parse!uint(octalChars, 8));
	case 'u':
		input.popFront();
		auto hexChars = popHexChars(input, index, 4);
		return to!string(cast(dchar) parse!uint(hexChars, 16));
	case 'U':
		input.popFront();
		auto hexChars = popHexChars(input, index, 8);
		return to!string(cast(dchar) parse!uint(hexChars, 16));
	case '&':
		input.popFront();
		++index;
		auto entity = appender!(char[])();
		while (!input.isEoF() && input.front != ';')
		{
			entity.put(input.front);
			input.popFront();
			++index;
		}
		if (!isEoF(input))
		{
			auto decoded = to!string(entity.data) in characterEntities;
			input.popFront();
			++index;
			if (decoded !is null)
				return to!string(*decoded);
		}
		return "";
	default:
		input.popFront();
		++index;
		// This is an error
		return "\\";
	}
}

unittest
{
	uint i;
	auto vals = [
		"\\&amp;": "&",
		"\\n": "\n",
		"\\?": "?",
		"\\u0033": "\u0033",
		"\\U00000076": "v",
		"\\075": "=",
		"\\'": "'",
		"\\a": "\a",
		"\\b": "\b",
		"\\f": "\f",
		"\\r": "\r",
		"\\t": "\t",
		"\\v": "\v",
		"\\y": "\\",
		"\\x20": " ",
		"\\&eeeeeeror;": "",
	];
	foreach (k, v; vals)
		assert (interpretEscapeSequence(k, i) == v);
}

/**
 * Params:
 *     inputString = the source code to examine
 *     endIndex = an index into inputString at the opening quote
 *     lineNumber = the line number that corresponds to endIndex
 *     quote = the opening (and closing) quote character for the string to be
 *         lexed
 * Returns: a string literal, including its opening and closing quote characters
 */
Token lexString(R)(ref R input, ref uint lineNumber, ref uint index,
	bool canEscape = true)
in
{
	assert (input.front == '\'' || input.front == '"' || input.front == '`');
}
body
{
	Token t;
	t.lineNumber = lineNumber;
	t.startIndex = index;
	auto quote = input.front;
	input.popFront();
	++index;
	auto app = appender!(char[])();
	while (!isEoF(input))
	{
		if (isNewline(input))
		{
			app.put(popNewline(input, index));
			lineNumber++;
		}
		else if (input.front == '\\' && canEscape)
			app.put(interpretEscapeSequence(input, index));
		else if (input.front == quote)
		{
			input.popFront();
			++index;
			break;
		}
		else
		{
			app.put(input.front);
			input.popFront();
			++index;
		}
	}
	if (!input.isEoF())
	{
		switch (input.front)
		{
		case 'w':
			t.type = TokenType.WStringLiteral;
			input.popFront();
			++index;
			break;
		case 'd':
			t.type = TokenType.DStringLiteral;
			input.popFront();
			++index;
			break;
		case 'c':
			input.popFront();
			++index;
			goto default;
		default:
			t.type = TokenType.StringLiteral;
			break;
		}
	}
	t.value = to!string(app.data);
	return t;
}

unittest
{
	uint l = 1;
	uint i;
	auto a = `"abcde"`;
	assert (lexString(a, i, l) == "abcde");
	auto b = "\"ab\\ncd\"";
	assert (lexString(b, i, l) == "ab\ncd");
	auto c = "`abc\\ndef`";
	assert (lexString(c, i, l, false) == "abc\\ndef");
	auto d = `"12345"w`;
	assert (lexString(d, i, l).type == TokenType.WStringLiteral);
	auto e = `"abc"c`;
	assert (lexString(e, i, l).type == TokenType.StringLiteral);
	auto f = `"abc"d`;
	assert (lexString(f, i, l).type == TokenType.DStringLiteral);
	auto g = "\"a\nb\"";
	assert (lexString(g, i, l) == "a\nb");
}

Token lexNumber(R)(ref R input, ref uint index, const uint lineNumber)
in
{
	assert(isDigit(input.front));
}
body
{
	auto app = appender!(char[])();
	// hex and binary can start with zero, anything else is decimal
	if (input.front != '0')
		return lexDecimal(input, index, lineNumber, app);
	else
	{
		app.put(input.front);
		input.popFront();
		++index;
		switch (input.front)
		{
		case 'x':
		case 'X':
			app.put(input.front);
			input.popFront();
			++index;
			return lexHex(input, index, lineNumber, app);
		case 'b':
		case 'B':
			app.put(input.front);
			input.popFront();
			++index;
			return lexBinary(input, index, lineNumber, app);
		default:
			return lexDecimal(input, index, lineNumber, app);
		}
	}
}

unittest
{
	uint i;
	uint l;
	auto a = "0q1239";
	assert (lexNumber(a, i, l) == "0");
}

Token lexBinary(R)(ref R input, ref uint index, const uint lineNumber,
	ref typeof(appender!(char[])()) app)
{
	Token token;
	token.lineNumber = lineNumber;
	token.startIndex = index;
	token.type = TokenType.IntLiteral;
	bool lexingSuffix = false;
	bool isLong = false;
	bool isUnsigned = false;
	binaryLoop: while (!input.isEoF())
	{
		switch (input.front)
		{
		case '0':
		case '1':
		case '_':
			if (lexingSuffix)
				break binaryLoop;
			app.put(input.front);
			input.popFront();
			++index;
			break;
		case 'u':
		case 'U':
			if (isUnsigned)
				break binaryLoop;
			app.put(input.front);
			input.popFront();
			++index;
			if (isLong)
			{
				token.type = TokenType.UnsignedLongLiteral;
				break binaryLoop;
			}
			else
				token.type = TokenType.UnsignedIntLiteral;
			isUnsigned = true;
			break;
		case 'L':
			if (isLong)
				break binaryLoop;
			app.put(input.front);
			input.popFront();
			++index;
			lexingSuffix = true;
			if (isUnsigned)
			{
				token.type = TokenType.UnsignedLongLiteral;
				break binaryLoop;
			}
			else
				token.type = TokenType.LongLiteral;
			isLong = true;
			break;
		default:
			break binaryLoop;
		}
	}
	token.value = to!string(app.data);
	return token;
}

unittest
{
	uint i;
	uint l;

	auto a = "0b000101";
	auto ar = lexNumber(a, i, l);
	assert (ar.value == "0b000101");
	assert (a == "");

	auto b = "0b001L_";
	auto br = lexNumber(b, i, l);
	assert (br.value == "0b001L");
	assert (br.type == TokenType.LongLiteral);

	auto c = "0b1101uLL";
	auto cr = lexNumber(c, i, l);
	assert (cr.value == "0b1101uL");
	assert (cr.type == TokenType.UnsignedLongLiteral);

	auto d = "0b1q";
	auto dr = lexNumber(d, i, l);
	assert (dr.value == "0b1");
	assert (dr.type == TokenType.IntLiteral);

	auto e = "0b1_0_1LU";
	auto er = lexNumber(e, i, l);
	assert (er.value == "0b1_0_1LU");
	assert (er.type == TokenType.UnsignedLongLiteral);

	auto f = "0b1_0_1uU";
	auto fr = lexNumber(f, i, l);
	assert (fr.value == "0b1_0_1u");
	assert (fr.type == TokenType.UnsignedIntLiteral);

	auto g = "0b1_0_1LL";
	auto gr = lexNumber(g, i, l);
	assert (gr.value == "0b1_0_1L");
	assert (gr.type == TokenType.LongLiteral);
}


Token lexDecimal(R)(ref R input, ref uint index, const uint lineNumber,
	ref typeof(appender!(char[])()) app)
{
	bool lexingSuffix = false;
	bool isLong = false;
	bool isUnsigned = false;
	bool isFloat = false;
	bool isReal = false;
	bool isDouble = false;
	bool foundDot = false;
	bool foundE = false;
	bool foundPlusMinus = false;
	Token token;
	token.type = TokenType.IntLiteral;
	token.startIndex = index;
	token.lineNumber = lineNumber;
	decimalLoop: while (!input.isEoF())
	{
		switch (input.front)
		{
		case '0': .. case '9':
		case '_':
			if (lexingSuffix)
				break decimalLoop;
			app.put(input.front);
			input.popFront();
			++index;
			break;
		case 'e':
		case 'E':
			// For this to be a valid exponent, the next character must be a
			// decimal character or a sign
			auto r = input.save();
			r.popFront();
			if (foundE || r.isEoF())
				break decimalLoop;
			switch (r.front)
			{
			case '+':
			case '-':
				r.popFront();
				if (r.isEoF() || r.front < '0' || r.front > '9')
				{
					break decimalLoop;
				}
				break;
			case '0': .. case '9':
				break;
			default:
				break decimalLoop;
			}
			app.put(input.front);
			input.popFront();
			++index;
			foundE = true;
			isDouble = true;
			token.type = TokenType.DoubleLiteral;
			break;
		case '+':
		case '-':
			if (foundPlusMinus || !foundE)
				break decimalLoop;
			foundPlusMinus = true;
			app.put(input.front);
			input.popFront();
			++index;
			break;
		case '.':
			auto r = input.save();
			r.popFront();
			if (!r.isEoF() && r.front == '.')
				break decimalLoop; // possibly slice expression
			if (foundDot)
				break decimalLoop; // two dots with other characters between them
			app.put(input.front);
			input.popFront();
			++index;
			foundDot = true;
			token.type = TokenType.DoubleLiteral;
			isDouble = true;
			break;
		case 'u':
		case 'U':
			if (isUnsigned)
				break decimalLoop;
			app.put(input.front);
			input.popFront();
			++index;
			lexingSuffix = true;
			if (isLong)
				token.type = TokenType.UnsignedLongLiteral;
			else
				token.type = TokenType.UnsignedIntLiteral;
			isUnsigned = true;
			break;
		case 'L':
			if (isLong || isReal)
				break decimalLoop;
			app.put(input.front);
			input.popFront();
			++index;
			lexingSuffix = true;
			if (isDouble)
			{
				token.type = TokenType.RealLiteral;
				isReal = true;
			}
			else if (isUnsigned)
			{
				token.type = TokenType.UnsignedLongLiteral;
				isLong = true;
			}
			else
			{
				token.type = TokenType.LongLiteral;
				isLong = true;
			}
			break;
		case 'f':
		case 'F':
			lexingSuffix = true;
			if (isUnsigned || isLong)
				break decimalLoop;
			app.put(input.front);
			input.popFront();
			++index;
			token.type = TokenType.FloatLiteral;
			isFloat = true;
			break;
		case 'i':
			// Spec says that this is the last suffix, so all cases break the
			// loop.
			if (isReal)
			{
				app.put(input.front);
				input.popFront();
				++index;
				token.type = TokenType.IRealLiteral;
			}
			else if (isFloat)
			{
				app.put(input.front);
				input.popFront();
				++index;
				token.type = TokenType.IFloatLiteral;
			}
			else if (isDouble)
			{
				app.put(input.front);
				input.popFront();
				++index;
				token.type = TokenType.IDoubleLiteral;
			}
			break decimalLoop;
		default:
			break decimalLoop;
		}
	}
	token.value = to!string(app.data());
	return token;
}


unittest {
	uint i;
	uint l;
	auto a = "55e-4";
	auto ar = lexNumber(a, i, l);
	assert (ar.value == "55e-4");
	assert (ar.type == TokenType.DoubleLiteral);

	auto b = "123.45f";
	auto br = lexNumber(b, i, l);
	assert (br.value == "123.45f");
	assert (br.type == TokenType.FloatLiteral);

	auto c = "3e+f";
	auto cr = lexNumber(c, i, l);
	assert (cr.value == "3");
	assert (cr.type == TokenType.IntLiteral);

	auto d = "3e++f";
	auto dr = lexNumber(d, i, l);
	assert (dr.value == "3");
	assert (dr.type == TokenType.IntLiteral);

	auto e = "1234..1237";
	auto er = lexNumber(e, i, l);
	assert (er.value == "1234");
	assert (er.type == TokenType.IntLiteral);

	auto f = "12L_";
	auto fr = lexNumber(f, i, l);
	assert (fr == "12L");

	auto g = "12e-12e";
	auto gr = lexNumber(g, i, l);
	assert (gr == "12e-12");

	auto h = "12e10";
	auto hr = lexNumber(h, i, l);
	assert (hr == "12e10");

	auto j = "12er";
	auto jr = lexNumber(j, i, l);
	assert (jr == "12");

	auto k = "12e+12-";
	auto kr = lexNumber(k, i, l);
	assert (kr == "12e+12");

	auto m = "1.1.";
	auto mr = lexNumber(m, i, l);
	assert (mr == "1.1");

	auto n = "12uu";
	auto nr = lexNumber(n, i, l);
	assert (nr == "12u");
	assert (nr.type == TokenType.UnsignedIntLiteral);

	auto o = "12LU";
	auto or = lexNumber(o, i, l);
	assert (or == "12LU");

	auto p = "3LL";
	auto pr = lexNumber(p, i, l);
	assert (pr == "3L");

	auto q = "3.0LL";
	auto qr = lexNumber(q, i, l);
	assert (qr == "3.0L");

	auto r = "5uL";
	auto rr = lexNumber(r, i, l);
	assert (rr == "5uL");

	auto s = "5Lf";
	auto sr = lexNumber(s, i, l);
	assert (sr == "5L");
	assert (sr == TokenType.LongLiteral);

	auto t = "5i";
	auto tr = lexNumber(t, i, l);
	assert (tr == "5");
	assert (tr == TokenType.IntLiteral);

	auto u = "894.3i";
	auto ur = lexNumber(u, i, l);
	assert (ur == "894.3i");
	assert (ur == TokenType.IDoubleLiteral);

	auto v = "894.3Li";
	auto vr = lexNumber(v, i, l);
	assert (vr == "894.3Li");
	assert (vr == TokenType.IRealLiteral);

	auto w = "894.3fi";
	auto wr = lexNumber(w, i, l);
	assert (wr == "894.3fi");
	assert (wr == TokenType.IFloatLiteral);

	auto x = "4892.4ee";
	auto xr = lexNumber(x, i, l);
	assert (xr == "4892.4");
	assert (xr == TokenType.DoubleLiteral);
}

Token lexHex(R)(ref R input, ref uint index, const uint lineNumber,
	ref typeof(appender!(char[])()) app)
{
	bool isLong = false;
	bool isUnsigned = false;
	bool isFloat = false;
	bool isReal = false;
	bool isDouble = false;
	bool foundDot = false;
	bool foundExp = false;
	bool foundPlusMinus = false;
	string backup;
	Token token;
	token.lineNumber = lineNumber;
	token.startIndex =  index;
	token.type = TokenType.IntLiteral;
	hexLoop: while (!input.isEoF())
	{
		switch (input.front)
		{
		case 'a': .. case 'f':
		case 'A': .. case 'F':
			if (foundExp)
				break hexLoop;
			else
				goto case;
		case '0': .. case '9':
		case '_':
			app.put(input.front);
			input.popFront();
			++index;
			break;
		case 'p':
		case 'P':
			if (foundExp)
				break hexLoop;
			auto r = input.save();
			r.popFront();
			switch (r.front)
			{
			case '-':
			case '+':
				r.popFront();
				if (r.isEoF() || !isDigit(r.front))
					break hexLoop;
				break;
			case '0': .. case '9':
				break;
			default:
				break hexLoop;
			}
			app.put(input.front);
			input.popFront();
			++index;
			foundExp = true;
			isDouble = true;
			token.type = TokenType.DoubleLiteral;
			break;
		case '+':
		case '-':
			if (foundPlusMinus || !foundExp)
				break hexLoop;
			foundPlusMinus = true;
			app.put(input.front);
			input.popFront();
			++index;
			break;
		case '.':
			auto r = input.save();
			r.popFront();
			if (!r.isEoF() && r.front == '.')
				break hexLoop; // slice expression
			if (foundDot)
				break hexLoop; // two dots with other characters between them
			app.put(input.front);
			input.popFront();
			++index;
			foundDot = true;
			token.type = TokenType.DoubleLiteral;
			break;
		default:
			break hexLoop;
		}
	}
	token.value = to!string(app.data);
	return token;
}

unittest
{
	uint i;
	uint l;

	auto a = "0x193abfq";
	auto ar = lexNumber(a, i, l);
	assert(ar.value == "0x193abf");
	assert(ar.type == TokenType.IntLiteral);

	auto b = "0x2130xabc";
	auto br = lexNumber(b, i, l);
	assert(br.value == "0x2130");
	assert(br.type == TokenType.IntLiteral);

	auto c = "0x123..0321";
	auto cr = lexNumber(c, i, l);
	assert (cr.value == "0x123");
	assert (cr.type == TokenType.IntLiteral);

	auto d = "0xabp5";
	auto dr = lexNumber(d, i, l);
	assert (dr == "0xabp5");
	assert (dr == TokenType.DoubleLiteral);

	auto e = "0x93p+5";
	auto er = lexNumber(e, i, l);
	assert (er == "0x93p+5");
	assert (er == TokenType.DoubleLiteral);

	auto f = "0x93pp";
	auto fr = lexNumber(f, i, l);
	assert (fr == "0x93");
	assert (fr == TokenType.IntLiteral);

	auto g = "0XF..7";
	auto gr = lexNumber(g, i, l);
	assert (gr == "0XF");
	assert (gr == TokenType.IntLiteral);

	auto h = "0x8.4p100";
	auto hr = lexNumber(h, i, l);
	assert (hr == "0x8.4p100");
	assert (hr == TokenType.DoubleLiteral);

	auto j = "0x8.4.100";
	auto jr = lexNumber(j, i, l);
	assert (jr == "0x8.4");
	assert (jr == TokenType.DoubleLiteral);

	auto k = "0x1p-t";
	auto kr = lexNumber(k, i, l);
	assert (kr == "0x1");
	assert (kr == TokenType.IntLiteral);

	auto m = "0x1p-5p";
	auto mr = lexNumber(m, i, l);
	assert (mr == "0x1p-5");
	assert (mr == TokenType.DoubleLiteral);

	auto n = "0x1p-c_";
	auto nr = lexNumber(n, i, l);
	assert (nr == "0x1");
	assert (nr == TokenType.IntLiteral);

	auto o = "0x1p-1a";
	auto or = lexNumber(o, i, l);
	assert (or == "0x1p-1");
	assert (or == TokenType.DoubleLiteral);

	auto p = "0x1p-1+";
	auto pr = lexNumber(p, i, l);
	assert (pr == "0x1p-1");
	assert (pr == TokenType.DoubleLiteral);
}

/**
 * Returns: true if  ch marks the ending of one token and the beginning of
 *     another, false otherwise
 */
pure nothrow bool isSeparating(C)(C ch) if (isSomeChar!C)
{
	switch (ch)
	{
		case '!': .. case '/':
		case ':': .. case '@':
		case '[': .. case '^':
		case '{': .. case '~':
		case 0x20: // space
		case 0x09: // tab
		case 0x0a: .. case 0x0d: // newline, vertical tab, form feed, carriage return
			return true;
		default:
			return false;
	}
}

/**
 * Configure the tokenize() function
 */
enum IterationStyle
{
	/// Only include code, not whitespace or comments
	CODE_ONLY,
	/// Include everything
	EVERYTHING
}

struct TokenRange(R) if (isInputRange(R))
{
	this(ref R range)
	{
		this.range = range;
	}

	bool empty() const @property
	{
		return _empty;
	}

	Token front() const @property
	{
		return current;
	}

	Token popFront()
	{
		Token c = current;

		return c;
	}

private:
	Token current;
	uint lineNumber;
	uint index;
	R range;
	bool _empty;
}

//Token[] tokenize(S)(S inputString, IterationStyle iterationStyle = IterationStyle.CODE_ONLY)
//	if (isSomeString!S)
//{
//	auto tokenAppender = appender!(Token[])();
//
//	// This is very likely a local maximum, but it does seem to take a few
//	// milliseconds off of the run time
//	tokenAppender.reserve(inputString.length / 4);
//
//	size_t endIndex = 0;
//	uint lineNumber = 1;
//
//	if (inputString.length > 1 && inputString[0..2] == "#!")
//	{
//		Token currentToken;
//		currentToken.lineNumber = lineNumber; // lineNumber is always 1
//		currentToken.value = lexScriptLine(inputString, endIndex, lineNumber);
//		currentToken.type = TokenType.ScriptLine;
//	}
//
//	while (!isEoF(inputString, endIndex))
//	{
//		size_t prevIndex = endIndex;
//		Token currentToken;
//		auto startIndex = endIndex;
//		if (isWhite(inputString[endIndex]))
//		{
//			if (iterationStyle == IterationStyle.EVERYTHING)
//			{
//				currentToken.lineNumber = lineNumber;
//				currentToken.value = lexWhitespace(inputString, endIndex,
//					lineNumber);
//				currentToken.type = TokenType.Whitespace;
//				tokenAppender.put(currentToken);
//			}
//			else
//				lexWhitespace(inputString, endIndex, lineNumber);
//			continue;
//		}
//		currentToken.startIndex = endIndex;
//
//		outerSwitch: switch(inputString[endIndex])
//		{
//		mixin(generateCaseTrie(
//			"=",    "TokenType.Assign",
//			"&",    "TokenType.BitAnd",
//			"&=",   "TokenType.BitAndEquals",
//			"|",    "TokenType.BitOr",
//			"|=",   "TokenType.BitOrEquals",
//			"~=",   "TokenType.CatEquals",
//			":",    "TokenType.Colon",
//			",",    "TokenType.Comma",
//			"$",    "TokenType.Dollar",
//			".",    "TokenType.Dot",
//			"==",   "TokenType.Equals",
//			"=>",   "TokenType.GoesTo",
//			">",    "TokenType.Greater",
//			">=",   "TokenType.GreaterEqual",
//			"#",    "TokenType.Hash",
//			"&&",   "TokenType.LogicAnd",
//			"{",    "TokenType.LBrace",
//			"[",    "TokenType.LBracket",
//			"<",    "TokenType.Less",
//			"<=",   "TokenType.LessEqual",
//			"<>=",  "TokenType.LessEqualGreater",
//			"<>",   "TokenType.LessOrGreater",
//			"||",   "TokenType.LogicOr",
//			"(",    "TokenType.LParen",
//			"-",    "TokenType.Minus",
//			"-=",   "TokenType.MinusEquals",
//			"%",    "TokenType.Mod",
//			"%=",   "TokenType.ModEquals",
//			"*=",   "TokenType.MulEquals",
//			"!",    "TokenType.Not",
//			"!=",   "TokenType.NotEquals",
//			"!>",   "TokenType.NotGreater",
//			"!>=",  "TokenType.NotGreaterEqual",
//			"!<",   "TokenType.NotLess",
//			"!<=",  "TokenType.NotLessEqual",
//			"!<>",  "TokenType.NotLessEqualGreater",
//			"+",    "TokenType.Plus",
//			"+=",   "TokenType.PlusEquals",
//			"^^",   "TokenType.Pow",
//			"^^=",  "TokenType.PowEquals",
//			"}",    "TokenType.RBrace",
//			"]",    "TokenType.RBracket",
//			")",    "TokenType.RParen",
//			";",    "TokenType.Semicolon",
//			"<<",   "TokenType.ShiftLeft",
//			"<<=",  "TokenType.ShiftLeftEqual",
//			">>",   "TokenType.ShiftRight",
//			">>=",  "TokenType.ShiftRightEqual",
//			"..",   "TokenType.Slice",
//			"*",    "TokenType.Star",
//			"?",    "TokenType.Ternary",
//			"~",    "TokenType.Tilde",
//			"--",   "TokenType.Decrement",
//			"!<>=", "TokenType.Unordered",
//			">>>",  "TokenType.UnsignedShiftRight",
//			">>>=", "TokenType.UnsignedShiftRightEqual",
//			"++",   "TokenType.Increment",
//			"...",  "TokenType.Vararg",
//			"^",    "TokenType.Xor",
//			"^=",   "TokenType.XorEquals",
//		));
//		case '0': .. case '9':
//			currentToken = lexNumber(inputString, endIndex);
//			break;
//		case '/':
//			++endIndex;
//			if (isEoF(inputString, endIndex))
//			{
//				currentToken.value = "/";
//				currentToken.type = TokenType.Div;
//				currentToken.lineNumber = lineNumber;
//				break;
//			}
//			currentToken.lineNumber = lineNumber;
//			switch (inputString[endIndex])
//			{
//			case '/':
//			case '+':
//			case '*':
//				if (iterationStyle == IterationStyle.CODE_ONLY)
//				{
//					lexComment(inputString, endIndex, lineNumber);
//					continue;
//				}
//				else
//				{
//					currentToken.value = lexComment(inputString, endIndex, lineNumber);
//					currentToken.type = TokenType.Comment;
//					break;
//				}
//			case '=':
//				currentToken.value = "/=";
//				currentToken.type = TokenType.DivEquals;
//				++endIndex;
//				break;
//			default:
//				currentToken.value = "/";
//				currentToken.type = TokenType.Div;
//				break;
//			}
//			break;
//		case 'r':
//			++endIndex;
//			if (isEoF(inputString, endIndex) || inputString[endIndex] != '"')
//				goto default;
//			currentToken.lineNumber = lineNumber;
//			currentToken.value = lexString(inputString, endIndex,
//				lineNumber, inputString[endIndex], false);
//			currentToken.type = TokenType.StringLiteral;
//			break;
//		case '`':
//			currentToken.lineNumber = lineNumber;
//			currentToken.value = lexString(inputString, endIndex, lineNumber,
//				inputString[endIndex], false);
//			currentToken.type = TokenType.StringLiteral;
//			break;
//		case 'x':
//			++endIndex;
//			if (isEoF(inputString, endIndex) || inputString[endIndex] != '"')
//				goto default;
//			else
//				goto case '"'; // BUG: this is incorrect! according to specification, hex data should be lexed differently than "normal" strings
//		case '\'':
//		case '"':
//			currentToken.lineNumber = lineNumber;
//			currentToken.value = lexString(inputString, endIndex, lineNumber,
//				inputString[endIndex]);
//			currentToken.type = TokenType.StringLiteral;
//			break;
//		case 'q':
//			currentToken.value = "q";
//			++endIndex;
//			if (!isEoF(inputString, endIndex))
//			{
//				switch (inputString[endIndex])
//				{
//					case '"':
//						currentToken.lineNumber = lineNumber;
//						currentToken.value ~= lexDelimitedString(inputString,
//							endIndex, lineNumber);
//						currentToken.type = TokenType.StringLiteral;
//						break outerSwitch;
//					case '{':
//						currentToken.lineNumber = lineNumber;
//						currentToken.value ~= lexTokenString(inputString,
//							endIndex, lineNumber);
//						currentToken.type = TokenType.StringLiteral;
//						break outerSwitch;
//					default:
//						break;
//				}
//			}
//			goto default;
//		case '@':
//			++endIndex;
//			goto default;
//		default:
//			while(!isEoF(inputString, endIndex) && !isSeparating(inputString[endIndex]))
//				++endIndex;
//			currentToken.value = inputString[startIndex .. endIndex];
//			currentToken.type = lookupTokenTypeOptimized(currentToken.value);
//			//currentToken.type = lookupTokenType(currentToken.value);
//			currentToken.lineNumber = lineNumber;
//			break;
//		}
//		//stderr.writeln(currentToken);
//		tokenAppender.put(currentToken);
//
//		// This should never happen.
//		if (endIndex <= prevIndex)
//		{
//			stderr.writeln("FAIL");
//			return [];
//		}
//	}
//	return tokenAppender.data;
//}
