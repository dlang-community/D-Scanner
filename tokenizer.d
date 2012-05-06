
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

import langutils;
import codegen;


/**
 * Increments endIndex until it indexes a non-whitespace character in
 * inputString.
 * Params:
 *     inputString = the source code to examine
 *     endIndex = an index into inputString
 *     lineNumber = the line number that corresponds to endIndex
 *     style = the code iteration style
 * Returns: The whitespace, or null if style was CODE_ONLY
 */
pure nothrow string lexWhitespace(S)(S inputString, ref size_t endIndex,
	ref uint lineNumber)
	if (isSomeString!S)
{
	immutable startIndex = endIndex;
	while (!isEoF(inputString, endIndex) && isWhite(inputString[endIndex]))
	{
		if (inputString[endIndex] == '\n')
			lineNumber++;
		++endIndex;
	}
	return inputString[startIndex .. endIndex];
}

/**
 * If inputString starts with #!, increments endIndex until it indexes the next line.
 * Params:
 *     inputString = the source code to examine
 *     endIndex = an index into inputString
 *     lineNumber = the line number that corresponds to endIndex
  * Returns: The script line, or null if this inputString doesn't start from script line
 */
pure nothrow string lexScriptLine(S)(ref S inputString, ref size_t endIndex,
	ref uint lineNumber) if (isSomeString!S)
{
	auto startIndex = endIndex; // in current implementation endIndex is 0, but that could change (e.g., if BOM is not stripped from inputString)
	string result = null;
	if(inputString.length > 1 && inputString[0..2] == "#!") // safety check
	{
		endIndex = 2; // skip #!
		while (!isEoF(inputString, endIndex) && inputString[endIndex] != '\n')
			++endIndex;

		result = inputString[startIndex..endIndex];
		++lineNumber;
	}
	return result;
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
pure nothrow string lexComment(S)(ref S inputString, ref size_t endIndex,
	ref uint lineNumber) if (isSomeString!S)
{
	if (isEoF(inputString, endIndex))
		return "";
	auto startIndex = endIndex - 1;
	switch(inputString[endIndex])
	{
	case '/':
		while (!isEoF(inputString, endIndex) && inputString[endIndex] != '\n')
		{
			if (inputString[endIndex] == '\n')
				++lineNumber;
			++endIndex;
		}
		break;
	case '*':
		while (!isEoF(inputString, endIndex)
			&& !inputString[endIndex..$].startsWith("*/"))
		{
			if (inputString[endIndex] == '\n')
				++lineNumber;
			++endIndex;
		}
		endIndex += 2;
		break;
	case '+':
		++endIndex;
		int depth = 1;
		while (depth > 0 && !isEoF(inputString, endIndex))
		{
			if (inputString[endIndex] == '\n')
				lineNumber++;
			else if (inputString[endIndex..$].startsWith("+/"))
				depth--;
			else if (inputString[endIndex..$].startsWith("/+"))
				depth++;
			++endIndex;
		}
		if (!isEoF(inputString, endIndex))
			++endIndex;
		break;
	default:
		break;
	}
	return inputString[startIndex..endIndex];
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
pure nothrow string lexString(S, C)(S inputString, ref size_t endIndex, ref uint lineNumber,
	C quote, bool canEscape = true) if (isSomeString!S && isSomeChar!C)
in
{
	assert (inputString[endIndex] == quote);
	assert (quote == '\'' || quote == '"' || quote == '`');
}
body
{
	if (inputString[endIndex] != quote)
		return "";
	auto startIndex = endIndex;
	++endIndex;
	bool escape = false;
	while (!isEoF(inputString, endIndex) && (inputString[endIndex] != quote || escape))
	{
		if (escape)
			escape = false;
		else
			escape = (canEscape && inputString[endIndex] == '\\');
		if (inputString[endIndex] == '\n')
			lineNumber++;
		++endIndex;
	}
	++endIndex;
	if (!isEoF(inputString, endIndex) && (inputString[endIndex] == 'w'
		|| inputString[endIndex] == 'd' || inputString[endIndex] == 'c'))
	{
		++endIndex;
	}
	auto e = endIndex > inputString.length ? inputString.length : endIndex;
	return inputString[startIndex .. e];
}

/**
 * Lexes the various crazy D string literals such as q"{}", q"WTF is this? WTF",
 * and q"<>".
 * Params:
 *     inputString = the source code to examine
 *     endIndex = an index into inputString at the opening quote
 *     lineNumber = the line number that corresponds to endIndex
 * Returns: a string literal, including its opening and closing quote characters
 */
string lexDelimitedString(S)(ref S inputString, ref size_t endIndex,
	ref uint lineNumber) if (isSomeString!S)
{
	auto startIndex = endIndex;
	++endIndex;
	assert(!isEoF(inputString, endIndex)); // todo: what should happen if this is EoF?
	string open = inputString[endIndex .. endIndex + 1];
	string close;
	bool nesting = false;
	switch (open[0])
	{
	case '[': close = "]"; ++endIndex; nesting = true; break;
	case '<': close = ">"; ++endIndex; nesting = true; break;
	case '{': close = "}"; ++endIndex; nesting = true; break;
	case '(': close = ")"; ++endIndex; nesting = true; break;
	default:
		while(!isEoF(inputString, endIndex) && !isWhite(inputString[endIndex]))
			endIndex++;
		close = open = inputString[startIndex + 1 .. endIndex];
		break;
	}
	int depth = 1;
	while (!isEoF(inputString, endIndex) && depth > 0)
	{
		if (inputString[endIndex] == '\n')
		{
			lineNumber++;
			endIndex++;
		}
		else if (inputString[endIndex..$].startsWith(open))
		{
			endIndex += open.length;
			if (!nesting && !isEoF(inputString, endIndex))
			{
				if (inputString[endIndex] == '"')
					++endIndex;
				break;
			}
			depth++;
		}
		else if (inputString[endIndex..$].startsWith(close))
		{
			endIndex += close.length;
			depth--;
			if (depth <= 0)
				break;
		}
		else
			++endIndex;
	}
	if (!isEoF(inputString, endIndex) && inputString[endIndex] == '"')
		++endIndex;
	// note: specification doesn't mention postfixes for delimited and token string literals, but DMD supports them
	if (!isEoF(inputString, endIndex) && (inputString[endIndex] == 'w'
		|| inputString[endIndex] == 'd' || inputString[endIndex] == 'c'))
	{
		++endIndex; // todo: add token annotation according to postfix
	}
	return inputString[startIndex .. endIndex];
}

string lexTokenString(S)(ref S inputString, ref size_t endIndex, ref uint lineNumber)
{
	/+auto r = byDToken(range, IterationStyle.EVERYTHING);
	string s = getBraceContent(r);
	range.popFrontN(s.length);
	return s;+/

	//// note: specification doesn't mention postfixes for delimited and token string literals, but DMD supports them
	//if (!isEoF(inputString, endIndex) && (inputString[endIndex] == 'w'
	//	|| inputString[endIndex] == 'd' || inputString[endIndex] == 'c'))
	//{
	//	++endIndex; // todo: add token annotation according to postfix
	//}
	return "";
}

pure nothrow Token lexNumber(S)(ref S inputString, ref size_t endIndex)
	if (isSomeString!S)
{
	Token token;
	size_t startIndex = endIndex;
	if (inputString[endIndex] == '0')
	{
		endIndex++;
		if (isEoF(inputString, endIndex))
		{
			token.type = TokenType.IntLiteral;
			token.value = inputString[startIndex .. endIndex];
			return token;
		}
		switch (inputString[endIndex])
		{
		case '0': .. case '9':
			// The current language spec doesn't cover octal literals, so this
			// is decimal.
			lexDecimal(inputString, startIndex, endIndex, token);
			return token;
		case 'b':
		case 'B':
			lexBinary(inputString, startIndex, ++endIndex, token);
			return token;
		case 'x':
		case 'X':
			lexHex(inputString, startIndex, ++endIndex, token);
			return token;
		default:
			token.type = TokenType.IntLiteral;
			token.value = inputString[startIndex .. endIndex];
			return token;
		}
	}
	else
	{
		lexDecimal(inputString, startIndex, endIndex, token);
		return token;
	}
}

pure nothrow void lexBinary(S)(ref S inputString, size_t startIndex,
	ref size_t endIndex, ref Token token) if (isSomeString!S)
{
	bool lexingSuffix = false;
	bool isLong = false;
	bool isUnsigned = false;
	token.type = TokenType.IntLiteral;
	binaryLoop: while (!isEoF(inputString, endIndex))
	{
		switch (inputString[endIndex])
		{
		case '0':
		case '1':
		case '_':
			if (lexingSuffix)
				break binaryLoop;
			++endIndex;
			break;
		case 'u':
		case 'U':
			if (isUnsigned)
				break;
			++endIndex;
			lexingSuffix = true;
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
			++endIndex;
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

	token.value = inputString[startIndex .. endIndex];
}

pure nothrow void lexDecimal(S)(ref S inputString, size_t startIndex,
	ref size_t endIndex, ref Token token) if (isSomeString!S)
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
	token.type = TokenType.IntLiteral;
	decimalLoop: while (!isEoF(inputString, endIndex))
	{
		switch (inputString[endIndex])
		{
		case '0': .. case '9':
		case '_':
			if (lexingSuffix)
				break decimalLoop;
			++endIndex;
			break;
		case 'e':
		case 'E':
			// For this to be a valid exponent, the next character must be a
			// decimal character or a sign
			if (foundE || isEoF(inputString, endIndex + 1))
				break decimalLoop;
			switch (inputString[endIndex + 1])
			{
			case '+':
			case '-':
				if (isEoF(inputString, endIndex + 2)
					|| inputString[endIndex + 2] < '0'
					|| inputString[endIndex + 2] > '9')
				{
					break decimalLoop;
				}
				break;
			case '0': .. case '9':
				break;
			default:
				break decimalLoop;
			}
			++endIndex;
			foundE = true;
			isDouble = true;
			token.type = TokenType.DoubleLiteral;
			break;
		case '+':
		case '-':
			if (foundPlusMinus || !foundE)
				break decimalLoop;
			foundPlusMinus = true;
			++endIndex;
			break;
		case '.':
			if (!isEoF(inputString, endIndex + 1) && inputString[endIndex + 1] == '.')
				break decimalLoop; // possibly slice expression
			if (foundDot)
				break decimalLoop; // two dots with other characters between them
			++endIndex;
			foundDot = true;
			token.type = TokenType.DoubleLiteral;
			isDouble = true;
			break;
		case 'u':
		case 'U':
			if (isUnsigned)
				break decimalLoop;
			++endIndex;
			lexingSuffix = true;
			if (isLong)
				token.type = TokenType.UnsignedLongLiteral;
			else
				token.type = TokenType.UnsignedIntLiteral;
			isUnsigned = true;
			break;
		case 'L':
			if (isLong)
				break decimalLoop;
			if (isReal)
				break decimalLoop;
			++endIndex;
			lexingSuffix = true;
			if (isDouble)
				token.type = TokenType.RealLiteral;
			else if (isUnsigned)
				token.type = TokenType.UnsignedLongLiteral;
			else
				token.type = TokenType.LongLiteral;
			isLong = true;
			break;
		case 'f':
		case 'F':
			lexingSuffix = true;
			if (isUnsigned || isLong)
				break decimalLoop;
			++endIndex;
			token.type = TokenType.FloatLiteral;
			break decimalLoop;
		case 'i':
			++endIndex;
			// Spec says that this is the last suffix, so all cases break the
			// loop.
			if (isDouble)
			{
				token.type = TokenType.Idouble;
				break decimalLoop;
			}
			else if (isFloat)
			{
				token.type = TokenType.Ifloat;
				break decimalLoop;
			}
			else if (isReal)
			{
				token.type = TokenType.Ireal;
				break decimalLoop;
			}
			else
			{
				// There is no imaginary int
				--endIndex;
				break decimalLoop;
			}
		default:
			break decimalLoop;
		}
	}

	token.value = inputString[startIndex .. endIndex];
}


unittest {
	Token t;
	size_t start, end;
	lexDecimal!string("55e-4", start, end, t);
	assert(t.value == "55e-4");
	assert(t.type == TokenType.DoubleLiteral);

	start = end = 0;
	lexDecimal!string("123.45f", start, end, t);
	assert(t.value == "123.45f");
	assert(t.type == TokenType.FloatLiteral);

	start = end = 0;
	lexDecimal!string("3e+f", start, end, t);
	assert(t.value == "3");
	assert(t.type == TokenType.IntLiteral);

	start = end = 0;
	lexDecimal!string("3e++f", start, end, t);
	assert(t.value == "3");
	assert(t.type == TokenType.IntLiteral);

	start = end = 0;
	lexDecimal!string("1234..1237", start, end, t);
	assert(t.value == "1234");
	assert(t.type == TokenType.IntLiteral);
}


nothrow void lexHex(S)(ref S inputString, ref size_t startIndex,
	ref size_t endIndex, ref Token token) if (isSomeString!S)
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
	token.type = TokenType.IntLiteral;
	hexLoop: while (!isEoF(inputString, endIndex))
	{
		switch (inputString[endIndex])
		{
		case '0': .. case '9':
		case 'a': .. case 'f':
		case 'A': .. case 'F':
		case '_':
			if (lexingSuffix)
				break hexLoop;
			++endIndex;
			break;
		case 'p':
		case 'P':
			if (foundE)
				break hexLoop;
			++endIndex;
			foundE = true;
			break;
		case '+':
		case '-':
			if (foundPlusMinus || !foundE)
				break hexLoop;
			foundPlusMinus = true;
			++endIndex;
			break;
		case '.':
			if (!isEoF(inputString, endIndex + 1) && inputString[endIndex + 1] == '.')
				break hexLoop; // possibly slice expression
			if (foundDot)
				break hexLoop; // two dots with other characters between them
			++endIndex;
			foundDot = true;
			token.type = TokenType.DoubleLiteral;
			isDouble = true;
			break;
		default:
			break hexLoop;
		}
	}

	token.value = inputString[startIndex .. endIndex];
}


/**
 * Returns: true if ch marks the ending of one token and the beginning of
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

Token[] tokenize(S)(S inputString, IterationStyle iterationStyle = IterationStyle.CODE_ONLY)
	if (isSomeString!S)
{
	auto tokenAppender = appender!(Token[])();

	// This is very likely a local maximum, but it does seem to take a few
	// milliseconds off of the run time
	tokenAppender.reserve(inputString.length / 4);

	size_t endIndex = 0;
	uint lineNumber = 1;

	if (inputString.length > 1 && inputString[0..2] == "#!")
	{
		Token currentToken;
		currentToken.lineNumber = lineNumber; // lineNumber is always 1
		currentToken.value = lexScriptLine(inputString, endIndex, lineNumber);
		currentToken.type = TokenType.ScriptLine;
	}

	while (!isEoF(inputString, endIndex))
	{
		size_t prevIndex = endIndex;
		Token currentToken;
		auto startIndex = endIndex;
		if (isWhite(inputString[endIndex]))
		{
			if (iterationStyle == IterationStyle.EVERYTHING)
			{
				currentToken.lineNumber = lineNumber;
				currentToken.value = lexWhitespace(inputString, endIndex,
					lineNumber);
				currentToken.type = TokenType.Whitespace;
				tokenAppender.put(currentToken);
			}
			else
				lexWhitespace(inputString, endIndex, lineNumber);
			continue;
		}
		currentToken.startIndex = endIndex;

		outerSwitch: switch(inputString[endIndex])
		{
		mixin(generateCaseTrie(
			"=",    "TokenType.Assign",
			"&",    "TokenType.BitAnd",
			"&=",   "TokenType.BitAndEquals",
			"|",    "TokenType.BitOr",
			"|=",   "TokenType.BitOrEquals",
			"~=",   "TokenType.CatEquals",
			":",    "TokenType.Colon",
			",",    "TokenType.Comma",
			"$",    "TokenType.Dollar",
			".",    "TokenType.Dot",
			"==",   "TokenType.Equals",
			"=>",   "TokenType.GoesTo",
			">",    "TokenType.Greater",
			">=",   "TokenType.GreaterEqual",
			"#",    "TokenType.Hash",
			"&&",   "TokenType.LogicAnd",
			"{",    "TokenType.LBrace",
			"[",    "TokenType.LBracket",
			"<",    "TokenType.Less",
			"<=",   "TokenType.LessEqual",
			"<>=",  "TokenType.LessEqualGreater",
			"<>",   "TokenType.LessOrGreater",
			"||",   "TokenType.LogicOr",
			"(",    "TokenType.LParen",
			"-",    "TokenType.Minus",
			"-=",   "TokenType.MinusEquals",
			"%",    "TokenType.Mod",
			"%=",   "TokenType.ModEquals",
			"*=",   "TokenType.MulEquals",
			"!",    "TokenType.Not",
			"!=",   "TokenType.NotEquals",
			"!>",   "TokenType.NotGreater",
			"!>=",  "TokenType.NotGreaterEqual",
			"!<",   "TokenType.NotLess",
			"!<=",  "TokenType.NotLessEqual",
			"!<>",  "TokenType.NotLessEqualGreater",
			"+",    "TokenType.Plus",
			"+=",   "TokenType.PlusEquals",
			"^^",   "TokenType.Pow",
			"^^=",  "TokenType.PowEquals",
			"}",    "TokenType.RBrace",
			"]",    "TokenType.RBracket",
			")",    "TokenType.RParen",
			";",    "TokenType.Semicolon",
			"<<",   "TokenType.ShiftLeft",
			"<<=",  "TokenType.ShiftLeftEqual",
			">>",   "TokenType.ShiftRight",
			">>=",  "TokenType.ShiftRightEqual",
			"..",   "TokenType.Slice",
			"*",    "TokenType.Star",
			"?",    "TokenType.Ternary",
			"~",    "TokenType.Tilde",
			"--",   "TokenType.Decrement",
			"!<>=", "TokenType.Unordered",
			">>>",  "TokenType.UnsignedShiftRight",
			">>>=", "TokenType.UnsignedShiftRightEqual",
			"++",   "TokenType.Increment",
			"...",  "TokenType.Vararg",
			"^",    "TokenType.Xor",
			"^=",   "TokenType.XorEquals",
		));
		case '0': .. case '9':
			currentToken = lexNumber(inputString, endIndex);
			break;
		case '/':
			++endIndex;
			if (isEoF(inputString, endIndex))
			{
				currentToken.value = "/";
				currentToken.type = TokenType.Div;
				currentToken.lineNumber = lineNumber;
				break;
			}
			currentToken.lineNumber = lineNumber;
			switch (inputString[endIndex])
			{
			case '/':
			case '+':
			case '*':
				if (iterationStyle == IterationStyle.CODE_ONLY)
				{
					lexComment(inputString, endIndex, lineNumber);
					continue;
				}
				else
				{
					currentToken.value = lexComment(inputString, endIndex, lineNumber);
					currentToken.type = TokenType.Comment;
					break;
				}
			case '=':
				currentToken.value = "/=";
				currentToken.type = TokenType.DivEquals;
				++endIndex;
				break;
			default:
				currentToken.value = "/";
				currentToken.type = TokenType.Div;
				break;
			}
			break;
		case 'r':
			++endIndex;
			if (isEoF(inputString, endIndex) || inputString[endIndex] != '"')
				goto default;
			currentToken.lineNumber = lineNumber;
			currentToken.value = lexString(inputString, endIndex,
				lineNumber, inputString[endIndex], false);
			currentToken.type = TokenType.StringLiteral;
			currentToken.annotations |= TokenAnnotation.WysiwygString;
			break;
		case '`':
			currentToken.lineNumber = lineNumber;
			currentToken.value = lexString(inputString, endIndex, lineNumber,
				inputString[endIndex], false);
			currentToken.type = TokenType.StringLiteral;
			currentToken.annotations |= TokenAnnotation.AlternateWysiwygString;
			break;
 		case 'x':
			++endIndex;
			if (isEoF(inputString, endIndex) || inputString[endIndex] != '"')
				goto default;
			currentToken.lineNumber = lineNumber;
			currentToken.value = lexString(inputString, endIndex, lineNumber,
				inputString[endIndex]); // todo: create lexHexString function
			currentToken.type = TokenType.StringLiteral;
			currentToken.annotations |= TokenAnnotation.HexString;
			break;
		case '\'':
			currentToken.lineNumber = lineNumber;
			currentToken.value = lexString(inputString, endIndex, lineNumber,
				inputString[endIndex]); // todo: create dedicated function for lexing character literals
			currentToken.type = TokenType.StringLiteral;
			currentToken.annotations |= TokenAnnotation.SomeCharacter;
			break;
		case '"':
			currentToken.lineNumber = lineNumber;
			currentToken.value = lexString(inputString, endIndex, lineNumber,
				inputString[endIndex]);
			currentToken.type = TokenType.StringLiteral;
			currentToken.annotations |= TokenAnnotation.DoubleQuotedString;
			break;
		case 'q':
			currentToken.value = "q";
			++endIndex;
			if (!isEoF(inputString, endIndex))
			{
				switch (inputString[endIndex])
				{
					case '"':
						currentToken.lineNumber = lineNumber;
						currentToken.value ~= lexDelimitedString(inputString,
							endIndex, lineNumber);
						currentToken.type = TokenType.StringLiteral;
						currentToken.annotations |= TokenAnnotation.DelimitedString;
						break outerSwitch;
					case '{':
						currentToken.lineNumber = lineNumber;
						currentToken.value ~= lexTokenString(inputString,
							endIndex, lineNumber);
						currentToken.type = TokenType.StringLiteral;
						currentToken.annotations |= TokenAnnotation.TokenString;
						break outerSwitch;
					default:
						break;
				}
			}
			goto default;
		case '@':
			++endIndex;
			goto default;
		default:
			while(!isEoF(inputString, endIndex) && !isSeparating(inputString[endIndex]))
				++endIndex;
			currentToken.value = inputString[startIndex .. endIndex];
			currentToken.type = lookupTokenType(currentToken.value);
			currentToken.lineNumber = lineNumber;
			break;
		}
		//stderr.writeln(currentToken);
		tokenAppender.put(currentToken);

		// This should never happen.
		if (endIndex <= prevIndex)
		{
			stderr.writeln("FAIL");
			return [];
		}
	}
	return tokenAppender.data;
}
