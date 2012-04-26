
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
	ref uint lineNumber, IterationStyle style = IterationStyle.CODE_ONLY) // I suggest to remove the last param
	if (isSomeString!S)
{
	immutable startIndex = endIndex;
	while (!isEoF(inputString, endIndex) && isWhite(inputString[endIndex]))
	{
		if (inputString[endIndex] == '\n')
			lineNumber++;
		++endIndex;
	}
	final switch (style)
	{
	case IterationStyle.EVERYTHING:
		return inputString[startIndex .. endIndex];
	case IterationStyle.CODE_ONLY:
		return null;
	}
}

/**
 * If inputString starts from #!, increments endIndex until it indexes the next line.
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
 * Bugs: Does not handle string suffixes
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
	return inputString[startIndex .. endIndex];
}

/**
 * Lexes the various crazy D string literals such as q{}, q"WTF is this? WTF",
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
	return inputString[startIndex .. endIndex];
}


string lexTokenString(S)(ref S inputString, ref size_t endIndex, ref uint lineNumber)
{
	/+auto r = byDToken(range, IterationStyle.EVERYTHING);
	string s = getBraceContent(r);
	range.popFrontN(s.length);
	return s;+/
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
			token.type = TokenType.intLiteral;
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
			token.type = TokenType.intLiteral;
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
	token.type = TokenType.intLiteral;
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
				token.type = TokenType.unsignedLongLiteral;
				break binaryLoop;
			}
			else
				token.type = TokenType.unsignedIntLiteral;
			isUnsigned = true;
			break;
		case 'L':
			if (isLong)
				break binaryLoop;
			++endIndex;
			lexingSuffix = true;
			if (isUnsigned)
			{
				token.type = TokenType.unsignedLongLiteral;
				break binaryLoop;
			}
			else
				token.type = TokenType.longLiteral;
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
	token.type = TokenType.intLiteral;
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
			if (foundE)
				break decimalLoop;
			++endIndex;
			foundE = true;
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
			token.type = TokenType.doubleLiteral;
			isDouble = true;
			break;
		case 'u':
		case 'U':
			if (isUnsigned)
				break decimalLoop;
			++endIndex;
			lexingSuffix = true;
			if (isLong)
				token.type = TokenType.unsignedLongLiteral;
			else
				token.type = TokenType.unsignedIntLiteral;
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
				token.type = TokenType.realLiteral;
			else if (isUnsigned)
				token.type = TokenType.unsignedLongLiteral;
			else
				token.type = TokenType.longLiteral;
			isLong = true;
			break;
		case 'f':
		case 'F':
			lexingSuffix = true;
			if (isUnsigned || isLong)
				break decimalLoop;
			++endIndex;
			token.type = TokenType.floatLiteral;
			break decimalLoop;
		default:
			break decimalLoop;
		}
	}

	// suggest to extract lexing integers into a separate function
	// please see unittest below

	token.value = inputString[startIndex .. endIndex];
}

unittest {
	dump!lexDecimal("55e-4"); // yeilds intLiteral, but should be float
	dump!lexDecimal("3e+f"); // floatLiteral, but should be considered invalid
	dump!lexDecimal("3e++f"); // intLiteral 3e+, but should be considered invalid
	// actually, there are lots of bugs. The point is that without decomposition of integer lexing from floating-point lexing
	// it is very hard to prove algorithm correctness
}

// Temporary function to illustrate some problems
// Executes T and dumps results to console
void dump(alias T)(string s) {
	size_t start;
	size_t end;
	Token tok;
	T!(string)(s, start, end, tok);
	// dump results
	writeln(tok.type);
	writeln(tok.value);
	writeln(start);
	writeln(end);
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
	token.type = TokenType.intLiteral;
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
			token.type = TokenType.doubleLiteral;
			isDouble = true;
			break;
		default:
			break hexLoop;
		}
	}

	token.value = inputString[startIndex .. endIndex];
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
		currentToken.type = TokenType.scriptLine;
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
					lineNumber, IterationStyle.EVERYTHING); // note: I suggest to remove the last parameter to simplify lexWhitespace
				currentToken.type = TokenType.whitespace;
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
			"=",    "TokenType.assign",
			"&",    "TokenType.bitAnd",
			"&=",   "TokenType.bitAndEquals",
			"|",    "TokenType.bitOr",
			"|=",   "TokenType.bitOrEquals",
			"~=",   "TokenType.catEquals",
			":",    "TokenType.colon",
			",",    "TokenType.comma",
			"$",    "TokenType.dollar",
			".",    "TokenType.dot",
			"==",   "TokenType.equals",
			"=>",   "TokenType.goesTo",
			">",    "TokenType.greater",
			">=",   "TokenType.greaterEqual",
			"#",    "TokenType.hash",
			"&&",   "TokenType.lAnd",
			"{",    "TokenType.lBrace",
			"[",    "TokenType.lBracket",
			"<",    "TokenType.less",
			"<=",   "TokenType.lessEqual",
			"<>=",  "TokenType.lessEqualGreater",
			"<>",   "TokenType.lessOrGreater",
			"||",   "TokenType.lOr",
			"(",    "TokenType.lParen",
			"-",    "TokenType.minus",
			"-=",   "TokenType.minusEquals",
			"%",    "TokenType.mod",
			"%=",   "TokenType.modEquals",
			"*=",   "TokenType.mulEquals",
			"!",    "TokenType.not",
			"!=",   "TokenType.notEquals",
			"!>",   "TokenType.notGreater",
			"!>=",  "TokenType.notGreaterEqual",
			"!<",   "TokenType.notLess",
			"!<=",  "TokenType.notLessEqual",
			"!<>",  "TokenType.notLessEqualGreater",
			"+",    "TokenType.plus",
			"+=",   "TokenType.plusEquals",
			"^^",   "TokenType.pow",
			"^^=",  "TokenType.powEquals",
			"}",    "TokenType.rBrace",
			"]",    "TokenType.rBracket",
			")",    "TokenType.rParen",
			";",    "TokenType.semicolon",
			"<<",   "TokenType.shiftLeft",
			"<<=",  "TokenType.shiftLeftEqual",
			">>",   "TokenType.shiftRight",
			">>=",  "TokenType.shiftRightEqual",
			"..",   "TokenType.slice",
			"*",    "TokenType.star",
			"?",    "TokenType.ternary",
			"~",    "TokenType.tilde",
			"--",   "TokenType.uMinus",
			"!<>=", "TokenType.unordered",
			">>>",  "TokenType.unsignedShiftRight",
			">>>=", "TokenType.unsignedShiftRightEqual",
			"++",   "TokenType.uPlus",
			"...",  "TokenType.vararg",
			"^",    "TokenType.xor",
			"^=",   "TokenType.xorEquals",
		));
		case '0': .. case '9':
			currentToken = lexNumber(inputString, endIndex);
			break;
		case '/':
			++endIndex;
			if (isEoF(inputString, endIndex))
			{
				currentToken.value = "/";
				currentToken.type = TokenType.div;
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
					currentToken.type = TokenType.comment;
					break;
				}
			case '=':
				currentToken.value = "/=";
				currentToken.type = TokenType.divEquals;
				++endIndex;
				break;
			default:
				currentToken.value = "/";
				currentToken.type = TokenType.div;
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
			currentToken.type = TokenType.stringLiteral;
			break;
		case '`':
			currentToken.lineNumber = lineNumber;
			currentToken.value = lexString(inputString, endIndex, lineNumber,
				inputString[endIndex], false);
			currentToken.type = TokenType.stringLiteral;
			break;
		case 'x':
			++endIndex;
			if (isEoF(inputString, endIndex) || inputString[endIndex] != '"')
				goto default;
			else
				goto case '"'; // BUG: this is incorrect! according to specification, hex data should be lexed differently than "normal" strings
		case '\'':
		case '"':
			currentToken.lineNumber = lineNumber;
			currentToken.value = lexString(inputString, endIndex, lineNumber,
				inputString[endIndex]);
			currentToken.type = TokenType.stringLiteral;
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
						currentToken.type = TokenType.stringLiteral;
						break outerSwitch;
					case '{':
						currentToken.lineNumber = lineNumber;
						currentToken.value ~= lexTokenString(inputString,
							endIndex, lineNumber);
						currentToken.type = TokenType.stringLiteral;
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
