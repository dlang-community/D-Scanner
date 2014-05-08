module std.d.lexer;

import std.typecons;
import std.typetuple;
import std.array;
import std.algorithm;
import std.range;
import std.lexer;
public import std.lexer : StringCache;

private enum operators = [
	",", ".", "..", "...", "/", "/=", "!", "!<", "!<=", "!<>", "!<>=", "!=",
	"!>", "!>=", "$", "%", "%=", "&", "&&", "&=", "(", ")", "*", "*=", "+", "++",
	"+=", "-", "--", "-=", ":", ";", "<", "<<", "<<=", "<=", "<>", "<>=", "=",
	"==", "=>", ">", ">=", ">>", ">>=", ">>>", ">>>=", "?", "@", "[", "]", "^",
	"^=", "^^", "^^=", "{", "|", "|=", "||", "}", "~", "~="
];

private enum keywords = [
	"abstract", "alias", "align", "asm", "assert", "auto", "body", "bool",
	"break", "byte", "case", "cast", "catch", "cdouble", "cent", "cfloat",
	"char", "class", "const", "continue", "creal", "dchar", "debug", "default",
	"delegate", "delete", "deprecated", "do", "double", "else", "enum",
	"export", "extern", "false", "final", "finally", "float", "for", "foreach",
	"foreach_reverse", "function", "goto", "idouble", "if", "ifloat",
	"immutable", "import", "in", "inout", "int", "interface", "invariant",
	"ireal", "is", "lazy", "long", "macro", "mixin", "module", "new", "nothrow",
	"null", "out", "override", "package", "pragma", "private", "protected",
	"public", "pure", "real", "ref", "return", "scope", "shared", "short",
	"static", "struct", "super", "switch", "synchronized", "template", "this",
	"throw", "true", "try", "typedef", "typeid", "typeof", "ubyte", "ucent",
	"uint", "ulong", "union", "unittest", "ushort", "version", "void",
	"volatile", "wchar", "while", "with", "__DATE__", "__EOF__", "__FILE__",
	"__FUNCTION__", "__gshared", "__LINE__", "__MODULE__", "__parameters",
	"__PRETTY_FUNCTION__", "__TIME__", "__TIMESTAMP__", "__traits", "__vector",
	"__VENDOR__", "__VERSION__"
];

private enum dynamicTokens = [
	"specialTokenSequence", "comment", "identifier", "scriptLine",
	"whitespace", "doubleLiteral", "floatLiteral", "idoubleLiteral",
	"ifloatLiteral", "intLiteral", "longLiteral", "realLiteral",
	"irealLiteral", "uintLiteral", "ulongLiteral", "characterLiteral",
	"dstringLiteral", "stringLiteral", "wstringLiteral"
];

private enum pseudoTokenHandlers = [
	"\"", "lexStringLiteral",
	"`", "lexWysiwygString",
	"//", "lexSlashSlashComment",
	"/*", "lexSlashStarComment",
	"/+", "lexSlashPlusComment",
	".", "lexDot",
	"'", "lexCharacterLiteral",
	"0", "lexNumber",
	"1", "lexDecimal",
	"2", "lexDecimal",
	"3", "lexDecimal",
	"4", "lexDecimal",
	"5", "lexDecimal",
	"6", "lexDecimal",
	"7", "lexDecimal",
	"8", "lexDecimal",
	"9", "lexDecimal",
	"q\"", "lexDelimitedString",
	"q{", "lexTokenString",
	"r\"", "lexWysiwygString",
	"x\"", "lexHexString",
	" ", "lexWhitespace",
	"\t", "lexWhitespace",
	"\r", "lexWhitespace",
	"\n", "lexWhitespace",
	"\u2028", "lexLongNewline",
	"\u2029", "lexLongNewline",
	"#!", "lexScriptLine",
	"#line", "lexSpecialTokenSequence"
];

public alias IdType = TokenIdType!(operators, dynamicTokens, keywords);
public alias str = tokenStringRepresentation!(IdType, operators, dynamicTokens, keywords);
public template tok(string token)
{
  alias tok = TokenId!(IdType, operators, dynamicTokens, keywords, token);
}
private enum extraFields = q{
	string comment;

	int opCmp(size_t i) const pure nothrow @safe {
		if (index < i) return -1;
		if (index > i) return 1;
		return 0;
	}
};
public alias Token = std.lexer.TokenStructure!(IdType, extraFields);

/**
 * Configure string lexing behavior
 */
public enum StringBehavior : ubyte
{
	/// Do not include quote characters, process escape sequences
	compiler = 0b0000_0000,
	/// Opening quotes, closing quotes, and string suffixes are included in the
	/// string token
	includeQuoteChars = 0b0000_0001,
	/// String escape sequences are not replaced
	notEscaped = 0b0000_0010,
	/// Not modified at all. Useful for formatters or highlighters
	source = includeQuoteChars | notEscaped
}

/**
 * Configure whitespace handling behavior
 */
public enum WhitespaceBehavior : ubyte
{
	/// Whitespace is skipped
	skip,
	/// Whitespace is treated as a token
	include
}

/**
 * Configure special token handling behavior
 */
public enum SpecialTokenBehavior : ubyte
{
	/// Special tokens are skipped
	skip,
	/// Special tokens are treated as a token
	include
}

/**
 * Configure comment handling behavior
 */
public enum CommentBehavior : ubyte
{
	/// Comments are attached to the non-whitespace token that follows them
	attach,
	/// Comments are tokens, and can be returned by calls to the token range's front()
	include
}

public struct LexerConfig
{
	string fileName;
	StringBehavior stringBehavior;
	WhitespaceBehavior whitespaceBehavior;
	CommentBehavior commentBehavior;
	SpecialTokenBehavior specialTokenBehavior;
}

public bool isBasicType(IdType type) nothrow pure @safe
{
	switch (type)
	{
	case tok!"int":
	case tok!"uint":
	case tok!"double":
	case tok!"idouble":
	case tok!"float":
	case tok!"ifloat":
	case tok!"short":
	case tok!"ushort":
	case tok!"long":
	case tok!"ulong":
	case tok!"char":
	case tok!"wchar":
	case tok!"dchar":
	case tok!"bool":
	case tok!"void":
	case tok!"cent":
	case tok!"ucent":
	case tok!"real":
	case tok!"ireal":
	case tok!"byte":
	case tok!"ubyte":
	case tok!"cdouble":
	case tok!"cfloat":
	case tok!"creal":
		return true;
	default:
		return false;
	}
}

public bool isNumberLiteral(IdType type) nothrow pure @safe
{
	switch (type)
	{
	case tok!"doubleLiteral":
	case tok!"floatLiteral":
	case tok!"idoubleLiteral":
	case tok!"ifloatLiteral":
	case tok!"intLiteral":
	case tok!"longLiteral":
	case tok!"realLiteral":
	case tok!"irealLiteral":
	case tok!"uintLiteral":
	case tok!"ulongLiteral":
		return true;
	default:
		return false;
	}
}

public bool isOperator(IdType type) nothrow pure @safe
{
	switch (type)
	{
	case tok!",":
	case tok!".":
	case tok!"..":
	case tok!"...":
	case tok!"/":
	case tok!"/=":
	case tok!"!":
	case tok!"!<":
	case tok!"!<=":
	case tok!"!<>":
	case tok!"!<>=":
	case tok!"!=":
	case tok!"!>":
	case tok!"!>=":
	case tok!"$":
	case tok!"%":
	case tok!"%=":
	case tok!"&":
	case tok!"&&":
	case tok!"&=":
	case tok!"(":
	case tok!")":
	case tok!"*":
	case tok!"*=":
	case tok!"+":
	case tok!"++":
	case tok!"+=":
	case tok!"-":
	case tok!"--":
	case tok!"-=":
	case tok!":":
	case tok!";":
	case tok!"<":
	case tok!"<<":
	case tok!"<<=":
	case tok!"<=":
	case tok!"<>":
	case tok!"<>=":
	case tok!"=":
	case tok!"==":
	case tok!"=>":
	case tok!">":
	case tok!">=":
	case tok!">>":
	case tok!">>=":
	case tok!">>>":
	case tok!">>>=":
	case tok!"?":
	case tok!"@":
	case tok!"[":
	case tok!"]":
	case tok!"^":
	case tok!"^=":
	case tok!"^^":
	case tok!"^^=":
	case tok!"{":
	case tok!"|":
	case tok!"|=":
	case tok!"||":
	case tok!"}":
	case tok!"~":
	case tok!"~=":
		return true;
	default:
		return false;
	}
}

public bool isKeyword(IdType type) pure nothrow @safe
{
	switch (type)
	{
	case tok!"abstract":
	case tok!"alias":
	case tok!"align":
	case tok!"asm":
	case tok!"assert":
	case tok!"auto":
	case tok!"body":
	case tok!"break":
	case tok!"case":
	case tok!"cast":
	case tok!"catch":
	case tok!"class":
	case tok!"const":
	case tok!"continue":
	case tok!"debug":
	case tok!"default":
	case tok!"delegate":
	case tok!"delete":
	case tok!"deprecated":
	case tok!"do":
	case tok!"else":
	case tok!"enum":
	case tok!"export":
	case tok!"extern":
	case tok!"false":
	case tok!"final":
	case tok!"finally":
	case tok!"for":
	case tok!"foreach":
	case tok!"foreach_reverse":
	case tok!"function":
	case tok!"goto":
	case tok!"if":
	case tok!"immutable":
	case tok!"import":
	case tok!"in":
	case tok!"inout":
	case tok!"interface":
	case tok!"invariant":
	case tok!"is":
	case tok!"lazy":
	case tok!"macro":
	case tok!"mixin":
	case tok!"module":
	case tok!"new":
	case tok!"nothrow":
	case tok!"null":
	case tok!"out":
	case tok!"override":
	case tok!"package":
	case tok!"pragma":
	case tok!"private":
	case tok!"protected":
	case tok!"public":
	case tok!"pure":
	case tok!"ref":
	case tok!"return":
	case tok!"scope":
	case tok!"shared":
	case tok!"static":
	case tok!"struct":
	case tok!"super":
	case tok!"switch":
	case tok!"synchronized":
	case tok!"template":
	case tok!"this":
	case tok!"throw":
	case tok!"true":
	case tok!"try":
	case tok!"typedef":
	case tok!"typeid":
	case tok!"typeof":
	case tok!"union":
	case tok!"unittest":
	case tok!"version":
	case tok!"volatile":
	case tok!"while":
	case tok!"with":
	case tok!"__DATE__":
	case tok!"__EOF__":
	case tok!"__FILE__":
	case tok!"__FUNCTION__":
	case tok!"__gshared":
	case tok!"__LINE__":
	case tok!"__MODULE__":
	case tok!"__parameters":
	case tok!"__PRETTY_FUNCTION__":
	case tok!"__TIME__":
	case tok!"__TIMESTAMP__":
	case tok!"__traits":
	case tok!"__vector":
	case tok!"__VENDOR__":
	case tok!"__VERSION__":
		return true;
	default:
		return false;
	}
}

public bool isStringLiteral(IdType type) pure nothrow @safe
{
	switch (type)
	{
	case tok!"dstringLiteral":
	case tok!"stringLiteral":
	case tok!"wstringLiteral":
		return true;
	default:
		return false;
	}
}

public bool isProtection(IdType type) pure nothrow @safe
{
	switch (type)
	{
	case tok!"export":
	case tok!"package":
	case tok!"private":
	case tok!"public":
	case tok!"protected":
		return true;
	default:
		return false;
	}
}

public struct DLexer
{
	import core.vararg;

	mixin Lexer!(Token, lexIdentifier, isSeparating, operators, dynamicTokens,
		keywords, pseudoTokenHandlers);

	this(ubyte[] range, const LexerConfig config, shared(StringCache)* cache)
	{
		this.range = LexerRange(range);
		this.config = config;
		this.cache = cache;
		popFront();
	}

	private static bool isDocComment(string comment) pure nothrow @safe
	{
		return comment.length >= 3 && (comment[0 .. 3] == "///"
			|| comment[0 .. 3] == "/++" || comment[0 .. 3] == "/**");
	}

	public void popFront() pure
	{
		_popFront();
		string comment = null;
		switch (front.type)
		{
			case tok!"comment":
				if (config.commentBehavior == CommentBehavior.attach)
				{
					import std.string;
					if (isDocComment(front.text))
					{
						comment = comment is null
							? front.text
							: format("%s\n%s", comment, front.text);
					}
					do _popFront(); while (front == tok!"comment");
					if (front == tok!"whitespace") goto case tok!"whitespace";
					if (front == tok!"specialTokenSequence") goto case tok!"specialTokenSequence";
				}
				break;
			case tok!"whitespace":
				if (config.whitespaceBehavior == WhitespaceBehavior.skip)
				{
					do _popFront(); while (front == tok!"whitespace");
					if (front == tok!"comment") goto case tok!"comment";
					if (front == tok!"specialTokenSequence") goto case tok!"specialTokenSequence";
				}
				break;
			case tok!"specialTokenSequence":
				if (config.specialTokenBehavior == SpecialTokenBehavior.skip)
				{
					do _popFront(); while (front == tok!"specialTokenSequence");
					if (front == tok!"comment") goto case tok!"comment";
					if (front == tok!"whitespace") goto case tok!"whitespace";
				}
				break;
			default:
				break;
		}
		_front.comment = comment;
	}


	bool isWhitespace() pure /*const*/ nothrow
	{
		switch (range.front)
		{
		case ' ':
		case '\r':
		case '\n':
		case '\t':
			return true;
		case 0xe2:
			auto peek = range.peek(2);
			return peek.length == 2
				&& peek[0] == 0x80
				&& (peek[1] == 0xa8 || peek[1] == 0xa9);
		default:
			return false;
		}
	}

	void popFrontWhitespaceAware() pure nothrow
	{
		switch (range.front)
		{
		case '\r':
			range.popFront();
			if (!range.empty && range.front == '\n')
			{
				range.popFront();
				range.incrementLine();
			}
			else
				range.incrementLine();
			return;
		case '\n':
			range.popFront();
			range.incrementLine();
			return;
		case 0xe2:
			auto lookahead = range.peek(3);
			if (lookahead.length == 3 && lookahead[1] == 0x80
				&& (lookahead[2] == 0xa8 || lookahead[2] == 0xa9))
			{
				range.popFront();
				range.popFront();
				range.popFront();
				range.incrementLine();
				return;
			}
			else
			{
				range.popFront();
				return;
			}
		default:
			range.popFront();
			return;
		}
	}

	Token lexWhitespace() pure nothrow
	{
		mixin (tokenStart);
		loop: do
		{
			switch (range.front)
			{
			case '\r':
				range.popFront();
				if (!range.empty && range.front == '\n')
					range.popFront();
				range.incrementLine();
				break;
			case '\n':
				range.popFront();
				range.incrementLine();
				break;
			case ' ':
			case '\t':
				range.popFront();
				break;
			case 0xe2:
				auto lookahead = range.peek(3);
				if (lookahead.length != 3)
					break loop;
				if (lookahead[1] != 0x80)
					break loop;
				if (lookahead[2] == 0xa8 || lookahead[2] == 0xa9)
				{
					range.popFront();
					range.popFront();
					range.popFront();
					range.incrementLine();
					break;
				}
				break loop;
			default:
				break loop;
			}
		} while (!range.empty);
		string text = config.whitespaceBehavior == WhitespaceBehavior.skip
			? null : cache.intern(range.slice(mark));
		return Token(tok!"whitespace", text, line, column, index);
	}

	Token lexNumber() pure nothrow
	{
		mixin (tokenStart);
		if (range.front == '0' && range.canPeek(1))
		{
			auto ahead = range.peek(1)[1];
			switch (ahead)
			{
			case 'x':
			case 'X':
				range.popFront();
				range.popFront();
				return lexHex(mark, line, column, index);
			case 'b':
			case 'B':
				range.popFront();
				range.popFront();
				return lexBinary(mark, line, column, index);
			default:
				return lexDecimal(mark, line, column, index);
			}
		}
		else
			return lexDecimal(mark, line, column, index);
	}

	Token lexHex() pure nothrow
	{
		mixin (tokenStart);
		return lexHex(mark, line, column, index);
	}

	Token lexHex(size_t mark, size_t line, size_t column, size_t index) pure nothrow
	{
		IdType type = tok!"intLiteral";
		bool foundDot;
		hexLoop: while (!range.empty)
		{
			switch (range.front)
			{
			case 'a': .. case 'f':
			case 'A': .. case 'F':
			case '0': .. case '9':
			case '_':
				range.popFront();
				break;
			case 'u':
			case 'U':
				lexIntSuffix(type);
				break hexLoop;
			case 'i':
				if (foundDot)
					lexFloatSuffix(type);
				break hexLoop;
			case 'L':
				if (foundDot)
					lexFloatSuffix(type);
				else
					lexIntSuffix(type);
                break hexLoop;
			case 'p':
			case 'P':
				lexExponent(type);
				break hexLoop;
			case '.':
				if (foundDot || !range.canPeek(1) || range.peekAt(1) == '.')
					break hexLoop;
				else
				{
					// The following bit of silliness tries to tell the
					// difference between "int dot identifier" and
					// "double identifier".
					if (range.canPeek(1))
					{
						switch (range.peekAt(1))
						{
						case '0': .. case '9':
						case 'A': .. case 'F':
						case 'a': .. case 'f':
							goto doubleLiteral;
						default:
							break hexLoop;
						}
					}
					else
					{
					doubleLiteral:
						range.popFront();
						foundDot = true;
						type = tok!"doubleLiteral";
					}
				}
				break;
			default:
				break hexLoop;
			}
		}
		return Token(type, cache.intern(range.slice(mark)), line, column,
			index);
	}

	Token lexBinary() pure nothrow
	{
		mixin (tokenStart);
		return lexBinary(mark, line, column, index);
	}

	Token lexBinary(size_t mark, size_t line, size_t column, size_t index) pure nothrow
	{
		IdType type = tok!"intLiteral";
		binaryLoop: while (!range.empty)
		{
			switch (range.front)
			{
			case '0':
			case '1':
			case '_':
				range.popFront();
				break;
			case 'u':
			case 'U':
			case 'L':
				lexIntSuffix(type);
				break binaryLoop;
			default:
				break binaryLoop;
			}
		}
		return Token(type, cache.intern(range.slice(mark)), line, column,
			index);
	}

	Token lexDecimal() pure nothrow
	{
		mixin (tokenStart);
		return lexDecimal(mark, line, column, index);
	}

	Token lexDecimal(size_t mark, size_t line, size_t column, size_t index) pure nothrow
	{
		bool foundDot = range.front == '.';
		IdType type = tok!"intLiteral";
		if (foundDot)
		{
			range.popFront();
			type = tok!"doubleLiteral";
		}

		decimalLoop: while (!range.empty)
		{
			switch (range.front)
			{
			case '0': .. case '9':
			case '_':
				range.popFront();
				break;
			case 'u':
			case 'U':
				if (!foundDot)
					lexIntSuffix(type);
				break decimalLoop;
			case 'i':
				lexFloatSuffix(type);
				break decimalLoop;
			case 'L':
				if (foundDot)
					lexFloatSuffix(type);
				else
					lexIntSuffix(type);
				break decimalLoop;
			case 'f':
			case 'F':
				lexFloatSuffix(type);
				break decimalLoop;
			case 'e':
			case 'E':
				lexExponent(type);
				break decimalLoop;
			case '.':
				if (foundDot || !range.canPeek(1) || range.peekAt(1) == '.')
					break decimalLoop;
				else
				{
					// The following bit of silliness tries to tell the
					// difference between "int dot identifier" and
					// "double identifier".
					if (range.canPeek(1))
					{
						auto ch = range.peekAt(1);
						if (ch <= 0x2f
							|| (ch >= '0' && ch <= '9')
							|| (ch >= ':' && ch <= '@')
							|| (ch >= '[' && ch <= '^')
							|| (ch >= '{' && ch <= '~')
							|| ch == '`' || ch == '_')
						{
							goto doubleLiteral;
						}
						else
							break decimalLoop;
					}
					else
					{
					doubleLiteral:
						range.popFront();
						foundDot = true;
						type = tok!"doubleLiteral";
					}
				}
				break;
			default:
				break decimalLoop;
			}
		}
		return Token(type, cache.intern(range.slice(mark)), line, column,
			index);
	}

	void lexIntSuffix(ref IdType type) pure nothrow @safe
	{
		bool secondPass;
		if (range.front == 'u' || range.front == 'U')
		{
	U:
			if (type == tok!"intLiteral")
				type = tok!"uintLiteral";
			else
				type = tok!"ulongLiteral";
			range.popFront();
			if (secondPass)
				return;
			if (range.front == 'L' || range.front == 'l')
				goto L;
			return;
		}
		if (range.front == 'L' || range.front == 'l')
		{
	L:
			if (type == tok!"uintLiteral")
				type = tok!"ulongLiteral";
			else
				type = tok!"longLiteral";
			range.popFront();
			if (range.front == 'U' || range.front == 'u')
			{
				secondPass = true;
				goto U;
			}
			return;
		}
	}

	void lexFloatSuffix(ref IdType type) pure nothrow @safe
	{
		switch (range.front)
		{
		case 'L':
			range.popFront();
			type = tok!"doubleLiteral";
			break;
		case 'f':
		case 'F':
			range.popFront();
			type = tok!"floatLiteral";
			break;
		default:
			break;
		}
		if (!range.empty && range.front == 'i')
		{
            warning("Complex number literals are deprecated");
			range.popFront();
			if (type == tok!"floatLiteral")
				type = tok!"ifloatLiteral";
			else
				type = tok!"idoubleLiteral";
		}
	}

	void lexExponent(ref IdType type) pure nothrow @safe
	{
		range.popFront();
		bool foundSign = false;
		bool foundDigit = false;
		while (!range.empty)
		{
			switch (range.front)
			{
			case '-':
			case '+':
				if (foundSign)
				{
					if (!foundDigit)
					error("Expected an exponent");
					return;
				}
				foundSign = true;
				range.popFront();
				break;
			case '0': .. case '9':
			case '_':
				foundDigit = true;
				range.popFront();
				break;
			case 'L':
			case 'f':
			case 'F':
			case 'i':
				lexFloatSuffix(type);
				return;
			default:
				if (!foundDigit)
					error("Expected an exponent");
				return;
			}
		}
	}

	Token lexScriptLine() pure
	{
		mixin (tokenStart);
		while (!range.empty && !isNewline)
			range.popFront();
		return Token(tok!"scriptLine", cache.intern(range.slice(mark)),
			line, column, index);
	}

	Token lexSpecialTokenSequence() pure
	{
		mixin (tokenStart);
		while (!range.empty && !isNewline)
			range.popFront();
		return Token(tok!"specialTokenSequence", cache.intern(range.slice(mark)),
			line, column, index);
	}

	Token lexSlashStarComment() pure
	{
		mixin (tokenStart);
		IdType type = tok!"comment";
		range.popFront();
		range.popFront();
		while (!range.empty)
		{
			if (range.front == '*')
			{
				range.popFront();
				if (!range.empty && range.front == '/')
				{
					range.popFront();
					break;
				}
			}
			else
				popFrontWhitespaceAware();
		}
		return Token(type, cache.intern(range.slice(mark)), line, column,
			index);
	}

	Token lexSlashSlashComment() pure nothrow
	{
		mixin (tokenStart);
		IdType type = tok!"comment";
		range.popFront();
		range.popFront();
		while (!range.empty)
		{
			if (range.front == '\r' || range.front == '\n')
				break;
			range.popFront();
		}
		return Token(type, cache.intern(range.slice(mark)), line, column,
			index);
	}

	Token lexSlashPlusComment() pure nothrow
	{
		mixin (tokenStart);
		IdType type = tok!"comment";
		range.popFront();
		range.popFront();
		int depth = 1;
		while (depth > 0 && !range.empty)
		{
			if (range.front == '+')
			{
				range.popFront();
				if (!range.empty && range.front == '/')
				{
					range.popFront();
					depth--;
				}
			}
			else if (range.front == '/')
			{
				range.popFront();
				if (!range.empty && range.front == '+')
				{
					range.popFront();
					depth++;
				}
			}
			else
				popFrontWhitespaceAware();
		}
		return Token(type, cache.intern(range.slice(mark)), line, column,
			index);
	}

	Token lexStringLiteral() pure nothrow
	{
		mixin (tokenStart);
		range.popFront();
		while (true)
		{
			if (range.empty)
			{
				error("Error: unterminated string literal");
				return Token();
			}
			else if (range.front == '"')
			{
				range.popFront();
				break;
			}
			else if (range.front == '\\')
			{
				lexEscapeSequence();
			}
			else
				popFrontWhitespaceAware();
		}
		IdType type = tok!"stringLiteral";
		lexStringSuffix(type);
		return Token(type, cache.intern(range.slice(mark)), line, column,
			index);
	}

	Token lexWysiwygString() pure nothrow
	{
		mixin (tokenStart);
		IdType type = tok!"stringLiteral";
		bool backtick = range.front == '`';
		if (backtick)
		{
			range.popFront();
			while (true)
			{
				if (range.empty)
				{
					error("Error: unterminated string literal");
					return Token(tok!"");
				}
				else if (range.front == '`')
				{
					range.popFront();
					break;
				}
				else
					popFrontWhitespaceAware();
			}
		}
		else
		{
			range.popFront();
			if (range.empty)
			{
				error("Error: unterminated string literal");
				return Token(tok!"");
			}
			range.popFront();
			while (true)
			{
				if (range.empty)
				{
					error("Error: unterminated string literal");
					return Token(tok!"");
				}
				else if (range.front == '"')
				{
					range.popFront();
					break;
				}
				else
					popFrontWhitespaceAware();
			}
		}
		lexStringSuffix(type);
		return Token(type, cache.intern(range.slice(mark)), line, column,
			index);
	}

	void lexStringSuffix(ref IdType type) pure nothrow
	{
		if (range.empty)
			type = tok!"stringLiteral";
		else
		{
			switch (range.front)
			{
			case 'w': range.popFront(); type = tok!"wstringLiteral"; break;
			case 'd': range.popFront(); type = tok!"dstringLiteral"; break;
			case 'c': range.popFront(); type = tok!"stringLiteral"; break;
			default: type = tok!"stringLiteral"; break;
			}
		}
	}

	Token lexDelimitedString() pure nothrow
	{
		import std.traits;
		mixin (tokenStart);
		range.popFront();
		range.popFront();
		ubyte open;
		ubyte close;
		switch (range.front)
		{
		case '<':
			open = '<';
			close = '>';
			range.popFront();
			return lexNormalDelimitedString(mark, line, column, index, open, close);
		case '{':
			open = '{';
			close = '}';
			range.popFront();
			return lexNormalDelimitedString(mark, line, column, index, open, close);
		case '[':
			open = '[';
			close = ']';
			range.popFront();
			return lexNormalDelimitedString(mark, line, column, index, open, close);
		case '(':
			open = '(';
			close = ')';
			range.popFront();
			return lexNormalDelimitedString(mark, line, column, index, open, close);
		default:
			return lexHeredocString(mark, line, column, index);
		}
	}

	Token lexNormalDelimitedString(size_t mark, size_t line, size_t column,
		size_t index, ubyte open, ubyte close)
		pure nothrow
	{
		int depth = 1;
		while (!range.empty && depth > 0)
		{
			if (range.front == open)
			{
				depth++;
				range.popFront();
			}
			else if (range.front == close)
			{
				depth--;
				range.popFront();
				if (depth <= 0)
				{
					if (range.front == '"')
						range.popFront();
					else
					{
						error("Error: \" expected to end delimited string literal");
						return Token(tok!"");
					}
				}
			}
			else
				popFrontWhitespaceAware();
		}
		IdType type = tok!"stringLiteral";
		lexStringSuffix(type);
		return Token(type, cache.intern(range.slice(mark)), line, column, index);
	}

	Token lexHeredocString(size_t mark, size_t line, size_t column, size_t index)
		pure nothrow
	{
		import std.regex;
		Token ident = lexIdentifier();
		if (isNewline())
			popFrontWhitespaceAware();
		else
			error("Newline expected");
		while (!range.empty)
		{
			if (isNewline())
			{
				popFrontWhitespaceAware();
				if (!range.canPeek(ident.text.length))
				{
					error(ident.text ~ " expected");
					break;
				}
				if (range.peek(ident.text.length - 1) == ident.text)
				{
					range.popFrontN(ident.text.length);
					break;
				}
			}
			else
				range.popFront();
		}
		if (!range.empty() && range.front == '"')
			range.popFront();
		else
			error(`" expected`);
		IdType type = tok!"stringLiteral";
		lexStringSuffix(type);
		return Token(type, cache.intern(range.slice(mark)), line, column, index);
	}

	Token lexTokenString() pure
	{
		mixin (tokenStart);
		assert(range.front == 'q');
		range.popFront();
		assert(range.front == '{');
		range.popFront();
		auto app = appender!string();
		app.put("q{");
		int depth = 1;

		LexerConfig c = config;
		scope(exit) config = c;
		config.whitespaceBehavior = WhitespaceBehavior.include;
		config.stringBehavior = StringBehavior.source;
		config.commentBehavior = CommentBehavior.include;

		_front = advance();
		while (depth > 0 && !empty)
		{
			auto t = front();
			if (t.text is null)
				app.put(str(t.type));
			else
				app.put(t.text);
			if (t.type == tok!"}")
			{
				depth--;
				if (depth > 0)
				popFront();
			}
			else if (t.type == tok!"{")
			{
				depth++;
				popFront();
			}
			else
				popFront();
		}
		IdType type = tok!"stringLiteral";
		lexStringSuffix(type);
		return Token(type, cache.intern(cast(const(ubyte)[]) app.data), line,
			column, index);
	}

	Token lexHexString() pure nothrow
	{
		mixin (tokenStart);
		range.popFront();
		range.popFront();

		loop: while (true)
		{
			if (range.empty)
			{
				error("Error: unterminated hex string literal");
				return Token();
			}
			else if (isWhitespace())
				popFrontWhitespaceAware();
			else switch (range.front)
			{
			case '0': .. case '9':
			case 'A': .. case 'F':
			case 'a': .. case 'f':
				range.popFront();
				break;
			case '"':
				range.popFront();
				break loop;
			default:
				error("Error: invalid character in hex string");
				return Token();
			}
		}

		IdType type = tok!"stringLiteral";
		lexStringSuffix(type);
		return Token(type, cache.intern(range.slice(mark)), line, column,
			index);
	}

	bool lexEscapeSequence() pure nothrow
	{
		range.popFront();
		if (range.empty)
		{
			error("Error: non-terminated character escape sequence.");
			return false;
		}
		switch (range.front)
		{
		case '\'':
		case '"':
		case '?':
		case '\\':
		case '0':
		case 'a':
		case 'b':
		case 'f':
		case 'n':
		case 'r':
		case 't':
		case 'v':
			range.popFront();
			break;
		case 'x':
			range.popFront();
			foreach (i; 0 .. 2)
			{
				if (range.empty)
				{
					error("Error: 2 hex digits expected.");
					return false;
				}
				switch (range.front)
				{
				case '0': .. case '9':
				case 'a': .. case 'f':
				case 'A': .. case 'F':
					range.popFront();
					break;
				default:
					error("Error: 2 hex digits expected.");
					return false;
				}
			}
			break;
		case '1': .. case '7':
			for (size_t i = 0; i < 3 && !range.empty && range.front >= '0' && range.front <= '7'; i++)
				range.popFront();
			break;
		case 'u':
			range.popFront();
			foreach (i; 0 .. 4)
			{
				if (range.empty)
				{
					error("Error: at least 4 hex digits expected.");
					return false;
				}
				switch (range.front)
				{
				case '0': .. case '9':
				case 'a': .. case 'f':
				case 'A': .. case 'F':
					range.popFront();
					break;
				default:
					error("Error: at least 4 hex digits expected.");
					return false;
				}
			}
			break;
		case 'U':
			range.popFront();
			foreach (i; 0 .. 8)
			{
				if (range.empty)
				{
					error("Error: at least 8 hex digits expected.");
					return false;
				}
				switch (range.front)
				{
				case '0': .. case '9':
				case 'a': .. case 'f':
				case 'A': .. case 'F':
					range.popFront();
					break;
				default:
					error("Error: at least 8 hex digits expected.");
					return false;
				}
			}
			break;
		default:
			while (true)
			{
				if (range.empty)
				{
					error("Error: non-terminated character escape sequence.");
					return false;
				}
				if (range.front == ';')
				{
					range.popFront();
					break;
				}
				else
					range.popFront();
			}
		}
		return true;
	}

	Token lexCharacterLiteral() pure nothrow
	{
		mixin (tokenStart);
		range.popFront();
		if (range.front == '\\')
		{
			lexEscapeSequence();
			goto close;
		}
		else if (range.front == '\'')
		{
			range.popFront();
			return Token(tok!"characterLiteral", cache.intern(range.slice(mark)),
				line, column, index);
		}
		else if (range.front & 0x80)
		{
			while (range.front & 0x80)
				range.popFront();
			goto close;
		}
		else
		{
			popFrontWhitespaceAware();
			goto close;
		}
	close:
		if (range.front == '\'')
		{
			range.popFront();
			return Token(tok!"characterLiteral", cache.intern(range.slice(mark)),
				line, column, index);
		}
		else
		{
			error("Error: Expected ' to end character literal");
			return Token();
		}
	}

	Token lexIdentifier() pure nothrow
	{
		import std.stdio;
		mixin (tokenStart);
		uint hash = 0;
		if (isSeparating(0) || range.empty)
		{
			error("Invalid identifier");
			range.popFront();
		}
		while (!range.empty && !isSeparating(0))
		{
			hash = StringCache.hashStep(range.front, hash);
			range.popFront();
		}
		return Token(tok!"identifier", cache.intern(range.slice(mark), hash), line,
			column, index);
	}

	Token lexDot() pure nothrow
	{
		mixin (tokenStart);
		if (!range.canPeek(1))
		{
			range.popFront();
			return Token(tok!".", null, line, column, index);
		}
		switch (range.peekAt(1))
		{
		case '0': .. case '9':
			return lexNumber();
		case '.':
			range.popFront();
			range.popFront();
			if (!range.empty && range.front == '.')
			{
				range.popFront();
				return Token(tok!"...", null, line, column, index);
			}
			else
				return Token(tok!"..", null, line, column, index);
		default:
			range.popFront();
			return Token(tok!".", null, line, column, index);
		}
	}

	Token lexLongNewline() pure nothrow
	{
		mixin (tokenStart);
		range.popFront();
		range.popFront();
		range.popFront();
		range.incrementLine();
		return Token(tok!"whitespace", cache.intern(range.slice(mark)), line,
			column, index);
	}

	bool isNewline() pure @safe nothrow
	{
		if (range.front == '\n') return true;
		if (range.front == '\r') return true;
		return (range.front & 0x80) && range.canPeek(2)
			&& (range.peek(2) == "\u2028" || range.peek(2) == "\u2029");
	}

	bool isSeparating(size_t offset) pure nothrow @safe
	{
		if (!range.canPeek(offset)) return true;
		auto c = range.peekAt(offset);
		if (c >= 'A' && c <= 'Z') return false;
		if (c >= 'a' && c <= 'z') return false;
		if (c <= 0x2f) return true;
		if (c >= ':' && c <= '@') return true;
		if (c >= '[' && c <= '^') return true;
		if (c >= '{' && c <= '~') return true;
		if (c == '`') return true;
		if (c & 0x80)
		{
			auto r = range;
			range.popFrontN(offset);
			return (r.canPeek(2) && (r.peek(2) == "\u2028"
				|| r.peek(2) == "\u2029"));
		}
		return false;
	}

	enum tokenStart = q{
		size_t index = range.index;
		size_t column = range.column;
		size_t line = range.line;
		auto mark = range.mark();
	};

	void error(string message) pure nothrow @safe
	{
		messages ~= Message(range.line, range.column, message, true);
	}

	void warning(string message) pure nothrow @safe
	{
		messages ~= Message(range.line, range.column, message, false);
		assert (messages.length > 0);
	}

	struct Message
	{
		size_t line;
		size_t column;
		string message;
		bool isError;
	}

	Message[] messages;
	shared(StringCache)* cache;
	LexerConfig config;
}

public auto byToken(ubyte[] range)
{
	LexerConfig config;
	shared(StringCache)* cache = new shared StringCache(StringCache.defaultBucketCount);
	return DLexer(range, config, cache);
}

public auto byToken(ubyte[] range, shared(StringCache)* cache)
{
	LexerConfig config;
	return DLexer(range, config, cache);
}

public auto byToken(ubyte[] range, const LexerConfig config, shared(StringCache)* cache)
{
	return DLexer(range, config, cache);
}
unittest
{
	import std.stdio;
	auto source = cast(ubyte[]) q{ import std.stdio;}c;
	auto tokens = byToken(source);
	assert (tokens.map!"a.type"().equal([tok!"import", tok!"identifier", tok!".",
		tok!"identifier", tok!";"]));
}

/// Test \x char sequence
unittest
{
	auto toks = (string s) => byToken(cast(ubyte[])s);

	// valid
	enum hex = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','A','B','C','D','E','F'];
	auto source = "";
	foreach(h1; hex)
		foreach(h2; hex)
			source ~= "'\\x" ~ h1 ~ h2 ~ "'";
	assert(toks(source).filter!(t => t.type != tok!"characterLiteral").empty);

	// invalid
	assert(toks(`'\x'`).messages[0] == DLexer.Message(1,4,"Error: 2 hex digits expected.",true));
	assert(toks(`'\x_'`).messages[0] == DLexer.Message(1,4,"Error: 2 hex digits expected.",true));
	assert(toks(`'\xA'`).messages[0] == DLexer.Message(1,5,"Error: 2 hex digits expected.",true));
	assert(toks(`'\xAY'`).messages[0] == DLexer.Message(1,5,"Error: 2 hex digits expected.",true));
	assert(toks(`'\xXX'`).messages[0] == DLexer.Message(1,4,"Error: 2 hex digits expected.",true));
}
