module std.d.lexer;

import std.typecons;
import std.typetuple;
import std.array;
import std.algorithm;
import std.range;
import std.lexer;

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
	"uint", "ulong", "union", "unittest", "ushort", "version", "virtual", "void",
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

	@disable this();

	this(ubyte[] range, const LexerConfig config, StringCache* cache)
	{
		auto r = (range.length >= 3 && range[0] == 0xef && range[1] == 0xbb && range[2] == 0xbf)
			? range[3 .. $] : range;
		this.range = LexerRange(r);
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
		assert (range.front == 'q');
		range.popFront();
		assert (range.front == '{');
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
	StringCache* cache;
	LexerConfig config;
}

public auto byToken(ubyte[] range)
{
	LexerConfig config;
	StringCache* cache = new StringCache(StringCache.defaultBucketCount);
	return DLexer(range, config, cache);
}

public auto byToken(ubyte[] range, StringCache* cache)
{
	LexerConfig config;
	return DLexer(range, config, cache);
}

public auto byToken(ubyte[] range, const LexerConfig config, StringCache* cache)
{
	return DLexer(range, config, cache);
}

/**
 * Removes "decoration" such as leading whitespace, leading + and * characters,
 * and places the result into the given output range
 */
public void unDecorateComment(T)(string comment, auto ref T outputRange)
	if (isOutputRange!(T, string))
in
{
	assert (comment.length >= 3);
}
body
{
	switch (comment[0 .. 3])
	{
	case "///":
		size_t i = 3;
		while (comment[i] == ' ' || comment[i] == '\t')
			i++;
		outputRange.put(comment[i .. $]);
		break;
	case "/++":
	case "/**":
		size_t i = 3;
		immutable char c = comment[1];
		// Skip leading * and + characters
		while (comment[i] == c) i++;
		// Skip trailing * and + characters
		size_t j = comment.length - 2;
		while (j > i && comment[j] == c)
			j--;
		while (j > i && (comment[j] == ' ' || comment[j] == '\t'))
			j--;
		if (comment[i] == '\r') i++;
		if (comment[i] == '\n') i++;
		while (comment[i] == ' ' || comment[i] == '\t') i++;
		immutable bool skipBeginningChar = comment[i] == c;
		if (skipBeginningChar)
			i++;
		size_t whitespaceToSkip;
		while (comment[i] == ' ' || comment[i] == '\t')
		{
			whitespaceToSkip++;
			i++;
		}
		size_t l = i;
		while (i < j)
		{
			if (comment[i++] == '\n')
				break;
		}
		outputRange.put(comment[l .. i]);
		while (true)
		{
			if (skipBeginningChar)
			{
				while (i < j && (comment[i] == ' ' || comment[i] == '\t')) i++;
				if (i < j && comment[i] == c) i++;
			}
			for (size_t s = 0; (i < j) && (s <= whitespaceToSkip)
				&& (comment[i] == ' ' || comment[i] == '\t');)
			{
				s++;
				i++;
			}
			size_t k = i;
			inner: while (k < j)
			{
				if (comment[k] == '\n')
				{
					k++;
					break inner;
				}
				k++;
			}
			outputRange.put(comment[i .. k]);
			i = k;
			if (i >= j)
				break;
		}
		break;
	default:
		assert (false, "Invalid doc comment");
	}
}


struct StringCache
{
public:

    @disable this();

    /**
     * Params: bucketCount = the initial number of buckets. Must be a
     * power of two
     */
    this(size_t bucketCount)
    {
        buckets = (cast(Node**) calloc((Node*).sizeof, bucketCount))[0 .. bucketCount];
    }

    ~this()
    {
        Block* current = rootBlock;
        while (current !is null)
        {
            Block* prev = current;
            current = current.next;
            free(cast(void*) prev.bytes.ptr);
            free(cast(void*) prev);
        }
        foreach (nodePointer; buckets)
        {
            Node* currentNode = nodePointer;
            while (currentNode !is null)
            {
                Node* prev = currentNode;
                currentNode = currentNode.next;
                free(prev);
            }
        }
        rootBlock = null;
		free(buckets.ptr);
        buckets = null;
    }

    /**
     * Caches a string.
     */
    string intern(const(ubyte)[] str) pure nothrow @safe
    {
        if (str is null || str.length == 0)
            return "";
        immutable uint hash = hashBytes(str);
        return intern(str, hash);
    }

    /**
     * ditto
     */
    string intern(string str) pure nothrow @trusted
    {
        return intern(cast(ubyte[]) str);
    }

    /**
     * Caches a string as above, but uses the given hash code instead of
     * calculating one itself. Use this alongside $(LREF hashStep)() can reduce the
     * amount of work necessary when lexing dynamic tokens.
     */
    string intern(const(ubyte)[] str, uint hash) pure nothrow @safe
    in
    {
        assert (str.length > 0);
    }
    body
    {
        return _intern(str, hash);
//		string s = _intern(str, hash);
//		size_t* ptr = s in debugMap;
//		if (ptr is null)
//			debugMap[s] = cast(size_t) s.ptr;
//		else
//			assert (*ptr == cast(size_t) s.ptr);
//        return s;
    }

    /**
     * Incremental hashing.
     * Params:
     *     b = the byte to add to the hash
     *     h = the hash that has been calculated so far
     * Returns: the new hash code for the string.
     */
    static uint hashStep(ubyte b, uint h) pure nothrow @safe
    {
        return (h ^ sbox[b]) * 3;
    }

    /**
     * The default bucket count for the string cache.
     */
    static enum defaultBucketCount = 2048;

    size_t allocated() pure nothrow @safe @property
    {
        return _allocated;
    }

private:

    string _intern(const(ubyte)[] bytes, uint hash) pure nothrow @trusted
    {
        if (bytes is null || bytes.length == 0)
            return "";
        immutable size_t index = hash & (buckets.length - 1);
        Node* s = find(bytes, hash);
        if (s !is null)
            return cast(string) s.str;
        _allocated += bytes.length;
        ubyte[] mem = allocate(bytes.length);
        mem[] = bytes[];
        Node* node = cast(Node*) malloc(Node.sizeof);
        node.str = mem;
        node.hash = hash;
        node.next = buckets[index];
        buckets[index] = node;
        return cast(string) mem;
    }

    Node* find(const(ubyte)[] bytes, uint hash) pure nothrow @trusted
    {
        import std.algorithm;
        immutable size_t index = hash & (buckets.length - 1);
        Node* node = buckets[index];
        while (node !is null)
        {
            if (node.hash == hash && bytes.equal(cast(ubyte[]) node.str))
                return node;
            node = node.next;
        }
        return node;
    }

    static uint hashBytes(const(ubyte)[] data) pure nothrow @trusted
    in
    {
        assert (data !is null);
        assert (data.length > 0);
    }
    body
    {
        uint hash = 0;
        foreach (ubyte b; data)
        {
            hash ^= sbox[b];
            hash *= 3;
        }
        return hash;
    }

    ubyte[] allocate(size_t numBytes) pure nothrow @trusted
    in
    {
        assert (numBytes != 0);
    }
    out (result)
    {
        assert (result.length == numBytes);
    }
    body
    {
        if (numBytes > (blockSize / 4))
            return (cast(ubyte*) malloc(numBytes))[0 .. numBytes];
        Block* r = rootBlock;
        size_t i = 0;
        while  (i <= 3 && r !is null)
        {

            immutable size_t available = r.bytes.length;
            immutable size_t oldUsed = r.used;
            immutable size_t newUsed = oldUsed + numBytes;
            if (newUsed <= available)
            {
                r.used = newUsed;
                return r.bytes[oldUsed .. newUsed];
            }
            i++;
            r = r.next;
        }
        Block* b = cast(Block*) malloc(Block.sizeof);
        b.bytes = (cast(ubyte*) malloc(blockSize))[0 .. blockSize];
        b.used = numBytes;
        b.next = rootBlock;
        rootBlock = b;
        return b.bytes[0 .. numBytes];
    }

    static struct Node
    {
        ubyte[] str;
        uint hash;
        Node* next;
    }

    static struct Block
    {
        ubyte[] bytes;
        size_t used;
        Block* next;
    }

    static enum blockSize = 1024 * 16;

    static immutable uint[] sbox = [
        0xF53E1837, 0x5F14C86B, 0x9EE3964C, 0xFA796D53,
        0x32223FC3, 0x4D82BC98, 0xA0C7FA62, 0x63E2C982,
        0x24994A5B, 0x1ECE7BEE, 0x292B38EF, 0xD5CD4E56,
        0x514F4303, 0x7BE12B83, 0x7192F195, 0x82DC7300,
        0x084380B4, 0x480B55D3, 0x5F430471, 0x13F75991,
        0x3F9CF22C, 0x2FE0907A, 0xFD8E1E69, 0x7B1D5DE8,
        0xD575A85C, 0xAD01C50A, 0x7EE00737, 0x3CE981E8,
        0x0E447EFA, 0x23089DD6, 0xB59F149F, 0x13600EC7,
        0xE802C8E6, 0x670921E4, 0x7207EFF0, 0xE74761B0,
        0x69035234, 0xBFA40F19, 0xF63651A0, 0x29E64C26,
        0x1F98CCA7, 0xD957007E, 0xE71DDC75, 0x3E729595,
        0x7580B7CC, 0xD7FAF60B, 0x92484323, 0xA44113EB,
        0xE4CBDE08, 0x346827C9, 0x3CF32AFA, 0x0B29BCF1,
        0x6E29F7DF, 0xB01E71CB, 0x3BFBC0D1, 0x62EDC5B8,
        0xB7DE789A, 0xA4748EC9, 0xE17A4C4F, 0x67E5BD03,
        0xF3B33D1A, 0x97D8D3E9, 0x09121BC0, 0x347B2D2C,
        0x79A1913C, 0x504172DE, 0x7F1F8483, 0x13AC3CF6,
        0x7A2094DB, 0xC778FA12, 0xADF7469F, 0x21786B7B,
        0x71A445D0, 0xA8896C1B, 0x656F62FB, 0x83A059B3,
        0x972DFE6E, 0x4122000C, 0x97D9DA19, 0x17D5947B,
        0xB1AFFD0C, 0x6EF83B97, 0xAF7F780B, 0x4613138A,
        0x7C3E73A6, 0xCF15E03D, 0x41576322, 0x672DF292,
        0xB658588D, 0x33EBEFA9, 0x938CBF06, 0x06B67381,
        0x07F192C6, 0x2BDA5855, 0x348EE0E8, 0x19DBB6E3,
        0x3222184B, 0xB69D5DBA, 0x7E760B88, 0xAF4D8154,
        0x007A51AD, 0x35112500, 0xC9CD2D7D, 0x4F4FB761,
        0x694772E3, 0x694C8351, 0x4A7E3AF5, 0x67D65CE1,
        0x9287DE92, 0x2518DB3C, 0x8CB4EC06, 0xD154D38F,
        0xE19A26BB, 0x295EE439, 0xC50A1104, 0x2153C6A7,
        0x82366656, 0x0713BC2F, 0x6462215A, 0x21D9BFCE,
        0xBA8EACE6, 0xAE2DF4C1, 0x2A8D5E80, 0x3F7E52D1,
        0x29359399, 0xFEA1D19C, 0x18879313, 0x455AFA81,
        0xFADFE838, 0x62609838, 0xD1028839, 0x0736E92F,
        0x3BCA22A3, 0x1485B08A, 0x2DA7900B, 0x852C156D,
        0xE8F24803, 0x00078472, 0x13F0D332, 0x2ACFD0CF,
        0x5F747F5C, 0x87BB1E2F, 0xA7EFCB63, 0x23F432F0,
        0xE6CE7C5C, 0x1F954EF6, 0xB609C91B, 0x3B4571BF,
        0xEED17DC0, 0xE556CDA0, 0xA7846A8D, 0xFF105F94,
        0x52B7CCDE, 0x0E33E801, 0x664455EA, 0xF2C70414,
        0x73E7B486, 0x8F830661, 0x8B59E826, 0xBB8AEDCA,
        0xF3D70AB9, 0xD739F2B9, 0x4A04C34A, 0x88D0F089,
        0xE02191A2, 0xD89D9C78, 0x192C2749, 0xFC43A78F,
        0x0AAC88CB, 0x9438D42D, 0x9E280F7A, 0x36063802,
        0x38E8D018, 0x1C42A9CB, 0x92AAFF6C, 0xA24820C5,
        0x007F077F, 0xCE5BC543, 0x69668D58, 0x10D6FF74,
        0xBE00F621, 0x21300BBE, 0x2E9E8F46, 0x5ACEA629,
        0xFA1F86C7, 0x52F206B8, 0x3EDF1A75, 0x6DA8D843,
        0xCF719928, 0x73E3891F, 0xB4B95DD6, 0xB2A42D27,
        0xEDA20BBF, 0x1A58DBDF, 0xA449AD03, 0x6DDEF22B,
        0x900531E6, 0x3D3BFF35, 0x5B24ABA2, 0x472B3E4C,
        0x387F2D75, 0x4D8DBA36, 0x71CB5641, 0xE3473F3F,
        0xF6CD4B7F, 0xBF7D1428, 0x344B64D0, 0xC5CDFCB6,
        0xFE2E0182, 0x2C37A673, 0xDE4EB7A3, 0x63FDC933,
        0x01DC4063, 0x611F3571, 0xD167BFAF, 0x4496596F,
        0x3DEE0689, 0xD8704910, 0x7052A114, 0x068C9EC5,
        0x75D0E766, 0x4D54CC20, 0xB44ECDE2, 0x4ABC653E,
        0x2C550A21, 0x1A52C0DB, 0xCFED03D0, 0x119BAFE2,
        0x876A6133, 0xBC232088, 0x435BA1B2, 0xAE99BBFA,
        0xBB4F08E4, 0xA62B5F49, 0x1DA4B695, 0x336B84DE,
        0xDC813D31, 0x00C134FB, 0x397A98E6, 0x151F0E64,
        0xD9EB3E69, 0xD3C7DF60, 0xD2F2C336, 0x2DDD067B,
        0xBD122835, 0xB0B3BD3A, 0xB0D54E46, 0x8641F1E4,
        0xA0B38F96, 0x51D39199, 0x37A6AD75, 0xDF84EE41,
        0x3C034CBA, 0xACDA62FC, 0x11923B8B, 0x45EF170A,
    ];

//	deprecated size_t[string] debugMap;
    size_t _allocated;
    Node*[] buckets;
    Block* rootBlock;
}

private extern(C) void* calloc(size_t, size_t) nothrow pure;
private extern(C) void* malloc(size_t) nothrow pure;
private extern(C) void free(void*) nothrow pure;

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
	foreach (h1; hex)
		foreach (h2; hex)
			source ~= "'\\x" ~ h1 ~ h2 ~ "'";
	assert (toks(source).filter!(t => t.type != tok!"characterLiteral").empty);

	// invalid
	assert (toks(`'\x'`).messages[0] == DLexer.Message(1,4,"Error: 2 hex digits expected.",true));
	assert (toks(`'\x_'`).messages[0] == DLexer.Message(1,4,"Error: 2 hex digits expected.",true));
	assert (toks(`'\xA'`).messages[0] == DLexer.Message(1,5,"Error: 2 hex digits expected.",true));
	assert (toks(`'\xAY'`).messages[0] == DLexer.Message(1,5,"Error: 2 hex digits expected.",true));
	assert (toks(`'\xXX'`).messages[0] == DLexer.Message(1,4,"Error: 2 hex digits expected.",true));
}

