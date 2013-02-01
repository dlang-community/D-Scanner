// Written in the D programming language

/**
 * This module contains a range-based _lexer for the D programming language.
 *
 * Examples:
 *
 * Generate HTML markup of D code.
 * ---
 * import std.stdio;
 * import std.array;
 * import std.file;
 * import std.d.lexer;
 *
 * void writeSpan(string cssClass, string value)
 * {
 * 	stdout.write(`<span class="`, cssClass, `">`, value.replace("&", "&amp;").replace("<", "&lt;"), `</span>`);
 * }
 *
 * // http://ethanschoonover.com/solarized
 * void highlight(R)(R tokens)
 * {
 * 	stdout.writeln(q"[<!DOCTYPE html>
 * <html>
 * <head>
 * <meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
 * <body>
 * <style type="text/css">
 * html  { background-color: #fdf6e3; color: #002b36; }
 * .kwrd { color: #b58900; font-weight: bold;  }
 * .com  { color: #93a1a1; font-style: italic; }
 * .num  { color: #dc322f; font-weigth: bold;  }
 * .str  { color: #2aa198; font-style: italic; }
 * .op   { color: #586e75; font-weight: bold;  }
 * .type { color: #268bd2; font-weight: bold;  }
 * .cons { color: #859900; font-weight: bold;  }
 * </style>
 * <pre>]");
 *
 *     foreach (Token t; tokens)
 *     {
 *         if (isType(t.type))
 *             writeSpan("type", t.value);
 *         else if (isKeyword(t.type))
 *             writeSpan("kwrd", t.value);
 *         else if (t.type == TokenType.Comment)
 *             writeSpan("com", t.value);
 *         else if (isStringLiteral(t.type))
 *             writeSpan("str", t.value);
 *         else if (isNumberLiteral(t.type))
 *             writeSpan("num", t.value);
 *         else if (isOperator(t.type))
 *             writeSpan("op", t.value);
 *         else
 *             stdout.write(t.value.replace("<", "&lt;"));
 *     }
 *     stdout.writeln("</pre>\n</body></html>");
 * }
 *
 * void main(string[] args)
 * {
 *     args[1].readText().byToken(args[1], IterationStyle.Everything, TokenStyle.Source).highlight();
 * }
 * ---
 * Iterate by tokens that would be significant to a parser
 * ---
 * import std.range;
 * import std.d.lexer;
 *
 * // ...
 *
 * string s = "import std.stdio; // comment";
 * auto tokens = byToken(s);
 * // The comment and whitespace are not included
 * assert (walkLength(tokens) == 5);
 * ---
 * Replace special tokens
 * ---
 * string s = "#line 5\n__VERSION__";
 * auto tokens = byToken(s, "example.d", IterationStyle.CodeOnly, TokenStyle.Default, "foo", "1.0");
 * assert (tokens.front.type == TokenType.IntLiteral);
 * assert (tokens.front.value == "1.0")
 * assert (tokens.front.lineNumber == 5);
 * ---
 *
 * Copyright: Brian Schott 2013
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt Boost, License 1.0)
 * Authors: Brian Schott
 * Source: $(PHOBOSSRC std/d/_lexer.d)
 */

module std.d.lexer;

import std.algorithm;
import std.ascii;
import std.conv;
import std.d.entities;
import std.datetime;
import std.exception;
import std.range;
import std.string;
import std.traits;
import std.uni;
import std.utf;

public:

/**
* Represents a D token
*/
struct Token
{
	/// The token type.
	TokenType type;

	/// The representation of the token in the original source code.
	string value;

	/// The number of the line the token is on.
	uint lineNumber;

	/// The character index of the start of the token in the original text.
	uint startIndex;

	/**
	* Check to see if the token is of the same type and has the same string
	* representation as the given token.
	*/
	bool opEquals(ref const(Token) other) const
	{
		return other.type == type && other.value == value;
	}

	/**
	* Checks to see if the token's string representation is equal to the given
	* string.
	*/
	bool opEquals(string value) const { return this.value == value; }

	/**
	* Checks to see if the token is of the given type.
	*/
	bool opEquals(TokenType type) const { return type == type; }

	/**
	* Comparison operator orders tokens by start index.
	*/
	int opCmp(size_t i) const
	{
		if (startIndex < i) return -1;
		if (startIndex > i) return 1;
		return 0;
	}
}

/**
 * Configure the behavior of the byToken() function. These flags may be
 * combined using a bitwise or.
 */
enum IterationStyle
{
	/// Only include code, not whitespace or comments
	CodeOnly = 0,
	/// Includes comments
	IncludeComments = 0b0001,
	/// Includes whitespace
	IncludeWhitespace = 0b0010,
	/// Include $(LINK2 http://dlang.org/lex.html#specialtokens, special tokens)
	IncludeSpecialTokens = 0b0100,
	/// Do not stop iteration on reaching the ___EOF__ token
	IgnoreEOF = 0b1000,
	/// Include everything
	Everything = IncludeComments | IncludeWhitespace | IgnoreEOF
}

/**
 * Configuration of the token lexing style. These flags may be combined with a
 * bitwise or.
 */
enum TokenStyle : uint
{
	/**
	 * Escape sequences will be replaced with their equivalent characters,
	 * enclosing quote characters will not be included. Special tokens such as
	 * __VENDOR__ will be replaced with their equivalent strings. Useful for
	 * creating a compiler or interpreter.
	 */
	Default = 0b0000,

	/**
	* Escape sequences will not be processed. An escaped quote character will
	* not terminate string lexing, but it will not be replaced with the quote
	* character in the token.
	*/
	NotEscaped = 0b0001,

	/**
	 * Strings will include their opening and closing quote characters as well
	 * as any prefixes or suffixes $(LPAREN)e.g.: $(D_STRING "abcde"w) will
	 * include the $(D_STRING 'w') character as well as the opening and closing
	 * quotes$(RPAREN)
	 */
	IncludeQuotes = 0b0010,

	/**
	 * Do not replace the value field of the special tokens such as ___DATE__
	 * with their string equivalents.
	 */
	DoNotReplaceSpecial = 0b0100,

	/**
	 * Strings will be read exactly as they appeared in the source, including
	 * their opening and closing quote characters. Useful for syntax
	 * highlighting.
	 */
	Source = NotEscaped | IncludeQuotes | DoNotReplaceSpecial
}

/// Default replacement for the ___VERSION__ special token
immutable string VERSION = "1.0";

/// Default replacement for the ___VENDOR__ special token
immutable string VENDOR = "std.d.lexer";

/**
 * Iterate over the given range of characters by D tokens.
 * Params:
 *     range = the range of characters
 *     iterationStyle = See IterationStyle
 *     stringStyle = see TokenStyle
 *     vendor = the string literal that should replace the ___VENDOR__ special token
 *     ver = the string literal that should replace the ___VERSION__ special token
 * Returns:
 *     an input range of tokens
 */
TokenRange!(R) byToken(R)(R range, string fileName = "",
	const IterationStyle iterationStyle = IterationStyle.CodeOnly,
	const TokenStyle stringStyle = TokenStyle.Default, string vendor = VENDOR,
	string ver = VERSION) if (isForwardRange!(R))
{
	auto r = TokenRange!(R)(range);
	r.stringStyle = stringStyle;
	r.iterStyle = iterationStyle;
	r.lineNumber = 1;
	r.popFront();
	return r;
}

/**
 * Range of tokens. Use byToken$(LPAREN)$(RPAREN) to instantiate.
 */
struct TokenRange(R) if (isForwardRange!(R))
{
	/**
	 * Returns: true if the range is empty
	 */
	bool empty() const @property
	{
		return _empty;
	}

	/**
	* Returns: the current token
	*/
	Token front() const @property
	{
		enforce(!_empty, "Cannot call front() on empty token range");
		return current;
	}

	/**
	* Returns the current token and then removes it from the range
	*/
	Token moveFront()
	{
		auto r = front();
		popFront();
		return r;
	}

	int opApply(int delegate(Token) dg)
	{
		int result = 0;
		while (!empty)
		{
			result = dg(front);
			if (result)
				break;
			popFront();
		}
		return result;
	}

	int opApply(int delegate(size_t, Token) dg)
	{
		int result = 0;
		int i = 0;
		while (!empty)
		{
			result = dg(i, front);
			if (result)
				break;
			popFront();
		}
		return result;
	}

	void popFront()
	{
		// Filter out tokens we don't care about
		loop: do
		{
			advance();
			switch (current.type)
			{
            case TokenType.Whitespace:
				if (iterStyle & IterationStyle.IncludeWhitespace)
					break loop;
				break;
			case TokenType.Comment:
				if (iterStyle & IterationStyle.IncludeComments)
					break loop;
				break;
			case TokenType.SpecialTokenSequence:
				if (iterStyle & IterationStyle.IncludeSpecialTokens)
					break loop;
				break;
			default:
				break loop;
			}
		}
		while (!empty());
	}

private:

	this(ref R range)
	{
		this.range = range;
		buffer = new ubyte[1024 * 4];
	}

	/*
	 * Advances the range to the next token
	 */
	void advance()
	{
		if (range.empty)
		{
			_empty = true;
			return;
		}

		current.lineNumber = lineNumber;
		current.startIndex = index;
		current.value = [];

		if (std.uni.isWhite(range.front))
		{
			current = lexWhitespace(range, index, lineNumber, buffer,
				(iterStyle & IterationStyle.IncludeWhitespace) > 0);
			return;
		}
		outer: switch (range.front)
		{
		mixin(generateCaseTrie(
			"=",    "TokenType.Assign",
			"@",    "TokenType.At",
			"&",    "TokenType.BitAnd",
			"&=",   "TokenType.BitAndEquals",
			"|",    "TokenType.BitOr",
			"|=",   "TokenType.BitOrEquals",
			"~=",   "TokenType.CatEquals",
			":",    "TokenType.Colon",
            ",",    "TokenType.Comma",
			"--",   "TokenType.Decrement",
			"$",    "TokenType.Dollar",
            ".",    "TokenType.Dot",
            "==",   "TokenType.Equals",
			"=>",   "TokenType.GoesTo",
			">",    "TokenType.Greater",
			">=",   "TokenType.GreaterEqual",
			"++",   "TokenType.Increment",
            "{",    "TokenType.LBrace",
            "[",    "TokenType.LBracket",
            "<",    "TokenType.Less",
			"<=",   "TokenType.LessEqual",
            "<>=",  "TokenType.LessEqualGreater",
			"<>",   "TokenType.LessOrGreater",
			"&&",   "TokenType.LogicAnd",
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
			"!<>=", "TokenType.Unordered",
			">>>",  "TokenType.UnsignedShiftRight",
			">>>=", "TokenType.UnsignedShiftRightEqual",
			"...",  "TokenType.Vararg",
			"^",    "TokenType.Xor",
			"^=",   "TokenType.XorEquals",
		));
        case '/':
			auto r = range.save();
			r.popFront();
			if (r.isEoF())
			{
				current.type = TokenType.Div;
				current.value = "/";
				range.popFront();
				++index;
				break;
			}
			switch (r.front)
			{
			case '/':
			case '*':
			case '+':
				current = lexComment(range, index, lineNumber, buffer,
					(iterStyle & IterationStyle.IncludeComments) > 0);
				break outer;
			case '=':
				current.type = TokenType.DivEquals;
				current.value = "/=";
				range.popFront();
				range.popFront();
				index += 2;
				break outer;
			default:
				current.type = TokenType.Div;
				current.value = "/";
				++index;
				range.popFront();
				break outer;
			}
		case '0': .. case '9':
			current = lexNumber(range, index, lineNumber, buffer);
			break;
		case '\'':
		case '"':
			current = lexString(range, index, lineNumber, buffer, stringStyle);
			break;
		case '`':
			current = lexString(range, index, lineNumber, buffer, stringStyle);
			break;
		case 'q':
			auto r = range.save;
			r.popFront();
			if (!r.isEoF() && r.front == '{')
			{
				current = lexTokenString(range, index, lineNumber, buffer, stringStyle);
				break;
			}
			else if (!r.isEoF() && r.front == '"')
			{
				current = lexDelimitedString(range, index, lineNumber,
					buffer, stringStyle);
				break;
			}
			else
				goto default;
		case 'r':
			auto r = range.save();
			r.popFront();
			if (!r.isEoF() && r.front == '"')
			{
				current = lexString(range, index, lineNumber, buffer, stringStyle);
				break;
			}
			else
				goto default;
		case 'x':
			auto r = range.save();
			r.popFront();
			if (!r.isEoF() && r.front == '"')
			{
				current = lexHexString(range, index, lineNumber, buffer);
				break;
			}
			else
				goto default;
		case '#':
			current = lexSpecialTokenSequence(range, index, lineNumber, buffer);
			break;
		default:
			size_t i;
			while(!range.isEoF() && !isSeparating(range.front))
			{
				buffer[i++] = range.front;
				range.popFront();
				++index;
			}

            current.type = lookupTokenType(cast(char[]) buffer[0 .. i]);
            current.value = getTokenValue(current.type);
            if (current.value is null)
                current.value = (cast(char[]) buffer[0 .. i]).idup;

			if (!(iterStyle & IterationStyle.IgnoreEOF) && current.type == TokenType.EOF)
			{
				_empty = true;
				return;
			}

			if (!(iterStyle & TokenStyle.DoNotReplaceSpecial))
				break;

			switch (current.type)
			{
			case TokenType.Date:
				current.type = TokenType.StringLiteral;
				auto time = Clock.currTime();
				current.value = format("%s %02d %04d", time.month, time.day, time.year);
				break;
			case TokenType.Time:
				auto time = Clock.currTime();
				current.type = TokenType.StringLiteral;
				current.value = (cast(TimeOfDay)(time)).toISOExtString();
				break;
			case TokenType.Timestamp:
				auto time = Clock.currTime();
				auto dt = cast(DateTime) time;
				current.type = TokenType.StringLiteral;
				current.value = format("%s %s %02d %02d:%02d:%02d %04d",
					dt.dayOfWeek, dt.month, dt.day, dt.hour, dt.minute,
					dt.second, dt.year);
				break;
			case TokenType.Vendor:
				current.type = TokenType.StringLiteral;
				current.value = vendor;
				break;
			case TokenType.CompilerVersion:
				current.type = TokenType.StringLiteral;
				current.value = ver;
				break;
			case TokenType.Line:
				current.type = TokenType.IntLiteral;
				current.value = format("%d", current.lineNumber);
				break;
			case TokenType.File:
				current.type = TokenType.StringLiteral;
				current.value = fileName;
				break;
			default:
				break;
			}
			break;
		}
	}

	Token current;
	uint lineNumber;
	uint index;
	R range;
	bool _empty;
	IterationStyle iterStyle;
	TokenStyle stringStyle;
	string ver;
	string vendor;
	string fileName;
	ubyte[] buffer;
}

unittest
{
	import std.stdio;
	auto a = "/**comment*/\n#lin #line 10 \"test.d\"\nint a;//test\n";
	foreach (t; byToken(a))
		writeln(t);
}

/**
 * Returns: true if the token is an operator
 */
pure nothrow bool isOperator(const TokenType t)
{
	return t >= TokenType.Assign && t <= TokenType.XorEquals;
}

/**
 * Returns: true if the token is a keyword
 */
pure nothrow bool isKeyword(const TokenType t)
{
	return t >= TokenType.Bool && t <= TokenType.With;
}

/**
 * Returns: true if the token is a built-in type
 */
pure nothrow bool isType(const TokenType t)
{
	return t >= TokenType.Bool && t <= TokenType.WString;
}

/**
 * Returns: true if the token is an attribute
 */
pure nothrow bool isAttribute(const TokenType t)
{
	return t >= TokenType.Align && t <= TokenType.Static;
}

/**
 * Returns: true if the token is a protection attribute
 */
pure nothrow bool isProtection(const TokenType t)
{
	return t >= TokenType.Export && t <= TokenType.Public;
}

/**
 * Returns: true if the token is a compile-time constant such as ___DATE__
 */
pure nothrow bool isConstant(const TokenType t)
{
	return t >= TokenType.Date && t <= TokenType.Traits;
}

/**
 * Returns: true if the token is a string or number literal
 */
pure nothrow bool isLiteral(const TokenType t)
{
	return t >= TokenType.DoubleLiteral && t <= TokenType.WStringLiteral;
}

/**
 * Returns: true if the token is a number literal
 */
pure nothrow bool isNumberLiteral(const TokenType t)
{
	return t >= TokenType.DoubleLiteral && t <= TokenType.UnsignedLongLiteral;
}

/**
 * Returns: true if the token is a string literal
 */
pure nothrow bool isStringLiteral(const TokenType t)
{
	return t >= TokenType.DStringLiteral && t <= TokenType.WStringLiteral;
}

/**
 * Returns: true if the token is whitespace, a commemnt, a special token
 *     sequence, or an identifier
 */
pure nothrow bool isMisc(const TokenType t)
{
	return t >= TokenType.Comment && t <= TokenType.SpecialTokenSequence;
}

/**
 * Listing of all the tokens in the D language.
 */
enum TokenType: ushort
{
	Assign,	/// =
	At, /// @
	BitAnd,	/// &
	BitAndEquals,	/// &=
	BitOr,	/// |
	BitOrEquals,	/// |=
	CatEquals,	/// ~=
	Colon,	/// :
	Comma,	/// ,
	Decrement,	/// --
	Div,	/// /
	DivEquals,	/// /=
	Dollar,	/// $
	Dot,	/// .
	Equals,	/// ==
	GoesTo, /// =>
	Greater,	/// >
	GreaterEqual,	/// >=
	Hash, /// #
	Increment,	/// ++
	LBrace,	/// {
	LBracket,	/// [
	Less,	/// <
	LessEqual,	/// <=
	LessEqualGreater, /// <>=
	LessOrGreater,	/// <>
	LogicAnd,	/// &&
	LogicOr,	/// ||
	LParen,	/// $(LPAREN)
	Minus,	/// -
	MinusEquals,	/// -=
	Mod,	/// %
	ModEquals,	/// %=
	MulEquals,	/// *=
	Not,	/// !
	NotEquals,	/// !=
	NotGreater,	/// !>
	NotGreaterEqual,	/// !>=
	NotLess,	/// !<
	NotLessEqual,	/// !<=
	NotLessEqualGreater,	/// !<>
	Plus,	/// +
	PlusEquals,	/// +=
	Pow,	/// ^^
	PowEquals,	/// ^^=
	RBrace,	/// }
	RBracket,	/// ]
	RParen,	/// $(RPAREN)
	Semicolon,	/// ;
	ShiftLeft,	/// <<
	ShiftLeftEqual,	/// <<=
	ShiftRight,	/// >>
	ShiftRightEqual,	/// >>=
	Slice, /// ..
	Star,	/// *
	Ternary,	/// ?
	Tilde,	/// ~
	Unordered,	/// !<>=
	UnsignedShiftRight,	/// >>>
	UnsignedShiftRightEqual,	/// >>>=
	Vararg,	/// ...
	Xor,	/// ^
	XorEquals,	/// ^=

	Bool, /// $(D_KEYWORD bool)
	Byte, /// $(D_KEYWORD byte)
	Cdouble, /// $(D_KEYWORD cdouble)
	Cent, /// $(D_KEYWORD cent)
	Cfloat, /// $(D_KEYWORD cfloat)
	Char, /// $(D_KEYWORD char)
	Creal, /// $(D_KEYWORD creal)
	Dchar, /// $(D_KEYWORD dchar)
	Double, /// $(D_KEYWORD double)
	DString, /// $(D_KEYWORD dstring)
	Float, /// $(D_KEYWORD float)
	Function, /// $(D_KEYWORD function)
	Idouble, /// $(D_KEYWORD idouble)
	Ifloat, /// $(D_KEYWORD ifloat)
	Int, /// $(D_KEYWORD int)
	Ireal, /// $(D_KEYWORD ireal)
	Long, /// $(D_KEYWORD long)
	Real, /// $(D_KEYWORD real)
	Short, /// $(D_KEYWORD short)
	String, /// $(D_KEYWORD string)
	Ubyte, /// $(D_KEYWORD ubyte)
	Ucent, /// $(D_KEYWORD ucent)
	Uint, /// $(D_KEYWORD uint)
	Ulong, /// $(D_KEYWORD ulong)
	Ushort, /// $(D_KEYWORD ushort)
	Void, /// $(D_KEYWORD void)
	Wchar, /// $(D_KEYWORD wchar)
	WString, /// $(D_KEYWORD wstring)

	Align, /// $(D_KEYWORD align)
	Deprecated, /// $(D_KEYWORD deprecated)
	Extern, /// $(D_KEYWORD extern)
	Pragma, /// $(D_KEYWORD pragma)
	Export, /// $(D_KEYWORD export)
	Package, /// $(D_KEYWORD package)
	Private, /// $(D_KEYWORD private)
	Protected, /// $(D_KEYWORD protected)
	Public, /// $(D_KEYWORD public)
	Abstract, /// $(D_KEYWORD abstract)
	Auto, /// $(D_KEYWORD auto)
	Const, /// $(D_KEYWORD const)
	Final, /// $(D_KEYWORD final)
	Gshared, /// $(D_KEYWORD __gshared)
	Immutable, // immutable
	Inout, // inout
	Scope, /// $(D_KEYWORD scope)
	Shared, // shared
	Static, /// $(D_KEYWORD static)

	Synchronized, /// $(D_KEYWORD synchronized)
	Alias, /// $(D_KEYWORD alias)
	Asm, /// $(D_KEYWORD asm)
	Assert, /// $(D_KEYWORD assert)
	Body, /// $(D_KEYWORD body)
	Break, /// $(D_KEYWORD break)
	Case, /// $(D_KEYWORD case)
	Cast, /// $(D_KEYWORD cast)
	Catch, /// $(D_KEYWORD catch)
	Class, /// $(D_KEYWORD class)
	Continue, /// $(D_KEYWORD continue)
	Debug, /// $(D_KEYWORD debug)
	Default, /// $(D_KEYWORD default)
	Delegate, /// $(D_KEYWORD delegate)
	Delete, /// $(D_KEYWORD delete)
	Do, /// $(D_KEYWORD do)
	Else, /// $(D_KEYWORD else)
	Enum, /// $(D_KEYWORD enum)
	False, /// $(D_KEYWORD false)
	Finally, /// $(D_KEYWORD finally)
	Foreach, /// $(D_KEYWORD foreach)
	Foreach_reverse, /// $(D_KEYWORD foreach_reverse)
	For, /// $(D_KEYWORD for)
	Goto, /// $(D_KEYWORD goto)
	If, /// $(D_KEYWORD if)
	Import, /// $(D_KEYWORD import)
	In, /// $(D_KEYWORD in)
	Interface, /// $(D_KEYWORD interface)
	Invariant, /// $(D_KEYWORD invariant)
	Is, /// $(D_KEYWORD is)
	Lazy, /// $(D_KEYWORD lazy)
	Macro, /// $(D_KEYWORD macro)
	Mixin, /// $(D_KEYWORD mixin)
	Module, /// $(D_KEYWORD module)
	New, /// $(D_KEYWORD new)
	Nothrow, /// $(D_KEYWORD nothrow)
	Null, /// $(D_KEYWORD null)
	Out, /// $(D_KEYWORD out)
	Override, /// $(D_KEYWORD override)
	Pure, /// $(D_KEYWORD pure)
	Ref, /// $(D_KEYWORD ref)
	Return, /// $(D_KEYWORD return)
	Struct, /// $(D_KEYWORD struct)
	Super, /// $(D_KEYWORD super)
	Switch, /// $(D_KEYWORD switch)
	Template, /// $(D_KEYWORD template)
	This, /// $(D_KEYWORD this)
	Throw, /// $(D_KEYWORD throw)
	True, /// $(D_KEYWORD true)
	Try, /// $(D_KEYWORD try)
	Typedef, /// $(D_KEYWORD typedef)
	Typeid, /// $(D_KEYWORD typeid)
	Typeof, /// $(D_KEYWORD typeof)
	Union, /// $(D_KEYWORD union)
	Unittest, /// $(D_KEYWORD unittest)
	Version, /// $(D_KEYWORD version)
	Volatile, /// $(D_KEYWORD volatile)
	While, /// $(D_KEYWORD while)
	With, /// $(D_KEYWORD with)

	Date, /// ___DATE__
	EOF, /// ___EOF__
	Time, /// ___TIME__
	Timestamp, /// ___TIMESTAMP__
	Vendor, /// ___VENDOR__
	CompilerVersion, /// ___VERSION__
	File, /// ___FILE__
	Line, /// ___LINE__
	Comment, /// $(D_COMMENT /** comment */) or $(D_COMMENT // comment) or $(D_COMMENT ///comment)
	Identifier, /// anything else
	ScriptLine, // Line at the beginning of source file that starts from #!
    Traits, /// $(D_KEYWORD ___traits)
    Parameters, /// $(D_KEYWORD ___parameters)
    Vector, /// $(D_KEYWORD ___vector)
	Whitespace, /// whitespace
	SpecialTokenSequence, /// #line 10 "file.d"
	DoubleLiteral, /// 123.456
	FloatLiteral, /// 123.456f or 0x123_45p-3
	IDoubleLiteral, /// 123.456i
	IFloatLiteral, /// 123.456fi
	IntLiteral, /// 123 or 0b1101010101
	LongLiteral, /// 123L
	RealLiteral, /// 123.456L
	IRealLiteral, /// 123.456Li
	UnsignedIntLiteral, /// 123u
	UnsignedLongLiteral, /// 123uL
	DStringLiteral, /// $(D_STRING "32-bit character string"d)
	StringLiteral, /// $(D_STRING "an 8-bit string")
	WStringLiteral, /// $(D_STRING "16-bit character string"w)
}

// Implementation details follow
private:


/*
 * To avoid memory allocations Token.value is set to a slice of this string
 * for operators and keywords.
 */
immutable string opKwdValues =
      "#/=*=+=++-=--^^=~=<<=%==>>>=||=&&=,;:!<=!<>=!=!>=?...()[]{}@$"
    ~ "boolcdoublecentcfloatcrealdchardstringfunctionidoubleifloatirealubyte"
    ~ "ucentuintulongushortvoidwcharwstringaligndeprecatedexternpragmaexport"
    ~ "packageprivateprotectedpublicabstractautoconstfinal__gsharedimmutable"
    ~ "inoutscopesharedstaticsynchronizedaliasasmassertbodybreakcasecastcatch"
    ~ "classcontinuedebugdefaultdelegatedeleteelseenumfalsefinally"
    ~ "foreach_reversegotoimportinterfaceinvariantlazymacromixinmodule"
    ~ "newnothrownulloverridepurerefreturnstructsuperswitchtemplatethistruetry"
    ~ "typedeftypeidtypeofunionunittestversionvolatilewhilewith__traits"
    ~ "__vector__parameters__DATE__EOF__TIME__TIMESTAMP__VENDOR__VERSION__"
    ~ "FILE__LINE__";

/*
 * Slices of the above string. This array is automatically generated.
 */
immutable(string[TokenType.max + 1]) tokenValues = [
	opKwdValues[2 .. 3], // =
	opKwdValues[59 .. 60], // @
	opKwdValues[31 .. 32], // &
	opKwdValues[32 .. 34], // &=
	opKwdValues[28 .. 29], // |
	opKwdValues[29 .. 31], // |=
	opKwdValues[16 .. 18], // ~=
	opKwdValues[36 .. 37], // :
	opKwdValues[34 .. 35], // ,
	opKwdValues[11 .. 13], // --
	opKwdValues[1 .. 2], // /
	opKwdValues[1 .. 3], // /=
	opKwdValues[60 .. 61], // $
	opKwdValues[50 .. 51], // .
	opKwdValues[22 .. 24], // ==
	opKwdValues[23 .. 25], // =>
	opKwdValues[24 .. 25], // >
	opKwdValues[26 .. 28], // >=
	opKwdValues[0 .. 1], // #
	opKwdValues[7 .. 9], // ++
	opKwdValues[57 .. 58], // {
	opKwdValues[55 .. 56], // [
	opKwdValues[18 .. 19], // <
	opKwdValues[19 .. 21], // <=
	opKwdValues[41 .. 44], // <>=
	opKwdValues[41 .. 43], // <>
	opKwdValues[31 .. 33], // &&
	opKwdValues[28 .. 30], // ||
	opKwdValues[53 .. 54], // (
	opKwdValues[9 .. 10], // -
	opKwdValues[9 .. 11], // -=
	opKwdValues[21 .. 22], // %
	opKwdValues[21 .. 23], // %=
	opKwdValues[3 .. 5], // *=
	opKwdValues[37 .. 38], // !
	opKwdValues[44 .. 46], // !=
	opKwdValues[46 .. 48], // !>
	opKwdValues[46 .. 49], // !>=
	opKwdValues[37 .. 39], // !<
	opKwdValues[37 .. 40], // !<=
	opKwdValues[40 .. 43], // !<>
	opKwdValues[5 .. 6], // +
	opKwdValues[5 .. 7], // +=
	opKwdValues[13 .. 15], // ^^
	opKwdValues[13 .. 16], // ^^=
	opKwdValues[58 .. 59], // }
	opKwdValues[56 .. 57], // ]
	opKwdValues[54 .. 55], // )
	opKwdValues[35 .. 36], // ;
	opKwdValues[18 .. 20], // <<
	opKwdValues[18 .. 21], // <<=
	opKwdValues[24 .. 26], // >>
	opKwdValues[25 .. 28], // >>=
	opKwdValues[50 .. 52], // ..
	opKwdValues[3 .. 4], // *
	opKwdValues[49 .. 50], // ?
	opKwdValues[16 .. 17], // ~
	opKwdValues[40 .. 44], // !<>=
	opKwdValues[24 .. 27], // >>>
	opKwdValues[24 .. 28], // >>>=
	opKwdValues[50 .. 53], // ...
	opKwdValues[13 .. 14], // ^
	opKwdValues[14 .. 16], // ^=
	opKwdValues[61 .. 65], // bool
	opKwdValues[126 .. 130], // byte
	opKwdValues[65 .. 72], // cdouble
	opKwdValues[72 .. 76], // cent
	opKwdValues[76 .. 82], // cfloat
	opKwdValues[88 .. 92], // char
	opKwdValues[82 .. 87], // creal
	opKwdValues[87 .. 92], // dchar
	opKwdValues[66 .. 72], // double
	opKwdValues[92 .. 99], // dstring
	opKwdValues[77 .. 82], // float
	opKwdValues[99 .. 107], // function
	opKwdValues[107 .. 114], // idouble
	opKwdValues[114 .. 120], // ifloat
	opKwdValues[136 .. 139], // int
	opKwdValues[120 .. 125], // ireal
	opKwdValues[140 .. 144], // long
	opKwdValues[83 .. 87], // real
	opKwdValues[145 .. 150], // short
	opKwdValues[93 .. 99], // string
	opKwdValues[125 .. 130], // ubyte
	opKwdValues[130 .. 135], // ucent
	opKwdValues[135 .. 139], // uint
	opKwdValues[139 .. 144], // ulong
	opKwdValues[144 .. 150], // ushort
	opKwdValues[150 .. 154], // void
	opKwdValues[154 .. 159], // wchar
	opKwdValues[159 .. 166], // wstring
	opKwdValues[166 .. 171], // align
	opKwdValues[171 .. 181], // deprecated
	opKwdValues[181 .. 187], // extern
	opKwdValues[187 .. 193], // pragma
	opKwdValues[193 .. 199], // export
	opKwdValues[199 .. 206], // package
	opKwdValues[206 .. 213], // private
	opKwdValues[213 .. 222], // protected
	opKwdValues[222 .. 228], // public
	opKwdValues[228 .. 236], // abstract
	opKwdValues[236 .. 240], // auto
	opKwdValues[240 .. 245], // const
	opKwdValues[245 .. 250], // final
	opKwdValues[250 .. 259], // __gshared
	opKwdValues[259 .. 268], // immutable
	opKwdValues[268 .. 273], // inout
	opKwdValues[273 .. 278], // scope
	opKwdValues[253 .. 259], // shared
	opKwdValues[284 .. 290], // static
	opKwdValues[290 .. 302], // synchronized
	opKwdValues[302 .. 307], // alias
	opKwdValues[307 .. 310], // asm
	opKwdValues[310 .. 316], // assert
	opKwdValues[316 .. 320], // body
	opKwdValues[320 .. 325], // break
	opKwdValues[325 .. 329], // case
	opKwdValues[329 .. 333], // cast
	opKwdValues[333 .. 338], // catch
	opKwdValues[338 .. 343], // class
	opKwdValues[343 .. 351], // continue
	opKwdValues[351 .. 356], // debug
	opKwdValues[356 .. 363], // default
	opKwdValues[363 .. 371], // delegate
	opKwdValues[371 .. 377], // delete
	opKwdValues[66 .. 68], // do
	opKwdValues[377 .. 381], // else
	opKwdValues[381 .. 385], // enum
	opKwdValues[385 .. 390], // false
	opKwdValues[390 .. 397], // finally
	opKwdValues[397 .. 404], // foreach
	opKwdValues[397 .. 412], // foreach_reverse
	opKwdValues[397 .. 400], // for
	opKwdValues[412 .. 416], // goto
	opKwdValues[114 .. 116], // if
	opKwdValues[416 .. 422], // import
	opKwdValues[96 .. 98], // in
	opKwdValues[422 .. 431], // interface
	opKwdValues[431 .. 440], // invariant
	opKwdValues[522 .. 524], // is
	opKwdValues[440 .. 444], // lazy
	opKwdValues[444 .. 449], // macro
	opKwdValues[449 .. 454], // mixin
	opKwdValues[454 .. 460], // module
	opKwdValues[460 .. 463], // new
	opKwdValues[463 .. 470], // nothrow
	opKwdValues[470 .. 474], // null
	opKwdValues[270 .. 273], // out
	opKwdValues[474 .. 482], // override
	opKwdValues[482 .. 486], // pure
	opKwdValues[486 .. 489], // ref
	opKwdValues[489 .. 495], // return
	opKwdValues[495 .. 501], // struct
	opKwdValues[501 .. 506], // super
	opKwdValues[506 .. 512], // switch
	opKwdValues[512 .. 520], // template
	opKwdValues[520 .. 524], // this
	opKwdValues[465 .. 470], // throw
	opKwdValues[524 .. 528], // true
	opKwdValues[528 .. 531], // try
	opKwdValues[531 .. 538], // typedef
	opKwdValues[538 .. 544], // typeid
	opKwdValues[544 .. 550], // typeof
	opKwdValues[550 .. 555], // union
	opKwdValues[555 .. 563], // unittest
	opKwdValues[563 .. 570], // version
	opKwdValues[570 .. 578], // volatile
	opKwdValues[578 .. 583], // while
	opKwdValues[583 .. 587], // with
	opKwdValues[615 .. 623], // __DATE__
	opKwdValues[621 .. 628], // __EOF__
	opKwdValues[626 .. 634], // __TIME__
	opKwdValues[632 .. 645], // __TIMESTAMP__
	opKwdValues[643 .. 653], // __VENDOR__
	opKwdValues[651 .. 662], // __VERSION__
	opKwdValues[660 .. 668], // __FILE__
	opKwdValues[666 .. 674], // __LINE__
	null,
	null,
	null,
	opKwdValues[587 .. 595], // __traits
	opKwdValues[603 .. 615], // __parameters
	opKwdValues[595 .. 603], // __vector
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
];

pure string getTokenValue(const TokenType type)
{
    return tokenValues[type];
}

private pure bool isNewline(R)(R range)
{
	return range.front == '\n' || range.front == '\r';
}

pure bool isEoF(R)(R range)
{
	return range.empty || range.front == 0 || range.front == 0x1a;
}

void popNewline(R)(ref R range, ref uint index, ref ubyte[] buffer, ref size_t i)
	if (isForwardRange!R)
{
	if (range.front == '\r')
	{
		buffer[i++] = range.front;
		range.popFront();
		++index;
	}
	if (range.front == '\n')
	{
		buffer[i++] = range.front;
		range.popFront();
		++index;
	}
}

unittest
{
	uint i;
	auto s = "\r\ntest";
	assert (popNewline(s, i) == "\r\n");
	assert (s == "test");
}

Token lexWhitespace(R)(ref R range, ref uint index,
	ref uint lineNumber, ref ubyte[] buffer, bool needValue) if (isForwardRange!R)
{
	Token t;
	t.type = TokenType.Whitespace;
	t.lineNumber = lineNumber;
	t.startIndex = index;
	size_t i = 0;
	while (!isEoF(range) && std.uni.isWhite(range.front))
	{
		if (i > buffer.length)
			assert(false);
		if (isNewline(range))
		{
			++lineNumber;
			popNewline(range, index, buffer, i);
		}
		else
		{
			buffer[i++] = range.front;
			range.popFront();
			++index;
		}
	}
	if (needValue)
		t.value = (cast(char[]) buffer[0..i]).idup;
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

Token lexComment(R)(ref R input, ref uint index, ref uint lineNumber,
	ref ubyte[] buffer, bool needValue) if (isForwardRange!R)
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
	size_t i;
	buffer[i++] = input.front;
	input.popFront();
	switch(input.front)
	{
	case '/':
		while (!isEoF(input) && !isNewline(input))
		{
			buffer[i++] = input.front;
			input.popFront();
			++index;
		}
		break;
	case '*':
		while (!isEoF(input))
		{
			if (isNewline(input))
			{
				popNewline(input, index, buffer, i);
				++lineNumber;
			}
			else if (input.front == '*')
			{
				buffer[i++] = input.front;
				input.popFront();
				++index;
				if (input.front == '/')
				{
					buffer[i++] = input.front;
					input.popFront();
					++index;
					break;
				}
			}
			else
			{
				buffer[i++] = input.front;
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
				popNewline(input, index, buffer, i);
				lineNumber++;
			}
			else if (input.front == '+')
			{
				buffer[i++] = input.front;
				input.popFront();
				++index;
				if (input.front == '/')
				{
					buffer[i++] = input.front;
					input.popFront();
					++index;
					--depth;
				}
			}
			else if (input.front == '/')
			{
				buffer[i++] = input.front;
				input.popFront();
				++index;
				if (input.front == '+')
				{
					buffer[i++] = input.front;
					input.popFront();
					++index;
					++depth;
				}
			}
			else
			{
				buffer[i++] = input.front;
				input.popFront();
				++index;
			}
		}
		break;
	default:
		Token errorToken;
		return errorToken;
	}
	if (needValue)
		t.value = (cast(char[]) buffer[0 .. i]).idup;
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

ubyte[] popDigitChars(R, alias isInterestingDigit)(ref R input, ref uint index,
	uint upTo) if (isForwardRange!R)
{
	ubyte[] chars;
	chars.reserve(upTo);
	for (uint i = 0; i != upTo; ++i)
	{
		if (isInterestingDigit(input.front))
		{
			chars ~= input.front;
			input.popFront();
		}
		else
			break;
	}
	return chars;
}

ubyte[] popHexChars(R)(ref R input, ref uint index, uint upTo)
{
	return popDigitChars!(R, isHexDigit)(input, index, upTo);
}

ubyte[] popOctalChars(R)(ref R input, ref uint index, uint upTo)
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

void interpretEscapeSequence(R)(ref R input, ref uint index, ref ubyte[] buffer,
	ref size_t i) if (isForwardRange!R)
in
{
	assert(input.front == '\\');
}
body
{
	input.popFront();
	short h = 0;
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
		auto s = to!string(cast(char) f);
		buffer[i .. i + s.length] = cast(ubyte[]) s;
		return;
	case 'a': input.popFront(); ++index; buffer[i++] = '\a'; return;
	case 'b': input.popFront(); ++index; buffer[i++] = '\b'; return;
	case 'f': input.popFront(); ++index; buffer[i++] = '\f'; return;
	case 'n': input.popFront(); ++index; buffer[i++] = '\n'; return;
	case 'r': input.popFront(); ++index; buffer[i++] = '\r'; return;
	case 't': input.popFront(); ++index; buffer[i++] = '\t'; return;
	case 'v': input.popFront(); ++index; buffer[i++] = '\v'; return;
	case 'x': h = 2; goto hex;
	case 'u': h = 4; goto hex;
	case 'U': h = 8; goto hex;
	case '0': .. case '7':
		auto octalChars = cast(char[]) popOctalChars(input, index, 3);
		char[4] b;
		auto n = encode(b, cast(dchar) parse!uint(octalChars, 8));
		buffer[i .. i + n] = cast(ubyte[]) b[0 .. n];
		i += n;
		return;
	case '&':
		input.popFront();
		++index;
		auto entity = appender!(ubyte[])();
		while (!input.isEoF() && input.front != ';')
		{
			entity.put(input.front);
			input.popFront();
			++index;
		}
		if (!isEoF(input))
		{
			auto decoded = to!string(cast(char[]) entity.data) in characterEntities;
			input.popFront();
			++index;
			if (decoded !is null)
			{
				buffer[i .. i + decoded.length] = cast(ubyte[]) *decoded;
				i += decoded.length;
			}
		}
		return;
	default:
		input.popFront();
		++index;
		// This is an error
		buffer[i++] = '\\';
		return;
	}

hex:
	input.popFront();
	auto hexChars = cast(char[]) popHexChars(input, index, h);
	char[4] b;
	auto n = encode(b, cast(dchar) parse!uint(hexChars, 16));
	buffer[i .. i + n] = cast(ubyte[]) b[0 .. n];
	i += n;
	return;
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

Token lexHexString(R)(ref R input, ref uint index, ref uint lineNumber,
	ref ubyte[] buffer, const TokenStyle style = TokenStyle.Default)
in
{
	assert (input.front == 'x');
}
body
{
	Token t;
	t.lineNumber = lineNumber;
	t.startIndex = index;
	t.type = TokenType.StringLiteral;
	size_t i;
	if (style & TokenStyle.IncludeQuotes)
	{
		buffer[i++] = 'x';
		buffer[i++] = '"';
	}
	input.popFront();
	input.popFront();
	index += 2;
	while (!input.isEoF())
	{
		if (isNewline(input))
		{
			popNewline(input, index, buffer, i);
			++lineNumber;
		}
		else if (isHexDigit(input.front))
		{
			buffer[i++] = input.front;
			input.popFront();
			++index;
		}
		else if (std.uni.isWhite(input.front) && (style & TokenStyle.NotEscaped))
		{
			buffer[i++] = input.front;
			input.popFront();
			++index;
		}
		else if (input.front == '"')
		{
			if (style & TokenStyle.IncludeQuotes)
				buffer[i++] = '"';
			input.popFront();
			++index;
			break;
		}
		else
		{
			// This is an error
		}
	}
	if (!input.isEoF())
	{
		switch (input.front)
		{
		case 'w':
			t.type = TokenType.WStringLiteral;
			goto case 'c';
		case 'd':
			t.type = TokenType.DStringLiteral;
			goto case 'c';
		case 'c':
			if (style & TokenStyle.IncludeQuotes)
				buffer[i++] = input.front;
			input.popFront();
			++index;
			break;
		default:
			break;
		}
	}
	if (style & TokenStyle.NotEscaped)
		t.value = (cast(char[]) buffer[0 .. i]).idup;
	else
	{
		auto a = appender!(ubyte[])();
		foreach (b; std.range.chunks(buffer[0 .. i], 2))
		{
			string s = to!string(cast(char[]) b);
			a.put(cast(ubyte[]) to!string(cast(dchar) parse!uint(s, 16)));
		}
		t.value = to!string(cast(char[]) a.data);
	}


	return t;
}

unittest
{
	uint i;
	uint l;

	auto a = `x"204041"`;
	auto ar = lexHexString(a, i, l);
	assert (ar == " @A");
	assert (ar == TokenType.StringLiteral);

	auto b = `x"20"w`;
	auto br = lexHexString(b, i, l);
	assert (br == " ");
	assert (br == TokenType.WStringLiteral);

	auto c = `x"6d"`;
	auto cr = lexHexString(c, i, l, TokenStyle.NotEscaped);
	assert (cr == "6d");

	auto d = `x"5e5f"d`;
	auto dr = lexHexString(d, i, l, TokenStyle.NotEscaped | TokenStyle.IncludeQuotes);
	assert (dr == `x"5e5f"d`);
	assert (dr == TokenType.DStringLiteral);
}

Token lexString(R)(ref R input, ref uint index, ref uint lineNumber,
	ref ubyte[] buffer, const TokenStyle style = TokenStyle.Default)
in
{
	assert (input.front == '\'' || input.front == '"' || input.front == '`' || input.front == 'r');
}
body
{
	Token t;
	t.lineNumber = lineNumber;
	t.startIndex = index;
	t.type = TokenType.StringLiteral;
	size_t i = 0;
	bool isWysiwyg = input.front == 'r' || input.front == '`';
	if (input.front == 'r')
	{
		if (style & TokenStyle.IncludeQuotes)
			buffer[i++] = 'r';
		input.popFront();
	}
	auto quote = input.front;
	input.popFront();
	++index;

	if (style & TokenStyle.IncludeQuotes)
		buffer[i++] = quote;
	while (!isEoF(input))
	{
		if (isNewline(input))
		{
			popNewline(input, index, buffer, i);
			lineNumber++;
		}
		else if (input.front == '\\')
		{
			if (style & TokenStyle.NotEscaped)
			{
				auto r = input.save();
				r.popFront();
				if (r.front == quote && !isWysiwyg)
				{
					buffer[i++] = '\\';
					buffer[i++] = quote;
					input.popFront();
					input.popFront();
					index += 2;
				}
				else if (r.front == '\\' && !isWysiwyg)
				{
					buffer[i++] = '\\';
					buffer[i++] = '\\';
					input.popFront();
					input.popFront();
					index += 2;
				}
				else
				{
					buffer[i++] = '\\';
					input.popFront();
					++index;
				}
			}
			else
				interpretEscapeSequence(input, index, buffer, i);
		}
		else if (input.front == quote)
		{
			if (style & TokenStyle.IncludeQuotes)
				buffer[i++] = quote;
			input.popFront();
			++index;
			break;
		}
		else
		{
			buffer[i++] = input.front;
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
			goto case 'c';
		case 'd':
			t.type = TokenType.DStringLiteral;
			goto case 'c';
		case 'c':
			if (style & TokenStyle.IncludeQuotes)
				buffer[i++] = input.front;
			input.popFront();
			++index;
			break;
		default:
			break;
		}
	}
	t.value = (cast(char[]) buffer[0..i]).idup;
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
	assert (lexString(c, i, l, TokenStyle.NotEscaped) == "abc\\ndef");
	auto d = `"12345"w`;
	assert (lexString(d, i, l).type == TokenType.WStringLiteral);
	auto e = `"abc"c`;
	assert (lexString(e, i, l).type == TokenType.StringLiteral);
	auto f = `"abc"d`;
	assert (lexString(f, i, l).type == TokenType.DStringLiteral);
	auto g = "\"a\nb\"";
	assert (lexString(g, i, l) == "a\nb");
}

Token lexDelimitedString(R)(ref R input, ref uint index,
	ref uint lineNumber, ref ubyte[] buffer,
	const TokenStyle stringStyle = TokenStyle.Default)
in
{
	assert(input.front == 'q');
}
body
{
	Token t;
	t.startIndex = index;
	t.lineNumber = lineNumber;
	t.type = TokenType.StringLiteral;
	size_t i;
	input.popFront(); // q
	input.popFront(); // "
	index += 2;
	if (stringStyle & TokenStyle.IncludeQuotes)
	{
		buffer[i++] = 'q';
		buffer[i++] = '"';
	}

	bool heredoc;
	ElementType!R open;
	ElementType!R close;

	switch (input.front)
	{
	case '[': open = '['; close = ']'; break;
	case '{': open = '{'; close = '}'; break;
	case '(': open = '('; close = ')'; break;
	case '<': open = '<'; close = '>'; break;
	default: heredoc = true; break;
	}

	if (heredoc)
	{
		auto hereOpen = appender!(ubyte[])();
		while (!input.isEoF() && !std.uni.isWhite(input.front))
		{
			hereOpen.put(input.front());
			input.popFront();
		}
		if (input.isNewline())
		{
			++lineNumber;
			popNewline(input, index, buffer, i);
		}
//		else
//			this is an error
		while (!input.isEoF())
		{
			if (isNewline(input))
			{
				++lineNumber;
				popNewline(input, index, buffer, i);
			}
			else if (input.front == '"' && buffer[0..i].endsWith(hereOpen.data))
			{
				buffer[i++] = '"';
				++index;
				input.popFront();
				if (stringStyle & TokenStyle.IncludeQuotes)
					t.value = (cast(char[]) buffer[0 .. i]).idup;
				else
					t.value = (cast(char[]) buffer[0 .. i - hereOpen.data.length - 1]).idup;
				break;
			}
			else
			{
				buffer[i++] = input.front;
				++index;
				input.popFront();
			}
		}
	}
	else
	{
		if (stringStyle & TokenStyle.IncludeQuotes)
			buffer[i++] = input.front;
		input.popFront();
		int depth = 1;
		while (depth > 0 && !input.isEoF())
		{
			if (isNewline(input))
				popNewline(input, index, buffer, i);
			else
			{
				if (input.front == close)
				{
					--depth;
					if (depth == 0)
					{
						if (stringStyle & TokenStyle.IncludeQuotes)
						{
							buffer[i++] = close;
							buffer[i++] = '"';
						}
						input.popFront();
						input.popFront();
						break;
					}
				}
				else if (input.front == open)
					++depth;
				buffer[i++] = input.front;
				input.popFront();
				++index;
			}
		}
	}
	if (!input.isEoF())
	{
		switch (input.front)
		{
		case 'w':
			t.type = TokenType.WStringLiteral;
			goto case 'c';
		case 'd':
			t.type = TokenType.DStringLiteral;
			goto case 'c';
		case 'c':
			if (stringStyle & TokenStyle.IncludeQuotes)
				buffer[i++] = input.front;
			input.popFront();
			++index;
			break;
		default:
			break;
		}
	}
	if (t.value is null)
		t.value = (cast(char[]) buffer[0 .. i]).idup;
	return t;
}

unittest
{
	uint i;
	uint l;
	auto a = `q"{abc{}de}"`;
	auto ar = lexDelimitedString(a, i, l);
	assert (ar == "abc{}de");
	assert (ar == TokenType.StringLiteral);

	auto b = "q\"abcde\n123\nabcde\"w";
	auto br = lexDelimitedString(b, i, l);
	assert (br == "123\n");
	assert (br == TokenType.WStringLiteral);

	auto c = `q"[<xml></xml>]");`;
	auto cr = lexDelimitedString(c, i, l, TokenStyle.Source);
	assert (cr == `q"[<xml></xml>]"`);
	assert (cr == TokenType.StringLiteral);
}

Token lexTokenString(R)(ref R input, ref uint index, ref uint lineNumber,
	ref ubyte[] buffer, const TokenStyle stringStyle = TokenStyle.Default)
in
{
	assert (input.front == 'q');
}
body
{
	Token t;
	t.startIndex = index;
	t.type = TokenType.StringLiteral;
	t.lineNumber = lineNumber;
	size_t i;
	input.popFront(); // q
	input.popFront(); // {
	index += 2;
	if (stringStyle & TokenStyle.IncludeQuotes)
	{
		buffer[i++] = 'q';
		buffer[i++] = '{';
	}
	auto r = byToken(input, "", IterationStyle.Everything, TokenStyle.Source);
	r.index = index;
	int depth = 1;
	while (!r.empty)
	{
		if (r.front.type == TokenType.LBrace)
		{
			++depth;
		}
		else if (r.front.type == TokenType.RBrace)
		{
			--depth;
			if (depth <= 0)
			{
				if (stringStyle & TokenStyle.IncludeQuotes)
					buffer[i++] = '}';
				r.popFront();
				break;
			}
		}
		buffer[i .. i + r.front.value.length] = cast(ubyte[]) r.front.value;
		i += r.front.value.length;
		r.popFront();
	}

	auto n = i - (stringStyle & TokenStyle.IncludeQuotes ? 2 : 0);
	input.popFrontN(n);
	if (!input.isEoF())
	{
		switch (input.front)
		{
		case 'w':
			t.type = TokenType.WStringLiteral;
			goto case 'c';
		case 'd':
			t.type = TokenType.DStringLiteral;
			goto case 'c';
		case 'c':
			if (stringStyle & TokenStyle.IncludeQuotes)
				buffer[i++] = input.front;
			input.popFront();
			++index;
			break;
		default:
			break;
		}
	}
	t.value = (cast(char[]) buffer[0 .. i]).idup;
	index = r.index;
	return t;
}

unittest
{
	import std.stdio;
	uint i;
	uint l;
	auto a = "q{import std.stdio;} abcd";
	auto ar = lexTokenString(a, i, l);
	assert (ar == TokenType.StringLiteral);
	assert (ar == "import std.stdio;");

	auto b = `q{writeln("hello world");}`;
	auto br = lexTokenString(b, i, l, TokenStyle.Source);
	assert (br == TokenType.StringLiteral);
	assert (br == `q{writeln("hello world");}`);
}

Token lexNumber(R)(ref R input, ref uint index, const uint lineNumber,
	ref ubyte[] buffer)
in
{
	assert(isDigit(input.front));
}
body
{
	size_t i;
	// hex and binary can start with zero, anything else is decimal
	if (input.front != '0')
		return lexDecimal(input, index, lineNumber, buffer, i);
	else
	{
		buffer[i++] = input.front;
		input.popFront();
		++index;
		switch (input.front)
		{
		case 'x':
		case 'X':
			buffer[i++] = input.front;
			input.popFront();
			++index;
			return lexHex(input, index, lineNumber, buffer, i);
		case 'b':
		case 'B':
			buffer[i++] = input.front;
			input.popFront();
			++index;
			return lexBinary(input, index, lineNumber, buffer, i);
		default:
			return lexDecimal(input, index, lineNumber, buffer, i);
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
	ref ubyte[] buffer, ref size_t i)
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
			buffer[i++] = input.front;
			input.popFront();
			++index;
			break;
		case 'u':
		case 'U':
			if (isUnsigned)
				break binaryLoop;
			buffer[i++] = input.front;
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
			buffer[i++] = input.front;
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
	token.value = (cast(char[]) buffer[0 .. i]).idup;
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
	ref ubyte[] buffer, ref size_t i)
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
			buffer[i++] = input.front;
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
			buffer[i++] = input.front;
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
			buffer[i++] = input.front;
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
			buffer[i++] = input.front;
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
			buffer[i++] = input.front;
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
			buffer[i++] = input.front;
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
			buffer[i++] = input.front;
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
				buffer[i++] = input.front;
				input.popFront();
				++index;
				token.type = TokenType.IRealLiteral;
			}
			else if (isFloat)
			{
				buffer[i++] = input.front;
				input.popFront();
				++index;
				token.type = TokenType.IFloatLiteral;
			}
			else if (isDouble)
			{
				buffer[i++] = input.front;
				input.popFront();
				++index;
				token.type = TokenType.IDoubleLiteral;
			}
			break decimalLoop;
		default:
			break decimalLoop;
		}
	}
	token.value = (cast(char[]) buffer[0 .. i]).idup;
	return token;
}


unittest
{
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
	ref ubyte[] buffer, ref size_t i)
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
			buffer[i++] = input.front;
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
			buffer[i++] = input.front;
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
			buffer[i++] = input.front;
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
			buffer[i++] = input.front;
			input.popFront();
			++index;
			foundDot = true;
			token.type = TokenType.DoubleLiteral;
			break;
		default:
			break hexLoop;
		}
	}
	token.value = (cast(char[]) buffer[0 .. i]).idup;
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

Token lexSpecialTokenSequence(R)(ref R input, ref uint index,
	ref uint lineNumber, ref ubyte[] buffer)
in
{
	assert (input.front == '#');
}
body
{
	input.popFront();
	Token t;

	// TODO: re-implement

	return t;
}

unittest
{
	uint i;
	uint l;
	auto a = "#line 10\n";
	auto ar = lexSpecialTokenSequence(a, i, l);
	assert (ar == "#line 10\n");
	assert (a == "");
	assert (l == 10);

	auto b = "#line 9201 \"test.d\"\n";
	auto br = lexSpecialTokenSequence(b, i, l);
	assert (l == 9201);
	assert (br == "#line 9201 \"test.d\"\n");
	assert (b == "");

	auto c = `#lin`;
	auto cr = lexSpecialTokenSequence(c, i, l);
	assert (l == 9201);
	assert (cr is null);
	assert (c == `#lin`);
}

pure nothrow bool isSeparating(ubyte ch)
{
	return (ch >= '!' && ch <= '/')
		|| (ch >= ':' && ch <= '@')
		|| (ch >= '[' && ch <= '^')
		|| (ch >= '{' && ch <= '~')
		|| ch == '`'
		|| ch == 0x20
		|| ch == 0x09
		|| ch == 0x0a;
}

pure nothrow TokenType lookupTokenType(const const(char)[] input)
{
	switch(input.length)
	{
	case 2:
		switch (input)
		{
		case "do": return TokenType.Do;
		case "if": return TokenType.If;
		case "in": return TokenType.In;
		case "is": return TokenType.Is;
		default: break;
		}
		break;
	case 3:
		switch (input)
		{
		case "asm": return TokenType.Asm;
		case "for": return TokenType.For;
		case "int": return TokenType.Int;
		case "new": return TokenType.New;
		case "out": return TokenType.Out;
		case "ref": return TokenType.Ref;
		case "try": return TokenType.Try;
		default: break;
		}
		break;
	case 4:
		switch (input)
		{
		case "auto": return TokenType.Auto;
		case "body": return TokenType.Body;
		case "bool": return TokenType.Bool;
		case "byte": return TokenType.Byte;
		case "case": return TokenType.Case;
		case "cast": return TokenType.Cast;
		case "cent": return TokenType.Cent;
		case "char": return TokenType.Char;
		case "else": return TokenType.Else;
		case "enum": return TokenType.Enum;
		case "goto": return TokenType.Goto;
		case "lazy": return TokenType.Lazy;
		case "long": return TokenType.Long;
		case "null": return TokenType.Null;
		case "pure": return TokenType.Pure;
		case "real": return TokenType.Real;
		case "this": return TokenType.This;
		case "true": return TokenType.True;
		case "uint": return TokenType.Uint;
		case "void": return TokenType.Void;
		case "with": return TokenType.With;
		default: break;
		}
		break;
	case 5:
		switch (input)
		{
		case "alias": return TokenType.Alias;
		case "align": return TokenType.Align;
		case "break": return TokenType.Break;
		case "catch": return TokenType.Catch;
		case "class": return TokenType.Class;
		case "const": return TokenType.Const;
		case "creal": return TokenType.Creal;
		case "dchar": return TokenType.Dchar;
		case "debug": return TokenType.Debug;
		case "false": return TokenType.False;
		case "final": return TokenType.Final;
		case "float": return TokenType.Float;
		case "inout": return TokenType.Inout;
		case "ireal": return TokenType.Ireal;
		case "macro": return TokenType.Macro;
		case "mixin": return TokenType.Mixin;
		case "scope": return TokenType.Scope;
		case "short": return TokenType.Short;
		case "super": return TokenType.Super;
		case "throw": return TokenType.Throw;
		case "ubyte": return TokenType.Ubyte;
		case "ucent": return TokenType.Ucent;
		case "ulong": return TokenType.Ulong;
		case "union": return TokenType.Union;
		case "wchar": return TokenType.Wchar;
		case "while": return TokenType.While;
		default: break;
		}
		break;
	case 6:
		switch (input)
		{
		case "assert": return TokenType.Assert;
		case "cfloat": return TokenType.Cfloat;
		case "delete": return TokenType.Delete;
		case "double": return TokenType.Double;
		case "export": return TokenType.Export;
		case "extern": return TokenType.Extern;
		case "ifloat": return TokenType.Ifloat;
		case "import": return TokenType.Import;
		case "module": return TokenType.Module;
		case "pragma": return TokenType.Pragma;
		case "public": return TokenType.Public;
		case "return": return TokenType.Return;
		case "shared": return TokenType.Shared;
		case "static": return TokenType.Static;
		case "string": return TokenType.String;
		case "struct": return TokenType.Struct;
		case "switch": return TokenType.Switch;
		case "typeid": return TokenType.Typeid;
		case "typeof": return TokenType.Typeof;
		case "ushort": return TokenType.Ushort;
		default: break;
		}
		break;
	case 7:
		switch (input)
		{
		case "__EOF__": return TokenType.EOF;
		case "cdouble": return TokenType.Cdouble;
		case "default": return TokenType.Default;
		case "dstring": return TokenType.DString;
		case "finally": return TokenType.Finally;
		case "foreach": return TokenType.Foreach;
		case "idouble": return TokenType.Idouble;
		case "nothrow": return TokenType.Nothrow;
		case "package": return TokenType.Package;
		case "private": return TokenType.Private;
		case "typedef": return TokenType.Typedef;
		case "version": return TokenType.Version;
		case "wstring": return TokenType.WString;
		default: break;
		}
		break;
	case 8:
		switch (input)
		{
		case "override": return TokenType.Override;
		case "continue": return TokenType.Continue;
		case "__LINE__": return TokenType.Line;
		case "template": return TokenType.Template;
		case "abstract": return TokenType.Abstract;
		case "__traits": return TokenType.Traits;
		case "volatile": return TokenType.Volatile;
		case "delegate": return TokenType.Delegate;
		case "function": return TokenType.Function;
		case "unittest": return TokenType.Unittest;
		case "__FILE__": return TokenType.File;
		case "__DATE__": return TokenType.Date;
		case "__TIME__": return TokenType.Date;
		default: break;
		}
		break;
	case 9:
		switch (input)
		{
		case "__gshared": return TokenType.Gshared;
		case "immutable": return TokenType.Immutable;
		case "interface": return TokenType.Interface;
		case "invariant": return TokenType.Invariant;
		case "protected": return TokenType.Protected;
		default: break;
		}
		break;
	case 10:
		switch (input)
		{
		case "deprecated": return TokenType.Deprecated;
		case "__VENDOR__": return TokenType.Vendor;
		default: break;
		}
		break;
	case 11:
		if (input == "__VERSION__")
			return TokenType.CompilerVersion;
		break;
	case 12:
		if (input == "synchronized")
			return TokenType.Synchronized;
		break;
	case 13:
		if (input == "__TIMESTAMP__")
			return TokenType.Timestamp;
		break;
	case 15:
		if (input == "foreach_reverse")
			return TokenType.Foreach_reverse;
		break;
	default: break;
	}
	return TokenType.Identifier;
}

class Trie(K, V) if (isInputRange!K): TrieNode!(K, V)
{
	/**
	* Adds the given value to the trie with the given key
	*/
	void add(K key, V value) pure
	{
		TrieNode!(K,V) current = this;
		foreach(keyPart; key)
		{
			if ((keyPart in current.children) is null)
			{
				auto node = new TrieNode!(K, V);
				current.children[keyPart] = node;
				current = node;
			}
			else
				current = current.children[keyPart];
		}
		current.value = value;
	}
}

class TrieNode(K, V) if (isInputRange!K)
{
	V value;
	TrieNode!(K,V)[ElementType!K] children;
}

string printCaseStatements(K, V)(TrieNode!(K,V) node, string indentString)
{
	string caseStatement = "";
	foreach(dchar k, TrieNode!(K,V) v; node.children)
	{
		caseStatement ~= indentString;
		caseStatement ~= "case '";
		caseStatement ~= k;
		caseStatement ~= "':\n";
		if (indentString == "")
		{
			caseStatement ~= indentString;
			caseStatement ~= "\tsize_t i = 0;\n";
		}
		caseStatement ~= indentString;
		caseStatement ~= "\tbuffer[i++] = '";
		caseStatement ~= k;
		caseStatement ~= "';\n";
		caseStatement ~= indentString;
		caseStatement ~= "\t++index;\n";
		caseStatement ~= indentString;
		caseStatement ~= "\trange.popFront();\n";
		if (v.children.length > 0)
		{
			caseStatement ~= indentString;
			caseStatement ~= "\tif (range.isEoF())\n";
			caseStatement ~= indentString;
			caseStatement ~= "\t{\n";
			caseStatement ~= indentString;
			caseStatement ~= "\tcurrent.value = getTokenValue(current.type);\n";
			caseStatement ~= indentString;
			caseStatement ~= "\t\tcurrent.type = " ~ node.children[k].value;
			caseStatement ~= ";\n";
			caseStatement ~= indentString;
			caseStatement ~= "\t\tbreak;\n";
			caseStatement ~= indentString;
			caseStatement ~= "\t}\n";
			caseStatement ~= indentString;
			caseStatement ~= "\tswitch (range.front)\n";
			caseStatement ~= indentString;
			caseStatement ~= "\t{\n";
			caseStatement ~= printCaseStatements(v, indentString ~ "\t");
			caseStatement ~= indentString;
			caseStatement ~= "\tdefault:\n";
			caseStatement ~= indentString;
			caseStatement ~= "\t\tcurrent.type = ";
			caseStatement ~= v.value;
			caseStatement ~= ";\n";
			caseStatement ~= indentString;
			caseStatement ~= "\tcurrent.value = getTokenValue(current.type);\n";
			caseStatement ~= indentString;
			caseStatement ~= "\t\tbreak;\n";
			caseStatement ~= indentString;
			caseStatement ~= "\t}\n";
			caseStatement ~= indentString;
			caseStatement ~= "\tbreak;\n";
		}
		else
		{
			caseStatement ~= indentString;
			caseStatement ~= "\tcurrent.type = ";
			caseStatement ~= v.value;
			caseStatement ~= ";\n";
			caseStatement ~= indentString;
			caseStatement ~= "\tcurrent.value = getTokenValue(current.type);\n";
			caseStatement ~= indentString;
			caseStatement ~= "\tbreak;\n";
		}
	}
	return caseStatement;
}

string generateCaseTrie(string[] args ...)
{
	auto t = new Trie!(string, string);
	for(int i = 0; i < args.length; i+=2)
	{
		t.add(args[i], args[i+1]);
	}
	return printCaseStatements(t, "");
}

//void main(string[] args) {}
