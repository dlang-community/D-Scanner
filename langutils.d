//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module langutils;

/**
 * Returns: true if input is a access attribute
 */
pure nothrow bool isAccessAttribute(TokenType input)
{
	return input > TokenType.PROTECTION_BEGIN && input < TokenType.PROTECTION_END;
}

/**
 * See_also: isAttribute(TokenType)
 */
pure nothrow bool isAttribute(ref const Token token)
{
	return isAttribute(token.type);
}

/**
 * Returns: true if the given token type is an attribute, false otherwise
 */
pure nothrow bool isAttribute(TokenType input)
{
	if (isAccessAttribute(input))
		return true;
	return input > TokenType.ATTRIBUTES_BEGIN && input < TokenType.ATTRIBUTES_END;
}

/**
 * Returns: the token type for the given string. Defaults to "identifier"
 */
pure nothrow TokenType lookupTokenType(const string input)
{
	immutable(TokenType)* type = input in tokenLookup;
	if (type !is null)
		return *type;
	else
		return TokenType.Identifier;
}


/**
 * Listing of all the tokens in the D language
 */
enum TokenType: uint
{
// Operators
	OPERATORS_BEGIN,
	Assign,	/// =
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
	GoesTo, // =>
	Greater,	/// >
	GreaterEqual,	/// >=
	Hash, // #
	Increment,	/// ++
	LBrace,	/// {
	LBracket,	/// [
	Less,	/// <
	LessEqual,	/// <=
	LessEqualGreater, // <>=
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
	Slice, // ..
	Star,	/// *
	Ternary,	/// ?
	Tilde,	/// ~
	Unordered,	/// !<>=
	UnsignedShiftRight,	/// >>>
	UnsignedShiftRightEqual,	/// >>>=
	Vararg,	/// ...
	Xor,	/// ^
	XorEquals,	/// ^=
	OPERATORS_END,

	// Types
	TYPES_BEGIN,
	Bool, /// bool,
	Byte, /// byte,
	Cdouble, /// cdouble,
	Cent, /// cent,
	Cfloat, /// cfloat,
	Char, /// char,
	Creal, /// creal,
	Dchar, /// dchar,
	Double, /// double,
	DString, /// dstring
	Float, /// float,
	Function, /// function,
	Idouble, /// idouble,
	Ifloat, /// ifloat,
	Int, /// int,
	Ireal, /// ireal,
	Long, /// long,
	Real, /// real,
	Short, /// short,
	String, /// string
	Ubyte, /// ubyte,
	Ucent, /// ucent,
	Uint, /// uint,
	Ulong, /// ulong,
	Ushort, /// ushort,
	Void, /// void,
	Wchar, /// wchar,
	WString, /// wstring
	TYPES_END,
	Template, /// template,

	// Keywords
	KEYWORDS_BEGIN,
	ATTRIBUTES_BEGIN,
		Align, /// align,
		Deprecated, /// deprecated,
		Extern, /// extern,
		Pragma, /// pragma,
		PROTECTION_BEGIN,
			Export, /// export,
			Package, /// package,
			Private, /// private,
			Protected, /// protected,
			Public, /// public,
		PROTECTION_END,
		Abstract, /// abstract,
		AtDisable, /// @disable
		Auto, /// auto,
		Const, /// const,
		Final, /// final
		Gshared, /// __gshared,
		Immutable, // immutable,
		Inout, // inout,
		Scope, /// scope,
		Shared, // shared,
		Static, /// static,
		Synchronized, /// synchronized,
	ATTRIBUTES_END,
	Alias, /// alias,
	Asm, /// asm,
	Assert, /// assert,
	Body, /// body,
	Break, /// break,
	Case, /// case,
	Cast, /// cast,
	Catch, /// catch,
	Class, /// class,
	Continue, /// continue,
	Debug, /// debug,
	Default, /// default,
	Delegate, /// delegate,
	Delete, /// delete,
	Do, /// do,
	Else, /// else,
	Enum, /// enum,
	False, /// false,
	Finally, /// finally,
	Foreach, /// foreach,
	Foreach_reverse, /// foreach_reverse,
	For, /// for,
	Goto, /// goto,
	If, /// if ,
	Import, /// import,
	In, /// in,
	Interface, /// interface,
	Invariant, /// invariant,
	Is, /// is,
	Lazy, /// lazy,
	Macro, /// macro,
	Mixin, /// mixin,
	Module, /// module,
	New, /// new,
	Nothrow, /// nothrow,
	Null, /// null,
	Out, /// out,
	Override, /// override,
	Pure, /// pure,
	Ref, /// ref,
	Return, /// return,
	Struct, /// struct,
	Super, /// super,
	Switch, /// switch ,
	This, /// this,
	Throw, /// throw,
	True, /// true,
	Try, /// try,
	Typedef, /// typedef,
	Typeid, /// typeid,
	Typeof, /// typeof,
	Union, /// union,
	Unittest, /// unittest,
	Version, /// version,
	Volatile, /// volatile,
	While, /// while ,
	With, /// with,
	KEYWORDS_END,

// Constants
	CONSTANTS_BEGIN,
	File, /// __FILE__,
	Line, /// __LINE__,
	Thread, /// __thread,
	Traits, /// __traits,
	CONSTANTS_END,

// Properties
	PROPERTIES_BEGIN,
	AtProperty, /// @property
	AtSafe, /// @safe
	AtSystem, /// @system
	AtTrusted, /// @trusted
	PROPERTIES_END,

// Misc
	MISC_BEGIN,
	Blank, /// unknown token type
	Comment, /// /** comment */ or // comment or ///comment
	Identifier, /// anything else
	ScriptLine, // Line at the beginning of source file that starts from #!
	Whitespace, /// whitespace
	NUMBERS_BEGIN,
	DoubleLiteral, /// 123.456
	FloatLiteral, /// 123.456f or 0x123_45p-af
	IntLiteral, /// 123 or 0b1101010101
	LongLiteral, /// 123L
	RealLiteral, /// 123.456L
	UnsignedIntLiteral, /// 123u
	UnsignedLongLiteral, /// 123uL
	NUMBERS_END,
	STRINGS_BEGIN,
	DStringLiteral, /// "32-bit character string"d
	StringLiteral, /// "a string"
	WStringLiteral, /// "16-bit character string"w
	STRINGS_END,
	MISC_END,
}


/**
 * lookup table for converting strings to tokens
 */
immutable TokenType[string] tokenLookup;


static this()
{
	tokenLookup = [
		"abstract" : TokenType.Abstract,
		"alias" : TokenType.Alias,
		"align" : TokenType.Align,
		"asm" : TokenType.Asm,
		"assert" : TokenType.Assert,
		"auto" : TokenType.Auto,
		"body" : TokenType.Body,
		"bool" : TokenType.Bool,
		"break" : TokenType.Break,
		"byte" : TokenType.Byte,
		"case" : TokenType.Case,
		"cast" : TokenType.Cast,
		"catch" : TokenType.Catch,
		"cdouble" : TokenType.Cdouble,
		"cent" : TokenType.Cent,
		"cfloat" : TokenType.Cfloat,
		"char" : TokenType.Char,
		"class" : TokenType.Class,
		"const" : TokenType.Const,
		"continue" : TokenType.Continue,
		"creal" : TokenType.Creal,
		"dchar" : TokenType.Dchar,
		"debug" : TokenType.Debug,
		"default" : TokenType.Default,
		"delegate" : TokenType.Delegate,
		"delete" : TokenType.Delete,
		"deprecated" : TokenType.Deprecated,
		"@disable" : TokenType.AtDisable,
		"do" : TokenType.Do,
		"double" : TokenType.Double,
		"dstring" : TokenType.DString,
		"else" : TokenType.Else,
		"enum" : TokenType.Enum,
		"export" : TokenType.Export,
		"extern" : TokenType.Extern,
		"false" : TokenType.False,
		"__FILE__" : TokenType.File,
		"finally" : TokenType.Finally,
		"final" : TokenType.Final,
		"float" : TokenType.Float,
		"foreach_reverse" : TokenType.Foreach_reverse,
		"foreach" : TokenType.Foreach,
		"for" : TokenType.For,
		"function" : TokenType.Function,
		"goto" : TokenType.Goto,
		"__gshared" : TokenType.Gshared,
		"idouble" : TokenType.Idouble,
		"ifloat" : TokenType.Ifloat,
		"if" : TokenType.If,
		"immutable" : TokenType.Immutable,
		"import" : TokenType.Import,
		"inout" : TokenType.Inout,
		"interface" : TokenType.Interface,
		"in" : TokenType.In,
		"int" : TokenType.Int,
		"invariant" : TokenType.Invariant,
		"ireal" : TokenType.Ireal,
		"is" : TokenType.Is,
		"lazy" : TokenType.Lazy,
		"__LINE__" : TokenType.Line,
		"long" : TokenType.Long,
		"macro" : TokenType.Macro,
		"mixin" : TokenType.Mixin,
		"module" : TokenType.Module,
		"new" : TokenType.New,
		"nothrow" : TokenType.Nothrow,
		"null" : TokenType.Null,
		"out" : TokenType.Out,
		"override" : TokenType.Override,
		"package" : TokenType.Package,
		"pragma" : TokenType.Pragma,
		"private" : TokenType.Private,
		"@property" : TokenType.AtProperty,
		"protected" : TokenType.Protected,
		"public" : TokenType.Public,
		"pure" : TokenType.Pure,
		"real" : TokenType.Real,
		"ref" : TokenType.Ref,
		"return" : TokenType.Return,
		"@safe" : TokenType.AtSafe,
		"scope" : TokenType.Scope,
		"shared" : TokenType.Shared,
		"short" : TokenType.Short,
		"static" : TokenType.Static,
		"string" : TokenType.String,
		"struct" : TokenType.Struct,
		"super" : TokenType.Super,
		"switch" : TokenType.Switch,
		"synchronized" : TokenType.Synchronized,
		"@system" : TokenType.AtSystem,
		"template" : TokenType.Template,
		"this" : TokenType.This,
		"__thread" : TokenType.Thread,
		"throw" : TokenType.Throw,
		"__traits" : TokenType.Traits,
		"true" : TokenType.True,
		"@trusted" : TokenType.AtTrusted,
		"try" : TokenType.Try,
		"typedef" : TokenType.Typedef,
		"typeid" : TokenType.Typeid,
		"typeof" : TokenType.Typeof,
		"ubyte" : TokenType.Ubyte,
		"ucent" : TokenType.Ucent,
		"uint" : TokenType.Uint,
		"ulong" : TokenType.Ulong,
		"union" : TokenType.Union,
		"unittest" : TokenType.Unittest,
		"ushort" : TokenType.Ushort,
		"version" : TokenType.Version,
		"void" : TokenType.Void,
		"volatile" : TokenType.Volatile,
		"wchar" : TokenType.Wchar,
		"while" : TokenType.While,
		"with" : TokenType.With,
		"wstring" : TokenType.WString,
	];
}

struct Token
{
	TokenType type;
	string value;
	uint lineNumber;
	size_t startIndex;
	bool opEquals(ref const(Token) other) const
	{
		return other.type == type && other.value == value;
	}
	bool opEquals(string range) const { return range == value; }
	bool opEquals(TokenType t) const { return type == t; }
	int opCmp(size_t i) const
	{
		if (i > startIndex) return -1;
		if (i < startIndex) return 1;
		return 0;
	}
}
