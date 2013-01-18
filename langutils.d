//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module langutils;
import std.array;


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

string combineTokens(ref const Token[] tokens)
{
	auto app = appender!string();
	foreach (t; tokens)
		app.put(t.value);
	return app.data;
}

pure nothrow TokenType lookupTokenTypeOptimized(const string input)
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
		case "@safe": return TokenType.AtSafe;
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
		case "@system": return TokenType.AtSystem;
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
		case "__thread": return TokenType.Thread;
		case "__traits": return TokenType.Traits;
		case "volatile": return TokenType.Volatile;
		case "@trusted": return TokenType.AtTrusted;
		case "delegate": return TokenType.Delegate;
		case "@disable": return TokenType.AtDisable;
		case "function": return TokenType.Function;
		case "unittest": return TokenType.Unittest;
		case "__FILE__": return TokenType.File;
		default: break;
		}
		break;
	case 9:
		switch (input)
		{
		case "__gshared": return TokenType.Gshared;
		case "@property": return TokenType.AtProperty;
		case "immutable": return TokenType.Immutable;
		case "interface": return TokenType.Interface;
		case "invariant": return TokenType.Invariant;
		case "protected": return TokenType.Protected;
		default: break;
		}
		break;
	case 10:
		if (input == "deprecated")
			return TokenType.Deprecated;
		break;
	case 11:
		if (input == "synchronized")
			return TokenType.Synchronized;
		break;
	case 13:
		if (input == "foreach_reverse")
			return TokenType.Foreach_reverse;
		break;
	default: break;
	}
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
    IDoubleLiteral, /// 123.456i
	IFloatLiteral, /// 123.456fi
	IntLiteral, /// 123 or 0b1101010101
	LongLiteral, /// 123L
	RealLiteral, /// 123.456L
	IRealLiteral, /// 123.456Li
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

pure string getTypeFromToken(ref const Token t)
{
	switch (t.type)
	{

	case TokenType.DoubleLiteral:
		return "double";
	case TokenType.FloatLiteral:
		return "float";
	case TokenType.IntLiteral:
		return "int";
	case TokenType.RealLiteral:
		return "real";
	case TokenType.UnsignedIntLiteral:
		return "uint";
	case TokenType.UnsignedLongLiteral:
		return "ulong";
	case TokenType.LongLiteral:
		return "long";
	case TokenType.DStringLiteral:
		return "dstring";
	case TokenType.StringLiteral:
		return "string";
	case TokenType.WStringLiteral:
		return "wstring";
	default:
		return null;
	}
}

pure bool isIdentifierOrType(ref const Token t)
{
	return t.type == TokenType.Identifier || (t.type > TokenType.TYPES_BEGIN
		&& TokenType.TYPES_END);
}

/**
 * Token structure
 */
struct Token
{
	/// The token type
	TokenType type;

	/// The representation of the token in the original source code
	string value;

	/// The number of the line the token is on
	uint lineNumber;

	/// The character index of the start of the token in the original text
	uint startIndex;

	/**
	 * Check to see if the token is of the same type and has the same string
	 * representation as the given token
	 */
	bool opEquals(ref const(Token) other) const
	{
		return other.type == type && other.value == value;
	}

	/**
	 * Checks to see if the token's string representation is equal to the given
	 * string
	 */
	bool opEquals(string range) const { return range == value; }

	/**
	 * Checks to see if the token is of the given type
	 */
	bool opEquals(TokenType t) const { return type == t; }

	/**
	 * Comparison operator orders by start index
	 */
	int opCmp(size_t i) const
	{
		if (startIndex < i) return -1;
		if (startIndex > i) return 1;
		return 0;
	}
}
