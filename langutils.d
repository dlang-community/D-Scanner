
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
		return TokenType.identifier;
}


/**
 * Listing of all the tokens in the D language
 */
enum TokenType: uint
{
// Operators
	OPERATORS_BEGIN,
	div,	/// /
	divEquals,	/// /=
	dot,	/// .
	slice, // ..
	vararg,	/// ...
	bitAnd,	/// &
	bitAndEquals,	/// &=
	lAnd,	/// &&
	bitOr,	/// |
	bitOrEquals,	/// |=
	lOr,	/// ||
	minus,	/// -
	minusEquals,	/// -=
	uMinus,	/// --
	plus,	/// +
	plusEquals,	/// +=
	uPlus,	/// ++
	less,	/// <
	lessEqual,	/// <=
	shiftLeft,	/// <<
	shiftLeftEqual,	/// <<=
	lessOrGreater,	/// <>
	lessEqualGreater, // <>=
	greater,	/// >
	greaterEqual,	/// >=
	shiftRightEqual,	/// >>=
	unsignedShiftRightEqual,	/// >>>=
	shiftRight,	/// >>
	unsignedShiftRight,	/// >>>
	not,	/// !
	notEquals,	/// !=
	notLessEqualGreater,	/// !<>
	unordered,	/// !<>=
	notLess,	/// !<
	notLessEqual,	/// !<=
	notGreater,	/// !>
	notGreaterEqual,	/// !>=
	lParen,	/// $(LPAREN)
	rParen,	/// $(RPAREN)
	lBracket,	/// [
	rBracket,	/// ]
	lBrace,	/// {
	rBrace,	/// }
	ternary,	/// ?
	comma,	/// ,
	semicolon,	/// ;
	colon,	/// :
	dollar,	/// $
	assign,	/// =
	equals,	/// ==
	star,	/// *
	mulEquals,	/// *=
	mod,	/// %
	modEquals,	/// %=
	xor,	/// ^
	xorEquals,	/// ^=
	pow,	/// ^^
	powEquals,	/// ^^=
	tilde,	/// ~
	catEquals,	/// ~=
	hash, // #
	goesTo, // =>
	OPERATORS_END,

// Types
	TYPES_BEGIN,
	tString, /// string
	tBool, /// bool,
	tByte, /// byte,
	tCdouble, /// cdouble,
	tCent, /// cent,
	tCfloat, /// cfloat,
	tChar, /// char,
	tCreal, /// creal,
	tDchar, /// dchar,
	tDouble, /// double,
	tFloat, /// float,
	tUbyte, /// ubyte,
	tUcent, /// ucent,
	tUint, /// uint,
	tUlong, /// ulong,
	tShort, /// short,
	tReal, /// real,
	tLong, /// long,
	tInt, /// int,
	tFunction, /// function,
	tIdouble, /// idouble,
	tIreal, /// ireal,
	tWchar, /// wchar,
	tVoid, /// void,
	tUshort, /// ushort,
	tIfloat, /// if loat,
	TYPES_END,
	tTemplate, /// template,

// Keywords
	KEYWORDS_BEGIN,
	ATTRIBUTES_BEGIN,
		tExtern, /// extern,
		tAlign, /// align,
		tPragma, /// pragma,
		tDeprecated, /// deprecated,
		PROTECTION_BEGIN,
			tPackage, /// package,
			tPrivate, /// private,
			tProtected, /// protected,
			tPublic, /// public,
			tExport, /// export,
		PROTECTION_END,
		tStatic, /// static,
		tSynchronized, /// synchronized,
		tFinal, /// final
		tAbstract, /// abstract,
		tConst, /// const,
		tAuto, /// auto,
		tScope, /// scope,
		t__gshared, /// __gshared,
		tShared, // shared,
		tImmutable, // immutable,
		tInout, // inout,
		atDisable, /// @disable
	ATTRIBUTES_END,
	tAlias, /// alias,
	tAsm, /// asm,
	tAssert, /// assert,
	tBody, /// body,
	tBreak, /// break,
	tCase, /// case,
	tCast, /// cast,
	tCatch, /// catch,
	tClass, /// class,
	tContinue, /// continue,
	tDebug, /// debug,
	tDefault, /// default,
	tDelegate, /// delegate,
	tDelete, /// delete,
	tDo, /// do,
	tElse, /// else,
	tEnum, /// enum,
	tFalse, /// false,
	tFinally, /// finally,
	tFor, /// for,
	tForeach, /// foreach,
	tForeach_reverse, /// foreach_reverse,
	tGoto, /// goto,
	tIf, /// if ,
	tImport, /// import,
	tIn, /// in,
	tInterface, /// interface,
	tInvariant, /// invariant,
	tIs, /// is,
	tLazy, /// lazy,
	tMacro, /// macro,
	tMixin, /// mixin,
	tModule, /// module,
	tNew, /// new,
	tNothrow, /// nothrow,
	tNull, /// null,
	tOut, /// out,
	tOverride, /// override,
	tPure, /// pure,
	tRef, /// ref,
	tReturn, /// return,
	tStruct, /// struct,
	tSuper, /// super,
	tSwitch, /// switch ,
	tThis, /// this,
	tThrow, /// throw,
	tTrue, /// true,
	tTry, /// try,
	tTypedef, /// typedef,
	tTypeid, /// typeid,
	tTypeof, /// typeof,
	tUnion, /// union,
	tUnittest, /// unittest,
	tVersion, /// version,
	tVolatile, /// volatile,
	tWhile, /// while ,
	tWith, /// with,
	KEYWORDS_END,

// Constants
	CONSTANTS_BEGIN,
	t__FILE__, /// __FILE__,
	t__LINE__, /// __LINE__,

	t__thread, /// __thread,
	t__traits, /// __traits,
	CONSTANTS_END,

// Properties
	PROPERTIES_BEGIN,

	atProperty, /// @property
	atSafe, /// @safe
	atSystem, /// @system
	atTrusted, /// @trusted
	PROPERTIES_END,

// Misc
	MISC_BEGIN,
	comment, /// /** comment */ or // comment or ///comment
	NUMBERS_BEGIN,
	floatLiteral, /// 123.456f or 0x123_45p-af
	doubleLiteral, /// 123.456
	realLiteral, /// 123.456L
	intLiteral, /// 123 or 0b1101010101
	unsignedIntLiteral, /// 123u
	longLiteral, /// 123L
	unsignedLongLiteral, /// 123uL
	NUMBERS_END,
	stringLiteral, /// "a string"
	identifier, /// anything else
	whitespace, /// whitespace
	blank, /// unknown token type
	MISC_END,
}


/**
 * lookup table for converting strings to tokens
 */
immutable TokenType[string] tokenLookup;


static this()
{
	tokenLookup = [
		"abstract" : TokenType.tAbstract,
		"alias" : TokenType.tAlias,
		"align" : TokenType.tAlign,
		"asm" : TokenType.tAsm,
		"assert" : TokenType.tAssert,
		"auto" : TokenType.tAuto,
		"body" : TokenType.tBody,
		"bool" : TokenType.tBool,
		"break" : TokenType.tBreak,
		"byte" : TokenType.tByte,
		"case" : TokenType.tCase,
		"cast" : TokenType.tCast,
		"catch" : TokenType.tCatch,
		"cdouble" : TokenType.tCdouble,
		"cent" : TokenType.tCent,
		"cfloat" : TokenType.tCfloat,
		"char" : TokenType.tChar,
		"class" : TokenType.tClass,
		"const" : TokenType.tConst,
		"continue" : TokenType.tContinue,
		"creal" : TokenType.tCreal,
		"dchar" : TokenType.tDchar,
		"debug" : TokenType.tDebug,
		"default" : TokenType.tDefault,
		"delegate" : TokenType.tDelegate,
		"delete" : TokenType.tDelete,
		"deprecated" : TokenType.tDeprecated,
		"do" : TokenType.tDo,
		"double" : TokenType.tDouble,
		"else" : TokenType.tElse,
		"enum" : TokenType.tEnum,
		"export" : TokenType.tExport,
		"extern" : TokenType.tExtern,
		"false" : TokenType.tFalse,
		"final" : TokenType.tFinal,
		"finally" : TokenType.tFinally,
		"float" : TokenType.tFloat,
		"for" : TokenType.tFor,
		"foreach" : TokenType.tForeach,
		"foreach_reverse" : TokenType.tForeach_reverse,
		"function" : TokenType.tFunction,
		"goto" : TokenType.tGoto,
		"idouble" : TokenType.tIdouble,
		"if" : TokenType.tIf,
		"ifloat" : TokenType.tIfloat,
		"immutable" : TokenType.tImmutable,
		"import" : TokenType.tImport,
		"in" : TokenType.tIn,
		"inout" : TokenType.tInout,
		"int" : TokenType.tInt,
		"interface" : TokenType.tInterface,
		"invariant" : TokenType.tInvariant,
		"ireal" : TokenType.tIreal,
		"is" : TokenType.tIs,
		"lazy" : TokenType.tLazy,
		"long" : TokenType.tLong,
		"macro" : TokenType.tMacro,
		"mixin" : TokenType.tMixin,
		"module" : TokenType.tModule,
		"new" : TokenType.tNew,
		"nothrow" : TokenType.tNothrow,
		"null" : TokenType.tNull,
		"out" : TokenType.tOut,
		"override" : TokenType.tOverride,
		"package" : TokenType.tPackage,
		"pragma" : TokenType.tPragma,
		"private" : TokenType.tPrivate,
		"protected" : TokenType.tProtected,
		"public" : TokenType.tPublic,
		"pure" : TokenType.tPure,
		"real" : TokenType.tReal,
		"ref" : TokenType.tRef,
		"return" : TokenType.tReturn,
		"scope" : TokenType.tScope,
		"shared" : TokenType.tShared,
		"short" : TokenType.tShort,
		"static" : TokenType.tStatic,
		"struct" : TokenType.tStruct,
		"string" : TokenType.tString,
		"super" : TokenType.tSuper,
		"switch" : TokenType.tSwitch,
		"synchronized" : TokenType.tSynchronized,
		"template" : TokenType.tTemplate,
		"this" : TokenType.tThis,
		"throw" : TokenType.tThrow,
		"true" : TokenType.tTrue,
		"try" : TokenType.tTry,
		"typedef" : TokenType.tTypedef,
		"typeid" : TokenType.tTypeid,
		"typeof" : TokenType.tTypeof,
		"ubyte" : TokenType.tUbyte,
		"ucent" : TokenType.tUcent,
		"uint" : TokenType.tUint,
		"ulong" : TokenType.tUlong,
		"union" : TokenType.tUnion,
		"unittest" : TokenType.tUnittest,
		"ushort" : TokenType.tUshort,
		"version" : TokenType.tVersion,
		"void" : TokenType.tVoid,
		"volatile" : TokenType.tVolatile,
		"wchar" : TokenType.tWchar,
		"while" : TokenType.tWhile,
		"with" : TokenType.tWith,
		"__FILE__" : TokenType.t__FILE__,
		"__LINE__" : TokenType.t__LINE__,
		"__gshared" : TokenType.t__gshared,
		"__thread" : TokenType.t__thread,
		"__traits" : TokenType.t__traits,
		"@disable" : TokenType.atDisable,
		"@property" : TokenType.atProperty,
		"@safe" : TokenType.atSafe,
		"@system" : TokenType.atSystem,
		"@trusted" : TokenType.atTrusted,
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
