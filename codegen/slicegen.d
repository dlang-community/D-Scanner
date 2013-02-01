import std.stdio;
import std.algorithm;

string[] opkwds = [
    "=", // Assign
	"@", // At
	"&", // BitAnd
	"&=", // BitAndEquals
	"|", // BitOr
	"|=", // BitOrEquals
	"~=", // CatEquals
	":", // Colon
	",", // Comma
	"--", // Decrement
	"/", // Div
	"/=", // DivEquals
	"$", // Dollar
	".", // Dot
	"==", // Equals
	"=>", // GoesTo
	">", // Greater
	">=", // GreaterEqual
	"#", // Hash
	"++", // Increment
	"{", // LBrace
	"[", // LBracket
	"<", // Less
	"<=", // LessEqual
	"<>=", // LessEqualGreater
	"<>", // LessOrGreater
	"&&", // LogicAnd
	"||", // LogicOr
	"(", // LParen
	"-", // Minus
	"-=", // MinusEquals
	"%", // Mod
	"%=", // ModEquals
	"*=", // MulEquals
	"!", // Not
	"!=", // NotEquals
	"!>", // NotGreater
	"!>=", // NotGreaterEqual
	"!<", // NotLess
	"!<=", // NotLessEqual
	"!<>", // NotLessEqualGreater
	"+", // Plus
	"+=", // PlusEquals
	"^^", // Pow
	"^^=", // PowEquals
	"}", // RBrace
	"]", // RBracket
	")", // RParen
	";", // Semicolon
	"<<", // ShiftLeft
	"<<=", // ShiftLeftEqual
	">>", // ShiftRight
	">>=", // ShiftRightEqual
	"..", // Slice
	"*", // Star
	"?", // Ternary
	"~", // Tilde
	"!<>=", // Unordered
	">>>", // UnsignedShiftRight
	">>>=", // UnsignedShiftRightEqual
	"...", // Vararg
	"^", // Xor
	"^=", // XorEquals
    "bool",
    "byte",
    "cdouble",
    "cent",
    "cfloat",
    "char",
    "creal",
    "dchar",
    "double",
    "dstring",
    "float",
    "function",
    "idouble",
    "ifloat",
    "int",
    "ireal",
    "long",
    "real",
    "short",
    "string",
    "ubyte",
    "ucent",
    "uint",
    "ulong",
    "ushort",
    "void",
    "wchar",
    "wstring",
    "align",
    "deprecated",
    "extern",
    "pragma",
    "export",
    "package",
    "private",
    "protected",
    "public",
    "abstract",
    "auto",
    "const",
    "final",
    "__gshared",
    "immutable",
    "inout",
    "scope",
    "shared",
    "static",
    "synchronized",
    "alias",
    "asm",
    "assert",
    "body",
    "break",
    "case",
    "cast",
    "catch",
    "class",
    "continue",
    "debug",
    "default",
    "delegate",
    "delete",
    "do",
    "else",
    "enum",
    "false",
    "finally",
    "foreach",
    "foreach_reverse",
    "for",
    "goto",
    "if",
    "import",
    "in",
    "interface",
    "invariant",
    "is",
    "lazy",
    "macro",
    "mixin",
    "module",
    "new",
    "nothrow",
    "null",
    "out",
    "override",
    "pure",
    "ref",
    "return",
    "struct",
    "super",
    "switch",
    "template",
    "this",
    "throw",
    "true",
    "try",
    "typedef",
    "typeid",
    "typeof",
    "union",
    "unittest",
    "version",
    "volatile",
    "while",
    "with",
    "__DATE__",
    "__EOF__",
    "__TIME__",
    "__TIMESTAMP__",
    "__VENDOR__",
    "__VERSION__",
    "__FILE__",
    "__LINE__",
    null, // Comment
    null, // Identifier
    null, // ScriptLine
    "__traits",
    "__parameters",
    "__vector",
    null, // Whitespace
	null, // SpecialTokenSequence
	null, // DoubleLiteral
	null, // FloatLiteral
	null, // IDoubleLiteral
	null, // IFloatLiteral
	null, // IntLiteral
	null, // LongLiteral
	null, // RealLiteral
	null, // IRealLiteral
	null, // UnsignedIntLiteral
	null, // UnsignedLongLiteral
	null, // DStringLiteral
	null, // StringLiteral
	null, // WStringLiteral
];

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

void main(string[] args)
{
    writeln("immutable(string[]) tokenValues = [");
    foreach (s; opkwds)
    {
        if (s is null)
        {
            writeln("\tnull,");
            continue;
        }
        auto n = opKwdValues.countUntil(s);
        writeln("\topKwdValues[", n, " .. ", n + s.length, "], // ", s);
    }
    writeln("];");
}
