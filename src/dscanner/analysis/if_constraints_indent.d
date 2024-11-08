// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.if_constraints_indent;

import dscanner.analysis.base;
import dmd.tokens : Token, TOK;

/**
Checks whether all if constraints have the same indention as their declaration.
*/
extern (C++) class IfConstraintsIndentCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"if_constraints_indent";

	private enum string KEY = "dscanner.style.if_constraints_indent";
	private enum string MSG = "If constraints should have the same indentation as the function";

	private Token[] tokens;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
		lexFile();
	}

	private void lexFile()
	{
		import dscanner.utils : readFile;
		import dmd.errorsink : ErrorSinkNull;
		import dmd.globals : global;
		import dmd.lexer : Lexer;

		auto bytes = readFile(fileName) ~ '\0';

		__gshared ErrorSinkNull errorSinkNull;
		if (!errorSinkNull)
			errorSinkNull = new ErrorSinkNull;

		scope lexer = new Lexer(null, cast(char*) bytes, 0, bytes.length, 0, 0,  errorSinkNull, &global.compileEnv);

		do
		{
			lexer.nextToken();
			tokens ~= lexer.token;
		}
		while (lexer.token.value != TOK.endOfFile);
	}

	override void visit(AST.TemplateDeclaration templateDecl)
	{
		import std.array : array;
		import std.algorithm : filter;
		import std.range : front, retro;

		super.visit(templateDecl);

		if (templateDecl.constraint is null || templateDecl.members is null)
			return;

		auto firstTemplateToken = tokens.filter!(t => t.loc.linnum == templateDecl.loc.linnum)
			.filter!(t => t.value != TOK.whitespace)
			.front;
		uint templateLine = firstTemplateToken.loc.linnum;
		uint templateCol = firstTemplateToken.loc.charnum;

		auto constraintToken = tokens.filter!(t => t.loc.fileOffset <= templateDecl.constraint.loc.fileOffset)
			.array()
			.retro()
			.filter!(t => t.value == TOK.if_)
			.front;
		uint constraintLine = constraintToken.loc.linnum;
		uint constraintCol = constraintToken.loc.charnum;

		if (templateLine == constraintLine || templateCol != constraintCol)
			addErrorMessage(cast(ulong) constraintLine, cast(ulong) constraintCol, KEY, MSG);
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import std.format : format;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.if_constraints_indent = Check.enabled;
	enum MSG = "If constraints should have the same indentation as the function";

	assertAnalyzerWarningsDMD(q{
void foo(R)(R r)
if (R == null)
{}

void foo(R)(R r)
	if (R == null) // [warn]: %s
{}
	}c.format(MSG), sac);

	assertAnalyzerWarningsDMD(q{
	void foo(R)(R r)
	if (R == null)
	{}

	void foo(R)(R r)
if (R == null) // [warn]: %s
	{}

	void foo(R)(R r)
		if (R == null) // [warn]: %s
	{}
	}c.format(MSG, MSG), sac);

	assertAnalyzerWarningsDMD(q{
	struct Foo(R)
	if (R == null)
	{}

	struct Foo(R)
if (R == null) // [warn]: %s
	{}

	struct Foo(R)
		if (R == null) // [warn]: %s
	{}
	}c.format(MSG, MSG), sac);

	// test example from Phobos
	assertAnalyzerWarningsDMD(q{
Num abs(Num)(Num x) @safe pure nothrow
if (is(typeof(Num.init >= 0)) && is(typeof(-Num.init)) &&
	!(is(Num* : const(ifloat*)) || is(Num* : const(idouble*))
	|| is(Num* : const(ireal*))))
{
	static if (isFloatingPoint!(Num))
		return fabs(x);
	else
		return x >= 0 ? x : -x;
}
	}, sac);

	// weird constraint formatting
	assertAnalyzerWarningsDMD(q{
	struct Foo(R)
	if
	(R == null)
	{}

	struct Foo(R)
	if
		(R == null)
	{}

	struct Foo(R)
if // [warn]: %s
	(R == null)
	{}

	struct Foo(R)
	if (
	R == null)
	{}

	struct Foo(R)
	if (
		R == null
	)
	{}

	struct Foo(R)
		if ( // [warn]: %s
		R == null
	) {}
	}c.format(MSG, MSG), sac);

	// constraint on the same line
	assertAnalyzerWarningsDMD(q{
	struct CRC(uint N, ulong P) if (N == 32 || N == 64) // [warn]: %s
	{}
	}c.format(MSG), sac);

	assertAnalyzerWarningsDMD(q{
private template sharedToString(alias field)
if (is(typeof(field) == shared))
{
    static immutable sharedToString = typeof(field).stringof;
}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
private union EndianSwapper(T)
if (canSwapEndianness!T)
{
    T value;
}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
void test(alias matchFn)()
{
	auto baz(Cap)(Cap m)
	if (is(Cap == Captures!(Cap.String)))
	{
	    return toUpper(m.hit);
	}
}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
ElementType!(A) pop (A) (ref A a)
if (isDynamicArray!(A) && !isNarrowString!(A) && isMutable!(A) && !is(A == void[]))
{
    auto e = a.back;
    a.popBack();
    return e;
}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
	template HMAC(H)
	if (isDigest!H && hasBlockSize!H)
	{
	    alias HMAC = HMAC!(H, H.blockSize);
	}
	}, sac);

	stderr.writeln("Unittest for IfConstraintsIndentCheck passed.");
}
