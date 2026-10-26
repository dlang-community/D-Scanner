//          Copyright Brian Schott (Hackerpilot) 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.alias_syntax_check;

import dscanner.analysis.base;
import dmd.tokens;

/**
 * Checks for uses of the old alias syntax.
 */
extern(C++) class AliasSyntaxCheck(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"alias_syntax_check";
	alias visit = BaseAnalyzerDmd.visit;

	extern(D) this(string fileName)
	{
		super(fileName);
	}

	override void visit(AST.AliasDeclaration ad)
	{
		import dscanner.analysis.source_range : findToken;

		// `alias a, b;` declares one AliasDeclaration per identifier, all at the `alias` token
		if (ad.loc.fileOffset == lastReportedOffset)
			return;

		auto tokens = lexAliasBody(source, ad.loc.fileOffset);
		if (tokens.length == 0 || hasTopLevelAssign(tokens))
			return;

		lastReportedOffset = ad.loc.fileOffset;
		const replacement = newSyntax(source, tokens);
		AutoFix[] autofixes;
		if (replacement.length)
			autofixes ~= AutoFix.replacement(tokens[0].start, tokens[$ - 1].end, replacement,
				"Rewrite alias to use assignment syntax");

		const declarationEnd = findToken(source, tokens[$ - 1].end, TOK.semicolon).end;
		addErrorMessage([ad.loc.fileOffset, declarationEnd], KEY,
				"Prefer the new \"'alias' identifier '=' type ';'\" syntax"
				~ " to the old \"'alias' type identifier ';'\" syntax.", autofixes);
	}

private:
	enum KEY = "dscanner.style.alias_syntax";

	extern(D) size_t lastReportedOffset = size_t.max;
}

private struct AliasToken
{
	TOK value;
	size_t depth;
	size_t start, end;
}

/// Lexes the tokens between the `alias` at `offset` and the closing `;`, recording their nesting depth.
private AliasToken[] lexAliasBody(const(char)[] source, size_t offset)
{
	import dscanner.analysis.source_range : tokensFrom;
	import std.range : drop;

	AliasToken[] tokens;
	size_t depth;
	foreach (token; tokensFrom(source, offset).drop(1))
	{
		if (token.value == TOK.semicolon && depth == 0)
			break;
		if (token.value == TOK.rightParenthesis || token.value == TOK.rightBracket || token.value == TOK.rightCurly)
			depth--;
		tokens ~= AliasToken(token.value, depth, token.start, token.end);
		if (token.value == TOK.leftParenthesis || token.value == TOK.leftBracket || token.value == TOK.leftCurly)
			depth++;
	}
	return tokens;
}

private bool hasTopLevelAssign(const AliasToken[] tokens)
{
	import std.algorithm : any;

	return tokens.any!(t => t.value == TOK.assign && t.depth == 0);
}

/**
 * Builds `ident = target, ident2 = target` from the old `target ident, ident2`
 * or `type ident(params) attributes` forms. Returns null when the declared
 * identifiers cannot be found.
 */
private string newSyntax(const(char)[] source, const AliasToken[] tokens)
{
	import std.algorithm : filter, map;
	import std.array : array, join;
	import std.range : iota;
	import std.string : strip, stripRight;

	const tokenText = (const AliasToken t) => source[t.start .. t.end];
	const isTopLevelComma = (const AliasToken t) => t.value == TOK.comma && t.depth == 0;

	string target;
	const(AliasToken)[] identifiers;
	auto commas = tokens.length.iota.filter!(i => isTopLevelComma(tokens[i])).array;
	if (commas.length)
	{
		identifiers = commas.map!(i => tokens[i - 1]).array ~ tokens[$ - 1];
		target = source[tokens[0].start .. identifiers[0].start].strip.idup;
	}
	else if (tokens[$ - 1].value == TOK.identifier && (tokens.length < 2 || tokens[$ - 2].value != TOK.at))
	{
		identifiers = tokens[$ - 1 .. $];
		target = source[tokens[0].start .. identifiers[0].start].stripRight.idup;
	}
	else
	{
		const paramsIndex = findParameterList(tokens);
		if (paramsIndex == 0)
			return null;
		identifiers = tokens[paramsIndex - 1 .. paramsIndex];
		target = (source[tokens[0].start .. identifiers[0].start].stripRight
			~ source[tokens[paramsIndex].start .. tokens[$ - 1].end]).idup;
	}

	return identifiers.map!(id => tokenText(id) ~ " = " ~ target).join(", ").idup;
}

/// Index of the `(` opening the parameter list of `type ident(params)`, or 0.
private size_t findParameterList(const AliasToken[] tokens)
{
	foreach (i; 1 .. tokens.length)
	{
		const isOpenParen = tokens[i].value == TOK.leftParenthesis && tokens[i].depth == 0;
		if (isOpenParen && tokens[i - 1].value == TOK.identifier)
			return i;
	}
	return 0;
}

unittest
{
	import dscanner.analysis.helpers : assertAnalyzerWarnings = assertAnalyzerWarningsDMD, assertAutoFix;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.alias_syntax_check = Check.enabled;
	assertAnalyzerWarnings(q{
		alias int abcde; /+
		^^^^^^^^^^^^^^^^ [warn]: Prefer the new "'alias' identifier '=' type ';'" syntax to the old "'alias' type identifier ';'" syntax.+/
		alias abcde = int;
		alias xyz[] a, b; /+
		^^^^^^^^^^^^^^^^^ [warn]: Prefer the new "'alias' identifier '=' type ';'" syntax to the old "'alias' type identifier ';'" syntax.+/
		alias a = xyz, b = xyz;
		alias void Func(); /+
		^^^^^^^^^^^^^^^^^^ [warn]: Prefer the new "'alias' identifier '=' type ';'" syntax to the old "'alias' type identifier ';'" syntax.+/
		alias Func = void();
		alias tem(T) = T!int;
	}c, sac);

	assertAutoFix(q{
		alias T * PT; // fix
		alias int[f(1, 2) + 3] sa; // fix
		alias void Func(int i) pure @safe; // fix
		alias extern (C) void function() FP; // fix
		alias xyz a, b; // fix
	}c, q{
		alias PT = T *; // fix
		alias sa = int[f(1, 2) + 3]; // fix
		alias Func = void(int i) pure @safe; // fix
		alias FP = extern (C) void function(); // fix
		alias a = xyz, b = xyz; // fix
	}c, sac);

	stderr.writeln("Unittest for AliasSyntaxCheck passed.");
}
