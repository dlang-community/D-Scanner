//          Copyright Brian Schott (Hackerpilot) 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.alias_syntax_check;

import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;

/**
 * Checks for uses of the old alias syntax.
 */
final class AliasSyntaxCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	mixin AnalyzerInfo!"alias_syntax_check";

	this(BaseAnalyzerArguments args)
	{
		super(args);
	}

	override void visit(const AliasDeclaration ad)
	{
		// new alias syntax uses `ad.initializers` instead
		if (ad.declaratorIdentifierList is null)
			return;
		assert(ad.declaratorIdentifierList.identifiers.length > 0,
				"Identifier list length is zero, libdparse has a bug");

		static string tokenStr(const Token[] tokens)
		{
			string r;
			foreach (i, t; tokens)
			{
				if (i)
				{
					// keep whitespace to separate multiple tokens like original
					foreach (tt; tokens[i - 1].trailingTrivia)
						r ~= tt.text;
				}
				foreach (lt; t.leadingTrivia)
					r ~= lt.text;

				string st = t.text ? t.text : t.type.str;
				assert(st.length);
				r ~= st;
			}
			return r;
		}
		// `alias storage target ident, ident2;`
		string target;
		foreach (i, sc; ad.storageClasses)
		{
			target ~= tokenStr(sc.tokens);
			target ~= ' ';
		}
		target ~= tokenStr(ad.type.tokens);
		// or single function type:
		// `alias storage type ident(params) attributes;`
		if (ad.parameters)
		{
			target ~= tokenStr(ad.parameters.tokens);
			foreach (fa; ad.memberFunctionAttributes)
			{
				target ~= ' ';
				target ~= tokenStr(fa.tokens);
			}
		}
		// need `ident = target, ident2 = target`
		string rep;
		foreach (i, t; ad.declaratorIdentifierList.identifiers)
		{
			if (i)
				rep ~= ", ";
			rep ~= t.text ~ " = " ~ target;
		}
		addErrorMessage(ad, KEY,
				"Prefer the new \"'alias' identifier '=' type ';'\" syntax"
				~ " to the old \"'alias' type identifier ';'\" syntax.",
				[AutoFix.replacement(ad.tokens[1 .. $ - 1], rep,
					"Rewrite alias to use assignment syntax")]);
	}

private:
	enum KEY = "dscanner.style.alias_syntax";
}

unittest
{
	import dscanner.analysis.helpers : assertAnalyzerWarnings, assertAutoFix;
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
		alias void Func(int i) pure @att(4 * 5.g); // fix
		alias extern (C) void function() FP; // fix
		alias xyz a, b; // fix
	}c, q{
		alias PT = T *; // fix
		alias sa = int[f(1, 2) + 3]; // fix
		alias Func = void(int i) pure @att(4 * 5.g); // fix
		alias FP = extern (C) void function(); // fix
		alias a = xyz, b = xyz; // fix
	}c, sac);

	stderr.writeln("Unittest for AliasSyntaxCheck passed.");
}
