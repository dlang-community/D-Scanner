//          Copyright Brian Schott (Hackerpilot) 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.alias_syntax_check;

import dscanner.analysis.base;
import dmd.tokens;
import dmd.lexer : Lexer;
import dmd.location : Loc;

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
		import dscanner.utils: readFile;
		import dmd.errorsink : ErrorSinkNull;
		import dmd.globals : global;

		__gshared ErrorSinkNull errorSinkNull;
		if (!errorSinkNull)
			errorSinkNull = new ErrorSinkNull;

		auto bytes = readFile(fileName);
		bool foundEq = false;
		Loc idLoc;

		bytes ~= '\0';
		bytes = bytes[ad.loc.fileOffset .. $];

		scope lexer = new Lexer(null, cast(char*) bytes, 0, bytes.length, 0, 0, errorSinkNull, &global.compileEnv);
		TOK nextTok;
		lexer.nextToken();

		do
		{
			if (lexer.token.value == TOK.assign)
				foundEq = true;

			if (lexer.token.value == TOK.identifier)
				idLoc = lexer.token.loc;

			nextTok = lexer.nextToken;
		}
		while(nextTok != TOK.semicolon && nextTok != TOK.endOfFile);

		if (!foundEq)
			// Re-lexing is done based on offsets, so the alias appears to be at line 1.
			// Fix this by computing the initial location.
			addErrorMessage(cast(ulong) (ad.loc.linnum + idLoc.linnum - 1), cast(ulong) idLoc.charnum, KEY,
							"Prefer the new \"'alias' identifier '=' type ';'\" syntax"
							~ " to the  old \"'alias' type identifier ';'\" syntax.");
	}


private:
	enum KEY = "dscanner.style.alias_syntax";
}

unittest
{
	import dscanner.analysis.helpers : assertAnalyzerWarnings = assertAnalyzerWarningsDMD;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.alias_syntax_check = Check.enabled;
	assertAnalyzerWarnings(q{
		alias int abcde; // [warn]: Prefer the new "'alias' identifier '=' type ';'" syntax to the  old "'alias' type identifier ';'" syntax.
		alias abcde = int;
	}c, sac);

	stderr.writeln("Unittest for AliasSyntaxCheck passed.");
}
