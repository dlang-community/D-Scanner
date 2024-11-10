// Copyright Chris Wright (dhasenan) 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.static_if_else;

import dscanner.analysis.base;
import dmd.tokens : Token, TOK;
import std.algorithm;
import std.array;

/**
 * Checks for potentially mistaken static if / else if.
 *
 * It's potentially valid to write:
 * ---
 * static if (foo) {
 * } else if (bar) {
 * }
 * ---
 * 
 * However, it's more likely that this is a mistake.
 */
extern (C++) class StaticIfElse(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"static_if_else_check";

	private Token[] tokens;

	private enum KEY = "dscanner.suspicious.static_if_else";
	private	enum MESSAGE = "Mismatched static if. Use 'else static if' here.";

	extern(D) this(string fileName, bool skipTests = false)
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

		scope lexer = new Lexer(null, cast(char*) bytes, 0, bytes.length, 0, 0, 1, errorSinkNull, &global.compileEnv);
		while (lexer.nextToken() != TOK.endOfFile)
			tokens ~= lexer.token;
	}

	override void visit(AST.UserAttributeDeclaration userAttribute)
	{
		if (shouldIgnoreDecl(userAttribute, KEY))
			return;

		super.visit(userAttribute);
	}

	override void visit(AST.Module mod)
	{
		if (shouldIgnoreDecl(mod.userAttribDecl(), KEY))
			return;

		super.visit(mod);
	}

	override void visit(AST.ConditionalStatement s)
	{
		import std.range : retro;

		if (!s.condition.isStaticIfCondition())
		{
			super.visit(s);
			return;
		}

		s.condition.accept(this);

		if (s.ifbody)
            s.ifbody.accept(this);
        
		if (s.elsebody)
		{
			if (auto ifStmt = s.elsebody.isIfStatement())
			{
				auto tokenRange = tokens.filter!(t => t.loc.linnum >= s.loc.linnum)
					.filter!(t => t.loc.fileOffset <= ifStmt.endloc.fileOffset);

				auto tabSize = tokenRange
					.until!(t => t.value == TOK.else_)
					.array
					.retro()
					.until!(t => t.value != TOK.whitespace)
					.count!(t => t.ptr[0] == '\t');

				string lineTerminator = "\n";
				version (Windows)
				{
					lineTerminator = "\r\n";
				}

				string braceStart = " {" ~ lineTerminator ~ "\t";
				string braceEnd = "}" ~ lineTerminator;
				for (int i = 0; i < tabSize - 1; i++)
				{
					braceStart ~= '\t';
					braceEnd ~= '\t';
				}
				braceStart ~= '\t';

				auto fileOffsets = tokenRange.find!(t => t.value == TOK.else_)
					.filter!(t => t.ptr[0] == '\n')
					.map!(t => t.loc.fileOffset + 1)
					.array;

				AutoFix autofix2 = AutoFix.insertionAt(ifStmt.endloc.fileOffset, braceEnd);
				foreach (fileOffset; fileOffsets)
					autofix2 = autofix2.concat(AutoFix.insertionAt(fileOffset, "\t"));
				autofix2 = autofix2.concat(AutoFix.insertionAt(ifStmt.loc.fileOffset, braceStart));

				auto ifRange = tokenRange.find!(t => t.loc.fileOffset >= ifStmt.ifbody.loc.fileOffset)
					.array;
				if (ifRange[0].value == TOK.leftCurly)
				{
					int idx = 1;
					while (ifRange[idx].value == TOK.whitespace)
						idx++;
					autofix2 = autofix2.concat(AutoFix.insertionAt(ifRange[idx].loc.fileOffset, "\t"));
				}
				else
				{
					autofix2 = autofix2.concat(AutoFix.insertionAt(ifStmt.ifbody.loc.fileOffset, "\t"));
				}


				addErrorMessage(
					cast(ulong) ifStmt.loc.linnum, cast(ulong) s.elsebody.loc.charnum, KEY, MESSAGE,
						[
						AutoFix.insertionAt(ifStmt.loc.fileOffset, "static "),
						autofix2
					]
				);
			}
		
			s.elsebody.accept(this);
		}
	}
}

unittest
{
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD, assertAutoFix;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.static_if_else_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{
		void foo() {
			static if (false)
				auto a = 0;
			else if (true) // [warn]: Mismatched static if. Use 'else static if' here.
				auto b = 1;
		}
	}c, sac);

	// Explicit braces, so no warning.
	assertAnalyzerWarningsDMD(q{
		void foo() {
			static if (false)
				auto a = 0;
			else {
				if (true)
					auto b = 1;
			}
		}
	}c, sac);

	assertAutoFix(q{
		void foo() {
			static if (false)
				auto a = 0;
			else if (true) // fix:0
				auto b = 1;
		}
		void bar() {
			static if (false)
				auto a = 0;
			else if (true) // fix:1
				auto b = 1;
		}
		void baz() {
			static if (false)
				auto a = 0;
			else if (true) { // fix:1
				auto b = 1;
			}
		}
	}c, q{
		void foo() {
			static if (false)
				auto a = 0;
			else static if (true) // fix:0
				auto b = 1;
		}
		void bar() {
			static if (false)
				auto a = 0;
			else {
				if (true) // fix:1
					auto b = 1;
			}
		}
		void baz() {
			static if (false)
				auto a = 0;
			else {
				if (true) { // fix:1
					auto b = 1;
				}
			}
		}
	}c, sac, true);

	stderr.writeln("Unittest for StaticIfElse passed.");
}
