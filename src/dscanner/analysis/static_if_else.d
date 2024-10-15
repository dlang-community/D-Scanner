// Copyright Chris Wright (dhasenan) 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.static_if_else;

import dscanner.analysis.base;
import std.stdio;

// TODO: check and fix AutoFix
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
extern(C++) class StaticIfElse(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"static_if_else_check";

	extern(D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
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
		import dmd.astenums : STMT;

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
			if (s.elsebody.stmt == STMT.If)
				addErrorMessage(cast(ulong) s.elsebody.loc.linnum, cast(ulong) s.elsebody.loc.charnum,
					KEY, MESSAGE);
		
			s.elsebody.accept(this);
		}
	}

private:
	enum KEY = "dscanner.suspicious.static_if_else";
	enum MESSAGE = "Mismatched static if. Use 'else static if' here.";
}

unittest
{
	import dscanner.analysis.helpers : assertAnalyzerWarnings = assertAnalyzerWarningsDMD;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.static_if_else_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void foo() {
			static if (false)
				auto a = 0;
			else if (true) // [warn]: Mismatched static if. Use 'else static if' here.
				auto b = 1;
		}
	}c, sac);
	// Explicit braces, so no warning.
	assertAnalyzerWarnings(q{
		void foo() {
			static if (false)
				auto a = 0;
			else {
				if (true)
					auto b = 1;
			}
		}
	}c, sac);

	stderr.writeln("Unittest for StaticIfElse passed.");
}
