// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.always_curly;

import dparse.lexer;
import dparse.ast;
import dscanner.analysis.base;
import dsymbol.scope_ : Scope;

import std.array : back, front;
import std.algorithm;
import std.range;
import std.stdio;

final class AlwaysCurlyCheck : BaseAnalyzer
{
	mixin AnalyzerInfo!"always_curly_check";

	alias visit = BaseAnalyzer.visit;

	///
	this(string fileName, const(Token)[] tokens, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	void test(L, B)(L loc, B s, string stmtKind)
	{
		if (!is(s == BlockStatement))
		{
			if (!s.tokens.empty)
			{
				AutoFix af = AutoFix.insertionBefore(s.tokens.front, " { ")
					.concat(AutoFix.insertionAfter(s.tokens.back, " } "));
				af.name = "Wrap in braces";

				addErrorMessage(loc, KEY, stmtKind ~ MESSAGE_POSTFIX, [af]);
			}
			else
			{
				addErrorMessage(loc, KEY, stmtKind ~ MESSAGE_POSTFIX);
			}
		}
	}

	override void visit(const(IfStatement) stmt)
	{
		auto s = stmt.thenStatement.statement;
		this.test(stmt.thenStatement, s, "if");
		if (stmt.elseStatement !is null)
		{
			auto e = stmt.elseStatement.statement;
			this.test(stmt.elseStatement, e, "else");
		}
	}

	override void visit(const(ForStatement) stmt)
	{
		auto s = stmt.declarationOrStatement;
		if (s.statement !is null)
		{
			this.test(s, s, "for");
		}
	}

	override void visit(const(ForeachStatement) stmt)
	{
		auto s = stmt.declarationOrStatement;
		if (s.statement !is null)
		{
			this.test(s, s, "foreach");
		}
	}

	override void visit(const(TryStatement) stmt)
	{
		auto s = stmt.declarationOrStatement;
		if (s.statement !is null)
		{
			this.test(s, s, "try");
		}

		if (stmt.catches !is null)
		{
			foreach (const(Catch) ct; stmt.catches.catches)
			{
				this.test(ct, ct.declarationOrStatement, "catch");
			}
			if (stmt.catches.lastCatch !is null)
			{
				auto sncnd = stmt.catches.lastCatch.statementNoCaseNoDefault;
				if (sncnd !is null)
				{
					this.test(stmt.catches.lastCatch, sncnd, "finally");
				}
			}
		}
	}

	override void visit(const(WhileStatement) stmt)
	{
		auto s = stmt.declarationOrStatement;
		if (s.statement !is null)
		{
			this.test(s, s, "while");
		}
	}

	override void visit(const(DoStatement) stmt)
	{
		auto s = stmt.statementNoCaseNoDefault;
		if (s !is null)
		{
			this.test(s, s, "do");
		}
	}

	enum string KEY = "dscanner.style.always_curly";
	enum string MESSAGE_POSTFIX = " must be follow by a BlockStatement aka. { }";
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings, assertAutoFix;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.always_curly_check = Check.enabled;

	assertAnalyzerWarnings(q{
		void testIf()
		{
			if(true) return; // [warn]: if must be follow by a BlockStatement aka. { }
		}
	}, sac);

	assertAnalyzerWarnings(q{
		void testIf()
		{
			if(true) return; /+
			         ^^^^^^^ [warn]: if must be follow by a BlockStatement aka. { } +/
		}
	}, sac);

	assertAnalyzerWarnings(q{
		void testIf()
		{
			for(int i = 0; i < 10; ++i) return; // [warn]: for must be follow by a BlockStatement aka. { }
		}
	}, sac);

	assertAnalyzerWarnings(q{
		void testIf()
		{
			foreach(it; 0 .. 10) return; // [warn]: foreach must be follow by a BlockStatement aka. { }
		}
	}, sac);

	assertAnalyzerWarnings(q{
		void testIf()
		{
			while(true) return; // [warn]: while must be follow by a BlockStatement aka. { }
		}
	}, sac);

	assertAnalyzerWarnings(q{
		void testIf()
		{
			do return; while(true); return; // [warn]: do must be follow by a BlockStatement aka. { }
		}
	}, sac);
}

unittest {
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings, assertAutoFix;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.always_curly_check = Check.enabled;

	assertAutoFix(q{
		void test() {
			if(true) return; // fix:0
		}
	}c, q{
		void test() {
			if(true) { return; } // fix:0
		}
	}c, sac);

	assertAutoFix(q{
		void test() {
			foreach(_; 0 .. 10 ) return; // fix:0
		}
	}c, q{
		void test() {
			foreach(_; 0 .. 10 ) { return; } // fix:0
		}
	}c, sac);

	assertAutoFix(q{
		void test() {
			for(int i = 0; i < 10; ++i) return; // fix:0
		}
	}c, q{
		void test() {
			for(int i = 0; i < 10; ++i) { return; } // fix:0
		}
	}c, sac);

	assertAutoFix(q{
		void test() {
			do return; while(true) // fix:0
		}
	}c, q{
		void test() {
			do { return; } while(true) // fix:0
		}
	}c, sac);


	stderr.writeln("Unittest for AlwaysCurly passed.");
}
