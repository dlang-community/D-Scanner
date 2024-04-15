// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.always_curly;

import dscanner.analysis.base;

// TODO: Fix Autofix
extern (C++) class AlwaysCurlyCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"always_curly_check";

	private enum string KEY = "dscanner.style.always_curly";
	private enum string MESSAGE_POSTFIX = " must be follow by a BlockStatement aka. { }";

	private bool hasCurlyBraces;
	private bool inCurlyStatement;

	///
	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	mixin VisitBraceStatement!(AST.IfStatement, "if");
	mixin VisitBraceStatement!(AST.ForStatement, "for");
	mixin VisitBraceStatement!(AST.ForeachStatement, "foreach");
	mixin VisitBraceStatement!(AST.ForeachRangeStatement, "foreach");
	mixin VisitBraceStatement!(AST.WhileStatement, "while");
	mixin VisitBraceStatement!(AST.DoStatement, "do");
	mixin VisitBraceStatement!(AST.Catch, "catch");

	private template VisitBraceStatement(NodeType, string nodeName)
	{
		override void visit(NodeType node)
		{
			auto oldHasCurlyBraces = hasCurlyBraces;
			auto oldInCurlyStatement = inCurlyStatement;
			hasCurlyBraces = false;
			inCurlyStatement = true;
			super.visit(node);

			if (!hasCurlyBraces)
			{
				auto msg = nodeName ~ MESSAGE_POSTFIX;
				addErrorMessage(node.loc.linnum, node.loc.charnum, KEY, msg);
			}

			hasCurlyBraces = oldHasCurlyBraces;
			inCurlyStatement = oldInCurlyStatement;
		}
	}

	override void visit(AST.CompoundStatement cs)
	{
		if (inCurlyStatement)
			hasCurlyBraces = true;
		super.visit(cs);
	}

	override void visit(AST.TryCatchStatement tryCatchStatement)
	{
		auto oldHasCurlyBraces = hasCurlyBraces;
		auto oldInCurlyStatement = inCurlyStatement;
		hasCurlyBraces = false;
		inCurlyStatement = true;

		checkStatement(tryCatchStatement._body, "try");

		hasCurlyBraces = oldHasCurlyBraces;
		inCurlyStatement = oldInCurlyStatement;

		foreach (catchStatement; *tryCatchStatement.catches)
			visit(catchStatement);
	}

	override void visit(AST.TryFinallyStatement tryFinallyStatement)
	{
		auto oldHasCurlyBraces = hasCurlyBraces;
		auto oldInCurlyStatement = inCurlyStatement;

		if (tryFinallyStatement._body.isTryCatchStatement())
		{
			tryFinallyStatement._body.accept(this);
		}
		else
		{
			hasCurlyBraces = false;
			inCurlyStatement = true;
			checkStatement(tryFinallyStatement._body, "try");
		}

		hasCurlyBraces = false;
		inCurlyStatement = true;
		checkStatement(tryFinallyStatement.finalbody, "finally");

		hasCurlyBraces = oldHasCurlyBraces;
		inCurlyStatement = oldInCurlyStatement;
	}

	extern (D) private void checkStatement(AST.Statement statement, string statementName)
	{
		statement.accept(this);

		if (!hasCurlyBraces)
		{
			auto msg = statementName ~ MESSAGE_POSTFIX;
			addErrorMessage(statement.loc.linnum, statement.loc.charnum, KEY, msg);
		}
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD, assertAutoFix;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.always_curly_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{
		void testIf()
		{
			if (true)
			{
				return;
			}

			if (true) return; // [warn]: if must be follow by a BlockStatement aka. { }
		}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		void testFor()
		{
			for (int i = 0; i < 10; ++i)
			{
				return;
			}

			for (int i = 0; i < 10; ++i) return; // [warn]: for must be follow by a BlockStatement aka. { }
		}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		void testForEach()
		{
			foreach (it; 0 .. 10)
			{
				return;
			}

			foreach (it; 0 .. 10) return; // [warn]: foreach must be follow by a BlockStatement aka. { }
		}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		void testWhile()
		{
			while (true)
			{
				return;
			}

			while (true) return; // [warn]: while must be follow by a BlockStatement aka. { }
		}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		void testDoWhile()
		{
			do
			{
				return;
			} while (true);

			do return; while (true); return; // [warn]: do must be follow by a BlockStatement aka. { }
		}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		void testTryCatchFinally()
		{
			try
			{
				return;
			}
			catch (Exception e)
			{
				return;
			}
			finally
			{
				return;
			}

			try return; // [warn]: try must be follow by a BlockStatement aka. { }
			catch (Exception e) return; // [warn]: catch must be follow by a BlockStatement aka. { }
			finally return; // [warn]: finally must be follow by a BlockStatement aka. { }
		}
	}c, sac);

	stderr.writeln("Unittest for AutoFix AlwaysCurly passed.");
}

/+ TODO: Fix Autofix
unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD, assertAutoFix;
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


	stderr.writeln("Unittest for AutoFix AlwaysCurly passed.");
}+/
