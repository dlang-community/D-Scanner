// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.assert_without_msg;

import dscanner.analysis.base;
import dscanner.analysis.source_range : balancedEnd, extentOf, findToken;
import dmd.tokens : TOK;
import std.stdio;

/**
 * Check that all asserts have an explanatory message.
 */
extern(C++) class AssertWithoutMessageCheck(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"assert_without_msg";
	alias visit = BaseAnalyzerDmd.visit;

	///
	extern(D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	// Avoid visiting in/out contracts for this check
	override void visitFuncBody(AST.FuncDeclaration f)
	{
		if (f.fbody)
        {
            f.fbody.accept(this);
        }
	}

	override void visit(AST.AssertExp ae)
	{
		if (!ae.msg)
			addErrorMessage(extentOf(source, ae), KEY, MESSAGE);
	}

	override void visit(AST.StaticAssert ae)
	{
		if (!ae.msgs)
		{
			const assertStart = findToken(source, ae.loc.fileOffset, TOK.assert_).start;
			const parenthesis = findToken(source, assertStart, TOK.leftParenthesis).start;
			addErrorMessage([assertStart, balancedEnd(source, parenthesis, parenthesis)], KEY, MESSAGE);
		}
	}

	override void visit(AST.CallExp call)
	{
		super.visit(call);

		auto callee = call.e1.isIdentifierExp();
		const argumentCount = call.arguments ? call.arguments.length : 0;
		if (isStdExceptionImported && callee && callee.ident.toString() == "enforce" && argumentCount < 2)
			addErrorMessage(extentOf(source, call), KEY, MESSAGE);
	}

	override void visit(AST.Import i)
	{
		const isStdException = i.packages.length == 1 && i.packages[0].toString() == "std"
			&& i.id.toString() == "exception";
		if (isStdException)
			isStdExceptionImported = true;
	}

	// imports only reach the end of the scope they appear in
	mixin ScopedVisit!(AST.ClassDeclaration);
	mixin ScopedVisit!(AST.InterfaceDeclaration);
	mixin ScopedVisit!(AST.StructDeclaration);
	mixin ScopedVisit!(AST.UnionDeclaration);
	mixin ScopedVisit!(AST.TemplateDeclaration);
	mixin ScopedVisit!(AST.FuncDeclaration);
	mixin ScopedVisit!(AST.UnitTestDeclaration);
	mixin ScopedVisit!(AST.IfStatement);
	mixin ScopedVisit!(AST.ScopeStatement);

	private template ScopedVisit(NodeType)
	{
		override void visit(NodeType node)
		{
			const outerImported = isStdExceptionImported;
			scope (exit)
				isStdExceptionImported = outerImported;
			super.visit(node);
		}
	}

private:
	bool isStdExceptionImported;
	enum string KEY = "dscanner.style.assert_without_msg";
	enum string MESSAGE = "An assert should have an explanatory message";
}

unittest
{
	import dmd.astcodegen : ASTCodegen;
	import std.stdio : stderr;
	import std.format : format;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings = assertAnalyzerWarningsDMD;

	StaticAnalysisConfig sac = disabledConfig();
	sac.assert_without_msg = Check.enabled;

	assertAnalyzerWarnings(q{
	unittest {
		assert(0, "foo bar");
		assert(0); /+
		^^^^^^^^^ [warn]: %s +/
	}
	}c.format(
		AssertWithoutMessageCheck!ASTCodegen.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
	unittest {
		static assert(0, "foo bar");
		static assert(0); /+
		       ^^^^^^^^^ [warn]: %s +/
	}
	}c.format(
		AssertWithoutMessageCheck!ASTCodegen.MESSAGE,
	), sac);

	// check for std.exception.enforce
	assertAnalyzerWarnings(q{
	unittest {
		enforce(0); // std.exception not imported yet - could be a user-defined symbol
		import std.exception;
		enforce(0, "foo bar");
		enforce(0); /+
		^^^^^^^^^^ [warn]: %s +/
	}
	}c.format(
		AssertWithoutMessageCheck!ASTCodegen.MESSAGE,
	), sac);

	// check for std.exception.enforce
	assertAnalyzerWarnings(q{
	unittest {
		import exception;
		class C {
			import std.exception;
		}
		enforce(0); // std.exception not imported yet - could be a user-defined symbol
		struct S {
			import std.exception;
		}
		enforce(0); // std.exception not imported yet - could be a user-defined symbol
		if (false) {
			import std.exception;
		}
		enforce(0); // std.exception not imported yet - could be a user-defined symbol
		{
			import std.exception;
		}
		enforce(0); // std.exception not imported yet - could be a user-defined symbol
	}
	}c, sac);

	// `i++` contains a literal without a source location
	assertAnalyzerWarnings(q{
	unittest {
		int i;
		assert(i++ == 0); /+
		^^^^^^^^^^^^^^^^ [warn]: %s +/
		assert(i++); /+
		^^^^^^^^^^^ [warn]: %s +/
	}
	}c.format(
		AssertWithoutMessageCheck!ASTCodegen.MESSAGE,
		AssertWithoutMessageCheck!ASTCodegen.MESSAGE,
	), sac);

	stderr.writeln("Unittest for AssertWithoutMessageCheck passed.");
}
