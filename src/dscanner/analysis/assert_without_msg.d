// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.assert_without_msg;

import dscanner.analysis.base : BaseAnalyzer;
import dscanner.utils : safeAccess;
import dsymbol.scope_ : Scope;
import dparse.lexer;
import dparse.ast;

import std.stdio;
import std.algorithm;

/**
 * Check that all asserts have an explanatory message.
 */
final class AssertWithoutMessageCheck : BaseAnalyzer
{
	enum string KEY = "dscanner.style.assert_without_msg";
	enum string MESSAGE = "An assert should have an explanatory message";

	///
	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const AssertExpression expr)
	{
		if (expr.message is null)
			addErrorMessage(expr.line, expr.column, KEY, MESSAGE);
	}

	override void visit(const FunctionCallExpression expr)
	{
		if (!isStdExceptionImported)
			return;

		if (const IdentifierOrTemplateInstance iot = safeAccess(expr)
			.unaryExpression.primaryExpression.identifierOrTemplateInstance)
		{
			auto ident = iot.identifier;
			if (ident.text == "enforce" && expr.arguments !is null && expr.arguments.argumentList !is null &&
					expr.arguments.argumentList.items.length < 2)
				addErrorMessage(ident.line, ident.column, KEY, MESSAGE);
		}
	}

	override void visit(const SingleImport sImport)
	{
		static immutable stdException = ["std", "exception"];
		if (sImport.identifierChain.identifiers.map!(a => a.text).equal(stdException))
			isStdExceptionImported = true;
	}

	// revert the stack after new scopes
	override void visit(const Declaration decl)
	{
		// be careful - ImportDeclarations don't introduce a new scope
		if (decl.importDeclaration is null)
		{
			bool tmp = isStdExceptionImported;
			scope(exit) isStdExceptionImported = tmp;
			decl.accept(this);
		}
		else
			decl.accept(this);
	}

	mixin ScopedVisit!IfStatement;
	mixin ScopedVisit!BlockStatement;

	alias visit = BaseAnalyzer.visit;

private:
	bool isStdExceptionImported;

	template ScopedVisit(NodeType)
	{
		override void visit(const NodeType n)
		{
			bool tmp = isStdExceptionImported;
			scope(exit) isStdExceptionImported = tmp;
			n.accept(this);
		}
	}
}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.assert_without_msg = Check.enabled;

	assertAnalyzerWarnings(q{
	unittest {
		assert(0, "foo bar");
		assert(0); // [warn]: %s
	}
	}c.format(
		AssertWithoutMessageCheck.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
	unittest {
		static assert(0, "foo bar");
		static assert(0); // [warn]: %s
	}
	}c.format(
		AssertWithoutMessageCheck.MESSAGE,
	), sac);

	// check for std.exception.enforce
	assertAnalyzerWarnings(q{
	unittest {
		enforce(0); // std.exception not imported yet - could be a user-defined symbol
		import std.exception;
		enforce(0, "foo bar");
		enforce(0); // [warn]: %s
	}
	}c.format(
		AssertWithoutMessageCheck.MESSAGE,
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

	stderr.writeln("Unittest for AssertWithoutMessageCheck passed.");
}
