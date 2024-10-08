// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.if_constraints_indent;

import dparse.lexer;
import dparse.ast;
import dscanner.analysis.base;
import dsymbol.scope_ : Scope;

import std.algorithm.iteration : filter;
import std.range;

/**
Checks whether all if constraints have the same indention as their declaration.
*/
final class IfConstraintsIndentCheck : BaseAnalyzer
{
	mixin AnalyzerInfo!"if_constraints_indent";

	///
	this(BaseAnalyzerArguments args)
	{
		super(args);

		// convert tokens to a list of token starting positions per line

		// libdparse columns start at 1
		foreach (t; tokens)
		{
			// pad empty positions if we skip empty token-less lines
			// t.line (unsigned) may be 0 if the token is uninitialized/broken, so don't subtract from it
			// equivalent to: firstSymbolAtLine.length < t.line - 1
			while (firstSymbolAtLine.length + 1 < t.line)
				firstSymbolAtLine ~= Pos(1, t.index);

			// insert a new line with positions if new line is reached
			// (previous while pads skipped lines)
			if (firstSymbolAtLine.length < t.line)
				firstSymbolAtLine ~= Pos(t.column, t.index, t.type == tok!"if");
		}
	}

	override void visit(const FunctionDeclaration decl)
	{
		if (decl.constraint !is null)
			checkConstraintSpace(decl.constraint, decl.name);
	}

	override void visit(const InterfaceDeclaration decl)
	{
		if (decl.constraint !is null)
			checkConstraintSpace(decl.constraint, decl.name);
	}


	override void visit(const ClassDeclaration decl)
	{
		if (decl.constraint !is null)
			checkConstraintSpace(decl.constraint, decl.name);
	}

	override void visit(const TemplateDeclaration decl)
	{
		if (decl.constraint !is null)
			checkConstraintSpace(decl.constraint, decl.name);
	}

	override void visit(const UnionDeclaration decl)
	{
		if (decl.constraint !is null)
			checkConstraintSpace(decl.constraint, decl.name);
	}

	override void visit(const StructDeclaration decl)
	{
		if (decl.constraint !is null)
			checkConstraintSpace(decl.constraint, decl.name);
	}

	override void visit(const Constructor decl)
	{
		if (decl.constraint !is null)
			checkConstraintSpace(decl.constraint, decl.line);
	}

	alias visit = ASTVisitor.visit;

private:

	enum string KEY = "dscanner.style.if_constraints_indent";
	enum string MESSAGE = "If constraints should have the same indentation as the function";

	Pos[] firstSymbolAtLine;
	static struct Pos
	{
		size_t column;
		size_t index;
		bool isIf;
	}

	/**
	Check indentation of constraints
	*/
	void checkConstraintSpace(const Constraint constraint, const Token token)
	{
		checkConstraintSpace(constraint, token.line);
	}

	void checkConstraintSpace(const Constraint constraint, size_t line)
	{
		import std.algorithm : min;

		// dscanner lines start at 1
		auto pDecl = firstSymbolAtLine[line - 1];

		// search for constraint if (might not be on the same line as the expression)
		auto r = firstSymbolAtLine[line .. constraint.expression.line].retro.filter!(s => s.isIf);

		auto if_ = constraint.tokens.findTokenForDisplay(tok!"if")[0];

		// no hit = constraint is on the same line
		if (r.empty)
			addErrorMessage(if_, KEY, MESSAGE);
		else if (pDecl.column != r.front.column)
			addErrorMessage([min(if_.index, pDecl.index), if_.index + 2], if_.line, [min(if_.column, pDecl.column), if_.column + 2], KEY, MESSAGE);
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;
	import std.format : format;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.if_constraints_indent = Check.enabled;

	assertAnalyzerWarnings(q{
void foo(R)(R r)
if (R == null)
{}

// note: since we are using tabs, the ^ look a bit off here. Use 1-wide tab stops to view tests.
void foo(R)(R r)
	if (R == null) /+
^^^ [warn]: %s +/
{}
	}c.format(
		IfConstraintsIndentCheck.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
	void foo(R)(R r)
	if (R == null)
	{}

	void foo(R)(R r)
if (R == null) /+
^^ [warn]: %s +/
	{}

	void foo(R)(R r)
		if (R == null) /+
	^^^ [warn]: %s +/
	{}
	}c.format(
		IfConstraintsIndentCheck.MESSAGE,
		IfConstraintsIndentCheck.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
	struct Foo(R)
	if (R == null)
	{}

	struct Foo(R)
if (R == null) /+
^^ [warn]: %s +/
	{}

	struct Foo(R)
		if (R == null) /+
	^^^ [warn]: %s +/
	{}
	}c.format(
		IfConstraintsIndentCheck.MESSAGE,
		IfConstraintsIndentCheck.MESSAGE,
	), sac);

	// test example from Phobos
	assertAnalyzerWarnings(q{
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
	assertAnalyzerWarnings(q{
	struct Foo(R)
	if
	(R == null)
	{}

	struct Foo(R)
	if
		(R == null)
	{}

	struct Foo(R)
if /+
^^ [warn]: %s +/
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
		if ( /+
	^^^ [warn]: %s +/
		R == null
	) {}
	}c.format(
		IfConstraintsIndentCheck.MESSAGE,
		IfConstraintsIndentCheck.MESSAGE,
	), sac);

	// constraint on the same line
	assertAnalyzerWarnings(q{
	struct CRC(uint N, ulong P) if (N == 32 || N == 64) /+
	                            ^^ [warn]: %s +/
	{}
	}c.format(
		IfConstraintsIndentCheck.MESSAGE,
	), sac);

	stderr.writeln("Unittest for IfConstraintsIndentCheck passed.");
}

@("issue #829")
unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;
	import std.format : format;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.if_constraints_indent = Check.enabled;

    assertAnalyzerWarnings(`void foo() {
    f;
}`, sac);
}
