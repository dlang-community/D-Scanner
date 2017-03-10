// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module analysis.if_constraints_index;

import dparse.lexer;
import dparse.ast;
import analysis.base : BaseAnalyzer, Message;
import dsymbol.scope_ : Scope;

/**
Checks whether all if constraints have the same indention as their declaration.
*/
class IfConstraintsIndexCheck : BaseAnalyzer
{
	///
	this(string fileName, const(ubyte)[] code, bool skipTests = false)
	{
		super(fileName, null, skipTests);
		this.code = code;
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
			checkConstraintSpace(decl.constraint, decl.location);
	}

	alias visit = ASTVisitor.visit;

private:

	const(ubyte)[] code;

	enum string KEY = "dscanner.style.if_constraints_index";
	enum string MESSAGE = "If constraints should have the same indentation as the function";

	/**
	Check indentation of constraints
	*/
	void checkConstraintSpace(const Constraint constraint, const Token token)
	{
		checkConstraintSpace(constraint, token.index);
	}

	void checkConstraintSpace(const Constraint constraint, size_t location)
	{
		import std.utf : byCodeUnit;
		import std.ascii : isWhite;

		import std.algorithm;
		import std.range;

		auto lineIndent = code[0 .. constraint.location].retro
						.until('\n')
						.walkLength;

		auto tokenLineStart = code[0 .. location].retro
								.until('\n')
								.walkLength;

		auto tokenIndent = code[location - tokenLineStart .. location]
							.until!(x => !x.isWhite)
							.walkLength;

		if (tokenIndent != lineIndent)
			addErrorMessage(constraint.expression.line, constraint.expression.column, KEY, MESSAGE);
	}
}

unittest
{
	import analysis.config : StaticAnalysisConfig, Check;
	import analysis.helpers;
	import std.format : format;
	import std.stdio : stderr;

	StaticAnalysisConfig sac;
	sac.if_constraints_index = Check.enabled;

	assertAnalyzerWarnings(q{
void foo(R)(R r)
if (R == int)
{}

void foo(R)(R r)
	if (R == int) // [warn]: %s
{}
	}c.format(
		IfConstraintsIndexCheck.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
	void foo(R)(R r)
	if (R == int)
	{}

	void foo(R)(R r)
if (R == int) // [warn]: %s
	{}

	void foo(R)(R r)
		if (R == int) // [warn]: %s
	{}
	}c.format(
		IfConstraintsIndexCheck.MESSAGE,
		IfConstraintsIndexCheck.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
	struct Foo(R)
	if (R == int)
	{}

	struct Foo(R)
if (R == int) // [warn]: %s
	{}

	struct Foo(R)
		if (R == int) // [warn]: %s
	{}
	}c.format(
		IfConstraintsIndexCheck.MESSAGE,
		IfConstraintsIndexCheck.MESSAGE,
	), sac);

	stderr.writeln("Unittest for IfConstraintsIndexCheck passed.");
}
