//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.incorrect_infinite_range;

import analysis.base;
import analysis.helpers;
import dparse.ast;
import dparse.lexer;

/**
 * Checks for incorrect infinite range definitions
 */
class IncorrectInfiniteRangeCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	///
	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	override void visit(const StructBody structBody)
	{
		inStruct++;
		structBody.accept(this);
		inStruct--;
	}

	override void visit(const FunctionDeclaration fd)
	{
		if (inStruct > 0 && fd.name.text == "empty")
		{
			line = fd.name.line;
			column = fd.name.column;
			fd.accept(this);
		}
	}

	override void visit(const FunctionBody fb)
	{
		if (fb.bodyStatement !is null)
			visit(fb.bodyStatement.blockStatement);
		else
			visit(fb.blockStatement);
	}

	override void visit(const BlockStatement bs)
	{
		if (bs.declarationsAndStatements is null)
			return;
		if (bs.declarationsAndStatements.declarationsAndStatements is null)
			return;
		if (bs.declarationsAndStatements.declarationsAndStatements.length != 1)
			return;
		visit(bs.declarationsAndStatements);
	}

	override void visit(const ReturnStatement rs)
	{
		if (rs.expression.items.length != 1)
			return;
		UnaryExpression unary = cast(UnaryExpression) rs.expression.items[0];
		if (unary is null)
			return;
		if (unary.primaryExpression is null)
			return;
		if (unary.primaryExpression.primary != tok!"false")
			return;
		addErrorMessage(line, column, KEY, MESSAGE);
	}

	override void visit(const Unittest u)
	{
	}

private:
	uint inStruct;
	enum string KEY = "dscanner.suspicios.incorrect_infinite_range";
	enum string MESSAGE = "Use `enum bool empty = false;` to define an infinite range.";
	size_t line;
	size_t column;
}

unittest
{
	import std.stdio : stderr;
	import analysis.config : StaticAnalysisConfig, Check;
	import std.format : format;

	StaticAnalysisConfig sac;
	sac.incorrect_infinite_range_check = Check.enabled;
	assertAnalyzerWarnings(q{struct InfiniteRange
{
	bool empty() // [warn]: %1$s
	{
		return false;
	}

	bool stuff()
	{
		return false;
	}

	unittest
	{
		return false;
	}
}

bool empty() { return false; }
class C { bool empty() { return false; } } // [warn]: %1$s

}c
			.format(IncorrectInfiniteRangeCheck.MESSAGE), sac);
	stderr.writeln("Unittest for IncorrectInfiniteRangeCheck passed.");
}
