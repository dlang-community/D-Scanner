//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.incorrect_infinite_range;

import dscanner.analysis.base;
import dscanner.analysis.helpers;
import dparse.ast;
import dparse.lexer;

import std.typecons : Rebindable;

/**
 * Checks for incorrect infinite range definitions
 */
final class IncorrectInfiniteRangeCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	mixin AnalyzerInfo!"incorrect_infinite_range_check";

	///
	this(BaseAnalyzerArguments args)
	{
		super(args);
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
			auto old = parentFunc;
			parentFunc = fd;
			fd.accept(this);
			parentFunc = old;
		}
	}

	override void visit(const FunctionBody fb)
	{
		if (fb.specifiedFunctionBody && fb.specifiedFunctionBody.blockStatement !is null)
			visit(fb.specifiedFunctionBody.blockStatement);
		else if (fb.shortenedFunctionBody && fb.shortenedFunctionBody.expression !is null)
			visitReturnExpression(fb.shortenedFunctionBody.expression);
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
		if (inStruct == 0 || parentFunc == null) // not within a struct yet
			return;
		visitReturnExpression(rs.expression);
	}

	void visitReturnExpression(const Expression expression)
	{
		if (!expression || expression.items.length != 1)
			return;
		UnaryExpression unary = cast(UnaryExpression) expression.items[0];
		if (unary is null)
			return;
		if (unary.primaryExpression is null)
			return;
		if (unary.primaryExpression.primary != tok!"false")
			return;
		addErrorMessage(parentFunc.get, KEY, MESSAGE);
	}

	override void visit(const Unittest u)
	{
	}

private:
	uint inStruct;
	enum string KEY = "dscanner.suspicious.incorrect_infinite_range";
	enum string MESSAGE = "Use `enum bool empty = false;` to define an infinite range.";
	Rebindable!(const FunctionDeclaration) parentFunc;
}

unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.format : format;

	StaticAnalysisConfig sac = disabledConfig();
	sac.incorrect_infinite_range_check = Check.enabled;
	assertAnalyzerWarnings(q{struct InfiniteRange
{
	bool empty()
	{
		return false;
	} /+
^^ [warn]: %1$s+/
	// TODO: test for multiline issues like this

	bool stuff()
	{
		return false;
	}

	unittest
	{
		return false;
	}

	// https://issues.dlang.org/show_bug.cgi?id=18409
	struct Foo
	{
		~this() nothrow @nogc;
	}
}

struct InfiniteRange
{
	bool empty() => false; /+
	^^^^^^^^^^^^^^^^^^^^^^ [warn]: %1$s +/
	bool stuff() => false;
	unittest
	{
		return false;
	}

	// https://issues.dlang.org/show_bug.cgi?id=18409
	struct Foo
	{
		~this() nothrow @nogc;
	}
}

bool empty() { return false; }
class C { bool empty() { return false; } } /+
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [warn]: %1$s +/

}c
			.format(IncorrectInfiniteRangeCheck.MESSAGE), sac);
}

// test for https://github.com/dlang-community/D-Scanner/issues/656
// unittests are skipped but D-Scanner sources are self checked
version(none) struct Foo
{
	void empty()
	{
		return;
	}
}

unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.format : format;

	StaticAnalysisConfig sac = disabledConfig();
	sac.incorrect_infinite_range_check = Check.enabled;
	assertAnalyzerWarnings(q{
		enum isAllZeroBits = ()
		{
			if (true)
				return true;
			else
				return false;
		}();
	}, sac);
	stderr.writeln("Unittest for IncorrectInfiniteRangeCheck passed.");
}
