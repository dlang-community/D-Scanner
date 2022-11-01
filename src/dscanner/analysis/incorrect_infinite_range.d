//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.incorrect_infinite_range;

import dscanner.analysis.base;
import dscanner.analysis.helpers;
import dparse.ast;
import dparse.lexer;

/**
 * Checks for incorrect infinite range definitions
 */
extern(C++) class IncorrectInfiniteRangeCheck(AST) : BaseAnalyzerDmd
{
	// alias visit = BaseAnalyzerDmd!AST.visit;
	alias visit = BaseAnalyzerDmd.visit;

	mixin AnalyzerInfo!"incorrect_infinite_range_check";

	///
	extern(D) this(string fileName)
	{
		super(fileName);
	}

	override void visit(AST.StructDeclaration sd)
	{
		inAggregate++;
		super.visit(sd);
		inAggregate--;
	}

	override void visit(AST.ClassDeclaration cd)
	{
		inAggregate++;
		super.visit(cd);
		inAggregate--;
	}

	override void visit(AST.FuncDeclaration fd)
	{
		import dmd.astenums : Tbool;

		if (!inAggregate)
			return;

		if (!fd.ident || fd.ident.toString() != "empty")
			return;

		AST.TypeFunction tf = fd.type.isTypeFunction();

		if (!tf || !tf.next || !tf.next.ty)
			return;

		AST.ReturnStatement rs = fd.fbody ? fd.fbody.isReturnStatement() : null;

		if (rs)
		{
			AST.IntegerExp ie = cast(AST.IntegerExp) rs.exp;

			if (ie && ie.getInteger() == 0)
				addErrorMessage(cast(ulong) fd.loc.linnum, cast(ulong) fd.loc.charnum, KEY,
				"Use `enum bool empty = false;` to define an infinite range.");
		}

		AST.CompoundStatement cs = fd.fbody ? fd.fbody.isCompoundStatement() : null;
		
		if (!cs || (*cs.statements).length == 0)
			return;

		if (auto rs1 = (*cs.statements)[0].isReturnStatement())
		{
			AST.IntegerExp ie = cast(AST.IntegerExp) rs1.exp;

			if (ie && ie.getInteger() == 0)
				addErrorMessage(cast(ulong) fd.loc.linnum, cast(ulong) fd.loc.charnum, KEY,
				"Use `enum bool empty = false;` to define an infinite range.");
		}

		super.visit(fd);
	}

	override void visit(AST.UnitTestDeclaration ud)
	{
		
	}

private:
	uint inAggregate;
	enum string KEY = "dscanner.suspicious.incorrect_infinite_range";
	enum string MESSAGE = "Use `enum bool empty = false;` to define an infinite range.";
}

unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.format : format;

	StaticAnalysisConfig sac = disabledConfig();
	sac.incorrect_infinite_range_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{struct InfiniteRange
{
	bool empty() // [warn]: Use `enum bool empty = false;` to define an infinite range.
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

	// https://issues.dlang.org/show_bug.cgi?id=18409
	struct Foo
	{
		~this() nothrow @nogc;
	}
}

struct InfiniteRange
{
	bool empty() => false; // [warn]: Use `enum bool empty = false;` to define an infinite range.
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
class C { bool empty() { return false; } } // [warn]: Use `enum bool empty = false;` to define an infinite range.

}c, sac);
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
	assertAnalyzerWarningsDMD(q{
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