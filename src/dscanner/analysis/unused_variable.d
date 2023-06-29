//          Copyright Brian Schott (Hackerpilot) 2014-2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.unused_variable;

import dparse.ast;
import dscanner.analysis.base;
import dscanner.analysis.unused;
import dsymbol.scope_ : Scope;
import std.algorithm.iteration : map;

/**
 * Checks for unused variables.
 */
final class UnusedVariableCheck : UnusedStorageCheck
{
	alias visit = UnusedStorageCheck.visit;

	mixin AnalyzerInfo!"unused_variable_check";

	/**
	 * Params:
	 *     fileName = the name of the file being analyzed
	 */
	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests, "Variable", "unused_variable");
	}

	override void visit(const VariableDeclaration variableDeclaration)
	{
		foreach (d; variableDeclaration.declarators)
			this.variableDeclared(d.name.text, d.name, false);
		variableDeclaration.accept(this);
	}

	override void visit(const AutoDeclaration autoDeclaration)
	{
		foreach (t; autoDeclaration.parts.map!(a => a.identifier))
			this.variableDeclared(t.text, t, false);
		autoDeclaration.accept(this);
	}
}

@system unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.unused_variable_check = Check.enabled;
	assertAnalyzerWarnings(q{

	// Issue 274
	unittest
	{
		size_t byteIndex = 0;
		*(cast(FieldType*)(retVal.ptr + byteIndex)) = item;
	}

	unittest
	{
		int a; /+
		    ^ [warn]: Variable a is never used. +/
	}

	// Issue 380
	int templatedEnum()
	{
		enum a(T) = T.init;
		return a!int;
	}

	// Issue 380
	int otherTemplatedEnum()
	{
		auto a(T) = T.init; /+
		     ^ [warn]: Variable a is never used. +/
		return 0;
	}

	// Issue 364
	void test364_1()
	{
		enum s = 8;
		immutable t = 2;
		int[s][t] a;
		a[0][0] = 1;
	}

	void test364_2()
	{
		enum s = 8;
		alias a = e!s;
		a = 1;
	}

	void oops ()
	{
		class Identity { int val; }
		Identity v;
		v.val = 0;
	}

	void main()
	{
		const int testValue;
		testValue.writeln;
	}

	// Issue 788
	void traits()
	{
		enum fieldName = "abc";
		__traits(hasMember, S, fieldName);

		__traits(compiles, { int i = 2; });
	}

	// segfault with null templateArgumentList
	void nullTest()
	{
		__traits(isPOD);
	}

	}c, sac);
	stderr.writeln("Unittest for UnusedVariableCheck passed.");
}
