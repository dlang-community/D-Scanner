//          Copyright Brian Schott (Hackerpilot) 2014-2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.unused_parameter;

import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import dscanner.analysis.unused;
import dsymbol.scope_ : Scope;

/**
 * Checks for unused variables.
 */
final class UnusedParameterCheck : UnusedStorageCheck
{
	alias visit = UnusedStorageCheck.visit;

	mixin AnalyzerInfo!"unused_parameter_check";

	/**
	 * Params:
	 *     fileName = the name of the file being analyzed
	 */
	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests, "Parameter", "unused_parameter");
	}

	override void visit(const Parameter parameter)
	{
		import std.algorithm : among;
		import std.algorithm.iteration : filter;
		import std.range : empty;

		if (parameter.name != tok!"")
		{
			immutable bool isRef = !parameter.parameterAttributes
				.filter!(a => a.idType.among(tok!"ref", tok!"out")).empty;
			immutable bool isPtr = parameter.type && !parameter.type
				.typeSuffixes.filter!(a => a.star != tok!"").empty;

			variableDeclared(parameter.name.text, parameter.name.line,
					parameter.name.column, isRef | isPtr);

			if (parameter.default_ !is null)
			{
				interestDepth++;
				parameter.default_.accept(this);
				interestDepth--;
			}
		}
	}
}

@system unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.unused_parameter_check = Check.enabled;
	assertAnalyzerWarnings(q{

	// bug encountered after correct DIP 1009 impl in dparse
	version (StdDdoc)
	{
		bool isAbsolute(R)(R path) pure nothrow @safe
		if (isRandomAccessRange!R && isSomeChar!(ElementType!R) ||
			is(StringTypeOf!R));
	}

	void inPSC(in int a){} // [warn]: Parameter a is never used.

	void doStuff(int a, int b) // [warn]: Parameter b is never used.
	{
		return a;
	}

	// Issue 352
	void test352_1()
	{
		void f(int *x) {*x = 1;}
	}

	void test352_2()
	{
		void f(Bat** bat) {*bat = bats.ptr + 8;}
	}

	// Issue 490
	void test490()
	{
		auto cb1 = delegate(size_t _) {};
		cb1(3);
		auto cb2 = delegate(size_t a) {}; // [warn]: Parameter a is never used.
		cb2(3);
	}

	bool hasDittos(int decl)
	{
		mixin("decl++;");
	}

	// https://github.com/dlang-community/D-Scanner/issues/794
	void traits()
	{
		struct S { int i; }

		static foo(S s)
		{
			__traits(getMember, s, "i") = 99;
		}
	}

	}c, sac);
	stderr.writeln("Unittest for UnusedParameterCheck passed.");
}
