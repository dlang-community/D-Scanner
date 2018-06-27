//          Copyright The dlang community - 2018
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.trust_too_much;

import std.stdio;
import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import dsymbol.scope_;

/**
 * Checks that `@trusted` is only applied to a a single function
 */
final class TrustTooMuchCheck : BaseAnalyzer
{
private:

	static immutable MESSAGE = "Trusting a whole scope is a bad idea, " ~
		"`@trusted` should only be attached to the functions individually";
	static immutable string KEY = "dscanner.trust_too_much";

	bool checkAtAttribute = true;

public:

	alias visit = BaseAnalyzer.visit;

	///
	this(string fileName, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const AtAttribute d)
	{
		if (checkAtAttribute && d.identifier.text == "trusted")
		{
			const Token t = d.identifier;
			addErrorMessage(t.line, t.column, KEY, MESSAGE);
		}
		d.accept(this);
	}

	// always applied to function body, so OK
	override void visit(const MemberFunctionAttribute d)
	{
		const oldCheckAtAttribute = checkAtAttribute;
		checkAtAttribute = false;
		d.accept(this);
		checkAtAttribute = oldCheckAtAttribute;
	}

	// handles `@trusted{}` and old style, leading, atAttribute for single funcs
	override void visit(const Declaration d)
	{
		const oldCheckAtAttribute = checkAtAttribute;

		checkAtAttribute = 	d.functionDeclaration is null && d.unittest_ is null &&
							d.constructor is null && d.destructor is null &&
							d.staticConstructor is null && d.staticDestructor is null &&
							d.sharedStaticConstructor is null && d.sharedStaticDestructor is null;
		d.accept(this);
		checkAtAttribute = oldCheckAtAttribute;
	}

	// issue #588
	override void visit(const AliasDeclaration d)
	{
		const oldCheckAtAttribute = checkAtAttribute;
		checkAtAttribute = false;
		d.accept(this);
		checkAtAttribute = oldCheckAtAttribute;
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;
	import std.format : format;

	StaticAnalysisConfig sac = disabledConfig();
	sac.trust_too_much = Check.enabled;
	const msg = TrustTooMuchCheck.MESSAGE;

	//--- fail cases ---//

	assertAnalyzerWarnings(q{
	@trusted: // [warn]: %s
		void test();
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@trusted @nogc: // [warn]: %s
		void test();
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@trusted { // [warn]: %s
		void test();
		void test();
	}
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@safe {
		@trusted @nogc { // [warn]: %s
		void test();
		void test();
	}}
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@nogc @trusted { // [warn]: %s
		void test();
		void test();
	}
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	@trusted template foo(){ // [warn]: %s
	}
	}c.format(msg), sac);

	assertAnalyzerWarnings(q{
	struct foo{
	@trusted:  // [warn]: %s
	}
	}c.format(msg), sac);
	//--- pass cases ---//

	assertAnalyzerWarnings(q{
	void test() @trusted {}
	}c, sac);

	assertAnalyzerWarnings(q{
	@trusted void test();
	}c, sac);

	assertAnalyzerWarnings(q{
	@nogc template foo(){
	}
	}c , sac);

	assertAnalyzerWarnings(q{
	alias nothrow @trusted uint F4();
	}c , sac);

	assertAnalyzerWarnings(q{
	@trusted ~this();
	@trusted this();
	}c , sac);

	stderr.writeln("Unittest for TrustTooMuchCheck passed.");
}
