//          Copyright Basile Burg 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.auto_function;

import analysis.base;
import analysis.helpers;
import dparse.ast;
import dparse.lexer;

import std.stdio;
import std.algorithm.searching : any;

/**
 * Checks for auto functions without return statement.
 *
 * Auto function without return statement can be an omission and are not
 * detected by the compiler. However sometimes they can be used as a trick
 * to infer attributes.
 */
final class AutoFunctionChecker : BaseAnalyzer
{

private:

	enum string KEY = "dscanner.suspicious.missing_return";
	enum string MESSAGE = "Auto function without return statement, prefer an explicit void";

	bool[] _returns;
	size_t _mixinDepth;

public:

	alias visit = BaseAnalyzer.visit;

	///
	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	override void visit(const(FunctionDeclaration) decl)
	{
		_returns.length += 1;
		scope(exit) _returns.length -= 1;
		_returns[$-1] = false;

		const bool autoFun = decl.storageClasses
			.any!(a => a.token.type == tok!"auto");

		decl.accept(this);

		if (decl.functionBody && autoFun && !_returns[$-1])
			addErrorMessage(decl.name.line, decl.name.column, KEY, MESSAGE);
	}

	override void visit(const(ReturnStatement) rst)
	{
		if (_returns.length)
			_returns[$-1] = true;
		rst.accept(this);
	}

	override void visit(const(AssertExpression) exp)
	{
		exp.accept(this);
		if (_returns.length)
		{
			const UnaryExpression u = cast(UnaryExpression) exp.assertion;
			if (!u)
				return;
			const PrimaryExpression p = u.primaryExpression;
			if (!p)
				return;

			immutable token = p.primary;
			if (token.type == tok!"false")
				_returns[$-1] = true;
			else if (token.text == "0")
				_returns[$-1] = true;
		}
	}

	override void visit(const(MixinExpression) mix)
	{
		++_mixinDepth;
		mix.accept(this);
		--_mixinDepth;
	}

	override void visit(const(PrimaryExpression) exp)
	{
		exp.accept(this);
		import std.algorithm.searching : find;
		import std.range : empty;
		if (_returns.length && _mixinDepth && !exp.primary.text.find("return").empty)
			_returns[$-1] = true;
	}
}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import analysis.config : StaticAnalysisConfig, Check;
	import analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac;
	sac.auto_function_check = Check.enabled;
	assertAnalyzerWarnings(q{
		auto ref doStuff(){} // [warn]: %s
		auto doStuff(){} // [warn]: %s
		int doStuff(){auto doStuff(){}} // [warn]: %s
		auto doStuff(){return 0;}
		int doStuff(){/*error but not the aim*/}
	}c.format(
		AutoFunctionChecker.MESSAGE,
		AutoFunctionChecker.MESSAGE,
		AutoFunctionChecker.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		auto doStuff(){assert(true);} // [warn]: %s
		auto doStuff(){assert(false);}
	}c.format(
		AutoFunctionChecker.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		auto doStuff(){assert(1);} // [warn]: %s
		auto doStuff(){assert(0);}
	}c.format(
		AutoFunctionChecker.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		auto doStuff(){mixin("0+0");} // [warn]: %s
		auto doStuff(){mixin("return 0;");}
	}c.format(
		AutoFunctionChecker.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		auto doStuff(){mixin("0+0");} // [warn]: %s
		auto doStuff(){mixin("static if (true)" ~ "  return " ~ 0.stringof ~ ";");}
	}c.format(
		AutoFunctionChecker.MESSAGE,
	), sac);

    assertAnalyzerWarnings(q{
        auto doStuff(){} // [warn]: %s
        extern(C) auto doStuff();
    }c.format(
        AutoFunctionChecker.MESSAGE,
    ), sac);

    assertAnalyzerWarnings(q{
        auto doStuff(){} // [warn]: %s
        @disable auto doStuff();
    }c.format(
        AutoFunctionChecker.MESSAGE,
    ), sac);

	stderr.writeln("Unittest for AutoFunctionChecker passed.");
}
