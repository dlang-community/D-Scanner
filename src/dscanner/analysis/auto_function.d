//          Copyright Basile Burg 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.auto_function;

import dscanner.analysis.base;
import dscanner.analysis.helpers;
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
	string[] _literalWithReturn;

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
			.any!(a => a.token.type == tok!"auto" || a.atAttribute !is null);

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

		import std.algorithm.searching : canFind;

		if (_returns.length && _mixinDepth)
		{
			if (findReturnInLiteral(exp.primary.text))
			    _returns[$-1] = true;
			else if (exp.identifierOrTemplateInstance &&
				_literalWithReturn.canFind(exp.identifierOrTemplateInstance.identifier.text))
					_returns[$-1] = true;
		}
	}

	bool findReturnInLiteral(const(string) value)
	{
		import std.algorithm.searching : find;
		import std.range : empty;

		return value == "return" || !value.find("return ").empty;
	}

	bool stringliteralHasReturn(const(NonVoidInitializer) nvi)
	{
		bool result;
		if (!nvi.assignExpression || (cast(UnaryExpression) nvi.assignExpression) is null)
			return result;

		const(UnaryExpression) u = cast(UnaryExpression) nvi.assignExpression;
		if (u.primaryExpression &&
			u.primaryExpression.primary.type.isStringLiteral &&
			findReturnInLiteral(u.primaryExpression.primary.text))
				result = true;

		return result;
	}

	override void visit(const(AutoDeclaration) decl)
	{
		decl.accept(this);

		foreach(const(AutoDeclarationPart) p; decl.parts)
			if (p.initializer &&
				p.initializer.nonVoidInitializer &&
				stringliteralHasReturn(p.initializer.nonVoidInitializer))
					_literalWithReturn ~= p.identifier.text.idup;
	}

	override void visit(const(VariableDeclaration) decl)
	{
		decl.accept(this);

		foreach(const(Declarator) d; decl.declarators)
			if (d.initializer &&
				d.initializer.nonVoidInitializer &&
				stringliteralHasReturn(d.initializer.nonVoidInitializer))
					_literalWithReturn ~= d.name.text.idup;
	}
}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
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

	assertAnalyzerWarnings(q{
		@property doStuff(){} // [warn]: %s
		@safe doStuff(){} // [warn]: %s
		@disable doStuff();
		@safe void doStuff();
	}c.format(
		AutoFunctionChecker.MESSAGE,
		AutoFunctionChecker.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		enum _genSave = "return true;";
		auto doStuff(){ mixin(_genSave);}
	}, sac);

	stderr.writeln("Unittest for AutoFunctionChecker passed.");
}
