//          Copyright Brian Schott (Hackerpilot) 2014-2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.unused_parameter;

import dscanner.analysis.base;
import dmd.astenums : STC;
import std.algorithm : all, canFind, each, filter, map;
import std.conv : to;

/**
 * Checks for unused function parameters.
 */
extern (C++) class UnusedParameterCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"unused_parameter_check";

	private enum KEY = "dscanner.suspicious.unused_parameter";
	private enum MSG = "Parameter %s is never used.";

	private static struct ParamInfo
	{
		string name;
		ulong lineNum;
		ulong charNum;
		bool isUsed = false;
	}

	private alias ParamSet = ParamInfo[string];
	private ParamSet[] usedParams;
	private bool inMixin;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
		pushScope();
	}

	override void visit(AST.FuncDeclaration funcDeclaration)
	{
		import std.format : format;

		pushScope();
		super.visit(funcDeclaration);

		bool shouldIgnoreWarns = funcDeclaration.fbody is null || funcDeclaration.storage_class & STC.override_;
		if (!shouldIgnoreWarns)
			currentScope.byValue
				.filter!(param => !param.isUsed)
				.each!(param => addErrorMessage(param.lineNum, param.charNum, KEY, MSG.format(param.name)));

		popScope();
	}

	override void visit(AST.Parameter parameter)
	{
		import dmd.astenums : TY;

		if (parameter.ident is null)
			return;

		auto varName = cast(string) parameter.ident.toString();
		bool shouldBeIgnored = varName.all!(c => c == '_') || parameter.storageClass & STC.ref_
			|| parameter.storageClass & STC.out_ || parameter.type.ty == TY.Tpointer;
		if (!shouldBeIgnored)
			currentScope[varName] = ParamInfo(varName, parameter.loc.linnum, parameter.loc.charnum);
	}

	override void visit(AST.TypeSArray newExp)
	{
		if (auto identifierExpression = newExp.dim.isIdentifierExp())
			identifierExpression.accept(this);
	}

	override void visit(AST.IdentifierExp identifierExp)
	{
		if (identifierExp.ident)
			markAsUsed(cast(string) identifierExp.ident.toString());

		super.visit(identifierExp);
	}

	mixin VisitMixin!(AST.MixinExp);
	mixin VisitMixin!(AST.MixinStatement);

	private template VisitMixin(NodeType)
	{
		override void visit(NodeType node)
		{
			inMixin = true;
			super.visit(node);
			inMixin = false;
		}
	}

	override void visit(AST.StringExp stringExp)
	{
		if (!inMixin)
			return;

		string str = cast(string) stringExp.toStringz();
		currentScope.byKey
			.filter!(param => canFind(str, param))
			.each!(param => markAsUsed(param));
	}

	override void visit(AST.TraitsExp traitsExp)
	{
		import dmd.dtemplate : isType;

		super.visit(traitsExp);

		if (traitsExp.args is null)
			return;

		(*traitsExp.args).opSlice()
			.map!(arg => isType(arg))
			.filter!(type => type !is null)
			.map!(type => type.isTypeIdentifier())
			.filter!(typeIdentifier => typeIdentifier !is null)
			.each!(typeIdentifier => markAsUsed(cast(string) typeIdentifier.toString()));
	}

	private extern (D) void markAsUsed(string varName)
	{
		import std.range : retro;

		foreach (funcScope; usedParams.retro())
		{
			if (varName in funcScope)
			{
				funcScope[varName].isUsed = true;
				break;
			}
		}
	}

	@property private extern (D) ParamSet currentScope()
	{
		return usedParams[$ - 1];
	}

	private void pushScope()
	{
		// Error with gdc-12
		//usedParams ~= new ParamSet;

		// Workaround for gdc-12
		ParamSet newScope;
		newScope["test"] = ParamInfo("test", 0, 0);
		usedParams ~= newScope;
		currentScope.remove("test");
	}

	private void popScope()
	{
		usedParams.length--;
	}
}

@system unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;

	StaticAnalysisConfig sac = disabledConfig();
	sac.unused_parameter_check = Check.enabled;

	assertAnalyzerWarningsDMD(q{

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

	assertAnalyzerWarningsDMD(q{
		void testMixinExpression(const Declaration decl)
		{
			foreach (property; possibleDeclarations)
				if (auto fn = mixin("decl." ~ property))
					addMessage(fn.name.type ? [fn.name] : fn.tokens, fn.name.text);
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		void testNestedFunction(int a)
		{
			int nestedFunction(int b)
			{
				return a + b;
			}

			nestedFunction(5);
		}

		void testNestedFunctionShadowing(int a) // [warn]: Parameter a is never used.
		{
			int nestedFunctionShadowing(int a)
			{
				return a + 5;
			}

			nestedFunction(5);
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		override protected void testOverrideFunction(int a)
		{
			return;
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		void testRefParam(ref LogEntry payload)
		{
			return;
		}

		void testOutParam(out LogEntry payload)
		{
			return;
		}

		void testPointerParam(LogEntry* payload)
		{
			return;
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		private char[] testStaticArray(size_t size) @safe pure nothrow
		{
    		return new char[size];
		}
	}c, sac);

	stderr.writeln("Unittest for UnusedParameterCheck passed.");
}
