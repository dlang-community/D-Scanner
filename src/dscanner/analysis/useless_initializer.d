//          Copyright Basile Burg 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.useless_initializer;

import dscanner.analysis.base;
import dmd.astenums : InitKind, STC, TY;
import std.format : format;

/*
Limitations:
	- Stuff s = Stuff.init does not work with type with postfixes`*` `[]`.
	- Stuff s = Stuff.init is only detected for struct within the module.
	- BasicType b = BasicType(v), default init used in BasicType ctor, e.g int(8).
*/

/**
 * Check that detects the initializers that are
 * not different from the implcit initializer.
 */
// TODO: Fix NoLint
extern (C++) class UselessInitializerChecker(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"useless_initializer";

	private enum KEY = "dscanner.useless-initializer";
	private enum MSG = "Variable '%s' initializer is useless because it does not differ from the default value";

	private struct StructInfo
	{
		string name;
		bool shouldErrorOnInit;
		bool isBeingVisited;
	}

	private StructInfo[string] visitedStructs;
	private string[] structStack;
	private bool inTest;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.UnitTestDeclaration unitTestDecl)
	{
		if (skipTests)
			return;

		inTest = true;
		super.visit(unitTestDecl);
		inTest = false;
	}

	override void visit(AST.StructDeclaration structDecl)
	{
		if (inTest || structDecl.ident is null)
			return;

		string structName = cast(string) structDecl.ident.toString();
		if (isNestedStruct())
			structName = structStack[$ - 1] ~ "." ~ structName;

		bool isDisabled = (structDecl.storage_class & STC.disable) != 0;
		visitedStructs[structName] = StructInfo(structName, !isDisabled, true);
		structStack ~= structName;
		super.visit(structDecl);

		visitedStructs[structName].isBeingVisited = false;
		structStack.length--;
	}

	private bool isNestedStruct()
	{
		if (structStack.length >= 1)
			return visitedStructs[structStack[$ - 1]].isBeingVisited;

		return false;
	}

	override void visit(AST.CtorDeclaration ctorDeclaration)
	{
		super.visit(ctorDeclaration);

		bool isDefaultCtor = ctorDeclaration.getParameterList().length() == 0;

		if (structStack.length == 0 || !isDefaultCtor)
			return;

		auto structName = structStack[$ - 1];
		if (!visitedStructs[structName].isBeingVisited || !visitedStructs[structName].shouldErrorOnInit)
			return;

		bool isDisabled = (ctorDeclaration.storage_class & STC.disable) != 0;
		visitedStructs[structName].shouldErrorOnInit = !isDisabled;
	}

	override void visit(AST.VarDeclaration varDecl)
	{
		import std.format : format;

		super.visit(varDecl);

		// issue 474, manifest constants HAVE to be initialized initializer has to appear clearly in generated ddoc
		// https://github.com/dlang-community/d-Scanner/issues/474
		if (varDecl._init is null || varDecl.storage_class & STC.manifest || varDecl.comment())
			return;

		ulong lineNum = cast(ulong) varDecl.loc.linnum;
		ulong charNum = cast(ulong) varDecl.loc.charnum;
		string msg = MSG.format(varDecl.ident.toString());

		if (auto expInit = varDecl._init.isExpInitializer())
		{
			bool isBasicType;
			if (varDecl.type)
				isBasicType = isBasicTypeConstant(varDecl.type.ty);

			if (isRedundantExpInit(expInit.exp, isBasicType))
				addErrorMessage(lineNum, charNum, KEY, msg);
		}
		else if (auto arrInit = varDecl._init.isArrayInitializer())
		{
			if (arrInit.dim == 0 && arrInit.index.length == 0 && arrInit.value.length == 0)
				addErrorMessage(lineNum, charNum, KEY, msg);
		}
	}

	private bool isBasicTypeConstant(TY type)
	{
		return (type >= TY.Tint8 && type <= TY.Tdchar) || type == TY.Tint128 || type == TY.Tuns128;
	}

	private bool isRedundantExpInit(AST.Expression exp, bool isBasicType)
	{
		if (auto intExp = exp.isIntegerExp())
			return intExp.getInteger() == 0;

		if (auto dotIdExp = exp.isDotIdExp())
		{
			if (dotIdExp.ident is null)
				return false;

			bool shouldLookForInit;

			if (isBasicType)
			{
				shouldLookForInit = true;
			}
			else
			{
				string structName = computeStructNameFromDotChain(dotIdExp);
				if (structName in visitedStructs)
					shouldLookForInit = visitedStructs[structName].shouldErrorOnInit;
			}

			if (shouldLookForInit)
				return cast(string) dotIdExp.ident.toString() == "init";

			return false;
		}

		return exp.isNullExp() !is null;
	}

	private extern (D) string computeStructNameFromDotChain(AST.DotIdExp dotIdExp)
	{
		if (dotIdExp.ident is null)
			return "";

		string name;
		auto parent = dotIdExp.e1;

		while (parent && parent.isDotIdExp())
		{
			auto dotIdParent = parent.isDotIdExp();
			if (dotIdParent.ident is null)
				return "";

			name = cast(string) dotIdParent.ident.toString() ~ "." ~ name;
			parent = dotIdParent.e1;
		}

		auto idExp = parent.isIdentifierExp();
		if (idExp && idExp.ident)
		{
			string structName = cast(string) idExp.ident.toString();
			if (name.length > 0)
				return structName = structName ~ "." ~ name[0 .. $ - 1];

			return structName;
		}

		return "";
	}

	// issue 473, prevent to visit delegates that contain duck type checkers.
	// https://github.com/dlang-community/d-Scanner/issues/473
	override void visit(AST.TypeTypeof _)
	{
	}

	// issue 473, prevent to check expressions in __traits(compiles, ...)
	// https://github.com/dlang-community/d-Scanner/issues/473
	override void visit(AST.TraitsExp traitsExp)
	{
		if (traitsExp.ident.toString() != "compiles")
			super.visit(traitsExp);
	}
}

@system unittest
{
	import dscanner.analysis.config : Check, disabledConfig, StaticAnalysisConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig;
	sac.useless_initializer = Check.enabled;
	enum msgA = "Variable 'a' initializer is useless because it does not differ from the default value";
	enum msgS = "Variable 's' initializer is useless because it does not differ from the default value";

	assertAnalyzerWarningsDMD(q{
		struct Outer
		{
			struct Inner {}
		}
		Outer.Inner s = Outer.Inner.init; // [warn]: %s
	}c.format(msgS), sac);

	// fails
	assertAnalyzerWarningsDMD(q{
		struct S {}
		ubyte a = 0x0;      // [warn]: %s
		int a = 0;          // [warn]: %s
		ulong a = 0;        // [warn]: %s
		int* a = null;      // [warn]: %s
		Foo* a = null;      // [warn]: %s
		int[] a = null;     // [warn]: %s
		int[] a = [];       // [warn]: %s
		string a = null;    // [warn]: %s
		string a = null;    // [warn]: %s
		wstring a = null;   // [warn]: %s
		dstring a = null;   // [warn]: %s
		size_t a = 0;       // [warn]: %s
		ptrdiff_t a = 0;    // [warn]: %s
		string a = [];      // [warn]: %s
		char[] a = null;    // [warn]: %s
		int a = int.init;   // [warn]: %s
		char a = char.init; // [warn]: %s
		S s = S.init;       // [warn]: %s
		bool a = false; 	// [warn]: %s
	}.format(msgA, msgA, msgA, msgA, msgA, msgA, msgA, msgA, msgA, msgA, msgA,
			msgA, msgA, msgA, msgA, msgA, msgA, msgS, msgA), sac);

	// passes
	assertAnalyzerWarningsDMD(q{
		struct D {@disable this();}
		struct E {this() @disable;}
		ubyte a = 0xFE;
		int a = 1;
		ulong a = 1;
		int* a = &a;
		Foo* a = &a;
		int[] a = &a;
		int[] a = [0];
		string a = "sdf";
		string a = "sdg"c;
		wstring a = "sdg"w;
		dstring a = "fgh"d;
		string a = q{int a;};
		size_t a = 1;
		ptrdiff_t a;
		ubyte a;
		int a;
		ulong a;
		int* a;
		Foo* a;
		int[] a;
		string a;
		wstring a;
		dstring a;
		string a = ['a'];
		string a = "";
		string a = ""c;
		wstring a = ""w;
		dstring a = ""d;
		string a = q{};
		char[] a = "ze";
		S s = S(0,1);
		S s = s.call();
		enum {a}
		enum ubyte a = 0;
		int a = 0; /// Documented with default initializer
		static assert(is(typeof((){T t = T.init;})));
		void foo(){__traits(compiles, (){int a = 0;}).writeln;}
		bool a;
		D d = D.init;
		E e = E.init;
		NotKnown nk = NotKnown.init;
	}, sac);

	// passes
	//assertAnalyzerWarnings(q{
	//	@("nolint(dscanner.useless-initializer)")
	//	int a = 0;
	//	int a = 0;          /+
	//	        ^ [warn]: X +/
	//
	//	@("nolint(dscanner.useless-initializer)")
	//	int f() {
	//		int a = 0;
	//	}
	//
	//	struct nolint { string s; }
	//
	//	@nolint("dscanner.useless-initializer")
	//	int a = 0;
	//	int a = 0;          /+
	//	        ^ [warn]: X +/
	//
	//	@("nolint(other_check, dscanner.useless-initializer, another_one)")
	//	int a = 0;
	//
	//	@nolint("other_check", "another_one", "dscanner.useless-initializer")
	//	int a = 0;
	//
	//}, sac);

	// passes (disable check at module level)
	//assertAnalyzerWarnings(q{
	//	@("nolint(dscanner.useless-initializer)")
	//	module my_module;
	//
	//	int a = 0;
	//
	//	int f() {
	//		int a = 0;
	//	}
	//}, sac);

	stderr.writeln("Unittest for UselessInitializerChecker passed.");
}
