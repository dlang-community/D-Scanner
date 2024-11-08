//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.undocumented;

import dscanner.analysis.base;
import dmd.astenums : STC;
import std.format : format;
import std.regex : ctRegex, matchAll;

/**
 * Checks for undocumented public declarations. Ignores some operator overloads,
 * main functions, and functions whose name starts with "get" or "set".
 */
extern (C++) class UndocumentedDeclarationCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"undocumented_declaration_check";

	private enum KEY = "dscanner.style.undocumented_declaration";
	private enum DEFAULT_MSG = "Public declaration is undocumented.";
	private enum MSG = "Public declaration '%s' is undocumented.";

	private immutable string[] ignoredFunctionNames = [
		"opCmp", "opEquals", "toString", "toHash", "main"
	];
	private enum getSetRe = ctRegex!`^(?:get|set)(?:\p{Lu}|_).*`;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.VisibilityDeclaration visibilityDecl)
	{
		import dmd.dsymbol : Visibility;

		if (visibilityDecl.visibility.kind == Visibility.Kind.public_)
			super.visit(visibilityDecl);
	}

	override void visit(AST.StorageClassDeclaration storageClassDecl)
	{
		if (!hasIgnorableStorageClass(storageClassDecl.stc))
			super.visit(storageClassDecl);
	}

	override void visit(AST.DeprecatedDeclaration _) {}

	override void visit(AST.FuncDeclaration funcDecl)
	{
		if (funcDecl.comment() !is null || funcDecl.ident is null)
			return;

		string funcName = cast(string) funcDecl.ident.toString();
		bool canBeUndocumented = hasIgnorableStorageClass(funcDecl.storage_class) || isIgnorableFunctionName(funcName);

		if (!canBeUndocumented)
		{
			addErrorMessage(funcDecl.loc.linnum, funcDecl.loc.charnum, KEY, MSG.format(funcName));
			super.visit(funcDecl);
		}
	}

	private extern (D) bool isIgnorableFunctionName(string funcName)
	{
		import std.algorithm : canFind;

		return ignoredFunctionNames.canFind(funcName) || !matchAll(funcName, getSetRe).empty;
	}

	override void visit(AST.CtorDeclaration ctorDecl)
	{
		if (ctorDecl.comment() !is null)
			return;

		addErrorMessage(ctorDecl.loc.linnum, ctorDecl.loc.charnum, KEY, DEFAULT_MSG);
	}

	override void visit(AST.TemplateDeclaration templateDecl)
	{
		if (templateDecl.comment() !is null || templateDecl.ident is null)
			return;

		if (!templateDecl.isDeprecated())
		{
			string templateName = cast(string) templateDecl.ident.toString();
			addErrorMessage(templateDecl.loc.linnum, templateDecl.loc.charnum, KEY, MSG.format(templateName));
		}
	}

	mixin VisitDeclaration!(AST.ClassDeclaration);
	mixin VisitDeclaration!(AST.InterfaceDeclaration);
	mixin VisitDeclaration!(AST.StructDeclaration);
	mixin VisitDeclaration!(AST.UnionDeclaration);
	mixin VisitDeclaration!(AST.EnumDeclaration);
	mixin VisitDeclaration!(AST.EnumMember);
	mixin VisitDeclaration!(AST.VarDeclaration);

	private template VisitDeclaration(NodeType)
	{
		override void visit(NodeType decl)
		{
			if (decl.comment() !is null || decl.ident is null)
			{
				super.visit(decl);
				return;
			}

			bool canBeUndocumented;
			static if (__traits(hasMember, NodeType, "storage_class"))
				canBeUndocumented = hasIgnorableStorageClass(decl.storage_class);

			if (!canBeUndocumented)
			{
				string declName = cast(string) decl.ident.toString();
				addErrorMessage(decl.loc.linnum, decl.loc.charnum, KEY, MSG.format(declName));
				super.visit(decl);
			}
		}
	}

	private bool hasIgnorableStorageClass(ulong storageClass)
	{
		return (storageClass & STC.deprecated_) || (storageClass & STC.override_)
			|| (storageClass & STC.disable) || (storageClass & STC.property);
	}

	override void visit(AST.UnitTestDeclaration _) {}

	override void visit(AST.TraitsExp _) {}

	override void visit(AST.ConditionalDeclaration conditionalDecl)
	{
		auto versionCond = conditionalDecl.condition.isVersionCondition();

		if (versionCond is null)
			super.visit(conditionalDecl);

		if (isIgnorableVersion(versionCond) && conditionalDecl.elsedecl)
		{
			foreach (decl; *(conditionalDecl.elsedecl))
				super.visit(decl);
		}
	}

	override void visit(AST.ConditionalStatement conditionalStatement)
	{
		auto versionCond = conditionalStatement.condition.isVersionCondition();

		if (versionCond is null)
			super.visit(conditionalStatement);

		if (isIgnorableVersion(versionCond) && conditionalStatement.elsebody)
		{
			super.visit(conditionalStatement.elsebody);
		}
	}

	private bool isIgnorableVersion(AST.VersionCondition versionCond)
	{
		if (versionCond is null || versionCond.ident is null)
			return false;

		string versionStr = cast(string) versionCond.ident.toString();

		return versionStr == "unittest" || versionStr == "none";
	}
}

unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;

	StaticAnalysisConfig sac = disabledConfig();
	sac.undocumented_declaration_check = Check.enabled;

	assertAnalyzerWarningsDMD(q{
		private int x;
		int y; // [warn]: Public declaration 'y' is undocumented.
		public int z; // [warn]: Public declaration 'z' is undocumented.

		///
		class C
		{
			int h; // [warn]: Public declaration 'h' is undocumented.

		public:
			int g; // [warn]: Public declaration 'g' is undocumented.
			void f() {} // [warn]: Public declaration 'f' is undocumented.

		private:
			int a;
			int b;
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		deprecated int y;

		///
		class C
		{
			private int b;
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		class C{} // [warn]: Public declaration 'C' is undocumented.
		interface I{} // [warn]: Public declaration 'I' is undocumented.
		enum e = 0; // [warn]: Public declaration 'e' is undocumented.
		void f(){} // [warn]: Public declaration 'f' is undocumented.
		struct S{} // [warn]: Public declaration 'S' is undocumented.
		template T(){} // [warn]: Public declaration 'T' is undocumented.
		union U{} // [warn]: Public declaration 'U' is undocumented.
	}, sac);

	assertAnalyzerWarningsDMD(q{
		/// C
		class C{}
		/// I
		interface I{}
		/// e
		enum e = 0;
		/// f
		void f(){}
		/// S
		struct S{}
		/// T
		template T(){}
		/// U
		union U{}
	}, sac);

	// https://github.com/dlang-community/D-Scanner/issues/760
	assertAnalyzerWarningsDMD(q{
		deprecated("This has been deprecated") auto func(){}
		deprecated auto func(){}
		deprecated auto func()(){}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		class C{} /// a
		interface I{} /// b
		enum e = 0; /// c
		void f(){} /// d
		struct S{} /// e
		template T(){} /// f
		union U{} /// g
	}, sac);

	assertAnalyzerWarningsDMD(q{
		int x; // [warn]: Public declaration 'x' is undocumented.
		int y; ///

		///
		class C
		{
			private int a;
			int b; ///
			int c; // [warn]: Public declaration 'c' is undocumented.
			protected int d;
		}

		///
		class D
		{
			///
			void fun()
			{
				class Inner
				{
					int z;
				}

				int a1, a2, a3;
			}
		}
	}c, sac);

	stderr.writeln("Unittest for UndocumentedDeclarationCheck passed.");
}
