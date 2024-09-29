// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.has_public_example;

import dscanner.analysis.base;

/**
 * Checks for public declarations without a documented unittests.
 * For now, variable and enum declarations aren't checked.
 */
extern (C++) class HasPublicExampleCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"has_public_example";

	private enum KEY = "dscanner.style.has_public_example";
	private enum DEFAULT_MSG = "Public declaration has no documented example.";
	private enum MSG = "Public declaration '%s' has no documented example.";

	private struct DeclarationInfo
	{
		bool ignore;
		string name;
		ulong lineNum;
		ulong charNum;
	}

	private DeclarationInfo lastDecl = DeclarationInfo(true);
	private bool isDocumented;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.Module mod)
	{
		super.visit(mod);
		checkLastDecl();
	}

	override void visit(AST.ConditionalStatement _) {}

	override void visit(AST.ConditionalDeclaration _) {}

	override void visit(AST.UnitTestDeclaration unitTestDecl)
	{
		if (skipTests)
			return;

		if (unitTestDecl.comment() !is null)
			isDocumented = true;
	}

	override void visit(AST.DeprecatedDeclaration _)
	{
		lastDecl = DeclarationInfo(true);
	}

	override void visit(AST.StorageClassDeclaration storageClassDecl)
	{
		if (!hasIgnorableStorageClass(storageClassDecl.stc))
			super.visit(storageClassDecl);
		else
			lastDecl = DeclarationInfo(true);
	}

	private bool hasIgnorableStorageClass(ulong storageClass)
	{
		import dmd.astenums : STC;

		return (storageClass & STC.deprecated_) || (storageClass & STC.manifest);
	}

	override void visit(AST.VisibilityDeclaration visibilityDecl)
	{
		import dmd.dsymbol : Visibility;

		auto visibilityKind = visibilityDecl.visibility.kind;
		bool isPrivate = visibilityKind == Visibility.Kind.private_
			|| visibilityKind == Visibility.Kind.package_
			|| visibilityKind == Visibility.Kind.protected_;

		if (isPrivate)
			checkLastDecl();
		else
			super.visit(visibilityDecl);
	}

	mixin VisitDeclaration!(AST.ClassDeclaration);
	mixin VisitDeclaration!(AST.InterfaceDeclaration);
	mixin VisitDeclaration!(AST.StructDeclaration);
	mixin VisitDeclaration!(AST.UnionDeclaration);
	mixin VisitDeclaration!(AST.FuncDeclaration);
	mixin VisitDeclaration!(AST.TemplateDeclaration);

	private template VisitDeclaration(NodeType)
	{
		override void visit(NodeType node)
		{
			import std.conv : to;
			import std.string : strip, toLower;

			static if (is(NodeType == AST.TemplateDeclaration))
			{
				if (shouldTemplateBeSkipped(node))
					return;
			}

			bool isCommented = node.comment() !is null;

			if (isCommented)
			{
				string comment = to!string(node.comment());
				if (comment.strip().toLower() == "ditto")
					return;
			}

			checkLastDecl();

			if (isCommented)
			{
				string name = node.ident ? cast(string) node.ident.toString() : null;
				lastDecl = DeclarationInfo(false, name, cast(ulong) node.loc.linnum, cast(ulong) node.loc.charnum);
			}

			isDocumented = false;
		}
	}

	private bool shouldTemplateBeSkipped(AST.TemplateDeclaration templateDecl)
	{
		if (templateDecl.members is null)
			return false;

		foreach (member; *(templateDecl.members))
			if (auto var = member.isVarDeclaration())
				if (hasIgnorableStorageClass(var.storage_class))
					return true;

		return false;
	}

	private void checkLastDecl()
	{
		import std.format : format;

		if (!lastDecl.ignore && !isDocumented)
		{
			string msg = lastDecl.name ? MSG.format(lastDecl.name) : DEFAULT_MSG;
			addErrorMessage(lastDecl.lineNum, lastDecl.charNum, KEY, msg);
		}

		lastDecl = DeclarationInfo(true);
	}
}

unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;

	StaticAnalysisConfig sac = disabledConfig();
	sac.has_public_example = Check.enabled;

	assertAnalyzerWarningsDMD(q{
		/// C
		class C{}
		///
		unittest {}

		/// I
		interface I{}
		///
		unittest {}

		/// e
		enum e = 0;
		///
		unittest {}

		/// f
		void f(){}
		///
		unittest {}

		/// S
		struct S{}
		///
		unittest {}

		/// T
		template T(){}
		///
		unittest {}

		/// U
		union U{}
		///
		unittest {}
	}, sac);

	// enums or variables don't need to have public unittest
	assertAnalyzerWarningsDMD(q{
		/// C
		class C{} // [warn]: Public declaration 'C' has no documented example.
		unittest {}

		/// I
		interface I{} // [warn]: Public declaration 'I' has no documented example.
		unittest {}

		/// f
		void f(){} // [warn]: Public declaration 'f' has no documented example.
		unittest {}

		/// S
		struct S{} // [warn]: Public declaration 'S' has no documented example.
		unittest {}

		/// T
		template T(){} // [warn]: Public declaration 'T' has no documented example.
		unittest {}

		/// U
		union U{} // [warn]: Public declaration 'U' has no documented example.
		unittest {}
	}, sac);

	// test module header unittest
	assertAnalyzerWarningsDMD(q{
		unittest {}
		/// C
		class C{} // [warn]: Public declaration 'C' has no documented example.
	}, sac);

	// test documented module header unittest
	assertAnalyzerWarningsDMD(q{
		///
		unittest {}
		/// C
		class C{} // [warn]: Public declaration 'C' has no documented example.
	}, sac);

	// test multiple unittest blocks
	assertAnalyzerWarningsDMD(q{
		/// C
		class C{} // [warn]: Public declaration 'C' has no documented example.
		unittest {}
		unittest {}
		unittest {}

		/// U
		union U{}
		unittest {}
		///
		unittest {}
		unittest {}
	}, sac);

	/// check private
	assertAnalyzerWarningsDMD(q{
		/// C
		private class C{}

		/// I
		protected interface I{}

		/// e
		package enum e = 0;

		/// f
		package(std) void f(){}

		/// S
		extern(C) struct S{}
		///
		unittest {}
	}, sac);

	// check intermediate private declarations
	// removed for issue #500
	/*assertAnalyzerWarningsDMD(q{
		/// C
		class C{}
		private void foo(){}
		///
		unittest {}
	}, sac);*/

	// check intermediate ditto-ed declarations
	assertAnalyzerWarningsDMD(q{
		/// I
		interface I{}
		/// ditto
		void f(){}
		///
		unittest {}
	}, sac);

	// test reset on private symbols (#500)
	assertAnalyzerWarningsDMD(q{
		///
		void dirName(C)(C[] path) {} // [warn]: Public declaration 'dirName' has no documented example.
		private void _dirName(R)(R path) {}
		///
		unittest {}
	}, sac);

	// deprecated symbols shouldn't require a test
	assertAnalyzerWarningsDMD(q{
		///
		deprecated void dirName(C)(C[] path) {}
	}, sac);

	stderr.writeln("Unittest for HasPublicExampleCheck passed.");
}
