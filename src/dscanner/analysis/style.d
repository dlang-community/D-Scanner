//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.style;

import dscanner.analysis.base;
import dmd.astenums : LINK;
import dmd.location : Loc;
import std.conv : to;
import std.format : format;
import std.regex;

extern (C++) class StyleChecker(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"style_check";
	alias visit = BaseAnalyzerDmd.visit;

	private enum KEY = "dscanner.suspicious.style_check";
	private enum MSG = "%s name '%s' does not match style guidelines.";

	private enum varFunNameRegex = `^([\p{Ll}_][_\w\d]*|[\p{Lu}\d_]+)$`;
	private enum aggregateNameRegex = `^\p{Lu}[\w\d]*$`;
	private enum moduleNameRegex = `^[\p{Ll}_\d]+$`;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.Module moduleNode)
	{
		if (shouldIgnoreDecl(moduleNode.userAttribDecl(), KEY))
			return;

		super.visit(moduleNode);

		if (moduleNode.md is null)
			return;

		auto moduleDecl = *moduleNode.md;
		auto moduleName = cast(string) moduleDecl.id.toString();

		if (moduleName.matchFirst(moduleNameRegex).length == 0)
			addError(moduleDecl.loc, "Module/package", moduleName);

		foreach (pkg; moduleDecl.packages)
		{
			auto pkgName = pkg.toString();

			if (pkgName.matchFirst(moduleNameRegex).length == 0)
				addError(moduleDecl.loc, "Module/package", moduleName);
		}
	}

	override void visit(AST.LinkDeclaration linkDeclaration)
	{
		if (linkDeclaration.decl is null)
			return;

		foreach (symbol; *linkDeclaration.decl)
			if (!isWindowsFunctionWithNoBody(symbol, linkDeclaration.linkage))
				symbol.accept(this);
	}

	private bool isWindowsFunctionWithNoBody(AST.Dsymbol symbol, LINK linkage)
	{
		auto fd = symbol.isFuncDeclaration();
		return linkage == LINK.windows && fd && !fd.fbody;
	}

	override void visit(AST.VarDeclaration varDeclaration)
	{
		import dmd.astenums : STC;

		super.visit(varDeclaration);

		if (varDeclaration.storage_class & STC.manifest || varDeclaration.ident is null)
			return;

		auto varName = cast(string) varDeclaration.ident.toString();

		if (varName.matchFirst(varFunNameRegex).length == 0)
			addError(varDeclaration.loc, "Variable", varName);
	}

	mixin VisitNode!(AST.ClassDeclaration, "Class", aggregateNameRegex);
	mixin VisitNode!(AST.StructDeclaration, "Struct", aggregateNameRegex);
	mixin VisitNode!(AST.InterfaceDeclaration, "Interface", aggregateNameRegex);
	mixin VisitNode!(AST.UnionDeclaration, "Union", aggregateNameRegex);
	mixin VisitNode!(AST.EnumDeclaration, "Enum", aggregateNameRegex);
	mixin VisitNode!(AST.FuncDeclaration, "Function", varFunNameRegex);
	mixin VisitNode!(AST.TemplateDeclaration, "Template", varFunNameRegex);

	private template VisitNode(NodeType, string nodeName, string regex)
	{
		override void visit(NodeType node)
		{
			super.visit(node);

			if (node.ident is null)
				return;

			auto nodeSymbolName = cast(string) node.ident.toString();

			if (nodeSymbolName.matchFirst(regex).length == 0)
				addError(node.loc, nodeName, nodeSymbolName);
		}
	}

	private extern (D) void addError(Loc loc, string nodeType, string nodeName)
	{
		auto fileOffset = cast(ulong) loc.fileOffset;
		auto lineNum = cast(ulong) loc.linnum;
		auto charNum = cast(ulong) loc.charnum;
		ulong[2] index = [fileOffset, fileOffset + nodeName.length];
		ulong[2] lines = [lineNum, lineNum];
		ulong[2] columns = [charNum, charNum + nodeName.length];
		addErrorMessage(index, lines, columns, KEY, MSG.format(nodeType, nodeName));
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.style_check = Check.enabled;

	assertAnalyzerWarningsDMD(q{
		module AMODULE; // [warn]: Module/package name 'AMODULE' does not match style guidelines.

		bool A_VARIABLE; // FIXME:
		bool a_variable; // ok
		bool aVariable; // ok

		void A_FUNCTION() {} // FIXME:
		class cat {} // [warn]: Class name 'cat' does not match style guidelines.
		interface puma {} // [warn]: Interface name 'puma' does not match style guidelines.
		struct dog {} // [warn]: Struct name 'dog' does not match style guidelines.
		enum racoon { a } // [warn]: Enum name 'racoon' does not match style guidelines.
		enum bool something = false;
		enum bool someThing = false;
		enum Cat { fritz, }
		enum Cat = Cat.fritz;
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		extern(Windows)
		{
			bool Fun0();
			extern(Windows) bool Fun1();
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		extern(Windows)
		{
			extern(D) bool Fun2(); // [warn]: Function name 'Fun2' does not match style guidelines.
			bool Fun3();
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		extern(Windows)
		{
			extern(C):
				extern(D) bool Fun4(); // [warn]: Function name 'Fun4' does not match style guidelines.
				bool Fun5(); // [warn]: Function name 'Fun5' does not match style guidelines.
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		extern(Windows):
			bool Fun6();
			bool Fun7();
		extern(D):
			void okOkay();
			void NotReallyOkay(); // [warn]: Function name 'NotReallyOkay' does not match style guidelines.
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		extern(Windows):
			bool WinButWithBody(){} // [warn]: Function name 'WinButWithBody' does not match style guidelines.
	}c, sac);

	stderr.writeln("Unittest for StyleChecker passed.");
}
