// Copyright (c) 2018, dlang-community
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.redundant_storage_class;

import std.string;
import dscanner.analysis.base;

/**
 * Checks for redundant storage classes such immutable and __gshared, static and __gshared
 */
extern (C++) class RedundantStorageClassCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"redundant_storage_classes";

	private enum KEY = "dscanner.unnecessary.duplicate_attribute";
	private enum string REDUNDANT_VARIABLE_ATTRIBUTES = "Variable declaration for `%s` has redundant attributes (%-(`%s`%|, %)).";

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.VarDeclaration varDecl)
	{
		import dmd.astenums : STC;

		if (varDecl.storage_class & STC.immutable_ && varDecl.storage_class & STC.shared_)
			addErrorFor(varDecl, "immutable", "shared");

		if (varDecl.storage_class & STC.immutable_ && varDecl.storage_class & STC.gshared)
			addErrorFor(varDecl, "immutable", "__gshared");

		if (varDecl.storage_class & STC.static_ && varDecl.storage_class & STC.gshared)
			addErrorFor(varDecl, "static", "__gshared");
	}

	extern (D) private void addErrorFor(AST.VarDeclaration varDecl, string attr1, string attr2)
	{
		auto lineNum = cast(ulong) varDecl.loc.linnum;
		auto charNum = cast(ulong) varDecl.loc.charnum;
		auto varName = varDecl.ident.toString();
		auto errorMsg = REDUNDANT_VARIABLE_ATTRIBUTES.format(varName, [
			attr1, attr2
		]);
		addErrorMessage(lineNum, charNum, KEY, errorMsg);
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import std.stdio : stderr;
	import std.format : format;

	StaticAnalysisConfig sac = disabledConfig();
	sac.redundant_storage_classes = Check.enabled;

	enum string erorMsg = "Variable declaration for `%s` has redundant attributes (%-(`%s`%|, %)).";
	auto immutableSharedMsg = erorMsg.format("a", ["immutable", "shared"]);
	auto immutableGSharedMsg = erorMsg.format("a", ["immutable", "__gshared"]);
	auto staticGSharedMsg = erorMsg.format("a", ["static", "__gshared"]);

	// https://github.com/dlang-community/D-Scanner/issues/438
	assertAnalyzerWarningsDMD(q{
		immutable int a;

		immutable shared int a; // [warn]: %s
		shared immutable int a; // [warn]: %s

		immutable __gshared int a; // [warn]: %s
		__gshared immutable int a; // [warn]: %s

		__gshared static int a; // [warn]: %s

		shared static int a;
		static shared int a;
		static immutable int a;
		immutable static int a;

		enum int a;
		extern(C++) immutable int a;
		immutable int function(immutable int, shared int) a;
	}c.format(immutableSharedMsg, immutableSharedMsg, immutableGSharedMsg,
			immutableGSharedMsg, staticGSharedMsg), sac);

	stderr.writeln("Unittest for RedundantStorageClassCheck passed.");
}
