// Copyright (c) 2018, dlang-community
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.redundant_storage_class;

import std.stdio;
import std.string;
import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import dscanner.analysis.helpers;
import dsymbol.scope_ : Scope;

/**
 * Checks for redundant storage classes such immutable and __gshared, static and __gshared
 */
final class RedundantStorageClassCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;
	enum string REDUNDANT_VARIABLE_ATTRIBUTES = "Variable declaration for `%s` has redundant attributes (%-(`%s`%|, %)).";

	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	override void visit(const Declaration node)
	{
		checkAttributes(node);
		node.accept(this);
	}

	void checkAttributes(const Declaration node)
	{
		if (node.variableDeclaration !is null && node.attributes !is null)
			checkVariableDeclaration(node.variableDeclaration, node.attributes);
	}

	void checkVariableDeclaration(const VariableDeclaration vd, const Attribute[] attributes)
	{
		import std.algorithm.comparison : among;
		import std.algorithm.searching: all;

		string[] globalAttributes;
		foreach (attrib; attributes)
		{
			if (attrib.attribute.type.among(tok!"shared", tok!"static", tok!"__gshared", tok!"immutable"))
				globalAttributes ~= attrib.attribute.type.str;
		}
		if (globalAttributes.length > 1)
		{
			if (globalAttributes.length == 2 && (
					globalAttributes.all!(a => a.among("shared", "static")) ||
					globalAttributes.all!(a => a.among("static", "immutable"))
			))
				return;
			auto t = vd.declarators[0].name;
			string message = REDUNDANT_VARIABLE_ATTRIBUTES.format(t.text, globalAttributes);
			addErrorMessage(t.line, t.column, "dscanner.unnecessary.duplicate_attribute", message);
		}
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.redundant_storage_classes = Check.enabled;

	// https://github.com/dlang-community/D-Scanner/issues/438
	assertAnalyzerWarnings(q{
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
	}c.format(
		RedundantStorageClassCheck.REDUNDANT_VARIABLE_ATTRIBUTES.format("a", ["immutable", "shared"]),
		RedundantStorageClassCheck.REDUNDANT_VARIABLE_ATTRIBUTES.format("a", ["shared", "immutable"]),
		RedundantStorageClassCheck.REDUNDANT_VARIABLE_ATTRIBUTES.format("a", ["immutable", "__gshared"]),
		RedundantStorageClassCheck.REDUNDANT_VARIABLE_ATTRIBUTES.format("a", ["__gshared", "immutable"]),
		RedundantStorageClassCheck.REDUNDANT_VARIABLE_ATTRIBUTES.format("a", ["__gshared", "static"]),
	), sac);

	stderr.writeln("Unittest for RedundantStorageClassCheck passed.");
}
