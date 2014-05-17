//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.style;

import std.stdio;
import std.d.ast;
import std.d.lexer;
import std.regex;
import std.array;
import std.conv;
import std.format;
import analysis.helpers;

import analysis.base;

class StyleChecker : BaseAnalyzer
{
	alias visit = ASTVisitor.visit;

	// FIXME: All variable and function names seem to never match this
	enum varFunNameRegex = `^([\p{Ll}_][_\w\d]*|[\p{Lu}\d_]+)$`;
	enum aggregateNameRegex = `^\p{Lu}[\w\d]*$`;
	enum moduleNameRegex = `^[\p{Ll}_\d]+$`;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const ModuleDeclaration dec)
	{
		foreach (part; dec.moduleName.identifiers)
		{
			if (part.text.matchFirst(moduleNameRegex).length == 0)
				addErrorMessage(part.line, part.column, "Module/package name '"
					~ part.text ~ "' does not match style guidelines");
		}
	}

	override void visit(const Declarator dec)
	{
		checkLowercaseName("Variable", dec.name);
	}

	override void visit(const FunctionDeclaration dec)
	{
		checkLowercaseName("Function", dec.name);
	}

	void checkLowercaseName(string type, ref const Token name)
	{
		if (name.text.matchFirst(varFunNameRegex).length == 0)
			addErrorMessage(name.line, name.column, type ~ " name "
				~ name.text ~ " does not match style guidelines");
	}

	override void visit(const ClassDeclaration dec)
	{
		checkAggregateName("Class", dec.name);
		dec.accept(this);
	}

	override void visit(const InterfaceDeclaration dec)
	{
		checkAggregateName("Interface", dec.name);
		dec.accept(this);
	}

	override void visit(const EnumDeclaration dec)
	{
		if (dec.name.text is null || dec.name.text.length == 0)
			return;
		checkAggregateName("Enum", dec.name);
		dec.accept(this);
	}

	override void visit(const StructDeclaration dec)
	{
		checkAggregateName("Struct", dec.name);
		dec.accept(this);
	}

	void checkAggregateName(string aggregateType, ref const Token name)
	{
		if (name.text.matchFirst(aggregateNameRegex).length == 0)
			addErrorMessage(name.line, name.column, aggregateType
				~ " name '" ~ name.text ~ "' does not match style guidelines");
	}
}

unittest
{
	shouldWarn(q{
		module AMODULE; // [warn]: Module/package name 'AMODULE' does not match style guidelines

		bool A_VARIABLE; // FIXME:
		bool a_variable; // ok
		bool aVariable; // ok

		void A_FUNCTION() {} // FIXME:
		class cat {} // [warn]: Class name 'cat' does not match style guidelines
		interface puma {} // [warn]: Interface name 'puma' does not match style guidelines
		struct dog {} // [warn]: Struct name 'dog' does not match style guidelines
		enum racoon {} // [warn]: Enum name 'racoon' does not match style guidelines
	}c, analysis.run.AnalyzerCheck.style_check);

	stderr.writeln("Unittest for StyleChecker passed.");
}

