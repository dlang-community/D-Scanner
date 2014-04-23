//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.style;

import std.d.ast;
import std.d.lexer;
import std.regex;
import std.array;
import std.conv;
import std.format;

import analysis.base;

class StyleChecker : BaseAnalyzer
{
	enum varFunNameRegex = `^([\p{Ll}_][_\w\d]*|[\p{Lu}\d_]+)$`;
	enum aggregateNameRegex = `^\p{Lu}[\w\d]*$`;
	enum moduleNameRegex = `^\p{Ll}+$`;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const ModuleDeclaration dec)
	{
		foreach (part; dec.moduleName.identifiers)
		{
			if (part.text.matchFirst(moduleNameRegex).length == 0)
				addErrorMessage(part.line, part.column, "Module/package name "
					~ part.text ~ " does not match style guidelines");
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

	alias visit = ASTVisitor.visit;
}
