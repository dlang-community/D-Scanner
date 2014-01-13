//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module style;

import stdx.d.ast;
import stdx.d.lexer;
import stdx.d.parser;
import std.stdio;
import std.regex;
import std.array;
import std.conv;

void doNothing(string, size_t, size_t, string) {}

void styleCheck(File output, string[] fileNames)
{
	foreach (fileName; fileNames)
	{
		File f = File(fileName);
		auto bytes = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(bytes);
		auto tokens = byToken(bytes);
		Module m = parseModule(tokens.array, fileName, &doNothing);
		auto checker = new StyleChecker;
		checker.fileName = fileName;
		checker.visit(m);
	}
}

class StyleChecker : ASTVisitor
{
	enum varFunNameRegex = `^([\p{Ll}_][_\w\d]*|[\p{Lu}\d_]+)$`;
	enum aggregateNameRegex = `^\p{Lu}[\w\d]*$`;
	enum moduleNameRegex = `^\p{Ll}+$`;
	
	override void visit(ModuleDeclaration dec)
	{
		foreach (part; dec.moduleName.identifiers)
		{
			if (part.text.matchFirst(moduleNameRegex).length == 0)
				writeln(fileName, "(", part.line, ":", part.column, ") ",
					"Module/package name ", part.text, " does not match style guidelines");
		}
	}
	
	override void visit(Declarator dec)
	{
		checkLowercaseName("Variable", dec.name);
	}
	
	override void visit(FunctionDeclaration dec)
	{
		checkLowercaseName("Function", dec.name);
	}
	
	void checkLowercaseName(string type, ref Token name)
	{
		if (name.text.matchFirst(varFunNameRegex).length == 0)
			writeln(fileName, "(", name.line, ":", name.column, ") ",
				type, " name ", name.text, " does not match style guidelines");
	}
	
	override void visit(ClassDeclaration dec)
	{
		checkAggregateName("Class", dec.name);
		dec.accept(this);
	}
	
	override void visit(InterfaceDeclaration dec)
	{
		checkAggregateName("Interface", dec.name);
		dec.accept(this);
	}
	
	override void visit(EnumDeclaration dec)
	{
		if (dec.name.text is null || dec.name.text.length == 0)
			return;
		checkAggregateName("Enum", dec.name);
		dec.accept(this);
	}
	
	override void visit(StructDeclaration dec)
	{
		checkAggregateName("Struct", dec.name);
		dec.accept(this);
	}
	
	void checkAggregateName(string aggregateType, ref Token name)
	{
		if (name.text.matchFirst(aggregateNameRegex).length == 0)
			writeln(fileName, "(", name.line, ":", name.column, ") ",
				aggregateType, " name ", name.text,
				" does not match style guidelines");
	}

	alias ASTVisitor.visit visit;
	string fileName;
}
