//          Copyright Brian Schott (Hackerpilot) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.outliner;

import dparse.lexer;
import dparse.ast;
import dparse.formatter;
import std.stdio;
import std.string;
import std.array;
import std.conv;

class Outliner : ASTVisitor
{
	alias visit = ASTVisitor.visit;

	this(File output)
	{
		this.output = output;
	}

	override void visit(const ClassDeclaration classDec)
	{
		printIndentation();
		output.writeln("class ", classDec.name.text, " : ", classDec.name.line);
		indent();
		classDec.accept(this);
		outdent();
		finish();
	}

	override void visit(const EnumDeclaration enumDec)
	{
		printIndentation();
		output.writeln("enum ", enumDec.name.text, " : ", enumDec.name.line);
		indent();
		enumDec.accept(this);
		outdent();
		finish();
	}

	override void visit(const AnonymousEnumMember enumMem)
	{
		printIndentation();
		if (enumMem.type !is null)
		{
			auto app = appender!(char[])();
			auto f = new Formatter!(typeof(app))(app);
			f.format(enumMem.type);
			output.writeln("enum ", app.data, " ", enumMem.name.text, " : ", enumMem.name.line);
		}
		else
			output.writeln("enum ", enumMem.name.text, " : ", enumMem.name.line);
		finish();
	}

	override void visit(const EnumMember enumMem)
	{
		printIndentation();
		output.writeln(enumMem.name.text, " : ", enumMem.name.line);
		finish();
	}

	override void visit(const FunctionDeclaration functionDec)
	{
		printIndentation();
		if (functionDec.hasAuto)
			output.write("auto ");
		if (functionDec.hasRef)
			output.write("ref ");
		auto app = appender!(char[])();
		auto f = new Formatter!(typeof(app))(app);
		if (functionDec.returnType !is null)
			f.format(functionDec.returnType);
		app.put(" ");
		app.put(functionDec.name.text);
		f.format(functionDec.parameters);
		app.put(" : ");
		app.put(to!string(functionDec.name.line));
		output.writeln(app.data);
		finish();
	}

	override void visit(const InterfaceDeclaration interfaceDec)
	{
		printIndentation();
		output.writeln("interface ", interfaceDec.name.text, " : ", interfaceDec.name.line);
		indent();
		interfaceDec.accept(this);
		outdent();
		finish();
	}

	override void visit(const StructDeclaration structDec)
	{
		printIndentation();
		output.writeln("struct ", structDec.name.text, " : ", structDec.name.line);
		indent();
		structDec.accept(this);
		outdent();
		finish();
	}

	override void visit(const TemplateDeclaration templateDeclaration)
	{
		printIndentation();
		output.writeln("template ", templateDeclaration.name.text, " : ",
				templateDeclaration.name.line);
		indent();
		templateDeclaration.accept(this);
		outdent();
		finish();
	}

	//dfmt off
	override void visit(const StaticConstructor s) {}
	override void visit(const StaticDestructor s) {}
	override void visit(const SharedStaticConstructor s) {}
	override void visit(const SharedStaticDestructor s) {}
	override void visit(const Constructor c) {}
	override void visit(const Unittest u) {}
	// dfmt on

	override void visit(const UnionDeclaration unionDeclaration)
	{
		printIndentation();
		output.writeln("union ", unionDeclaration.name.text, " : ", unionDeclaration.name.line);
		indent();
		unionDeclaration.accept(this);
		outdent();
		finish();
	}

	override void visit(const VariableDeclaration variableDeclaration)
	{
		foreach (const Declarator d; variableDeclaration.declarators)
		{
			printIndentation();
			auto app = appender!(char[])();
			if (variableDeclaration.type !is null)
			{
				auto f = new Formatter!(typeof(app))(app);
				f.format(variableDeclaration.type);
			}
			output.writeln(app.data, " ", d.name.text, " : ", d.name.line);
		}
		finish();
	}

private:

	void finish()
	{
		if (indentLevel == 0)
			output.writeln();
	}

	void printIndentation()
	{
		foreach (i; 0 .. indentLevel)
			output.write("    ");
	}

	void indent()
	{
		indentLevel++;
	}

	void outdent()
	{
		indentLevel--;
	}

	int indentLevel;

	File output;
}
