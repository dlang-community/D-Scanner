//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

import stdx.d.lexer;
import stdx.d.ast;
import std.stdio;
import std.string;
import std.array;
import std.conv;
import formatter;

class Outliner : ASTVisitor
{
	this(File output)
	{
		this.output = output;
	}

	override void visit(ClassDeclaration classDec)
	{
		printIndentation();
		output.writeln("class ", classDec.name.text, " : ", classDec.name.line);
		indent();
		classDec.accept(this);
		outdent();
		finish();
	}

	override void visit(EnumDeclaration enumDec)
	{
		printIndentation();
		output.writeln("enum ", enumDec.name.text, " : ", enumDec.name.line);
		indent();
		enumDec.accept(this);
		outdent();
		finish();
	}

	override void visit(EnumMember enumMem)
	{
		printIndentation();
		output.writeln(enumMem.name.text, " : ", enumMem.name.line);
		finish();
	}

	override void visit(FunctionDeclaration functionDec)
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

	override void visit(InterfaceDeclaration interfaceDec)
	{
		printIndentation();
		output.writeln("interface ", interfaceDec.name.text, " : ",
			interfaceDec.name.line);
		indent();
		interfaceDec.accept(this);
		outdent();
		finish();
	}

	override void visit(StructDeclaration structDec)
	{
		printIndentation();
		output.writeln("struct ", structDec.name.text, " : ",
			structDec.name.line);
		indent();
		structDec.accept(this);
		outdent();
		finish();
	}

	override void visit(TemplateDeclaration templateDeclaration)
	{
		printIndentation();
		output.writeln("template", templateDeclaration.name.text, " : ",
			templateDeclaration.name.line);
		finish();
	}

	override void visit(StaticConstructor s) {}
	override void visit(StaticDestructor s) {}
	override void visit(SharedStaticConstructor s) {}
	override void visit(SharedStaticDestructor s) {}
	override void visit(Constructor c) {}
	override void visit(Unittest u) {}

	override void visit(UnionDeclaration unionDeclaration)
	{
		printIndentation();
		output.writeln("union ", unionDeclaration.name.text, " : ",
			unionDeclaration.name.line);
		indent();
		unionDeclaration.accept(this);
		outdent();
		finish();
	}

	override void visit(VariableDeclaration variableDeclaration)
	{
		foreach (Declarator d; variableDeclaration.declarators)
		{
			printIndentation();
			auto app = appender!(char[])();
			if (variableDeclaration.type !is null)
			{
				auto f = new Formatter!(typeof(app))(app);
				f.format(variableDeclaration.type);
			}
			app.put(" ");
			app.put(d.name.text);
			app.put(" : ");
			app.put(to!string(d.name.line));
			output.writeln(app.data);
		}
		finish();
	}

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

	alias ASTVisitor.visit visit;

	File output;
}
