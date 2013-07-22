//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module ctags;

import std.d.parser;
import std.d.lexer;
import std.d.ast;
import std.algorithm;
import std.stdio;
import std.array;

void doNothing(string, int, int, string) {}

void printCtags(Tokens)(File output, ref Tokens tokens, string fileName)
{
	Module m = parseModule(tokens.array(), fileName, &doNothing);
	auto printer = new CTagsPrinter;
	printer.fileName = fileName;
	printer.visit(m);
	printer.print(output);
}

class CTagsPrinter : ASTVisitor
{

	alias ASTVisitor.visit visit;

	override void visit(ClassDeclaration dec)
	{
		tagLines ~= "%s\t%s\t%d;\"\tc".format(dec.name.value, fileName, dec.name.line);
		dec.accept(this);
	}

	override void visit(InterfaceDeclaration dec)
	{
		tagLines ~= "%s\t%s\t%d;\"\tc".format(dec.name.value, fileName, dec.name.line);
		dec.accept(this);
	}

	override void visit(FunctionDeclaration dec)
	{
		tagLines ~= "%s\t%s\t%d;\"\tf\tarity:%d".format(dec.name.value, fileName,
			dec.name.line, dec.parameters.parameters.length);
		dec.accept(this);
	}

	override void visit(EnumDeclaration dec)
	{
		tagLines ~= "%s\t%s\t%d;\"\tg".format(dec.name.value, fileName, dec.name.line);
		dec.accept(this);
	}

	override void visit(VariableDeclaration dec)
	{
		foreach (d; dec.declarators)
			tagLines ~= "%s\t%s\t%d;\"\tv".format(d.name.value, fileName, d.name.line);
		dec.accept(this);
	}

	void print(File output)
	{
		output.write("!_TAG_FILE_FORMAT\t2\n"
			~ "!_TAG_FILE_SORTED\t1\n"
			~ "!_TAG_FILE_AUTHOR\tBrian Schott\n"
			~ "!_TAG_PROGRAM_URL\thttps://github.com/Hackerpilot/Dscanner/\n");
		foreach (str; sort(tagLines))
		{
			output.writeln(str);
		}
	}

	string fileName;
	string[] tagLines;
}
