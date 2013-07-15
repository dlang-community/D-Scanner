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
		dec.structBody.accept(this);
	}

	override void visit(InterfaceDeclaration dec)
	{
		tagLines ~= "%s\t%s\t%d;\"\tc".format(dec.name.value, fileName, dec.name.line);
		dec.structBody.accept(this);
	}

	override void visit(FunctionDeclaration dec)
	{
		tagLines ~= "%s\t%s\t%d;\"\tf\tarity:%d".format(dec.name.value, fileName,
			dec.name.line, dec.parameters.parameters.length);
	}

	override void visit(EnumDeclaration dec)
	{
		tagLines ~= "%s\t%s\t%d;\"\tg".format(dec.name.value, fileName, dec.name.line);
	}

	void print(File output)
	{
		output.write("!_TAG_FILE_FORMAT 2\n"
			~ "!_TAG_FILE_SORTED 1\n"
			~ "!_TAG_FILE_AUTHOR Brian Schott\n"
			~ "!_TAG_PROGRAM_URL https://github.com/Hackerpilot/Dscanner/\n");
		foreach (str; sort(tagLines))
		{
			output.writeln(str);
		}
	}

	string fileName;
	string[] tagLines;
}
