//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module ctags;

import stdx.d.parser;
import stdx.d.lexer;
import stdx.d.ast;
import std.algorithm;
import std.range;
import std.stdio;
import std.array;
import std.conv;

void doNothing(string, int, int, string) {}

void printCtags(File output, string[] fileNames)
{
	string[] tags;
	foreach (fileName; fileNames)
	{
		File f = File(fileName);
		auto bytes = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(bytes);
		LexerConfig config;
		auto tokens = byToken(bytes, config);
		Module m = parseModule(tokens.array(), fileName);
		auto printer = new CTagsPrinter;
		printer.fileName = fileName;
		printer.visit(m);
		tags ~= printer.tagLines;
	}
	output.write("!_TAG_FILE_FORMAT\t2\n"
			~ "!_TAG_FILE_SORTED\t1\n"
			~ "!_TAG_FILE_AUTHOR\tBrian Schott\n"
			~ "!_TAG_PROGRAM_URL\thttps://github.com/Hackerpilot/Dscanner/\n");
	tags.sort().copy(output.lockingTextWriter);
}

class CTagsPrinter : ASTVisitor
{

	alias ASTVisitor.visit visit;

	override void visit(ClassDeclaration dec)
	{
		tagLines ~= "%s\t%s\t%d;\"\tc%s\n".format(dec.name.value, fileName, dec.name.line, context);
		auto c = context;
		context = "\tclass:" ~ dec.name.value;
		dec.accept(this);
		context = c;
	}

	override void visit(StructDeclaration dec)
	{
		tagLines ~= "%s\t%s\t%d;\"\ts%s\n".format(dec.name.value, fileName, dec.name.line, context);
		auto c = context;
		context = "\tstruct:" ~ dec.name.value;
		dec.accept(this);
		context = c;
	}

	override void visit(InterfaceDeclaration dec)
	{
		tagLines ~= "%s\t%s\t%d;\"\ti%s\n".format(dec.name.value, fileName, dec.name.line, context);
		auto c = context;
		context = "\tclass:" ~ dec.name.value;
		dec.accept(this);
		context = c;
	}

	override void visit(TemplateDeclaration dec)
	{
		tagLines ~= "%s\t%s\t%d;\"\tT%s\n".format(dec.name.value, fileName, dec.name.line, context);
		auto c = context;
		context = "\ttemplate:" ~ dec.name.value;
		dec.accept(this);
		context = c;
	}

	override void visit(FunctionDeclaration dec)
	{
		tagLines ~= "%s\t%s\t%d;\"\tf\tarity:%d%s\n".format(dec.name.value, fileName,
			dec.name.line, dec.parameters.parameters.length, context);
		auto c = context;
		context = "\tfunction:" ~ dec.name.value;
		dec.accept(this);
		context = c;
	}

	override void visit(EnumDeclaration dec)
	{
		if (dec.name == TokenType.invalid)
		{
			dec.accept(this);
			return;
		}
		tagLines ~= "%s\t%s\t%d;\"\tg%s\n".format(dec.name.value, fileName,
			dec.name.line, context);
		auto c = context;
		context = "\tenum:" ~ dec.name.value;
		dec.accept(this);
		context = c;
	}

	override void visit(UnionDeclaration dec)
	{
		if (dec.name == TokenType.invalid)
		{
			dec.accept(this);
			return;
		}
		tagLines ~= "%s\t%s\t%d;\"\tu%s\n".format(dec.name.value, fileName,
			dec.name.line, context);
		auto c = context;
		context = "\tunion:" ~ dec.name.value;
		dec.accept(this);
		context = c;
	}

	override void visit(EnumMember mem)
	{
		tagLines ~= "%s\t%s\t%d;\"\te%s\n".format(mem.name.value, fileName,
			mem.name.line, context);
	}

	override void visit(VariableDeclaration dec)
	{
		foreach (d; dec.declarators)
		{
			tagLines ~= "%s\t%s\t%d;\"\tv%s\n".format(d.name.value, fileName,
				d.name.line, context);
		}
		dec.accept(this);
	}

	string fileName;
	string[] tagLines;
	int suppressDepth;
	string context;
}
