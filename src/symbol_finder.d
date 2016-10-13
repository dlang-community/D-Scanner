//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module symbol_finder;

import std.stdio : File;
import dparse.lexer;
import dparse.parser;
import dparse.ast;
import dparse.rollback_allocator;
import std.stdio;
import std.file : isFile;

void findDeclarationOf(File output, string symbolName, string[] fileNames)
{
	import std.array : uninitializedArray, array;
	import std.conv : to;

	LexerConfig config;
	StringCache cache = StringCache(StringCache.defaultBucketCount);
	auto visitor = new FinderVisitor(output, symbolName);
	foreach (fileName; fileNames)
	{
		File f = File(fileName);
		assert(isFile(fileName));
		if (f.size == 0)
			continue;
		auto bytes = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(bytes);
		auto tokens = getTokensForParser(bytes, config, &cache);
		RollbackAllocator rba;
		Module m = parseModule(tokens.array, fileName, &rba, &doNothing);
		visitor.fileName = fileName;
		visitor.visit(m);
	}
}

private:

void doNothing(string, size_t, size_t, string, bool)
{
}

class FinderVisitor : ASTVisitor
{
	this(File output, string symbolName)
	{
		this.output = output;
		this.symbolName = symbolName;
	}

	mixin generateVisit!FunctionDeclaration;
	mixin generateVisit!ClassDeclaration;
	mixin generateVisit!InterfaceDeclaration;
	mixin generateVisit!StructDeclaration;
	mixin generateVisit!UnionDeclaration;
	mixin generateVisit!TemplateDeclaration;

	override void visit(const EnumDeclaration dec)
	{
		if (dec.name.text == symbolName)
			output.writefln("%s(%d:%d)", fileName, dec.name.line, dec.name.column);
	}

	override void visit(const AnonymousEnumMember member)
	{
		if (member.name.text == symbolName)
			output.writefln("%s(%d:%d)", fileName, member.name.line, member.name.column);
	}

	override void visit(const EnumMember member)
	{
		if (member.name.text == symbolName)
			output.writefln("%s(%d:%d)", fileName, member.name.line, member.name.column);
	}

	override void visit(const AliasDeclaration dec)
	{
		if (dec.identifierList !is null)
		{
			foreach (ident; dec.identifierList.identifiers)
			{
				if (ident.text == symbolName)
					output.writefln("%s(%d:%d)", fileName, ident.line, ident.column);
			}
		}
		foreach (initializer; dec.initializers)
		{
			if (initializer.name.text == symbolName)
				output.writefln("%s(%d:%d)", fileName, initializer.name.line,
						initializer.name.column);
		}
	}

	override void visit(const Declarator dec)
	{
		if (dec.name.text == symbolName)
			output.writefln("%s(%d:%d)", fileName, dec.name.line, dec.name.column);
	}

	override void visit(const AutoDeclaration ad)
	{
		foreach (part; ad.parts)
		{
			if (part.identifier.text == symbolName)
				output.writefln("%s(%d:%d)", fileName, part.identifier.line, part.identifier.column);
		}
	}

	override void visit(const FunctionBody)
	{
	}

	mixin template generateVisit(T)
	{
		override void visit(const T t)
		{
			if (t.name.text == symbolName)
				output.writefln("%s(%d:%d)", fileName, t.name.line, t.name.column);
			t.accept(this);
		}
	}

	alias visit = ASTVisitor.visit;

	File output;
	string symbolName;
	string fileName;
}
