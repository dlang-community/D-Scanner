//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.symbol_finder;

import std.stdio : File;
import dparse.lexer;
import dparse.parser;
import dparse.ast;
import dparse.rollback_allocator;
import std.stdio;
import std.file : isFile;
import std.functional : toDelegate;

void findDeclarationOf(File output, string symbolName, string[] fileNames)
{
	findDeclarationOf((string fileName, size_t line, size_t column)
	{
		output.writefln("%s(%d:%d)", fileName, line, column);
	}, symbolName, fileNames);
}

/// Delegate that gets called every time a declaration gets found
alias OutputHandler = void delegate(string fileName, size_t line, size_t column);

/// Finds all declarations of a symbol in the given fileNames and calls a handler on every occurence.
/// Params:
///   output = Callback which gets called when a declaration is found
///   symbolName = Symbol name to search for
///   fileNames = An array of file names which might contain stdin to read from stdin
void findDeclarationOf(scope OutputHandler output, string symbolName, string[] fileNames)
{
	import std.array : uninitializedArray, array;
	import std.conv : to;

	LexerConfig config;
	StringCache cache = StringCache(StringCache.defaultBucketCount);
	auto visitor = new FinderVisitor(output, symbolName);
	foreach (fileName; fileNames)
	{
		File f = fileName == "stdin" ? std.stdio.stdin : File(fileName);
		assert(fileName == "stdin" || isFile(fileName));
		if (f.size == 0)
			continue;
		auto bytes = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(bytes);
		auto tokens = getTokensForParser(bytes, config, &cache);
		RollbackAllocator rba;
		Module m = parseModule(tokens.array, fileName, &rba, toDelegate(&doNothing));
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
	this(OutputHandler output, string symbolName)
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
			output(fileName, dec.name.line, dec.name.column);
	}

	override void visit(const AnonymousEnumMember member)
	{
		if (member.name.text == symbolName)
			output(fileName, member.name.line, member.name.column);
	}

	override void visit(const EnumMember member)
	{
		if (member.name.text == symbolName)
			output(fileName, member.name.line, member.name.column);
	}

	override void visit(const AliasDeclaration dec)
	{
		if (dec.declaratorIdentifierList !is null)
		{
			foreach (ident; dec.declaratorIdentifierList.identifiers)
			{
				if (ident.text == symbolName)
					output(fileName, ident.line, ident.column);
			}
		}
		foreach (initializer; dec.initializers)
		{
			if (initializer.name.text == symbolName)
				output(fileName, initializer.name.line,
						initializer.name.column);
		}
	}

	override void visit(const Declarator dec)
	{
		if (dec.name.text == symbolName)
			output(fileName, dec.name.line, dec.name.column);
	}

	override void visit(const AutoDeclaration ad)
	{
		foreach (part; ad.parts)
		{
			if (part.identifier.text == symbolName)
				output(fileName, part.identifier.line, part.identifier.column);
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
				output(fileName, t.name.line, t.name.column);
			t.accept(this);
		}
	}

	alias visit = ASTVisitor.visit;

	OutputHandler output;
	string symbolName;
	string fileName;
}
