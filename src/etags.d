// Emacs tags based on ctags.d with this header:
//			Copyright Brian Schott (Hackerpilot) 2012.
// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module etags;

import dparse.parser;
import dparse.lexer;
import dparse.ast;
import dparse.rollback_allocator;
import std.algorithm;
import std.range;
import std.stdio;
import std.path;
import std.array;
import std.conv;
import std.string;

// Prefix tags with their module name.	Seems like correct behavior, but just
// in case, make it an option.
version = UseModuleContext;

// Could not find "official" definition of protection (public/private/etc).
// Behavior modeled here was reversed engineered based on dmd 2.067.
// class/interface and non-anonymous struct/union/enum members default to
// public, regardless of the enclosing declaration.	 template and anonymous
// struct/union/enum members default to the enclosing protection.

/**
 * Prints ETAGS information to the given file.
 * Params:
 *	   outpt = the file that ETAGS info is written to
 *	   tagAll = if set, tag private/package declaration too
 *	   fileNames = tags will be generated from these files
 */
void printEtags(File output, bool tagAll, string[] fileNames)
{
	LexerConfig config;
	StringCache cache = StringCache(StringCache.defaultBucketCount);
	foreach (fileName; fileNames)
	{
		RollbackAllocator rba;
		File f = fileName == "stdin" ? std.stdio.stdin : File(fileName);
		if (f.size == 0)
			continue;
		auto bytes = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(bytes);
		auto tokens = getTokensForParser(bytes, config, &cache);
		Module m = parseModule(tokens.array, fileName, &rba, &doNothing);

		auto printer = new EtagsPrinter;
		printer.moduleName = m.moduleFullName(fileName);
		version (UseModuleContext)
			printer.context = printer.moduleName ~ ".";
		printer.privateVisibility = tagAll ? Visibility.exposed : Visibility.hidden;
		printer.bytes = bytes.sansBOM;
		printer.visit(m);

		output.writef("\f\n%s,%u\n", fileName, printer.tags.length);
		printer.tags.copy(output.lockingTextWriter);
	}
}

private:

enum Visibility
{
	exposed,
	hidden
}

void doNothing(string, size_t, size_t, string, bool)
{
}

ubyte[] sansBOM(ubyte[] bytes)
{
	// At least handle UTF-8 since there is some in druntime/phobos
	return (bytes.length >= 3 && bytes[0] == 0xef && bytes[1] == 0xbb && bytes[2] == 0xbf)
		? bytes[3 .. $] : bytes;
}

string moduleFullName(Module m, string fileName)
{
	// When no module declaration, just use filename and hope its valid
	if (!m.moduleDeclaration)
		return fileName.baseName.stripExtension;

	// reconstruct module full name
	return m.moduleDeclaration.moduleName.identifiers.map!(i => i.text).join(".");
}

final class EtagsPrinter : ASTVisitor
{
	override void visit(const ModuleDeclaration dec)
	{
		auto tok0 = dec.moduleName.identifiers[0];
		auto was = context;
		context = "";
		maketag(moduleName, tok0.index, tok0.line);
		context = was;
		dec.accept(this);
	}

	override void visit(const Declaration dec)
	{
		// Update value of visibility based on this 'dec'.
		if (dec.attributeDeclaration)
		{
			auto attr = dec.attributeDeclaration.attribute;
			updateVisibility(attr.attribute.type);
		}

		// visibility needs to be restored to what it was when changed by
		// attribute.
		auto was = visibility;
		foreach (attr; dec.attributes)
		{
			updateVisibility(attr.attribute.type);
		}

		dec.accept(this);
		visibility = was;
	}

	override void visit(const ClassDeclaration dec)
	{
		maketag(dec.name);
		// class members default to public
		visibility = Visibility.exposed;
		acceptInContext(dec, dec.name.text);
	}

	override void visit(const StructDeclaration dec)
	{
		if (dec.name == tok!"")
		{
			dec.accept(this);
			return;
		}
		maketag(dec.name);
		// struct members default to public
		visibility = Visibility.exposed;
		acceptInContext(dec, dec.name.text);
	}

	override void visit(const InterfaceDeclaration dec)
	{
		maketag(dec.name);
		// interface members default to public
		visibility = Visibility.exposed;
		acceptInContext(dec, dec.name.text);
	}

	override void visit(const TemplateDeclaration dec)
	{
		maketag(dec.name);
		acceptInContext(dec, dec.name.text);
	}

	override void visit(const FunctionDeclaration dec)
	{
		maketag(dec.name);
		// don't tag declarations in a function like thing
		visibility = Visibility.hidden;
		acceptInContext(dec, dec.name.text);
	}

	override void visit(const Constructor dec)
	{
		maketag("this", dec.location, dec.line);
		// don't tag declarations in a function like thing
		visibility = Visibility.hidden;
		acceptInContext(dec, "this");
	}

	override void visit(const Destructor dec)
	{
		maketag("~this", dec.index, dec.line);
		// don't tag declarations in a function like thing
		visibility = Visibility.hidden;
		acceptInContext(dec, "~this");
	}

	override void visit(const EnumDeclaration dec)
	{
		maketag(dec.name);
		// enum members default to public
		visibility = Visibility.exposed;
		acceptInContext(dec, dec.name.text);
	}

	override void visit(const UnionDeclaration dec)
	{
		if (dec.name == tok!"")
		{
			dec.accept(this);
			return;
		}
		maketag(dec.name);
		// union members default to public
		visibility = Visibility.exposed;
		acceptInContext(dec, dec.name.text);
	}

	override void visit(const AnonymousEnumMember mem)
	{
		maketag(mem.name);
	}

	override void visit(const EnumMember mem)
	{
		maketag(mem.name);
	}

	override void visit(const Unittest dec)
	{
		bool was = inUnittest;
		inUnittest = true;
		dec.accept(this);
		inUnittest = was;
	}

	override void visit(const VariableDeclaration dec)
	{
		foreach (d; dec.declarators)
		{
			maketag(d.name);
		}
		dec.accept(this);
	}

	override void visit(const AutoDeclaration dec)
	{
		foreach (part; dec.parts)
		{
			maketag(part.identifier);
		}
		dec.accept(this);
	}

	override void visit(const AliasDeclaration dec)
	{
		// Old style alias
		if (dec.identifierList)
		{
			foreach (i; dec.identifierList.identifiers)
				maketag(i);
		}
		dec.accept(this);
	}

	override void visit(const AliasInitializer dec)
	{
		maketag(dec.name);
		dec.accept(this);
	}

	override void visit(const Invariant dec)
	{
		maketag("invariant", dec.index, dec.line);
	}

private:
	void updateVisibility(IdType type)
	{
		// maybe change visibility based on attribute 'type'
		switch (type)
		{
		case tok!"export":
		case tok!"public":
		case tok!"protected":
			visibility = Visibility.exposed;
			break;
		case tok!"package":
		case tok!"private":
			visibility = privateVisibility;
			break;
		default:
			// no change
			break;
		}
	}

	void acceptInContext(const ASTNode dec, string name)
	{
		// nest context before journeying on
		auto c = context;
		context ~= name ~ ".";
		dec.accept(this);
		context = c;
	}

	void maketag(Token name)
	{
		maketag(name.text, name.index, name.line);
	}

	void maketag(string text, size_t index, ulong line)
	{
		// skip unittests and hidden declarations
		if (inUnittest || visibility == Visibility.hidden)
			return;

		// tag is a searchable string from beginning of line
		size_t b = index;
		while (b > 0 && bytes[b - 1] != '\n')
			--b;

		// tag end is one char beyond tag name
		size_t e = index + text.length;
		if (e < bytes.length && bytes[e] != '\n')
			++e;

		auto tag = cast(char[]) bytes[b .. e];
		auto tagname = context.empty ? text : context ~ text;

		// drum roll...	 the etags tag format
		tags ~= format("%s\x7f%s\x01%u,%u\n", tag, tagname, line, b);
	}

	alias visit = ASTVisitor.visit;

	// state
	// visibility of declarations (i.e. should we tag)
	Visibility visibility = Visibility.exposed;
	bool inUnittest;

	// inputs
	ubyte[] bytes;
	string moduleName;
	string context;
	Visibility privateVisibility = Visibility.hidden;

	// ouput
	string tags;
}
