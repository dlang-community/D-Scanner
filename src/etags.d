// Emacs tags based on ctags.d with this header:
//			Copyright Brian Schott (Hackerpilot) 2012.
// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module etags;

import std.d.parser;
import std.d.lexer;
import std.d.ast;
import std.algorithm;
import std.range;
import std.stdio;
import std.path;
import std.array;
import std.conv;

/**
 * Prints ETAGS information to the given file.
 * Params:
 *	   outpt = the file that ETAGS info is written to
 *	   fileNames = tags will be generated from these files
 */
void printEtags(File output, string[] fileNames)
{
	LexerConfig config;
	StringCache cache = StringCache(StringCache.defaultBucketCount);
	foreach (fileName; fileNames)
	{
		File f = File(fileName);
		if (f.size == 0) continue;
		auto bytes = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(bytes);
		auto tokens = getTokensForParser(bytes, config, &cache);
		Module m = parseModule(tokens.array, fileName, null, &doNothing);
		auto printer = new EtagsPrinter;

		// I think I like this
		enum useModuleContext = true;
		if (useModuleContext)
			printer.context = m.moduleFullName(fileName) ~ ".";
		
		printer.bytes = bytes.sansBOM;
		printer.visit(m);
		output.writef("\f\n%s,%u\n", fileName, printer.tags.length);
		printer.tags.copy(output.lockingTextWriter);
	}
}

private:

void doNothing(string, size_t, size_t, string, bool) {}

ubyte[] sansBOM(ubyte[] bytes)
{
	// At least handle UTF-8 since there is some in druntime/phobos
	return (bytes.length >= 3 &&
			bytes[0] == 0xef && bytes[1] == 0xbb && bytes[2] == 0xbf)
		? bytes[3 .. $] : bytes;
	
}

string moduleFullName(Module m, string fileName)
{
	// When no module declaration, just use filename and hope its valid
	if (!m.moduleDeclaration)
		return fileName.baseName.stripExtension;

	// reconstruct module full name
	return m.moduleDeclaration.moduleName.identifiers.map!(i=>i.text).join(".");
}

class EtagsPrinter : ASTVisitor
{
	override void visit(const ClassDeclaration dec)
	{
		maketag(dec.name);
		acceptInContext(dec, dec.name.text);
	}

	override void visit(const StructDeclaration dec)
	{
		maketag(dec.name);
		acceptInContext(dec, dec.name.text);
	}

	override void visit(const InterfaceDeclaration dec)
	{
		maketag(dec.name);
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
		bool was = inFunc;
		inFunc = true;
		auto c = context;
		acceptInContext(dec, dec.name.text);
		inFunc = was;
	}

	override void visit(const Constructor dec)
	{
		maketag("this", dec.location, dec.line);
		bool was = inFunc;
		inFunc = true;
		auto c = context;
		acceptInContext(dec, "this");
		inFunc = was;
	}

	override void visit(const Destructor dec)
	{
		maketag("~this", dec.index, dec.line);
		bool was = inFunc;
		inFunc = true;
		auto c = context;
		acceptInContext(dec, "~this");
		inFunc = was;
	}

	override void visit(const EnumDeclaration dec)
	{
		maketag(dec.name);
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
		foreach (i; dec.identifiers)
		{
			maketag(i);
		}
		dec.accept(this);
	}

	override void visit(const Invariant dec)
	{
		maketag("invariant", dec.index, dec.line);
	}

	void acceptInContext(Dec)(Dec dec, string name)
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
		// skip declaration in unittests and funcs
		if (inUnittest || inFunc) return;

		// tag is a searchable string from beginning of line
		size_t b = index;
		while (b > 0 && bytes[--b] != '\n') {}
		++b;

		// tag end is one char beyond tag name
		size_t e = index + text.length;
		if (e < bytes.length && bytes[e] != '\n') ++e;

		auto tag = cast(char[])bytes[b..e];
		auto tagname = context.empty ? text : context~text;

		// drum roll...	 the etags tag format
		tags ~= format("%s\x7f%s\x01%u,%u\n",
					   tag,
					   tagname,
					   line,
					   b);
	}
	
	alias visit = ASTVisitor.visit;

	bool inUnittest;
	bool inFunc;
	ubyte[] bytes;
	string tags;
	string context;
}

