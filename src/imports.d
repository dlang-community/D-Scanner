//          Copyright Brian Schott (Hackerpilot) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module imports;

import dparse.ast;
import dparse.lexer;
import dparse.parser;
import dparse.rollback_allocator;
import std.stdio;
import std.container.rbtree;
import std.functional : toDelegate;
import readers;

/**
 * AST visitor that collects modules imported to an R-B tree.
 */
class ImportPrinter : ASTVisitor
{
	this()
	{
		imports = new RedBlackTree!string;
	}

	override void visit(const SingleImport singleImport)
	{
		ignore = false;
		singleImport.accept(this);
		ignore = true;
	}

	override void visit(const IdentifierChain identifierChain)
	{
		if (ignore)
			return;
		bool first = true;
		string s;
		foreach (ident; identifierChain.identifiers)
		{
			if (!first)
				s ~= ".";
			s ~= ident.text;
			first = false;
		}
		imports.insert(s);
	}

	alias visit = ASTVisitor.visit;

	/// Collected imports
	RedBlackTree!string imports;

private:
	bool ignore = true;
}

private void visitFile(bool usingStdin, string fileName, RedBlackTree!string importedModules, StringCache* cache)
{
	RollbackAllocator rba;
	LexerConfig config;
	config.fileName = fileName;
	config.stringBehavior = StringBehavior.source;
	auto visitor = new ImportPrinter;
	auto tokens = getTokensForParser(usingStdin ? readStdin() : readFile(fileName), config, cache);
	auto mod = parseModule(tokens, fileName, &rba, toDelegate(&doNothing));
	visitor.visit(mod);
	importedModules.insert(visitor.imports[]);
}

private void doNothing(string, size_t, size_t, string, bool)
{
}

void printImports(bool usingStdin, string[] args, string[] importPaths, StringCache* cache, bool recursive)
{
	string[] fileNames = usingStdin ? ["stdin"] : expandArgs(args);
	import std.path : buildPath, dirSeparator;
	import std.file : isFile, exists;
	import std.array : replace, empty;
	import std.range : chain, only;

	auto resolvedModules = new RedBlackTree!(string);
	auto resolvedLocations = new RedBlackTree!(string);
	auto importedFiles = new RedBlackTree!(string);
	foreach (name; fileNames)
		visitFile(usingStdin, name, importedFiles, cache);
	if (importPaths.empty)
	{
		foreach (item; importedFiles[])
			writeln(item);
		return;
	}
	while (!importedFiles.empty)
	{
		auto newlyDiscovered = new RedBlackTree!(string);
		itemLoop: foreach (item; importedFiles[])
		{
			foreach (path; importPaths)
			{
				auto d = buildPath(path, item.replace(".", dirSeparator) ~ ".d");
				auto di = buildPath(path, item.replace(".", dirSeparator) ~ ".di");
				auto p = buildPath(path, item.replace(".", dirSeparator), "package.d");
				auto pi = buildPath(path, item.replace(".", dirSeparator), "package.di");
				foreach (alt; [d, di, p, pi])
				{
					if (exists(alt) && isFile(alt))
					{
						resolvedModules.insert(item);
						resolvedLocations.insert(alt);
						if (recursive)
							visitFile(false, alt, newlyDiscovered, cache);
						continue itemLoop;
					}
				}
			}
			writeln("Could not resolve location of ", item);
		}
		foreach (item; importedFiles[])
			newlyDiscovered.removeKey(item);
		foreach (resolved; resolvedModules[])
			newlyDiscovered.removeKey(resolved);
		importedFiles = newlyDiscovered;
	}
	foreach (resolved; resolvedLocations[])
		writeln(resolved);
}
