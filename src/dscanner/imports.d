//          Copyright Brian Schott (Hackerpilot) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.imports;

import std.stdio;
import std.container.rbtree;
import std.functional : toDelegate;
import dscanner.utils;
import dmd.visitor.permissive;
import dmd.visitor.transitive;
import dmd.tokens;
import dmd.common.outbuffer;
import core.stdc.stdio;
import dmd.parse;
import dmd.astbase;
import dmd.id;
import dmd.globals;
import dmd.identifier;
import core.memory;
import std.stdio;
import std.file;
import std.conv : to;

extern(C++) class ImportVisitor(AST) : ParseTimeTransitiveVisitor!AST
{
    alias visit = ParseTimeTransitiveVisitor!AST.visit;

	this()
	{
		imports = new RedBlackTree!string;
	}

    override void visit(AST.Import imp)
    {
		import std.conv : to;
		string s;

        foreach (const pid; imp.packages)
			s = s ~ to!string(pid.toChars()) ~ ".";

		s ~= to!string(imp.id.toChars());
		imports.insert(s);
    }

	RedBlackTree!string imports;
}

private void visitFile(bool usingStdin, string fileName, RedBlackTree!string importedModules)
{
	import dmd.errorsink : ErrorSinkNull;

	Id.initialize();
	global._init();
	global.params.useUnitTests = true;
	ASTBase.Type._init();

	auto id = Identifier.idPool(fileName);
	auto m = new ASTBase.Module(&(fileName.dup)[0], id, false, false);
	ubyte[] bytes = usingStdin ? readStdin() : readFile(fileName);
	auto input = cast(char[]) bytes;

	__gshared ErrorSinkNull errorSinkNull;
	if (!errorSinkNull)
		errorSinkNull = new ErrorSinkNull;

	scope p = new Parser!ASTBase(m, input, false, new ErrorSinkNull, null, false);
	p.nextToken();
	m.members = p.parseModule();

	scope vis = new ImportVisitor!ASTBase();
	m.accept(vis);
	importedModules.insert(vis.imports[]);
}

private void doNothing(string, size_t, size_t, string, bool)
{
}

void printImports(bool usingStdin, string[] args, string[] importPaths, bool recursive)
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
		visitFile(usingStdin, name, importedFiles);
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
							visitFile(false, alt, newlyDiscovered);
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

unittest
{
	import std.stdio : File;
	import std.file : exists, remove;

	auto deleteme = "test.txt";
	File file = File(deleteme, "w");
	scope(exit)
	{
		assert(exists(deleteme));
        remove(deleteme);
	}

	file.write(q{
		import std.stdio;
		import std.fish : scales, head;
		import DAGRON = std.experimental.dragon;
		import std.file;
	});

	file.close();

	auto importedFiles = new RedBlackTree!(string);
	visitFile(false, deleteme, importedFiles);

	auto expected = new RedBlackTree!(string);
	expected.insert("std.stdio");
	expected.insert("std.fish");
	expected.insert("std.file");
	expected.insert("std.experimental.dragon");

	assert(expected == importedFiles);
}
