// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.imports_sortedness;

import analysis.base : BaseAnalyzer;
import dparse.lexer;
import dparse.ast;

import std.stdio;

/**
 * Checks the sortedness of module imports
 */
class ImportSortednessCheck : BaseAnalyzer
{
	enum string KEY = "dscanner.style.imports_sortedness";
	enum string MESSAGE = "The imports are not sorted in alphabetical order";

	///
	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	override void visit(const Module mod)
	{
		level = 0;
		imports[level] = [];
		mod.accept(this);
	}

	override void visit(const Statement decl)
	{
		imports[++level] = [];
		decl.accept(this);
		level--;
	}

	override void visit(const BlockStatement decl)
	{
		imports[++level] = [];
		decl.accept(this);
		level--;
	}

	override void visit(const StructBody decl)
	{
		imports[++level] = [];
		decl.accept(this);
		level--;
	}

	override void visit(const ImportDeclaration id)
	{
		import std.algorithm.iteration : map;
		import std.array : join;
		import std.string : strip;

		if (id.importBindings is null || id.importBindings.importBinds.length == 0)
		{
			foreach (singleImport; id.singleImports)
			{
				string importModuleName = singleImport.identifierChain.identifiers.map!`a.text`.join(".");
				addImport(importModuleName, singleImport);
			}
		}
		else
		{
			string importModuleName = id.importBindings.singleImport.identifierChain.identifiers.map!`a.text`.join(".");

			foreach (importBind; id.importBindings.importBinds)
			{
				addImport(importModuleName ~ "_" ~ importBind.left.text, id.importBindings.singleImport);
			}
		}
	}

	alias visit = BaseAnalyzer.visit;

private:

	int level = 0;
	string[][int] imports;

	void addImport(string importModuleName, const SingleImport singleImport)
	{
		import std.uni : sicmp;

		if (imports[level].length > 0 && imports[level][$ -1].sicmp(importModuleName) > 0)
		{
			addErrorMessage(singleImport.identifierChain.identifiers[0].line,
					singleImport.identifierChain.identifiers[0].column, KEY, MESSAGE);
		}
		else
		{
			imports[level] ~= importModuleName;
		}
	}
}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import analysis.config : StaticAnalysisConfig, Check;
	import analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac;
	sac.imports_sortedness = Check.enabled;

	assertAnalyzerWarnings(q{
		import bar.foo;
		import foo.bar;
	}c, sac);

	assertAnalyzerWarnings(q{
		import foo.bar;
		import bar.foo; // [warn]: %s
	}c.format(
		ImportSortednessCheck.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		import c;
		import c.b;
		import c.a; // [warn]: %s
		import d.a;
		import d; // [warn]: %s
	}c.format(
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		import a.b, a.c, a.d;
		import a.b, a.d, a.c; // [warn]: %s
		import a.c, a.b, a.c; // [warn]: %s
		import foo.bar, bar.foo; // [warn]: %s
	}c.format(
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
	), sac);

	// multiple items out of order
	assertAnalyzerWarnings(q{
		import foo.bar;
		import bar.foo; // [warn]: %s
		import bar.bar.foo; // [warn]: %s
	}c.format(
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		import test : bar;
		import test : foo;
	}c, sac);

	// selective imports
	assertAnalyzerWarnings(q{
		import test : foo;
		import test : bar; // [warn]: %s
	}c.format(
		ImportSortednessCheck.MESSAGE,
	), sac);

	// selective imports
	assertAnalyzerWarnings(q{
		import test : foo, bar; // [warn]: %s
	}c.format(
		ImportSortednessCheck.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		import b;
		import c : foo;
		import c : bar; // [warn]: %s
		import a; // [warn]: %s
	}c.format(
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		import c;
		import c : bar;
		import d : bar;
		import d; // [warn]: %s
		import a : bar; // [warn]: %s
	}c.format(
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		import t0;
		import t1 : a, b = foo;
		import t2;
	}c, sac);

	assertAnalyzerWarnings(q{
		import t1 : a, b = foo;
		import t1 : b, a = foo; // [warn]: %s
		import t0 : a, b = foo; // [warn]: %s
	}c.format(
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
	), sac);

	// local imports in functions
	assertAnalyzerWarnings(q{
		import t2;
		import t1; // [warn]: %s
		void foo()
		{
			import f2;
			import f1; // [warn]: %s
			import f3;
		}
		void bar()
		{
			import f1;
			import f2;
		}
	}c.format(
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
	), sac);

	// local imports in scopes
	assertAnalyzerWarnings(q{
		import t2;
		import t1; // [warn]: %s
		void foo()
		{
			import f2;
			import f1; // [warn]: %s
			import f3;
			{
				import f2;
				import f1; // [warn]: %s
				import f3;
			}
			{
				import f1;
				import f2;
				import f3;
			}
		}
	}c.format(
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
	), sac);

	// local imports in functions
	assertAnalyzerWarnings(q{
		import t2;
		import t1; // [warn]: %s
		void foo()
		{
			import f2;
			import f1; // [warn]: %s
			import f3;
			while (true) {
				import f2;
				import f1; // [warn]: %s
				import f3;
			}
			for (;;) {
				import f1;
				import f2;
				import f3;
			}
			foreach (el; arr) {
				import f2;
				import f1; // [warn]: %s
				import f3;
			}
		}
	}c.format(
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
	), sac);

	// nested scopes
	assertAnalyzerWarnings(q{
		import t2;
		import t1; // [warn]: %s
		void foo()
		{
			import f2;
			import f1; // [warn]: %s
			import f3;
			{
				import f2;
				import f1; // [warn]: %s
				import f3;
				{
					import f2;
					import f1; // [warn]: %s
					import f3;
					{
						import f2;
						import f1; // [warn]: %s
						import f3;
					}
				}
			}
		}
	}c.format(
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
	), sac);

	// local imports in functions
	assertAnalyzerWarnings(q{
		import t2;
		import t1; // [warn]: %s
		struct foo()
		{
			import f2;
			import f1; // [warn]: %s
			import f3;
		}
		class bar()
		{
			import f1;
			import f2;
		}
	}c.format(
		ImportSortednessCheck.MESSAGE,
		ImportSortednessCheck.MESSAGE,
	), sac);

	stderr.writeln("Unittest for ImportSortednessCheck passed.");
}
