// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.imports_sortedness;

import dscanner.analysis.base;
import dscanner.analysis.source_range : findIdentifier, findToken, qualifiedNameEnd;
import dmd.tokens : TOK;

/**
 * Checks the sortedness of module imports
 */
extern(C++) class ImportSortednessCheck(AST) : BaseAnalyzerDmd
{
	enum string KEY = "dscanner.style.imports_sortedness";
	enum string MESSAGE = "The imports are not sorted in alphabetical order";
	mixin AnalyzerInfo!"imports_sortedness";
	alias visit = BaseAnalyzerDmd.visit;
	// alias visit = BaseAnalyzerDmd!AST.visit;

	///
	extern(D) this(string fileName)
	{
		super(fileName);
	}

	mixin ScopedVisit!(AST.StructDeclaration);
	mixin ScopedVisit!(AST.ClassDeclaration);
	mixin ScopedVisit!(AST.UnitTestDeclaration);
	mixin ScopedVisit!(AST.FuncDeclaration);
	mixin ScopedVisit!(AST.InterfaceDeclaration);
	mixin ScopedVisit!(AST.UnionDeclaration);
	mixin ScopedVisit!(AST.TemplateDeclaration);
	mixin ScopedVisit!(AST.IfStatement);
	mixin ScopedVisit!(AST.WhileStatement);
	mixin ScopedVisit!(AST.ForStatement);
	mixin ScopedVisit!(AST.ForeachStatement);
	mixin ScopedVisit!(AST.ScopeStatement);
	mixin ScopedVisit!(AST.ConditionalDeclaration);


	override void visit(AST.VarDeclaration vd)
	{
		imports[level] = [];
	}

	override void visit(AST.Import i)
	{
		import std.algorithm : map;
		import std.array : join;
		import std.conv : to;

		string importModuleName = i.packages.map!(a => a.toString().dup).join(".");

		if (importModuleName != "")
			importModuleName ~= "." ~ i.id.toString();
		else
			importModuleName ~= i.id.toString();

		const size_t[2] moduleRange = [i.loc.fileOffset, qualifiedNameEnd(source, i.loc.fileOffset)];
		if (i.names.length)
		{
			size_t cursor = findToken(source, moduleRange[1], TOK.colon).end;
			foreach (index, name; i.names)
			{
				// `a = foo` is sorted by and reported from its alias `a`
				auto alias_ = i.aliases[index];
				const bindingName = alias_ ? alias_.toString() : name.toString();
				const bindingStart = findIdentifier(source, cursor, bindingName);
				const bindingEnd = alias_ ? findIdentifier(source, bindingStart.end, name.toString()) : bindingStart;
				cursor = bindingEnd.end;
				string aux = to!string(importModuleName ~ "-" ~ bindingName);
				addImport(aux, i, [bindingStart.start, bindingEnd.end], moduleRange);
			}
		}
		else addImport(importModuleName, i, moduleRange, moduleRange);
	}

private:
	int level;
	string[][int] imports;
	bool[int] levelAvailable;

	template ScopedVisit(NodeType)
	{
		override void visit(NodeType n)
		{
			imports[level] = [];
			imports[++level] = [];
			levelAvailable[level] = true;
			super.visit(n);
			level--;
		}
	}

	extern(D) void addImport(string importModuleName, AST.Import i, size_t[2] range, size_t[2] moduleRange)
	{
		import std.algorithm : findSplit;
		import std.uni : sicmp;

		if (!levelAvailable.get(level, false))
		{
			imports[level] = [];
			levelAvailable[level] = true;
		}

		if (imports[level].length == 0 || imports[level][$ - 1].sicmp(importModuleName) <= 0)
		{
			imports[level] ~= importModuleName;
			return;
		}

		// one warning per import declaration, which dmd splits into one Import per module
		const declarationEnd = findToken(source, i.loc.fileOffset, TOK.semicolon).start;
		if (declarationEnd == reportedDeclarationEnd)
			return;
		reportedDeclarationEnd = declarationEnd;

		// a module out of place is reported on its name rather than on the selected symbols
		const previousModule = imports[level][$ - 1].findSplit("-")[0];
		const modulePlacement = i.names.length ? previousModule.sicmp(importModuleName.findSplit("-")[0]) : 0;
		addErrorMessage(modulePlacement > 0 ? moduleRange : range, KEY, MESSAGE);
	}

	size_t reportedDeclarationEnd = size_t.max;
}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings = assertAnalyzerWarningsDMD;
	import dmd.astcodegen : ASTCodegen;

	StaticAnalysisConfig sac = disabledConfig();
	sac.imports_sortedness = Check.enabled;

	assertAnalyzerWarnings(q{
		import bar.foo;
		import foo.bar;
	}c, sac);

	assertAnalyzerWarnings(q{
		import foo.bar;
		import bar.foo; /+
		       ^^^^^^^ [warn]: %s +/
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		import c;
		import c.b;
		import c.a; /+
		       ^^^ [warn]: %s +/
		import d.a;
		import d; /+
		       ^ [warn]: %s +/
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		unittest
		{
			import a.b, a.c, a.d;
		}
		unittest
		{
			import a.b, a.d, a.c; /+
			                 ^^^ [warn]: %s +/
		}
		unittest
		{
			import a.c, a.b, a.c; /+
			            ^^^ [warn]: %s +/
		}
		unittest
		{
			import foo.bar, bar.foo; /+
			                ^^^^^^^ [warn]: %s +/
		}
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	// multiple items out of order
	assertAnalyzerWarnings(q{
		import foo.bar;
		import bar.foo; /+
		       ^^^^^^^ [warn]: %s +/
		import bar.bar.foo; /+
		       ^^^^^^^^^^^ [warn]: %s +/
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		import test : bar;
		import test : foo;
	}c, sac);

	// selective imports
	assertAnalyzerWarnings(q{
		import test : foo;
		import test : bar; /+
		              ^^^ [warn]: %s +/
		import before : zzz; /+
		       ^^^^^^ [warn]: %s +/
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	// selective imports
	assertAnalyzerWarnings(q{
		import test : foo, bar; /+
		                   ^^^ [warn]: %s +/
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		import b;
		import c : foo;
		import c : bar; /+
		           ^^^ [warn]: %s +/
		import a; /+
		       ^ [warn]: %s +/
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		import c;
		import c : bar;
		import d : bar;
		import d; /+
		       ^ [warn]: %s +/
		import a : bar; /+
		       ^ [warn]: %s +/
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		import t0;
		import t1 : a, b = foo;
		import t2;
	}c, sac);

	assertAnalyzerWarnings(q{
		import t1 : a, b = foo;
		import t1 : b, a = foo; /+
		               ^^^^^^^ [warn]: %s +/
		import t0 : a, b = foo; /+
		       ^^ [warn]: %s +/
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	// local imports in functions
	assertAnalyzerWarnings(q{
		import t2;
		import t1; /+
		       ^^ [warn]: %s +/
		void foo()
		{
			import f2;
			import f1; /+
			       ^^ [warn]: %s +/
			import f3;
		}
		void bar()
		{
			import f1;
			import f2;
		}
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	// local imports in scopes
	assertAnalyzerWarnings(q{
		import t2;
		import t1; /+
		       ^^ [warn]: %s +/
		void foo()
		{
			import f2;
			import f1; /+
			       ^^ [warn]: %s +/
			import f3;
			{
				import f2;
				import f1; /+
				       ^^ [warn]: %s +/
				import f3;
			}
			{
				import f1;
				import f2;
				import f3;
			}
		}
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	// local imports in functions
	assertAnalyzerWarnings(q{
		import t2;
		import t1; /+
		       ^^ [warn]: %s +/
		void foo()
		{
			import f2;
			import f1; /+
			       ^^ [warn]: %s +/
			import f3;
			while (true) {
				import f2;
				import f1; /+
				       ^^ [warn]: %s +/
				import f3;
			}
			for (;;) {
				import f1;
				import f2;
				import f3;
			}
			foreach (el; arr) {
				import f2;
				import f1; /+
				       ^^ [warn]: %s +/
				import f3;
			}
		}
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	// nested scopes
	assertAnalyzerWarnings(q{
		import t2;
		import t1; /+
		       ^^ [warn]: %s +/
		void foo()
		{
			import f2;
			import f1; /+
			       ^^ [warn]: %s +/
			import f3;
			{
				import f2;
				import f1; /+
				       ^^ [warn]: %s +/
				import f3;
				{
					import f2;
					import f1; /+
					       ^^ [warn]: %s +/
					import f3;
					{
						import f2;
						import f1; /+
						       ^^ [warn]: %s +/
						import f3;
					}
				}
			}
		}
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	// local imports in functions
	assertAnalyzerWarnings(q{
		import t2;
		import t1; /+
		       ^^ [warn]: %s +/
		struct foo()
		{
			import f2;
			import f1; /+
			       ^^ [warn]: %s +/
			import f3;
		}
		class bar()
		{
			import f1;
			import f2;
		}
	}c.format(
		ImportSortednessCheck!ASTCodegen.MESSAGE,
		ImportSortednessCheck!ASTCodegen.MESSAGE,
	), sac);

	// issue 422 - sorted imports with :
	assertAnalyzerWarnings(q{
		import foo.bar : bar;
		import foo.barbar;
	}, sac);

	// issue 422 - sorted imports with :
	assertAnalyzerWarnings(q{
		import foo;
		import foo.bar;
		import fooa;
		import std.range : Take;
		import std.range.primitives : isInputRange, walkLength;
	}, sac);

	// condition declaration
	assertAnalyzerWarnings(q{
		import t2;
		version(unittest)
		{
			import t1;
		}
	}, sac);

	// if statements
	assertAnalyzerWarnings(q{
	unittest
	{
		import t2;
		if (true)
		{
			import t1;
		}
	}
	}, sac);

	// intermediate imports
	assertAnalyzerWarnings(q{
	unittest
	{
		import t2;
		int a = 1;
		import t1;
	}
	}, sac);

	// imports are checked at any nesting depth
	{
		import std.array : replicate;

		enum depth = 25;
		assertAnalyzerWarnings("void f()\n{\n" ~ "{\n".replicate(depth)
			~ "import b;\nimport a; /+\n       ^ [warn]: " ~ ImportSortednessCheck!ASTCodegen.MESSAGE ~ " +/\n"
			~ "}\n".replicate(depth) ~ "}\n", sac);
	}

	stderr.writeln("Unittest for ImportSortednessCheck passed.");
}
