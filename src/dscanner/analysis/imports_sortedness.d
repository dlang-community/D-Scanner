// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.imports_sortedness;

import dscanner.analysis.base;

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

		if (i.names.length)
		{
			foreach (name; i.names)
			{
				string aux = to!string(importModuleName ~ "-" ~ name.toString());
				addImport(aux, i);
			}
		}
		else addImport(importModuleName, i);
	}

private:
	enum maxDepth = 20;
	int level;
	string[][int] imports;
	bool[maxDepth] levelAvailable;

	template ScopedVisit(NodeType)
	{
		override void visit(NodeType n)
		{
			if (level >= maxDepth)
				return;

			imports[level] = [];
			imports[++level] = [];
			levelAvailable[level] = true;
			super.visit(n);
			level--;
		}
	}

	extern(D) void addImport(string importModuleName, AST.Import i)
	{
		import std.uni : sicmp;

		if (!levelAvailable[level])
		{
			imports[level] = [];
			levelAvailable[level] = true;
		}

		if (imports[level].length > 0 && imports[level][$ - 1].sicmp(importModuleName) > 0)
		{
			addErrorMessage(cast(ulong) i.loc.linnum, cast(ulong) i.loc.charnum, KEY, MESSAGE);
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
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings = assertAnalyzerWarningsDMD;

	StaticAnalysisConfig sac = disabledConfig();
	sac.imports_sortedness = Check.enabled;

	assertAnalyzerWarnings(q{
		import bar.foo;
		import foo.bar;
	}c, sac);

	assertAnalyzerWarnings(q{
		import foo.bar;
		import bar.foo; // [warn]: The imports are not sorted in alphabetical order
	}c, sac);

	assertAnalyzerWarnings(q{
		import c;
		import c.b;
		import c.a; // [warn]: The imports are not sorted in alphabetical order
		import d.a;
		import d; // [warn]: The imports are not sorted in alphabetical order
	}c, sac);

	assertAnalyzerWarnings(q{
		import a.b, a.c, a.d;
		import a.b, a.d, a.c; // [warn]: The imports are not sorted in alphabetical order
		import a.c, a.b, a.c; // [warn]: The imports are not sorted in alphabetical order
		import foo.bar, bar.foo; // [warn]: The imports are not sorted in alphabetical order
	}c, sac);

	// multiple items out of order
	assertAnalyzerWarnings(q{
		import foo.bar;
		import bar.foo; // [warn]: The imports are not sorted in alphabetical order
		import bar.bar.foo; // [warn]: The imports are not sorted in alphabetical order
	}c, sac);

	assertAnalyzerWarnings(q{
		import test : bar;
		import test : foo;
	}c, sac);

	// selective imports
	assertAnalyzerWarnings(q{
		import test : foo;
		import test : bar; // [warn]: The imports are not sorted in alphabetical order
	}c, sac);

	// selective imports
	assertAnalyzerWarnings(q{
		import test : foo, bar; // [warn]: The imports are not sorted in alphabetical order
	}c, sac);

	assertAnalyzerWarnings(q{
		import b;
		import c : foo;
		import c : bar; // [warn]: The imports are not sorted in alphabetical order
		import a; // [warn]: The imports are not sorted in alphabetical order
	}c, sac);

	assertAnalyzerWarnings(q{
		import c;
		import c : bar;
		import d : bar;
		import d; // [warn]: The imports are not sorted in alphabetical order
		import a : bar; // [warn]: The imports are not sorted in alphabetical order
	}c, sac);

	assertAnalyzerWarnings(q{
		import t0;
		import t1 : a, b = foo;
		import t2;
	}c, sac);

	assertAnalyzerWarnings(q{
		import t1 : a, b = foo;
		import t1 : b, a = foo; // [warn]: The imports are not sorted in alphabetical order
		import t0 : a, b = foo; // [warn]: The imports are not sorted in alphabetical order
	}c, sac);

	// local imports in functions
	assertAnalyzerWarnings(q{
		import t2;
		import t1; // [warn]: The imports are not sorted in alphabetical order
		void foo()
		{
			import f2;
			import f1; // [warn]: The imports are not sorted in alphabetical order
			import f3;
		}
		void bar()
		{
			import f1;
			import f2;
		}
	}c, sac);

	// local imports in scopes
	assertAnalyzerWarnings(q{
		import t2;
		import t1; // [warn]: The imports are not sorted in alphabetical order
		void foo()
		{
			import f2;
			import f1; // [warn]: The imports are not sorted in alphabetical order
			import f3;
			{
				import f2;
				import f1; // [warn]: The imports are not sorted in alphabetical order
				import f3;
			}
			{
				import f1;
				import f2;
				import f3;
			}
		}
	}c, sac);

	// local imports in functions
	assertAnalyzerWarnings(q{
		import t2;
		import t1; // [warn]: The imports are not sorted in alphabetical order
		void foo()
		{
			import f2;
			import f1; // [warn]: The imports are not sorted in alphabetical order
			import f3;
			while (true) {
				import f2;
				import f1; // [warn]: The imports are not sorted in alphabetical order
				import f3;
			}
			for (;;) {
				import f1;
				import f2;
				import f3;
			}
			foreach (el; arr) {
				import f2;
				import f1; // [warn]: The imports are not sorted in alphabetical order
				import f3;
			}
		}
	}c, sac);

	// nested scopes
	assertAnalyzerWarnings(q{
		import t2;
		import t1; // [warn]: The imports are not sorted in alphabetical order
		void foo()
		{
			import f2;
			import f1; // [warn]: The imports are not sorted in alphabetical order
			import f3;
			{
				import f2;
				import f1; // [warn]: The imports are not sorted in alphabetical order
				import f3;
				{
					import f2;
					import f1; // [warn]: The imports are not sorted in alphabetical order
					import f3;
					{
						import f2;
						import f1; // [warn]: The imports are not sorted in alphabetical order
						import f3;
					}
				}
			}
		}
	}c, sac);

	// local imports in functions
	assertAnalyzerWarnings(q{
		import t2;
		import t1; // [warn]: The imports are not sorted in alphabetical order
		struct foo()
		{
			import f2;
			import f1; // [warn]: The imports are not sorted in alphabetical order
			import f3;
		}
		class bar()
		{
			import f1;
			import f2;
		}
	}c, sac);

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

	stderr.writeln("Unittest for ImportSortednessCheck passed.");
}