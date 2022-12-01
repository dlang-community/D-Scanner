//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.local_imports;

import dscanner.analysis.base;
import dscanner.analysis.helpers;

import std.stdio : writeln;

/**
 * Checks for local imports that import all symbols.
 * See_also: $(LINK https://issues.dlang.org/show_bug.cgi?id=10378)
 */
extern(C++) class LocalImportCheck(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"local_import_check";
	alias visit = BaseAnalyzerDmd.visit;

	mixin ScopedVisit!(AST.FuncDeclaration);
	mixin ScopedVisit!(AST.IfStatement);
	mixin ScopedVisit!(AST.WhileStatement);
	mixin ScopedVisit!(AST.ForStatement);
	mixin ScopedVisit!(AST.ForeachStatement);
	mixin ScopedVisit!(AST.ClassDeclaration);
	mixin ScopedVisit!(AST.StructDeclaration);
	
	extern(D) this(string fileName)
	{
		super(fileName);
		this.localImport = false;
	}

	override void visit(AST.Import i)
	{
		// Look for import foo.bar : x or foo.bar : y = x
		if (!i.isstatic && localImport && i.names.length == 0 && !i.aliasId)
		{
			addErrorMessage(cast(ulong) i.loc.linnum, cast(ulong) i.loc.charnum, KEY, MESSAGE);
		}
	}

	// Skip unittests for now
	override void visit(AST.UnitTestDeclaration ud)
	{
		return;
	}

private:
	template ScopedVisit(NodeType)
	{
		override void visit(NodeType n)
		{
			bool prevState = localImport;
			localImport = true;
			super.visit(n);
			localImport = prevState;
		}
	}

	bool localImport;
	enum KEY = "dscanner.suspicious.local_imports";
	enum MESSAGE = "Local imports should specify the symbols being imported to avoid hiding local symbols.";
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.local_import_check = Check.enabled;

	assertAnalyzerWarningsDMD(q{
		import std.experimental;

		void foo()
		{
			import std.stdio; // [warn]: Local imports should specify the symbols being imported to avoid hiding local symbols.
			import std.fish : scales, head;
			import DAGRON = std.experimental.dragon;

			if (1)
			{
				import foo.bar; // [warn]: Local imports should specify the symbols being imported to avoid hiding local symbols.
			}
			else
			{
				import foo.bar; // [warn]: Local imports should specify the symbols being imported to avoid hiding local symbols.
			}

			foreach (i; [1, 2, 3])
			{
				import foo.bar; // [warn]: Local imports should specify the symbols being imported to avoid hiding local symbols.
				import std.stdio : writeln;
			}
		}

		import std.experimental.dragon;
	}c, sac);

	stderr.writeln("Unittest for LocalImportCheck passed.");
}