//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.local_imports;

import std.stdio;
import dparse.ast;
import dparse.lexer;
import analysis.base;
import analysis.helpers;
import dsymbol.scope_;

/**
 * Checks for local imports that import all symbols.
 * See_also: $(LINK https://issues.dlang.org/show_bug.cgi?id=10378)
 */
class LocalImportCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	/**
	 * Construct with the given file name.
	 */
	this(string fileName, const(Scope)* sc)
	{
		super(fileName, sc);
	}

	mixin visitThing!StructBody;
	mixin visitThing!BlockStatement;

	override void visit(const Declaration dec)
	{
		if (dec.importDeclaration is null)
		{
			dec.accept(this);
			return;
		}
		foreach (attr; dec.attributes)
		{
			if (attr.attribute == tok!"static")
				isStatic = true;
		}
		dec.accept(this);
		isStatic = false;
	}

	override void visit(const ImportDeclaration id)
	{
		if ((!isStatic && interesting) && (id.importBindings is null
				|| id.importBindings.importBinds.length == 0))
		{
			foreach (singleImport; id.singleImports)
			{
				if (singleImport.rename.text.length == 0)
				{
					addErrorMessage(singleImport.identifierChain.identifiers[0].line,
							singleImport.identifierChain.identifiers[0].column,
							"dscanner.suspicious.local_imports", "Local imports should specify"
							~ " the symbols being imported to avoid hiding local symbols.");
				}
			}
		}
	}

private:

	mixin template visitThing(T)
	{
		override void visit(const T thing)
		{
			auto b = interesting;
			interesting = true;
			thing.accept(this);
			interesting = b;
		}
	}

	bool interesting;
	bool isStatic;
}

unittest
{
	import analysis.config : StaticAnalysisConfig;

	StaticAnalysisConfig sac;
	sac.local_import_check = true;
	assertAnalyzerWarnings(q{
		void testLocalImport()
		{
			import std.stdio; // [warn]: Local imports should specify the symbols being imported to avoid hiding local symbols.
			import std.fish : scales, head;
			import DAGRON = std.experimental.dragon;
		}
	}c, sac);

	stderr.writeln("Unittest for LocalImportCheck passed.");
}
