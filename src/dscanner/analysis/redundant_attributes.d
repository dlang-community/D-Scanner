// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.redundant_attributes;

import dscanner.analysis.base;
import dscanner.analysis.helpers;

import dmd.dsymbol;
import std.string : format;

/**
 * Checks for redundant attributes. At the moment only visibility attributes.
 */
extern(C++) class RedundantAttributesCheck(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"redundant_attributes_check";
	alias visit = BaseAnalyzerDmd.visit;
	
	Visibility.Kind currVisibility;
	uint currLine;

	extern(D) this(string fileName)
	{
		super(fileName);
	}

	template ScopedVisit(NodeType)
	{
		override void visit(NodeType n)
		{
			Visibility.Kind prevVisibility = currVisibility;
			currVisibility = Visibility.Kind.undefined;
			super.visit(n);
			currVisibility = prevVisibility;
		}
	}

	mixin ScopedVisit!(AST.StructDeclaration);
	mixin ScopedVisit!(AST.ClassDeclaration);
	mixin ScopedVisit!(AST.InterfaceDeclaration);
	mixin ScopedVisit!(AST.UnionDeclaration);
	mixin ScopedVisit!(AST.StaticIfCondition);
	mixin ScopedVisit!(AST.StaticIfDeclaration);
	mixin ScopedVisit!(AST.TemplateDeclaration);
	mixin ScopedVisit!(AST.ConditionalDeclaration);

	override void visit(AST.VisibilityDeclaration vd)
	{
		if (currVisibility == vd.visibility.kind)
		addErrorMessage(cast(ulong) vd.loc.linnum, cast(ulong) vd.loc.charnum, KEY,
			"Same visibility attribute used as defined on line %u.".format(currLine));
		Visibility.Kind prevVisibility = currVisibility;
		uint prevLine = currLine;
		currVisibility = vd.visibility.kind;
		currLine = vd.loc.linnum;
		super.visit(vd);
		currVisibility = prevVisibility;
		currLine = prevLine;
	}

	enum string KEY = "dscanner.suspicious.redundant_attributes";
}


version(unittest)
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;
}

unittest
{
	StaticAnalysisConfig sac = disabledConfig();
	sac.redundant_attributes_check = Check.enabled;

	// test labels vs. block attributes
	assertAnalyzerWarningsDMD(q{
class C
{
private:
	private int blah; // [warn]: Same visibility attribute used as defined on line 4.
protected
{
	protected int blah; // [warn]: Same visibility attribute used as defined on line 6.
}
	private int blah; // [warn]: Same visibility attribute used as defined on line 4.
}}c, sac);

	// test labels vs. block attributes
	assertAnalyzerWarningsDMD(q{
class C
{
	private:
	private: // [warn]: Same visibility attribute used as defined on line 4.
	public:
		private int a;
		public int b; // [warn]: Same visibility attribute used as defined on line 6.
		public // [warn]: Same visibility attribute used as defined on line 6.
		{
			int c;
		}
}}c, sac);

	// test scopes
	assertAnalyzerWarningsDMD(q{
class C
{
private:
	private int foo2; // [warn]: Same visibility attribute used as defined on line 4.
	private void foo() // [warn]: Same visibility attribute used as defined on line 4.
	{
		private int blah;
	}
}}c, sac);

	// check duplicated visibility attributes
	assertAnalyzerWarningsDMD(q{
class C
{
private:
	public int a;
private: // [warn]: Same visibility attribute used as defined on line 4.
}}c, sac);

	// test conditional compilation
	assertAnalyzerWarningsDMD(q{
class C
{
version(unittest)
{
	private:
	private int foo; // [warn]: Same visibility attribute used as defined on line 6.
}
private int foo2;
}}c, sac);

// test scopes
	assertAnalyzerWarningsDMD(q{
class C
{
public:
	static if (1 == 1)
	{
		private int b;
	}
	else
	{
		public int b;
	}
	public int b; // [warn]: Same visibility attribute used as defined on line 4.
}}c, sac);
}

// test other attribute (not yet implemented, thus shouldn't trigger warnings)
unittest
{
	StaticAnalysisConfig sac = disabledConfig();
	sac.redundant_attributes_check = Check.enabled;

	// test labels vs. block attributes
	assertAnalyzerWarningsDMD(q{
class C
{
@safe:
	@safe void foo();
@system
{
	@system void foo();
}
	@safe void foo();
}}c, sac);


	stderr.writeln("Unittest for RedundantAttributesCheck passed.");
}
