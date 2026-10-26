//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.builtin_property_names;

import dscanner.analysis.base;

/**
 * The following code should be killed with fire:
 * ---
 * class SomeClass
 * {
 * 	void init();
 * 	int init;
 * 	string mangleof = "LOL";
 * 	auto init = 10;
 * 	enum sizeof = 10;
 * }
 * ---
 */

extern(C++) class BuiltinPropertyNameCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"builtin_property_names_check";

	extern(D) this(string fileName)
	{
		super(fileName);
	}

	mixin AggregateVisit!(AST.StructDeclaration);
	mixin AggregateVisit!(AST.ClassDeclaration);
	mixin AggregateVisit!(AST.InterfaceDeclaration);
	mixin AggregateVisit!(AST.UnionDeclaration);

	override void visit(AST.VarDeclaration vd)
	{
		if (inAggregate && isBuiltinProperty(vd.ident.toString()))	
			addErrorMessage(nameRange(vd), KEY, generateErrorMessage(vd.ident.toString()));
	}

	override void visit(AST.FuncDeclaration fd)
	{
		if (inAggregate && isBuiltinProperty(fd.ident.toString()))	
			addErrorMessage(nameRange(fd), KEY, generateErrorMessage(fd.ident.toString()));
	}

	override void visit(AST.AliasDeclaration ad)
	{
		if (inAggregate && isBuiltinProperty(ad.ident.toString()))	
			addErrorMessage(nameRange(ad), KEY, generateErrorMessage(ad.ident.toString()));
	}

	override void visit(AST.TemplateDeclaration td)
	{
		if (inAggregate && isBuiltinProperty(td.ident.toString()))	
			addErrorMessage(nameRange(td), KEY, generateErrorMessage(td.ident.toString()));
	}

private:
	enum string KEY = "dscanner.confusing.builtin_property_names";

	template AggregateVisit(NodeType)
	{
		override void visit(NodeType n)
		{
			inAggregate++;
			super.visit(n);
			inAggregate--;
		}
	}

	extern(D) string generateErrorMessage(const(char)[] name)
	{
		import std.string : format;

		return format("Avoid naming members '%s'. This can"
				~ " confuse code that depends on the '.%s' property of a type.", name, name);
	}

	extern(D) bool isBuiltinProperty(const(char)[] name)
	{
		import std.algorithm : canFind;

		return BuiltinProperties.canFind(name);
	}

	enum string[] BuiltinProperties = ["init", "sizeof", "mangleof", "alignof", "stringof"];
	int inAggregate;
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings = assertAnalyzerWarningsDMD;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.builtin_property_names_check = Check.enabled;
	assertAnalyzerWarnings(q{
class SomeClass
{
	void init(); /+
	     ^^^^ [warn]: Avoid naming members 'init'. This can confuse code that depends on the '.init' property of a type. +/
	int init; /+
	    ^^^^ [warn]: Avoid naming members 'init'. This can confuse code that depends on the '.init' property of a type. +/
	auto init = 10; /+
	     ^^^^ [warn]: Avoid naming members 'init'. This can confuse code that depends on the '.init' property of a type. +/
}
	}c, sac);

	// aliases and templates are members too
	assertAnalyzerWarnings(q{
struct S
{
	alias init = int; /+
	      ^^^^ [warn]: Avoid naming members 'init'. This can confuse code that depends on the '.init' property of a type. +/
	template init() {} /+
	         ^^^^ [warn]: Avoid naming members 'init'. This can confuse code that depends on the '.init' property of a type. +/
	alias stringof = int; /+
	      ^^^^^^^^ [warn]: Avoid naming members 'stringof'. This can confuse code that depends on the '.stringof' property of a type. +/
}
	}c, sac);

	stderr.writeln("Unittest for BuiltinPropertyNamesCheck passed.");
}
