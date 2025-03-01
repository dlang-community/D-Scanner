//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.auto_ref_assignment;

import dscanner.analysis.base;

/**
 * Checks for assignment to auto-ref function parameters.
 */
extern(C++) class AutoRefAssignmentCheck(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"auto_ref_assignment_check";
	alias visit = BaseAnalyzerDmd.visit;

	mixin ScopedVisit!(AST.ClassDeclaration);
	mixin ScopedVisit!(AST.StructDeclaration);
	mixin ScopedVisit!(AST.FuncDeclaration);
	mixin ScopedVisit!(AST.InterfaceDeclaration);
	mixin ScopedVisit!(AST.UnionDeclaration);
	mixin ScopedVisit!(AST.ScopeStatement);

	///
	extern(D) this(string fileName)
	{
		super(fileName);
	}

	override void visit(AST.TemplateDeclaration td)
	{
		auto autoRefParamsOld = autoRefParams;
		autoRefParams = [];
		auto temp = inTemplateScope;
		inTemplateScope = true;

		super.visit(td);
		
		inTemplateScope = temp;
		autoRefParams = autoRefParamsOld;
	}

	override void visit(AST.Parameter p)
	{
		import dmd.astenums : STC;

		if (p.storageClass & STC.auto_ && p.storageClass & STC.ref_ && p.ident)
			autoRefParams ~= p.ident.toString();
	}

	override void visit(AST.AssignExp ae)
	{
		import std.algorithm: canFind;

		auto ie = ae.e1.isIdentifierExp();

		if (ie && inTemplateScope && autoRefParams.canFind(ie.ident.toString()))
			addErrorMessage(cast(ulong) ae.loc.linnum, cast(ulong) ae.loc.charnum, KEY,
				"Assignment to auto-ref function parameter.");
	}

	template ScopedVisit(NodeType)
	{
		override void visit(NodeType n)
		{
			auto temp = inTemplateScope;
			inTemplateScope = false;
			super.visit(n);
			inTemplateScope = temp;
		}
	}

private:
	const(char[])[] autoRefParams;
	bool inTemplateScope;

	enum KEY = "dscanner.suspicious.object_const";
}

unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings = assertAnalyzerWarningsDMD;

	StaticAnalysisConfig sac = disabledConfig();
	sac.auto_ref_assignment_check = Check.enabled;
	assertAnalyzerWarnings(q{
		int doStuff(T)(auto ref int a)
		{
			a = 10; // [warn]: Assignment to auto-ref function parameter.
		}

		int doStuff(T)(ref int a)
		{
			a = 10;
		}
	}c, sac);
	stderr.writeln("Unittest for AutoRefAssignmentCheck passed.");
}
