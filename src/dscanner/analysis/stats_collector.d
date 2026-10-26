//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.stats_collector;

import dscanner.analysis.base;

extern (C++) class StatsCollector(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"stats_collector";

	public uint interfaceCount;
	public uint classCount;
	public uint functionCount;
	public uint templateCount;
	public uint structCount;
	public uint statementCount;
	// TODO: Count lines of code
	public uint lineOfCodeCount;
	// TODO: Count undocumented public symbols
	public uint undocumentedPublicSymbols;

	extern (D) this(string fileName = "", bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.InterfaceDeclaration interfaceDecl)
	{
		interfaceCount++;
		super.visit(interfaceDecl);
	}

	override void visit(AST.ClassDeclaration classDecl)
	{
		classCount++;
		super.visit(classDecl);
	}

	override void visit(AST.FuncDeclaration funcDecl)
	{
		functionCount++;
		super.visit(funcDecl);
	}

	override void visit(AST.TemplateDeclaration templateDecl)
	{
		templateCount++;
		super.visit(templateDecl);
	}

	override void visit(AST.StructDeclaration structDecl)
	{
		structCount++;
		super.visit(structDecl);
	}

	mixin VisitStatement!(AST.ErrorStatement);
	mixin VisitStatement!(AST.PeelStatement);
	mixin VisitStatement!(AST.ScopeStatement);
	mixin VisitStatement!(AST.ExpStatement);
	mixin VisitStatement!(AST.ReturnStatement);
	mixin VisitStatement!(AST.IfStatement);
	mixin VisitStatement!(AST.CaseStatement);
	mixin VisitStatement!(AST.DefaultStatement);
	mixin VisitStatement!(AST.LabelStatement);
	mixin VisitStatement!(AST.GotoStatement);
	mixin VisitStatement!(AST.GotoDefaultStatement);
	mixin VisitStatement!(AST.GotoCaseStatement);
	mixin VisitStatement!(AST.BreakStatement);
	mixin VisitStatement!(AST.DtorExpStatement);
	mixin VisitStatement!(AST.MixinStatement);
	mixin VisitStatement!(AST.ForwardingStatement);
	mixin VisitStatement!(AST.DoStatement);
	mixin VisitStatement!(AST.WhileStatement);
	mixin VisitStatement!(AST.ForStatement);
	mixin VisitStatement!(AST.ForeachStatement);
	mixin VisitStatement!(AST.SwitchStatement);
	mixin VisitStatement!(AST.ContinueStatement);
	mixin VisitStatement!(AST.WithStatement);
	mixin VisitStatement!(AST.TryCatchStatement);
	mixin VisitStatement!(AST.ThrowStatement);
	mixin VisitStatement!(AST.DebugStatement);
	mixin VisitStatement!(AST.TryFinallyStatement);
	mixin VisitStatement!(AST.ScopeGuardStatement);
	mixin VisitStatement!(AST.SwitchErrorStatement);
	mixin VisitStatement!(AST.UnrolledLoopStatement);
	mixin VisitStatement!(AST.ForeachRangeStatement);
	mixin VisitStatement!(AST.CompoundDeclarationStatement);
	mixin VisitStatement!(AST.CompoundAsmStatement);
	mixin VisitStatement!(AST.StaticAssertStatement);
	mixin VisitStatement!(AST.CaseRangeStatement);
	mixin VisitStatement!(AST.SynchronizedStatement);
	mixin VisitStatement!(AST.AsmStatement);
	mixin VisitStatement!(AST.InlineAsmStatement);
	mixin VisitStatement!(AST.GccAsmStatement);
	mixin VisitStatement!(AST.ImportStatement);

	private template VisitStatement(NodeType)
	{
		override void visit(NodeType node)
		{
			statementCount++;
			super.visit(node);
		}
	}
}

unittest
{
	import std.file : exists, remove;
	import std.path : dirName;
	import std.stdio : File, stderr;
	import dscanner.analysis.rundmd : parseDmdModule;
	import dmd.astcodegen : ASTCodegen;

	string code = q{
		interface I {}
		class C {}
		void f() {}
		template T() {}
		struct S {}

		void funcWithStatements()
		{
			int a = 1;
			if (a == 1)
				a = 2;
			a++;
		}
	}c;

	auto testFileName = "test.d";
	File f = File(testFileName, "w");
	scope(exit)
	{
		assert(exists(testFileName));
		remove(testFileName);
	}

	f.rawWrite(code);
	f.close();
	auto dmdModule = parseDmdModule(testFileName, code);
	auto collector = new StatsCollector!ASTCodegen();
	dmdModule.accept(collector);

	assert(collector.interfaceCount == 1);
	assert(collector.classCount == 1);
	assert(collector.functionCount == 2);
	assert(collector.templateCount == 1);
	assert(collector.structCount == 1);
	assert(collector.statementCount == 4);

	stderr.writeln("Unittest for StatsCollector passed.");
}
