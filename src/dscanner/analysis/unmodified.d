//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.unmodified;

import dscanner.analysis.base;

/**
 * Checks for variables that could have been declared const or immutable
 */
// TODO: many similarities to unused_param.d, maybe refactor into a common base class
extern (C++) class UnmodifiedFinder(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"could_be_immutable_check";

	private enum KEY = "dscanner.suspicious.unmodified";
	private enum MSG = "Variable %s is never modified and could have been declared const or immutable.";

	private static struct VarInfo
	{
		string name;
		ulong lineNum;
		ulong charNum;
		bool isUsed = false;
	}

	private alias VarSet = VarInfo[string];
	private VarSet[] usedVars;
	private bool inAggregate;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
		pushScope();
	}

	override void visit(AST.CompoundStatement compoundStatement)
	{
		pushScope();
		super.visit(compoundStatement);
		popScope();
	}

	override void visit(AST.TemplateDeclaration templateDeclaration)
	{
		auto oldInTemplate = inAggregate;
		inAggregate = true;
		super.visit(templateDeclaration);
		inAggregate = oldInTemplate;
	}

	override void visit(AST.StructDeclaration structDecl)
	{
		auto oldInAggregate = inAggregate;
		inAggregate = true;
		super.visit(structDecl);
		inAggregate = oldInAggregate;
	}

	override void visit(AST.VarDeclaration varDeclaration)
	{
		import dmd.astenums : STC;

		super.visit(varDeclaration);

		if (varDeclaration.ident is null)
			return;

		string varName = cast(string) varDeclaration.ident.toString();
		bool isConst = varDeclaration.storage_class & STC.const_ || varDeclaration.storage_class & STC.immutable_
			|| varDeclaration.storage_class & STC.manifest || isConstType(varDeclaration.type);

		bool markAsUsed = isConst || isFromCastOrNew(varDeclaration._init) || inAggregate;
		currentScope[varName] = VarInfo(varName, varDeclaration.loc.linnum, varDeclaration.loc.charnum, markAsUsed);
	}

	private bool isConstType(AST.Type type)
	{
		import dmd.astenums : MODFlags;

		if (type is null)
			return false;

		bool isConst = type.mod & MODFlags.const_ || type.mod & MODFlags.immutable_;

		if (auto typePtr = type.isTypePointer())
			isConst = isConst || typePtr.next.mod & MODFlags.const_ || typePtr.next.mod & MODFlags.immutable_;

		return isConst;
	}

	private bool isFromCastOrNew(AST.Initializer initializer)
	{
		if (initializer is null)
			return false;

		auto initExpr = initializer.isExpInitializer();
		if (initExpr is null)
			return false;

		return initExpr.exp.isNewExp() !is null || initExpr.exp.isCastExp() !is null;
	}

	override void visit(AST.IntervalExp intervalExp)
	{
		super.visit(intervalExp);

		auto identifier1 = intervalExp.lwr.isIdentifierExp();
		if (identifier1 && identifier1.ident)
			markAsUsed(cast(string) identifier1.ident.toString());

		auto identifier2 = intervalExp.upr.isIdentifierExp();
		if (identifier2 && identifier2.ident)
			markAsUsed(cast(string) identifier2.ident.toString());
	}

	override void visit(AST.IndexExp indexExpression)
	{
		super.visit(indexExpression);

		auto identifier1 = indexExpression.e1.isIdentifierExp();
		if (identifier1 && identifier1.ident)
			markAsUsed(cast(string) identifier1.ident.toString());

		auto identifier2 = indexExpression.e2.isIdentifierExp();
		if (identifier2 && identifier2.ident)
			markAsUsed(cast(string) identifier2.ident.toString());
	}

	mixin VisitAssignNode!(AST.AssignExp);
	mixin VisitAssignNode!(AST.BinAssignExp);
	mixin VisitAssignNode!(AST.PtrExp);
	mixin VisitAssignNode!(AST.AddrExp);
	mixin VisitAssignNode!(AST.PreExp);
	mixin VisitAssignNode!(AST.PostExp);

	private template VisitAssignNode(NodeType)
	{
		override void visit(NodeType node)
		{
			super.visit(node);

			if (node.e1 is null)
				return;

			auto identifier = node.e1.isIdentifierExp();
			if (identifier && identifier.ident)
				markAsUsed(cast(string) identifier.ident.toString());
		}
	}

	mixin VisitFunctionNode!(AST.CallExp);
	mixin VisitFunctionNode!(AST.NewExp);

	private template VisitFunctionNode(NodeType)
	{
		override void visit(NodeType node)
		{
			super.visit(node);

			if (node.arguments is null)
				return;

			foreach (arg; *node.arguments)
			{
				auto identifier = arg.isIdentifierExp();
				if (identifier && identifier.ident)
					markAsUsed(cast(string) arg.isIdentifierExp().ident.toString());
			}
		}
	}

	mixin VisitDotExpressionNode!(AST.DotIdExp);
	mixin VisitDotExpressionNode!(AST.DotTemplateInstanceExp);

	private template VisitDotExpressionNode(NodeType)
	{
		override void visit(NodeType node)
		{
			super.visit(node);
			auto identifierExp = node.e1.isIdentifierExp();
			if (identifierExp && identifierExp.ident)
				markAsUsed(cast(string) identifierExp.ident.toString());
		}
	}

	private extern (D) void markAsUsed(string varName)
	{
		import std.range : retro;

		foreach (funcScope; usedVars.retro())
		{
			if (varName in funcScope)
			{
				funcScope[varName].isUsed = true;
				break;
			}
		}
	}

	@property private extern (D) VarSet currentScope()
	{
		return usedVars[$ - 1];
	}

	private void pushScope()
	{
		// Error with gdc-12
		//usedVars ~= new VarSet;

		// Workaround for gdc-12
		VarSet newScope;
		newScope["test"] = VarInfo("test", 0, 0);
		usedVars ~= newScope;
		currentScope.remove("test");
	}

	private void popScope()
	{
		import std.algorithm : each, filter;
		import std.format : format;

		currentScope.byValue
			.filter!(var => !var.isUsed)
			.each!(var => addErrorMessage(var.lineNum, var.charNum, KEY, MSG.format(var.name)));

		usedVars.length--;
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.could_be_immutable_check = Check.enabled;

	// fails
	assertAnalyzerWarningsDMD(q{
		void foo()
		{
			int i = 1; // [warn]: Variable i is never modified and could have been declared const or immutable.
		}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		void foo()
		{
			int i = 5; // [warn]: Variable i is never modified and could have been declared const or immutable.
			int j = 6;
			j = i + 5;
		}
	}c, sac);

	// pass
	assertAnalyzerWarningsDMD(q{
		void foo()
		{
			const(int) i;
			const int j;
			const(int)* a;
			const int* b;
		}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		void foo()
		{
			immutable(int) i;
			immutable int j;
			immutable(int)* b;
			immutable int* a;
		}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		void foo()
		{
			enum i = 1;
			enum string j = "test";
		}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		void foo()
		{
			E e = new E;
			auto f = new F;
		}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		void issue640()
		{
			size_t i1;
			new Foo(i1);

			size_t i2;
			foo(i2);
		}
	}, sac);

	assertAnalyzerWarningsDMD(q{
		void foo()
		{
			int i = 5; // [warn]: Variable i is never modified and could have been declared const or immutable.
			int j = 6;
			j = i + 5;
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		void foo()
		{
			int i = 5;
			if (true)
				--i;
			else
				i++;
		}
	}c, sac);

	stderr.writeln("Unittest for UnmodifiedFinder passed.");
}
