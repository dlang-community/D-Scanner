//          Copyright Vladimir Panteleev 2020
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.unused_result;

import dscanner.analysis.base;

/**
 * Checks for function call statements which call non-void functions.
 *
 * In case the function returns a value indicating success/failure,
 * ignoring this return value and continuing execution can lead to
 * undesired results.
 *
 * When the return value is intentionally discarded, `cast(void)` can
 * be prepended to silence the check.
 */
extern (C++) class UnusedResultChecker(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"unused_result";

	private enum KEY = "dscanner.performance.enum_array_literal";
	private enum string MSG = "Function return value is discarded";

	private bool[string] nonVoidFuncs;
	private string[] aggregateStack;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	private template VisitAggregate(NodeType)
	{
		override void visit(NodeType aggregate)
		{
			string name = cast(string) aggregate.ident.toString();
			aggregateStack ~= name;
			super.visit(aggregate);
			aggregateStack.length -= 1;
		}
	}

	mixin VisitAggregate!(AST.StructDeclaration);
	mixin VisitAggregate!(AST.ClassDeclaration);

	override void visit(AST.FuncDeclaration funcDeclaration)
	{
		import dmd.astenums : TY;

		auto typeFunc = funcDeclaration.type.isTypeFunction();
		if (typeFunc && typeFunc.next && typeFunc.next.ty != TY.Tvoid && typeFunc.next.ty != TY.Tnoreturn)
		{
			auto typeIdent = typeFunc.next.isTypeIdentifier();
			bool isNoReturn = typeIdent is null ? false : typeIdent.ident.toString() == "noreturn";

			if (!isNoReturn)
			{
				string funcName = buildFullyQualifiedName(cast(string) funcDeclaration.ident.toString());
				nonVoidFuncs[funcName] = true;
			}
		}

		super.visit(funcDeclaration);
	}

	override void visit(AST.AliasDeclaration aliasDecl)
	{
		import std.algorithm : canFind, endsWith;
		import std.array : replace;

		auto typeIdent = aliasDecl.type.isTypeIdentifier();
		if (typeIdent is null)
			return;

		string aliasName = cast(string) aliasDecl.ident.toString();
		string targetName = cast(string) typeIdent.ident.toString();

		foreach(func; nonVoidFuncs.byKey())
		{
			if (func.endsWith(targetName) || func.canFind(targetName ~ "."))
			{
				string newAliasName = func.replace(targetName, aliasName);
				nonVoidFuncs[newAliasName] = true;
			}
		}
	}

	private extern (D) string buildFullyQualifiedName(string funcName)
	{
		import std.algorithm : fold;

		if (aggregateStack.length == 0)
			return funcName;

		return aggregateStack.fold!((a, b) => a ~ "." ~ b) ~ "." ~ funcName;
	}

	mixin VisitInstructionBlock!(AST.WhileStatement);
	mixin VisitInstructionBlock!(AST.ForStatement);
	mixin VisitInstructionBlock!(AST.DoStatement);
	mixin VisitInstructionBlock!(AST.ForeachRangeStatement);
	mixin VisitInstructionBlock!(AST.ForeachStatement);
	mixin VisitInstructionBlock!(AST.SwitchStatement);
	mixin VisitInstructionBlock!(AST.SynchronizedStatement);
	mixin VisitInstructionBlock!(AST.WithStatement);
	mixin VisitInstructionBlock!(AST.TryCatchStatement);
	mixin VisitInstructionBlock!(AST.TryFinallyStatement);

	override void visit(AST.CompoundStatement compoundStatement)
	{
		foreach (statement; *compoundStatement.statements)
		{
			if (hasUnusedResult(statement))
			{
				auto lineNum = cast(ulong) statement.loc.linnum;
				auto charNum = cast(ulong) statement.loc.charnum;
				addErrorMessage(lineNum, charNum, KEY, MSG);
			}

			statement.accept(this);
		}
	}

	override void visit(AST.IfStatement ifStatement)
	{
		if (hasUnusedResult(ifStatement.ifbody))
		{
			auto lineNum = cast(ulong) ifStatement.ifbody.loc.linnum;
			auto charNum = cast(ulong) ifStatement.ifbody.loc.charnum;
			addErrorMessage(lineNum, charNum, KEY, MSG);
		}

		if (ifStatement.elsebody && hasUnusedResult(ifStatement.elsebody))
		{
			auto lineNum = cast(ulong) ifStatement.elsebody.loc.linnum;
			auto charNum = cast(ulong) ifStatement.elsebody.loc.charnum;
			addErrorMessage(lineNum, charNum, KEY, MSG);
		}

		super.visit(ifStatement);
	}

	private template VisitInstructionBlock(NodeType)
	{
		override void visit(NodeType statement)
		{
			if (hasUnusedResult(statement._body))
			{
				auto lineNum = cast(ulong) statement._body.loc.linnum;
				auto charNum = cast(ulong) statement._body.loc.charnum;
				addErrorMessage(lineNum, charNum, KEY, MSG);
			}

			super.visit(statement);
		}
	}

	private bool hasUnusedResult(AST.Statement statement)
	{
		import dmd.astenums : TY;

		auto exprStatement = statement.isExpStatement();
		if (exprStatement is null)
			return false;

		auto callExpr = exprStatement.exp.isCallExp();
		if (callExpr is null)
			return false;

		string funcName = "";

		if (auto identExpr = callExpr.e1.isIdentifierExp())
			funcName = cast(string) identExpr.ident.toString();
		else if (auto dotIdExpr = callExpr.e1.isDotIdExp())
			funcName = buildFullyQualifiedCallName(dotIdExpr);

		return (funcName in nonVoidFuncs) !is null;
	}

	private extern (D) string buildFullyQualifiedCallName(AST.DotIdExp dotIdExpr)
	{
		import std.algorithm : fold, reverse;

		string[] nameStack;
		nameStack ~= cast(string) dotIdExpr.ident.toString();

		auto lastExpr = dotIdExpr.e1;
		while (lastExpr.isDotIdExp())
		{
			auto current = lastExpr.isDotIdExp();
			nameStack ~= cast(string) current.ident.toString();
			lastExpr = current.e1;
		}

		if (auto identExpr = lastExpr.isIdentifierExp())
			nameStack ~= cast(string) identExpr.ident.toString();

		return nameStack.reverse.fold!((a, b) => a ~ "." ~ b);
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import std.stdio : stderr;
	import std.format : format;

	enum string MSG = "Function return value is discarded";
	StaticAnalysisConfig sac = disabledConfig();
	sac.unused_result = Check.enabled;

	assertAnalyzerWarningsDMD(q{
		void fun() {}
        void main()
        {
            fun();
        }
    }c, sac);

	assertAnalyzerWarningsDMD(q{
        alias noreturn = typeof(*null);
        noreturn fun() { while (1) {} }
        noreturn main()
        {
            fun();
        }
    }c, sac);

	assertAnalyzerWarningsDMD(q{
        int fun() { return 1; }
        void main()
        {
            fun(); // [warn]: %s
        }
    }c.format(MSG), sac);

	assertAnalyzerWarningsDMD(q{
        struct Foo
        {
            static bool get()
            {
                return false;
            }
        }
        alias Bar = Foo;
        void main()
        {
            Bar.get(); // [warn]: %s
            Foo.bar.get();
        }
    }c.format(MSG), sac);

	assertAnalyzerWarningsDMD(q{
        void main()
        {
            void fun() {}
            fun();
        }
    }c, sac);

	assertAnalyzerWarningsDMD(q{
        void main()
        {
            int fun() { return 1; }
            fun(); // [warn]: %s
        }
    }c.format(MSG), sac);

	assertAnalyzerWarningsDMD(q{
        int fun() { return 1; }
        void main()
        {
            cast(void) fun();
        }
    }c, sac);

	assertAnalyzerWarningsDMD(q{
        void fun() { }
        alias gun = fun;
        void main()
        {
            gun();
        }
    }c, sac);

	assertAnalyzerWarningsDMD(q{
        int fun() { return 1; }
        void main()
        {
        	if (true)
            	fun(); // [warn]: %s
            else
            	fun(); // [warn]: %s
        }
    }c.format(MSG, MSG), sac);

	assertAnalyzerWarningsDMD(q{
        int fun() { return 1; }
        void main()
        {
        	while (true)
            	fun(); // [warn]: %s
        }
    }c.format(MSG), sac);

	assertAnalyzerWarningsDMD(q{
        int fun() { return 1; }
        alias gun = fun;
        void main()
        {
            gun(); // [warn]: %s
        }
    }c.format(MSG), sac);

	assertAnalyzerWarningsDMD(q{
        void main()
        {
            void fun() {}
            fun();
        }
    }c, sac);

	stderr.writeln("Unittest for UnusedResultChecker passed");
}
