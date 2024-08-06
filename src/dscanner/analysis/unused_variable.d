//          Copyright Brian Schott (Hackerpilot) 2014-2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.unused_variable;

import dscanner.analysis.base;
import std.algorithm : all, canFind, each, endsWith, filter, map;

/**
 * Checks for unused variables.
 */
// TODO: many similarities to unused_param.d, maybe refactor into a common base class
extern (C++) class UnusedVariableCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"unused_variable_check";

	private enum KEY = "dscanner.suspicious.unused_variable";
	private enum MSG = "Variable %s is never used.";

	private static struct VarInfo
	{
		string name;
		ulong lineNum;
		ulong charNum;
		bool isUsed = false;
	}

	private alias VarSet = VarInfo[string];
	private VarSet[] usedVars;
	private bool inMixin;
	private bool shouldIgnoreDecls;
	private bool inFunction;
	private bool inAggregate;
	private bool shouldNotPushScope;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
		pushScope();
	}

	override void visit(AST.FuncDeclaration funcDeclaration)
	{
		auto oldInFunction = inFunction;
		inFunction = true;
		super.visit(funcDeclaration);
		inFunction = oldInFunction;
	}

	mixin VisitAggregate!(AST.ClassDeclaration);
	mixin VisitAggregate!(AST.StructDeclaration);
	mixin VisitAggregate!(AST.UnionDeclaration);

	private template VisitAggregate(NodeType)
	{
		override void visit(NodeType node)
		{
			auto oldInFunction = inFunction;
			auto oldInAggregate = inAggregate;
			inFunction = false;
			inAggregate = true;
			super.visit(node);
			inFunction = oldInFunction;
			inAggregate = oldInAggregate;
		}
	}

	mixin VisitConditional!(AST.ConditionalDeclaration);
	mixin VisitConditional!(AST.ConditionalStatement);

	private template VisitConditional(NodeType)
	{
		override void visit(NodeType node)
		{
			auto oldShouldNotPushScope = shouldNotPushScope;
			shouldNotPushScope = true;
			super.visit(node);
			shouldNotPushScope = oldShouldNotPushScope;
		}
	}

	override void visit(AST.CompoundStatement compoundStatement)
	{
		if (!shouldNotPushScope)
			pushScope();

		super.visit(compoundStatement);

		if (!shouldNotPushScope)
			popScope();
	}

	override void visit(AST.VarDeclaration varDeclaration)
	{
		super.visit(varDeclaration);

		if (varDeclaration.ident)
		{
			string varName = cast(string) varDeclaration.ident.toString();
			bool isAggregateField = inAggregate && !inFunction;
			bool ignore = isAggregateField || shouldIgnoreDecls || varName.all!(c => c == '_');
			currentScope[varName] = VarInfo(varName, varDeclaration.loc.linnum, varDeclaration.loc.charnum, ignore);
		}
	}

	override void visit(AST.TypeAArray typeAArray)
	{
		import std.array : split;

		super.visit(typeAArray);

		string assocArrayStr = cast(string) typeAArray.toString();
		assocArrayStr.split('[')
			.filter!(key => key.endsWith(']'))
			.map!(key => key.split(']')[0])
			.each!(key => markAsUsed(key));
	}

	override void visit(AST.TemplateDeclaration templateDecl)
	{
		super.visit(templateDecl);

		if (templateDecl.ident)
		{
			string varName = cast(string) templateDecl.ident.toString();
			bool isAggregateField = inAggregate && !inFunction;
			bool ignore = isAggregateField || shouldIgnoreDecls || varName.all!(c => c == '_');
			currentScope[varName] = VarInfo(varName, templateDecl.loc.linnum, templateDecl.loc.charnum, ignore);
		}
	}

	override void visit(AST.TypeSArray staticArray)
	{
		if (auto identifierExpression = staticArray.dim.isIdentifierExp())
			identifierExpression.accept(this);
	}

	override void visit(AST.IdentifierExp identifierExp)
	{
		if (identifierExp.ident)
			markAsUsed(cast(string) identifierExp.ident.toString());

		super.visit(identifierExp);
	}

	mixin VisitMixin!(AST.MixinExp);
	mixin VisitMixin!(AST.MixinStatement);

	private template VisitMixin(NodeType)
	{
		override void visit(NodeType node)
		{
			inMixin = true;
			super.visit(node);
			inMixin = false;
		}
	}

	override void visit(AST.StringExp stringExp)
	{
		if (!inMixin)
			return;

		string str = cast(string) stringExp.toStringz();
		currentScope.byKey
			.filter!(param => canFind(str, param))
			.each!(param => markAsUsed(param));
	}

	override void visit(AST.TraitsExp traitsExp)
	{
		import dmd.dtemplate : isType;

		auto oldShouldIgnoreDecls = shouldIgnoreDecls;

		if (cast(string) traitsExp.ident.toString() == "compiles")
			shouldIgnoreDecls = true;

		super.visit(traitsExp);
		shouldIgnoreDecls = oldShouldIgnoreDecls;

		if (traitsExp.args is null)
			return;

		(*traitsExp.args).opSlice().map!(arg => isType(arg))
			.filter!(type => type !is null)
			.map!(type => type.isTypeIdentifier())
			.filter!(typeIdentifier => typeIdentifier !is null)
			.each!(typeIdentifier => markAsUsed(cast(string) typeIdentifier.toString()));
	}

	override void visit(AST.TypeTypeof typeOf)
	{
		auto oldShouldIgnoreDecls = shouldIgnoreDecls;
		shouldIgnoreDecls = true;
		super.visit(typeOf);
		shouldIgnoreDecls = oldShouldIgnoreDecls;
	}

	override void visit(AST.TemplateInstance templateInstance)
	{
		import dmd.dtemplate : isExpression, isType;
		import dmd.mtype : Type;

		super.visit(templateInstance);

		if (templateInstance.name)
			markAsUsed(cast(string) templateInstance.name.toString());

		if (templateInstance.tiargs is null)
			return;

		auto argSlice = (*templateInstance.tiargs).opSlice();

		argSlice.map!(arg => arg.isExpression())
			.filter!(arg => arg !is null)
			.map!(arg => arg.isIdentifierExp())
			.filter!(identifierExp => identifierExp !is null)
			.each!(identifierExp => markAsUsed(cast(string) identifierExp.ident.toString()));

		argSlice.map!(arg => arg.isType())
			.filter!(arg => arg !is null)
			.map!(arg => arg.isTypeIdentifier())
			.filter!(identifierType => identifierType !is null)
			.each!(identifierType => markAsUsed(cast(string) identifierType.ident.toString()));
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
		import std.format : format;

		currentScope.byValue
			.filter!(var => !var.isUsed)
			.each!(var => addErrorMessage(var.lineNum, var.charNum, KEY, MSG.format(var.name)));

		usedVars.length--;
	}
}

@system unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;

	StaticAnalysisConfig sac = disabledConfig();
	sac.unused_variable_check = Check.enabled;

	assertAnalyzerWarningsDMD(q{

	// Issue 274
	unittest
	{
		size_t byteIndex = 0;
		*(cast(FieldType*)(retVal.ptr + byteIndex)) = item;
	}

	unittest
	{
		int a; // [warn]: Variable a is never used.
	}

	// Issue 380
	int templatedEnum()
	{
		enum a(T) = T.init;
		return a!int;
	}

	// Issue 380
	int otherTemplatedEnum()
	{
		auto a(T) = T.init; // [warn]: Variable a is never used.
		return 0;
	}

	// Issue 364
	void test364_1()
	{
		enum s = 8;
		immutable t = 2;
		int[s][t] a;
		a[0][0] = 1;
	}

	void test364_2()
	{
		enum s = 8;
		alias a = e!s;
		a = 1;
	}

	void oops ()
	{
		class Identity { int val; }
		Identity v;
		v.val = 0;
	}

	void main()
	{
		const int testValue;
		testValue.writeln;
	}

	// Issue 788
	void traits()
	{
		enum fieldName = "abc";
		__traits(hasMember, S, fieldName);

		__traits(compiles, { int i = 2; });
	}

	// segfault with null templateArgumentList
	void nullTest()
	{
		__traits(isPOD);
	}

	void unitthreaded()
	{
		auto testVar = foo.sort!myComp;
		genVar.should == testVar;
	}

	}c, sac);

	assertAnalyzerWarningsDMD(q{
		void testMixinExpression()
		{
			int a;
			mixin("a = 5");
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		bool f()
		{
			static if (is(S == bool) && is(typeof({ T s = "string"; })))
    	    {
    	        return src ? "true" : "false";
    	    }

    	    return false;
		}
	}c, sac);

	stderr.writeln("Unittest for UnusedVariableCheck passed.");
}
