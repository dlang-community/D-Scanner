//          Copyright Brian Schott (Hackerpilot) 2014-2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.unused;

import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import std.container;
import std.regex : Regex, regex, matchAll;
import dsymbol.scope_ : Scope;
import std.algorithm.iteration : map;
import std.algorithm : all;

/**
 * Checks for unused variables.
 */
final class UnusedVariableCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	/**
	 * Params:
	 *     fileName = the name of the file being analyzed
	 */
	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
		re = regex("[\\p{Alphabetic}_][\\w_]*");
	}

	override void visit(const Module mod)
	{
		pushScope();
		mod.accept(this);
		popScope();
	}

	override void visit(const Declaration declaration)
	{
		if (!isOverride)
			foreach (attribute; declaration.attributes)
				isOverride = isOverride || (attribute.attribute == tok!"override");
		declaration.accept(this);
		isOverride = false;
	}

	override void visit(const FunctionDeclaration functionDec)
	{
		pushScope();
		if (functionDec.functionBody !is null)
		{
			immutable bool ias = inAggregateScope;
			inAggregateScope = false;
			if (!isOverride)
				functionDec.parameters.accept(this);
			functionDec.functionBody.accept(this);
			inAggregateScope = ias;
		}
		popScope();
	}

	mixin PartsUseVariables!AliasInitializer;
	mixin PartsUseVariables!ArgumentList;
	mixin PartsUseVariables!AssertExpression;
	mixin PartsUseVariables!ClassDeclaration;
	mixin PartsUseVariables!FunctionBody;
	mixin PartsUseVariables!FunctionCallExpression;
	mixin PartsUseVariables!FunctionDeclaration;
	mixin PartsUseVariables!IndexExpression;
	mixin PartsUseVariables!Initializer;
	mixin PartsUseVariables!InterfaceDeclaration;
	mixin PartsUseVariables!NewExpression;
	mixin PartsUseVariables!StaticIfCondition;
	mixin PartsUseVariables!StructDeclaration;
	mixin PartsUseVariables!TemplateArgumentList;
	mixin PartsUseVariables!ThrowStatement;
	mixin PartsUseVariables!CastExpression;

	override void visit(const SwitchStatement switchStatement)
	{
		if (switchStatement.expression !is null)
		{
			interestDepth++;
			switchStatement.expression.accept(this);
			interestDepth--;
		}
		switchStatement.accept(this);
	}

	override void visit(const WhileStatement whileStatement)
	{
		if (whileStatement.expression !is null)
		{
			interestDepth++;
			whileStatement.expression.accept(this);
			interestDepth--;
		}
		if (whileStatement.declarationOrStatement !is null)
			whileStatement.declarationOrStatement.accept(this);
	}

	override void visit(const DoStatement doStatement)
	{
		if (doStatement.expression !is null)
		{
			interestDepth++;
			doStatement.expression.accept(this);
			interestDepth--;
		}
		if (doStatement.statementNoCaseNoDefault !is null)
			doStatement.statementNoCaseNoDefault.accept(this);
	}

	override void visit(const ForStatement forStatement)
	{
		if (forStatement.initialization !is null)
			forStatement.initialization.accept(this);
		if (forStatement.test !is null)
		{
			interestDepth++;
			forStatement.test.accept(this);
			interestDepth--;
		}
		if (forStatement.increment !is null)
		{
			interestDepth++;
			forStatement.increment.accept(this);
			interestDepth--;
		}
		if (forStatement.declarationOrStatement !is null)
			forStatement.declarationOrStatement.accept(this);
	}

	override void visit(const IfStatement ifStatement)
	{
		if (ifStatement.expression !is null)
		{
			interestDepth++;
			ifStatement.expression.accept(this);
			interestDepth--;
		}
		if (ifStatement.thenStatement !is null)
			ifStatement.thenStatement.accept(this);
		if (ifStatement.elseStatement !is null)
			ifStatement.elseStatement.accept(this);
	}

	override void visit(const ForeachStatement foreachStatement)
	{
		if (foreachStatement.low !is null)
		{
			interestDepth++;
			foreachStatement.low.accept(this);
			interestDepth--;
		}
		if (foreachStatement.high !is null)
		{
			interestDepth++;
			foreachStatement.high.accept(this);
			interestDepth--;
		}
		foreachStatement.accept(this);
	}

	override void visit(const AssignExpression assignExp)
	{
		if (assignExp.ternaryExpression !is null)
			assignExp.ternaryExpression.accept(this);
		if (assignExp.expression !is null)
		{
			interestDepth++;
			assignExp.expression.accept(this);
			interestDepth--;
		}
	}

	override void visit(const TemplateDeclaration templateDeclaration)
	{
		immutable inAgg = inAggregateScope;
		inAggregateScope = true;
		templateDeclaration.accept(this);
		inAggregateScope = inAgg;
	}

	override void visit(const IdentifierOrTemplateChain chain)
	{
		if (interestDepth > 0 && chain.identifiersOrTemplateInstances[0].identifier != tok!"")
			variableUsed(chain.identifiersOrTemplateInstances[0].identifier.text);
		chain.accept(this);
	}

	override void visit(const TemplateSingleArgument single)
	{
		if (single.token != tok!"")
			variableUsed(single.token.text);
	}

	override void visit(const UnaryExpression unary)
	{
		const bool interesting = unary.prefix == tok!"*" || unary.unaryExpression !is null;
		interestDepth += interesting;
		unary.accept(this);
		interestDepth -= interesting;
	}

	override void visit(const MixinExpression mix)
	{
		interestDepth++;
		mixinDepth++;
		mix.accept(this);
		mixinDepth--;
		interestDepth--;
	}

	override void visit(const PrimaryExpression primary)
	{
		if (interestDepth > 0)
		{
			const IdentifierOrTemplateInstance idt = primary.identifierOrTemplateInstance;

			if (idt !is null)
			{
				if (idt.identifier != tok!"")
					variableUsed(idt.identifier.text);
				else if (idt.templateInstance && idt.templateInstance.identifier != tok!"")
					variableUsed(idt.templateInstance.identifier.text);
			}
			if (mixinDepth > 0 && primary.primary == tok!"stringLiteral"
					|| primary.primary == tok!"wstringLiteral"
					|| primary.primary == tok!"dstringLiteral")
			{
				foreach (part; matchAll(primary.primary.text, re))
				{
					void checkTree(in size_t treeIndex)
					{
						auto uu = UnUsed(part.hit);
						auto r = tree[treeIndex].equalRange(&uu);
						if (!r.empty)
							r.front.uncertain = true;
					}
					checkTree(tree.length - 1);
					if (tree.length >= 2)
						checkTree(tree.length - 2);
				}
			}
		}
		primary.accept(this);
	}

	override void visit(const ReturnStatement retStatement)
	{
		if (retStatement.expression !is null)
		{
			interestDepth++;
			visit(retStatement.expression);
			interestDepth--;
		}
	}

	override void visit(const BlockStatement blockStatement)
	{
		immutable bool sb = inAggregateScope;
		inAggregateScope = false;
		if (blockStatementIntroducesScope)
			pushScope();
		blockStatement.accept(this);
		if (blockStatementIntroducesScope)
			popScope();
		inAggregateScope = sb;
	}

	override void visit(const VariableDeclaration variableDeclaration)
	{
		foreach (d; variableDeclaration.declarators)
			this.variableDeclared(d.name.text, d.name.line, d.name.column, false, false);
		variableDeclaration.accept(this);
	}

	override void visit(const Type2 tp)
	{
		if (tp.typeIdentifierPart &&
			tp.typeIdentifierPart.identifierOrTemplateInstance)
		{
			const IdentifierOrTemplateInstance idt = tp.typeIdentifierPart.identifierOrTemplateInstance;
			if (idt.identifier != tok!"")
				variableUsed(idt.identifier.text);
			else if (idt.templateInstance)
			{
				const TemplateInstance ti = idt.templateInstance;
				if (ti.identifier != tok!"")
					variableUsed(idt.templateInstance.identifier.text);
				if (ti.templateArguments && ti.templateArguments.templateSingleArgument)
					variableUsed(ti.templateArguments.templateSingleArgument.token.text);
			}
		}
		tp.accept(this);
	}

	override void visit(const AutoDeclaration autoDeclaration)
	{
		foreach (t; autoDeclaration.parts.map!(a => a.identifier))
			this.variableDeclared(t.text, t.line, t.column, false, false);
		autoDeclaration.accept(this);
	}

	override void visit(const WithStatement withStatetement)
	{
		interestDepth++;
		withStatetement.expression.accept(this);
		interestDepth--;
		withStatetement.declarationOrStatement.accept(this);
	}

	override void visit(const Parameter parameter)
	{
		import std.algorithm : among;
		import std.algorithm.iteration : filter;
		import std.range : empty;
		import std.array : array;

		if (parameter.name != tok!"")
		{
			immutable bool isRef = !parameter.parameterAttributes
				.filter!(a => a.idType.among(tok!"ref", tok!"out")).empty;
			immutable bool isPtr = parameter.type && !parameter.type
				.typeSuffixes.filter!(a => a.star != tok!"").empty;

			variableDeclared(parameter.name.text, parameter.name.line,
					parameter.name.column, true, isRef | isPtr);

			if (parameter.default_ !is null)
			{
				interestDepth++;
				parameter.default_.accept(this);
				interestDepth--;
			}
		}
	}

	override void visit(const StructBody structBody)
	{
		immutable bool sb = inAggregateScope;
		inAggregateScope = true;
		foreach (dec; structBody.declarations)
			visit(dec);
		inAggregateScope = sb;
	}

	override void visit(const ConditionalStatement conditionalStatement)
	{
		immutable bool cs = blockStatementIntroducesScope;
		blockStatementIntroducesScope = false;
		conditionalStatement.accept(this);
		blockStatementIntroducesScope = cs;
	}

	override void visit(const AsmPrimaryExp primary)
	{
		if (primary.token != tok!"")
			variableUsed(primary.token.text);
		if (primary.identifierChain !is null)
			variableUsed(primary.identifierChain.identifiers[0].text);
	}

	override void visit(const TraitsExpression)
	{
		// issue #266: Ignore unused variables inside of `__traits` expressions
	}

	override void visit(const TypeofExpression)
	{
		// issue #270: Ignore unused variables inside of `typeof` expressions
	}

private:

	mixin template PartsUseVariables(NodeType)
	{
		override void visit(const NodeType node)
		{
			interestDepth++;
			node.accept(this);
			interestDepth--;
		}
	}

	void variableDeclared(string name, size_t line, size_t column, bool isParameter, bool isRef)
	{
		if (inAggregateScope || name.all!(a => a == '_'))
			return;
		tree[$ - 1].insert(new UnUsed(name, line, column, isParameter, isRef));
	}

	void variableUsed(string name)
	{
		size_t treeIndex = tree.length - 1;
		auto uu = UnUsed(name);
		while (true)
		{
			if (tree[treeIndex].removeKey(&uu) != 0 || treeIndex == 0)
				break;
			treeIndex--;
		}
	}

	void popScope()
	{
		foreach (uu; tree[$ - 1])
		{
			if (!uu.isRef && tree.length > 1)
			{
			    if (uu.uncertain)
			        continue;
				immutable string certainty = uu.uncertain ? " might not be used."
					: " is never used.";
				immutable string errorMessage = (uu.isParameter ? "Parameter " : "Variable ")
					~ uu.name ~ certainty;
				addErrorMessage(uu.line, uu.column, uu.isParameter ? "dscanner.suspicious.unused_parameter"
						: "dscanner.suspicious.unused_variable", errorMessage);
			}
		}
		tree = tree[0 .. $ - 1];
	}

	void pushScope()
	{
		tree ~= new RedBlackTree!(UnUsed*, "a.name < b.name");
	}

	struct UnUsed
	{
		string name;
		size_t line;
		size_t column;
		bool isParameter;
		bool isRef;
		bool uncertain;
	}

	RedBlackTree!(UnUsed*, "a.name < b.name")[] tree;

	uint interestDepth;

	uint mixinDepth;

	bool isOverride;

	bool inAggregateScope;

	bool blockStatementIntroducesScope = true;

	Regex!char re;
}

@system unittest
{
	import std.stdio : stderr;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.unused_variable_check = Check.enabled;
	assertAnalyzerWarnings(q{

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

	void inPSC(in int a){} // [warn]: Parameter a is never used.

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

	void doStuff(int a, int b) // [warn]: Parameter b is never used.
	{
		return a;
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

	// Issue 352
	void test352_1()
	{
		void f(int *x) {*x = 1;}
	}

	void test352_2()
	{
		void f(Bat** bat) {*bat = bats.ptr + 8;}
	}

	// Issue 490
	void test490()
	{
		auto cb1 = delegate(size_t _) {};
		cb1(3);
		auto cb2 = delegate(size_t a) {}; // [warn]: Parameter a is never used.
		cb2(3);
	}
	
	bool hasDittos(int decl)
	{
		mixin("decl++;");
	}

	void main()
	{
	    const int testValue;
	    testValue.writeln;
	}

	}c, sac);
	stderr.writeln("Unittest for UnusedVariableCheck passed.");
}

