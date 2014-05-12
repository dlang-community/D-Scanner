//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.unused;

import std.d.ast;
import std.d.lexer;
import analysis.base;
import std.container;

/**
 * Checks for unused variables.
 */
class UnusedVariableCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const Module mod)
	{
		pushScope();
		mod.accept(this);
		popScope();
	}

	override void visit(const Declaration declaration)
	{
		if (!isOverride) foreach (attribute; declaration.attributes)
			isOverride = isOverride || (attribute.storageClass !is null &&
				attribute.storageClass.token == tok!"override");
		declaration.accept(this);
	}

	override void visit(const FunctionDeclaration functionDec)
	{
		pushScope();
		if (functionDec.functionBody !is null)
		{
			if (!isOverride)
				functionDec.parameters.accept(this);
			functionDec.functionBody.accept(this);
		}
		popScope();
	}

	mixin template PartsUseVariables(NodeType)
	{
		override void visit(const NodeType node)
		{
			interestDepth++;
			node.accept(this);
			interestDepth--;
		}
	}

	mixin PartsUseVariables!AssertExpression;
	mixin PartsUseVariables!FunctionCallExpression;
	mixin PartsUseVariables!NewExpression;
	mixin PartsUseVariables!TemplateArgumentList;
	mixin PartsUseVariables!MixinExpression;
	mixin PartsUseVariables!ArgumentList;
	mixin PartsUseVariables!Initializer;
	mixin PartsUseVariables!SliceExpression;
	mixin PartsUseVariables!StaticIfCondition;

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
		interestDepth++;
		whileStatement.expression.accept(this);
		interestDepth--;
		whileStatement.declarationOrStatement.accept(this);
	}

	override void visit(const DoStatement doStatement)
	{
		interestDepth++;
		doStatement.expression.accept(this);
		interestDepth--;
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
		assignExp.ternaryExpression.accept(this);
		if (assignExp.assignExpression !is null)
		{
			interestDepth++;
			assignExp.assignExpression.accept(this);
			interestDepth--;
		}
	}

	override void visit(const TemplateDeclaration templateDeclaration)
	{
		auto inAgg = inAggregateScope;
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

	override void visit(const PrimaryExpression primary)
	{
		if (interestDepth > 0 && primary.identifierOrTemplateInstance !is null
			&& primary.identifierOrTemplateInstance.identifier != tok!"")
		{
			variableUsed(primary.identifierOrTemplateInstance.identifier.text);
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
		bool sb = inAggregateScope;
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

	override void visit(const AutoDeclaration autoDeclaration)
	{
		foreach (t; autoDeclaration.identifiers)
			this.variableDeclared(t.text, t.line, t.column, false, false);
		autoDeclaration.accept(this);
	}

	override void visit(const WithStatement withStatetement)
	{
		interestDepth++;
		withStatetement.expression.accept(this);
		interestDepth--;
		withStatetement.statementNoCaseNoDefault.accept(this);
	}

	override void visit(const Parameter parameter)
	{
		import std.algorithm;
		import std.array;
//		import std.stdio;
		if (parameter.name != tok!"")
		{
//			stderr.writeln("Adding parameter ", parameter.name.text);
			variableDeclared(parameter.name.text, parameter.name.line,
				parameter.name.column, true, canFind(parameter.parameterAttributes,
				cast(IdType) tok!"ref"));
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
		bool sb = inAggregateScope;
		inAggregateScope = true;
		foreach (dec; structBody.declarations)
			visit(dec);
		inAggregateScope = sb;
	}

	override void visit(const ConditionalStatement conditionalStatement)
	{
		bool cs = blockStatementIntroducesScope;
		blockStatementIntroducesScope = false;
		conditionalStatement.accept(this);
		blockStatementIntroducesScope = cs;
	}

	void variableDeclared(string name, size_t line, size_t column,
		bool isParameter, bool isRef)
	{
//		import std.stdio;
		if (inAggregateScope)
			return;
//		stderr.writeln("Adding ", name, " ", isParameter, " ", isRef);
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
			if (!uu.isRef)
				addErrorMessage(uu.line, uu.column,
					(uu.isParameter ? "Parameter " : "Variable ")
					~ uu.name ~ " is never used");
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
	}

	RedBlackTree!(UnUsed*, "a.name < b.name")[] tree;

	uint interestDepth;

	bool isOverride;

	bool inAggregateScope;

	bool blockStatementIntroducesScope = true;
}
