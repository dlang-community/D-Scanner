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
import std.algorithm : all;

/**
 * Checks for unused variables.
 */
abstract class UnusedIdentifierCheck : BaseAnalyzer
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
		if (functionDec.functionBody && functionDec.functionBody.specifiedFunctionBody)
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
		interestDepth++;
		assignExp.accept(this);
		interestDepth--;
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

	override void visit(const WithStatement withStatetement)
	{
		interestDepth++;
		if (withStatetement.expression)
			withStatetement.expression.accept(this);
		interestDepth--;
		if (withStatetement.declarationOrStatement)
			withStatetement.declarationOrStatement.accept(this);
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

	abstract protected void popScope();

	protected uint interestDepth;

	protected Tree[] tree;

	protected void variableDeclared(string name, size_t line, size_t column, bool isRef)
	{
		if (inAggregateScope || name.all!(a => a == '_'))
			return;
		tree[$ - 1].insert(new UnUsed(name, line, column, isRef));
	}

	protected void pushScope()
	{
		tree ~= new Tree;
	}

private:

	struct UnUsed
	{
		string name;
		size_t line;
		size_t column;
		bool isRef;
		bool uncertain;
	}

	alias Tree = RedBlackTree!(UnUsed*, "a.name < b.name");

	mixin template PartsUseVariables(NodeType)
	{
		override void visit(const NodeType node)
		{
			interestDepth++;
			node.accept(this);
			interestDepth--;
		}
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

	Regex!char re;

	bool inAggregateScope;

	uint mixinDepth;

	bool isOverride;

	bool blockStatementIntroducesScope = true;
}

/// Base class for unused parameter/variables checks
abstract class UnusedStorageCheck : UnusedIdentifierCheck
{
	alias visit = UnusedIdentifierCheck.visit;

	/**
	Ignore declarations which are allowed to be unused, e.g. inside of a
	speculative compilation: __traits(compiles, { S s = 0; })
	**/
	uint ignoreDeclarations = 0;

	/// Kind of declaration for error messages e.g. "Variable"
	const string publicType;

	/// Kind of declaration for error reports e.g. "unused_variable"
	const string reportType;

	/**
	 * Params:
	 *      fileName	= the name of the file being analyzed
	 *		sc			= the scope
	 *		skipTest	= whether tests should be analyzed
	 *		publicType	= declaration kind used in error messages, e.g. "Variable"s
	 *		reportType	= declaration kind used in error reports, e.g. "unused_variable"
	 */
	this(string fileName, const(Scope)* sc, bool skipTests = false, string publicType = null, string reportType = null)
	{
		super(fileName, sc, skipTests);
		this.publicType = publicType;
		this.reportType = reportType;
	}

	override void visit(const TraitsExpression traitsExp)
	{
		// issue #788: Enum values might be used inside of `__traits` expressions, e.g.:
		// enum name = "abc";
		// __traits(hasMember, S, name);
		ignoreDeclarations++;
		traitsExp.templateArgumentList.accept(this);
		ignoreDeclarations--;
	}

	override final protected void popScope()
	{
		if (!ignoreDeclarations)
		{
			foreach (uu; tree[$ - 1])
			{
				if (!uu.isRef && tree.length > 1)
				{
					if (uu.uncertain)
						continue;
					immutable string errorMessage = publicType ~ ' ' ~ uu.name ~ " is never used.";
					addErrorMessage(uu.line, uu.column,
							"dscanner.suspicious." ~ reportType, errorMessage);
				}
			}
		}
		tree = tree[0 .. $ - 1];
	}
}
