//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.unmodified;

import dscanner.analysis.base;
import dscanner.utils : safeAccess;
import dsymbol.scope_ : Scope;
import std.container;
import dparse.ast;
import dparse.lexer;

/**
 * Checks for variables that could have been declared const or immutable
 */
final class UnmodifiedFinder : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	///
	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const Module mod)
	{
		pushScope();
		mod.accept(this);
		popScope();
	}

	override void visit(const BlockStatement blockStatement)
	{
		pushScope();
		blockStatementDepth++;
		blockStatement.accept(this);
		blockStatementDepth--;
		popScope();
	}

	override void visit(const StructBody structBody)
	{
		pushScope();
		immutable oldBlockStatementDepth = blockStatementDepth;
		blockStatementDepth = 0;
		structBody.accept(this);
		blockStatementDepth = oldBlockStatementDepth;
		popScope();
	}

	override void visit(const VariableDeclaration dec)
	{
		if (dec.autoDeclaration is null && blockStatementDepth > 0
				&& isImmutable <= 0 && !canFindImmutable(dec))
		{
			foreach (d; dec.declarators)
			{
				if (initializedFromCast(d.initializer))
					continue;
				if (initializedFromNew(d.initializer))
					continue;
				tree[$ - 1].insert(new VariableInfo(d.name.text, d.name.line,
						d.name.column, isValueTypeSimple(dec.type)));
			}
		}
		dec.accept(this);
	}

	override void visit(const AutoDeclaration autoDeclaration)
	{
		import std.algorithm : canFind;

		if (blockStatementDepth > 0 && isImmutable <= 0
				&& (!autoDeclaration.storageClasses.canFind!(a => a.token == tok!"const"
					|| a.token == tok!"enum" || a.token == tok!"immutable")))
		{
			foreach (part; autoDeclaration.parts)
			{
				if (initializedFromCast(part.initializer))
					continue;
				if (initializedFromNew(part.initializer))
					continue;
				tree[$ - 1].insert(new VariableInfo(part.identifier.text,
						part.identifier.line, part.identifier.column));
			}
		}
		autoDeclaration.accept(this);
	}

	override void visit(const AssignExpression assignExpression)
	{
		if (assignExpression.operator != tok!"")
		{
			interest++;
			guaranteeUse++;
			assignExpression.ternaryExpression.accept(this);
			guaranteeUse--;
			interest--;

			if (assignExpression.operator == tok!"~=")
				interest++;
			assignExpression.expression.accept(this);
			if (assignExpression.operator == tok!"~=")
				interest--;
		}
		else
			assignExpression.accept(this);
	}

	override void visit(const Declaration dec)
	{
		if (canFindImmutableOrConst(dec))
		{
			isImmutable++;
			dec.accept(this);
			isImmutable--;
		}
		else
			dec.accept(this);
	}

	override void visit(const IdentifierChain ic)
	{
		if (ic.identifiers.length && interest > 0)
			variableMightBeModified(ic.identifiers[0].text);
		ic.accept(this);
	}

	override void visit(const IdentifierOrTemplateInstance ioti)
	{
		if (ioti.identifier != tok!"" && interest > 0)
			variableMightBeModified(ioti.identifier.text);
		ioti.accept(this);
	}

	mixin PartsMightModify!AsmPrimaryExp;
	mixin PartsMightModify!IndexExpression;
	mixin PartsMightModify!FunctionCallExpression;
	mixin PartsMightModify!NewExpression;
	mixin PartsMightModify!IdentifierOrTemplateChain;
	mixin PartsMightModify!ReturnStatement;

	override void visit(const UnaryExpression unary)
	{
		if (unary.prefix == tok!"++" || unary.prefix == tok!"--"
				|| unary.suffix == tok!"++" || unary.suffix == tok!"--"
				|| unary.prefix == tok!"*" || unary.prefix == tok!"&")
		{
			interest++;
			guaranteeUse++;
			unary.accept(this);
			guaranteeUse--;
			interest--;
		}
		else
			unary.accept(this);
	}

	override void visit(const ForeachStatement foreachStatement)
	{
		if (foreachStatement.low !is null)
		{
			interest++;
			foreachStatement.low.accept(this);
			interest--;
		}
		if (foreachStatement.declarationOrStatement !is null)
			foreachStatement.declarationOrStatement.accept(this);
	}

	override void visit(const TraitsExpression)
	{
		// issue #266: Ignore unmodified variables inside of `__traits` expressions
	}

	override void visit(const TypeofExpression)
	{
		// issue #270: Ignore unmodified variables inside of `typeof` expressions
	}

	override void visit(const AsmStatement a)
	{
		inAsm = true;
		a.accept(this);
		inAsm = false;
	}

private:

	template PartsMightModify(T)
	{
		override void visit(const T t)
		{
			interest++;
			t.accept(this);
			interest--;
		}
	}

	void variableMightBeModified(string name)
	{
		size_t index = tree.length - 1;
		auto vi = VariableInfo(name);
		if (guaranteeUse == 0)
		{
			auto r = tree[index].equalRange(&vi);
			if (!r.empty && r.front.isValueType && !inAsm)
				return;
		}
		while (true)
		{
			if (tree[index].removeKey(&vi) != 0 || index == 0)
				break;
			index--;
		}
	}

	bool initializedFromNew(const Initializer initializer)
	{
		if (const UnaryExpression ue = cast(UnaryExpression) safeAccess(initializer)
			.nonVoidInitializer.assignExpression)
		{
			return ue.newExpression !is null;
		}
		return false;
	}

	bool initializedFromCast(const Initializer initializer)
	{
		import std.typecons : scoped;

		static class CastFinder : ASTVisitor
		{
			alias visit = ASTVisitor.visit;
			override void visit(const CastExpression castExpression)
			{
				foundCast = true;
				castExpression.accept(this);
			}

			bool foundCast;
		}

		if (initializer is null)
			return false;
		auto finder = scoped!CastFinder();
		finder.visit(initializer);
		return finder.foundCast;
	}

	bool canFindImmutableOrConst(const Declaration dec)
	{
		import std.algorithm : canFind, map, filter;

		return !dec.attributes.map!(a => a.attribute)
			.filter!(a => a == tok!"immutable" || a == tok!"const").empty;
	}

	bool canFindImmutable(const VariableDeclaration dec)
	{
		import std.algorithm : canFind;

		foreach (storageClass; dec.storageClasses)
		{
			if (storageClass.token == tok!"enum")
				return true;
		}
		foreach (sc; dec.storageClasses)
		{
			if (sc.token == tok!"immutable" || sc.token == tok!"const")
				return true;
		}
		if (dec.type !is null)
		{
			foreach (tk; dec.type.typeConstructors)
				if (tk == tok!"immutable" || tk == tok!"const")
					return true;
			if (dec.type.type2)
			{
				const tk = dec.type.type2.typeConstructor;
				if (tk == tok!"immutable" || tk == tok!"const")
					return true;
			}
		}
		return false;
	}

	static struct VariableInfo
	{
		string name;
		size_t line;
		size_t column;
		bool isValueType;
	}

	void popScope()
	{
		foreach (vi; tree[$ - 1])
		{
			immutable string errorMessage = "Variable " ~ vi.name
				~ " is never modified and could have been declared const or immutable.";
			addErrorMessage(vi.line, vi.column, "dscanner.suspicious.unmodified", errorMessage);
		}
		tree = tree[0 .. $ - 1];
	}

	void pushScope()
	{
		tree ~= new RedBlackTree!(VariableInfo*, "a.name < b.name");
	}

	int blockStatementDepth;

	int interest;

	int guaranteeUse;

	int isImmutable;

	bool inAsm;

	RedBlackTree!(VariableInfo*, "a.name < b.name")[] tree;
}

bool isValueTypeSimple(const Type type) pure nothrow @nogc
{
	if (type.type2 is null)
		return false;
	return type.type2.builtinType != tok!"" && type.typeSuffixes.length == 0;
}

@system unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;
	import std.stdio : stderr;
	import std.format : format;

	StaticAnalysisConfig sac = disabledConfig();
	sac.could_be_immutable_check = Check.enabled;

	// fails

	assertAnalyzerWarnings(q{
		void foo(){int i = 1;} // [warn]: Variable i is never modified and could have been declared const or immutable.
	}, sac);

	// pass

	assertAnalyzerWarnings(q{
		void foo(){const(int) i;}
	}, sac);

	assertAnalyzerWarnings(q{
		void foo(){immutable(int)* i;}
	}, sac);

	assertAnalyzerWarnings(q{
		void foo(){enum i = 1;}
	}, sac);

	assertAnalyzerWarnings(q{
		void foo(){E e = new E;}
	}, sac);

	assertAnalyzerWarnings(q{
		void foo(){auto e = new E;}
	}, sac);

	assertAnalyzerWarnings(q{
		void issue640()
		{
			size_t i1;
			new Foo(i1);

			size_t i2;
			foo(i2);
		}
	}, sac);
}

