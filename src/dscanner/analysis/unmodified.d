//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.unmodified;

import dscanner.analysis.base;
import dscanner.analysis.nolint;
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

	mixin AnalyzerInfo!"could_be_immutable_check";

	///
	this(BaseAnalyzerArguments args)
	{
		super(args);
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
				tree[$ - 1].insert(new VariableInfo(d.name.text, d.name, isValueTypeSimple(dec.type)));
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
				tree[$ - 1].insert(new VariableInfo(part.identifier.text, part.identifier));
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
			with (noLint.push(NoLintFactory.fromDeclaration(dec)))
				dec.accept(this);
			isImmutable--;
		}
		else
		{
			with (noLint.push(NoLintFactory.fromDeclaration(dec)))
				dec.accept(this);
		}
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
	mixin PartsMightModify!IdentifierOrTemplateChain;
	mixin PartsMightModify!ReturnStatement;

	override void visit(const FunctionCallExpression functionCallExpression)
	{
		interest++;
		if (functionCallExpression.type !is null)
			functionCallExpression.type.accept(this);
		if (functionCallExpression.unaryExpression !is null)
			functionCallExpression.unaryExpression.accept(this);
		if (functionCallExpression.templateArguments !is null)
			functionCallExpression.templateArguments.accept(this);
		// Arguments may be bound to `ref`/`out` parameters, which modifies
		// them regardless of their type.
		callArgument++;
		if (functionCallExpression.arguments !is null)
			functionCallExpression.arguments.accept(this);
		callArgument--;
		interest--;
	}

	override void visit(const NewExpression newExpression)
	{
		interest++;
		if (newExpression.newAnonClassExpression !is null)
			newExpression.newAnonClassExpression.accept(this);
		if (newExpression.type !is null)
			newExpression.type.accept(this);
		callArgument++;
		if (newExpression.arguments !is null)
			newExpression.arguments.accept(this);
		callArgument--;
		if (newExpression.assignExpression !is null)
			newExpression.assignExpression.accept(this);
		interest--;
	}

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

		// A member access (`a.b`) may require a mutable `a` although it reads
		// like an expression: the member can be a non-const method or property
		// (e.g. range accessors) or return a mutable reference. Without
		// semantic analysis, const-ness of the base cannot be proven.
		if (unary.identifierOrTemplateInstance !is null)
			markMemberAccessBase(unary);
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

	enum string KEY = "dscanner.suspicious.unmodified";

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
		if (guaranteeUse == 0 && callArgument == 0)
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

	void markMemberAccessBase(const UnaryExpression memberAccess)
	{
		const UnaryExpression base = memberAccess.unaryExpression;
		if (base is null)
			return;
		if (base.identifierOrTemplateInstance !is null)
		{
			// chained access `a.b.c`: descend to the leftmost base
			markMemberAccessBase(base);
			return;
		}
		if (base.primaryExpression !is null
				&& base.primaryExpression.identifierOrTemplateInstance !is null)
			variableMightBeModified(
					base.primaryExpression.identifierOrTemplateInstance.identifier.text);
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
		Token token;
		bool isValueType;
	}

	void popScope()
	{
		foreach (vi; tree[$ - 1])
		{
			immutable string errorMessage = "Variable " ~ vi.name
				~ " is never modified and could have been declared const or immutable.";
			addErrorMessage(vi.token, KEY, errorMessage);
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

	int callArgument;

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
		void foo(){int i = 1;} /+
		               ^ [warn]: Variable i is never modified and could have been declared const or immutable. +/
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

	// a value type passed to a function may be bound to a `ref` parameter

	assertAnalyzerWarnings(q{
		void mutate(ref int x)
		{
			x = 42;
		}

		int refMutation()
		{
			int value = 0;
			mutate(value);
			return value;
		}
	}, sac);

	// a value type passed to a function may be bound to an `out` parameter

	assertAnalyzerWarnings(q{
		long produce(out bool createdNow)
		{
			createdNow = true;
			return 1;
		}

		long outParam()
		{
			bool createdNow;
			immutable id = produce(createdNow);
			return id + (createdNow ? 1 : 0);
		}
	}, sac);

	// range accessors are methods that may not be const-callable, so the
	// range variable cannot always be const or immutable

	assertAnalyzerWarnings(q{
		struct SinglePassRange
		{
			int i = 0;

			@property bool empty()
			{
				return i >= 1;
			}

			@property int front()
			{
				return i;
			}
		}

		int rangeAccessors()
		{
			auto r = SinglePassRange();
			if (r.empty)
				return 0;
			return r.front;
		}
	}, sac);

	// member access on a variable may prevent const or immutable even without
	// a visible modification, e.g. Nullable.get on a struct holding an array

	assertAnalyzerWarnings(q{
		struct Document
		{
			string name;
			int[] items;
		}

		string nullableWithArray()
		{
			import std.typecons : Nullable;

			auto doc = Nullable!Document(Document("x", [1, 2]));
			if (doc.isNull)
				return "";
			Document copy = doc.get;
			copy.name = "y";
			return copy.name;
		}
	}, sac);

	// member access without a call is treated as a potential modification
	// because the member can be a non-const method or property

	assertAnalyzerWarnings(q{
		struct Point
		{
			int x;
		}

		int readMember()
		{
			Point p;
			return p.x;
		}
	}, sac);

	// simple reads of value types are still reported

	assertAnalyzerWarnings(q{
		int simpleRead()
		{
			int i = 1; /+
			    ^ [warn]: Variable i is never modified and could have been declared const or immutable. +/
			return i;
		}
	}, sac);

	assertAnalyzerWarnings(q{
		int readIndex(int[] arr)
		{
			int i = 0; /+
			    ^ [warn]: Variable i is never modified and could have been declared const or immutable. +/
			return arr[i];
		}
	}, sac);

	assertAnalyzerWarnings(q{
		@("nolint(dscanner.suspicious.unmodified)")
		void foo(){
			int i = 1;
		}
	}, sac);
}

