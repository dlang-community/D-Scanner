//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module analysis.immutable_finder;

import std.container;
import std.d.ast;
import std.d.lexer;
import analysis.base;

/**
 * Checks for variables that could have been declared immutable
 */
class ImmutableFinder:BaseAnalyzer
{
    alias visit = BaseAnalyzer.visit;

	///
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
		auto oldBlockStatementDepth = blockStatementDepth;
		blockStatementDepth = 0;
		structBody.accept(this);
		blockStatementDepth = oldBlockStatementDepth;
		popScope();
	}

	override void visit(const VariableDeclaration dec)
	{
		if (dec.autoDeclaration is null && blockStatementDepth > 0 && isImmutable <= 0 && !canFindImmutable(dec))
		{
			foreach (d; dec.declarators)
			{
				tree[$ - 1].insert(new VariableInfo(d.name.text, d.name.line,
					d.name.column));
			}
		}
		dec.accept(this);
	}

	override void visit(const AutoDeclaration autoDeclaration)
	{
		if (blockStatementDepth > 0 && isImmutable <= 0
			&& (autoDeclaration.storageClass !is null
				&& autoDeclaration.storageClass.token != tok!"enum"
				&& autoDeclaration.storageClass.token != tok!"immutable"))
		{
			foreach (id; autoDeclaration.identifiers)
				tree[$ - 1].insert(new VariableInfo(id.text, id.line,
					id.column));
		}
		autoDeclaration.accept(this);
	}

	override void visit(const AssignExpression assignExpression)
	{
		if (assignExpression.operator != tok!"")
		{
			interest++;
			assignExpression.ternaryExpression.accept(this);
			interest--;
			assignExpression.ternaryExpression.accept(this);
		}
		else
			assignExpression.accept(this);
	}

	override void visit(const Declaration dec)
	{
		if (canFindImmutable(dec))
		{
			isImmutable++;
			dec.accept(this);
			isImmutable--;
		}
		else
			dec.accept(this);
	}

	override void visit(const IdentifierOrTemplateInstance ioti)
	{
//		import std.stdio : stderr;
//		stderr.writeln(ioti.identifier.text, " ", ioti.identifier.line);
		if (ioti.identifier != tok!"" && interest > 0)
		{
			variableMightBeModified(ioti.identifier.text);
		}
		ioti.accept(this);
	}

	mixin PartsMightModify!IndexExpression;
	mixin PartsMightModify!SliceExpression;
	mixin PartsMightModify!FunctionCallExpression;
	mixin PartsMightModify!IdentifierOrTemplateChain;
	mixin PartsMightModify!ReturnStatement;

	override void visit(const UnaryExpression unary)
	{
		if (unary.prefix == tok!"++" || unary.prefix == tok!"--"
			|| unary.suffix == tok!"++" || unary.suffix == tok!"--")
		{
			interest++;
			unary.accept(this);
			interest--;
		}
		else
			unary.accept(this);
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
//		import std.stdio : stderr;
//		stderr.writeln("Marking ", name, " as possibly modified");
		size_t index = tree.length - 1;
		auto vi = VariableInfo(name);
		while (true)
		{
			if (tree[index].removeKey(&vi) != 0 || index == 0)
				break;
			index--;
		}
	}

	bool canFindImmutable(const Declaration dec)
	{
		import std.algorithm : canFind, map;
		return dec.attributes.map!(a => a.attribute).canFind(cast(IdType) tok!"immutable");
	}

	bool canFindImmutable(const VariableDeclaration dec)
	{
		import std.algorithm : canFind;
		foreach (attr; dec.attributes)
		{
			if (attr.attribute.type == tok!"immutable")
				return true;
		}
		if (dec.type !is null)
		{
			if (dec.type.typeConstructors.canFind(cast(IdType) tok!"immutable"))
				return true;
		}
		return false;
	}

	static struct VariableInfo
	{
		string name;
		size_t line;
		size_t column;
	}

	void popScope()
	{
		foreach (vi; tree[$ - 1])
		{
			immutable string errorMessage = "Variable " ~ vi.name
				~ " could have been declared immutable";
			addErrorMessage(vi.line, vi.column, "dscanner.suspicious.could_be_immutable",
				errorMessage);
		}
		tree = tree[0 .. $ - 1];
	}

	void pushScope()
	{
		tree ~= new RedBlackTree!(VariableInfo*, "a.name < b.name");
	}

	int blockStatementDepth;

	int interest;

	int isImmutable;

	RedBlackTree!(VariableInfo*, "a.name < b.name")[] tree;
}

