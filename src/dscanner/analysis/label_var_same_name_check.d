// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.label_var_same_name_check;

import dscanner.analysis.base;
import dmd.cond : Include;
import std.conv : to;

/**
 * Checks for labels and variables that have the same name.
 */
extern (C++) class LabelVarNameCheck(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"label_var_same_name_check";
	alias visit = BaseAnalyzerDmd.visit;

	mixin ScopedVisit!(AST.Module);
	mixin ScopedVisit!(AST.IfStatement);
	mixin ScopedVisit!(AST.WithStatement);
	mixin ScopedVisit!(AST.WhileStatement);
	mixin ScopedVisit!(AST.DoStatement);
	mixin ScopedVisit!(AST.ForStatement);
	mixin ScopedVisit!(AST.CaseStatement);
	mixin ScopedVisit!(AST.ForeachStatement);
	mixin ScopedVisit!(AST.ForeachRangeStatement);
	mixin ScopedVisit!(AST.ScopeStatement);
	mixin ScopedVisit!(AST.FuncAliasDeclaration);
	mixin ScopedVisit!(AST.CtorDeclaration);
	mixin ScopedVisit!(AST.DtorDeclaration);
	mixin ScopedVisit!(AST.InvariantDeclaration);

	mixin FunctionVisit!(AST.FuncDeclaration);
	mixin FunctionVisit!(AST.TemplateDeclaration);
	mixin FunctionVisit!(AST.FuncLiteralDeclaration);

	mixin AggregateVisit!(AST.ClassDeclaration);
	mixin AggregateVisit!(AST.StructDeclaration);
	mixin AggregateVisit!(AST.InterfaceDeclaration);
	mixin AggregateVisit!(AST.UnionDeclaration);

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName);
	}

	override void visit(AST.VarDeclaration vd)
	{
		import dmd.astenums : STC;

		if (!(vd.storage_class & STC.scope_) && !isInLocalFunction)
		{
			auto thing = Thing(to!string(vd.ident.toChars()), vd.loc.linnum, vd.loc.charnum);
			duplicateCheck(thing, false, conditionalDepth > 0);
		}

		super.visit(vd);
	}

	override void visit(AST.LabelStatement ls)
	{
		auto thing = Thing(to!string(ls.ident.toChars()), ls.loc.linnum, ls.loc.charnum);
		duplicateCheck(thing, true, conditionalDepth > 0);
		super.visit(ls);
	}

	override void visit(AST.ConditionalDeclaration conditionalDeclaration)
	{
		import dmd.root.array : peekSlice;

		conditionalDeclaration.condition.accept(this);

		if (conditionalDeclaration.condition.isVersionCondition())
			++conditionalDepth;

		if (conditionalDeclaration.elsedecl || conditionalDeclaration.condition.inc != Include.yes)
			pushScope();

		foreach (decl; conditionalDeclaration.decl.peekSlice())
			decl.accept(this);

		if (conditionalDeclaration.elsedecl || conditionalDeclaration.condition.inc != Include.yes)
			popScope();

		if (conditionalDeclaration.elsedecl)
			foreach (decl; conditionalDeclaration.elsedecl.peekSlice())
				decl.accept(this);

		if (conditionalDeclaration.condition.isVersionCondition())
			--conditionalDepth;
	}

	override void visit(AST.ConditionalStatement conditionalStatement)
	{
		conditionalStatement.condition.accept(this);

		if (conditionalStatement.condition.isVersionCondition)
			++conditionalDepth;

		if (conditionalStatement.ifbody)
		{
			if (conditionalStatement.elsebody)
				pushScope();

			conditionalStatement.ifbody.accept(this);

			if (conditionalStatement.elsebody)
				popScope();
		}

		if (conditionalStatement.elsebody)
		{
			if (conditionalStatement.condition.inc == Include.no)
				pushScope();

			conditionalStatement.elsebody.accept(this);

			if (conditionalStatement.condition.inc == Include.no)
				popScope();
		}

		if (conditionalStatement.condition.isVersionCondition)
			--conditionalDepth;
	}

	override void visit(AST.AnonDeclaration ad)
	{
		pushScope();
		pushAggregateName("", ad.loc.linnum, ad.loc.charnum);
		super.visit(ad);
		popScope();
		popAggregateName();
	}

	override void visit(AST.UnitTestDeclaration unitTestDecl)
	{
		if (skipTests)
			return;

		auto oldIsInFunction = isInFunction;
		auto oldIsInLocalFunction = isInLocalFunction;

		pushScope();

		if (isInFunction)
			isInLocalFunction = true;
		else
			isInFunction = true;

		super.visit(unitTestDecl);
		popScope();

		isInFunction = oldIsInFunction;
		isInLocalFunction = oldIsInLocalFunction;
	}

private:
	extern (D) Thing[string][] stack;
	int conditionalDepth;
	extern (D) Thing[] parentAggregates;
	extern (D) string parentAggregateText;
	bool isInFunction;
	bool isInLocalFunction;
	enum string KEY = "dscanner.suspicious.label_var_same_name";

	template FunctionVisit(NodeType)
	{
		override void visit(NodeType n)
		{
			auto oldIsInFunction = isInFunction;
			auto oldIsInLocalFunction = isInLocalFunction;

			pushScope();

			if (isInFunction)
				isInLocalFunction = true;
			else
				isInFunction = true;

			super.visit(n);
			popScope();

			isInFunction = oldIsInFunction;
			isInLocalFunction = oldIsInLocalFunction;
		}
	}

	template AggregateVisit(NodeType)
	{
		override void visit(NodeType n)
		{
			pushScope();
			pushAggregateName(to!string(n.ident.toString()), n.loc.linnum, n.loc.charnum);
			super.visit(n);
			popScope();
			popAggregateName();
		}
	}

	template ScopedVisit(NodeType)
	{
		override void visit(NodeType n)
		{
			pushScope();
			super.visit(n);
			popScope();
		}
	}

	extern (D) void duplicateCheck(const Thing id, bool fromLabel, bool isConditional)
	{
		import std.range : retro;

		size_t i;
		foreach (s; retro(stack))
		{
			string fqn = parentAggregateText ~ id.name;
			const(Thing)* thing = fqn in s;
			if (thing is null)
			{
				currentScope[fqn] = Thing(fqn, id.line, id.column, !fromLabel);
			}
			else if (i != 0 || !isConditional)
			{
				immutable thisKind = fromLabel ? "Label" : "Variable";
				immutable otherKind = thing.isVar ? "variable" : "label";
				auto msg = thisKind ~ " \"" ~ fqn ~ "\" has the same name as a "
					~ otherKind ~ " defined on line " ~ to!string(thing.line) ~ ".";
				addErrorMessage(id.line, id.column, KEY, msg);
			}
			++i;
		}
	}

	extern (D) static struct Thing
	{
		string name;
		size_t line;
		size_t column;
		bool isVar;
	}

	extern (D) ref currentScope() @property
	{
		return stack[$ - 1];
	}

	extern (D) void pushScope()
	{
		stack.length++;
	}

	extern (D) void popScope()
	{
		stack.length--;
	}

	extern (D) void pushAggregateName(string name, size_t line, size_t column)
	{
		parentAggregates ~= Thing(name, line, column);
		updateAggregateText();
	}

	extern (D) void popAggregateName()
	{
		parentAggregates.length -= 1;
		updateAggregateText();
	}

	extern (D) void updateAggregateText()
	{
		import std.algorithm : map;
		import std.array : join;

		if (parentAggregates.length)
			parentAggregateText = parentAggregates.map!(a => a.name).join(".") ~ ".";
		else
			parentAggregateText = "";
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.label_var_same_name_check = Check.enabled;

	assertAnalyzerWarningsDMD(q{
unittest
{
blah:
	int blah; // [warn]: Variable "blah" has the same name as a label defined on line 4.
}
int blah;
unittest
{
	static if (stuff)
		int a;
	int a; // [warn]: Variable "a" has the same name as a variable defined on line 11.
}

unittest
{
	static if (stuff)
		int a = 10;
	else
		int a = 20;
}

unittest
{
	static if (stuff)
		int a = 10;
	else
		int a = 20;
	int a; // [warn]: Variable "a" has the same name as a variable defined on line 28.
}
template T(stuff)
{
	int b;
}

void main(string[] args)
{
	for (int a = 0; a < 10; a++)
		things(a);

	for (int a = 0; a < 10; a++)
		things(a);
	int b;
}

unittest
{
	version (Windows)
		int c = 10;
	else
		int c = 20;
	int c; // [warn]: Variable "c" has the same name as a variable defined on line 51.
}

unittest
{
	version(LittleEndian) { enum string NAME = "UTF-16LE"; }
	else version(BigEndian)    { enum string NAME = "UTF-16BE"; }
}

unittest
{
	int a;
	struct A {int a;}
}

unittest
{
	int a;
	struct A { struct A {int a;}}
}

unittest
{
	int a;
	class A { class A {int a;}}
}

unittest
{
	int a;
	interface A { interface A {int a;}}
}

unittest
{
	interface A
	{
		int a;
		int a; // [warn]: Variable "A.a" has the same name as a variable defined on line 89.
	}
}

unittest
{
	int aa;
	struct a { int a; }
}

unittest
{
	switch (1) {
	case 1:
		int x, c1;
		break;
	case 2:
		int x, c2;
		break;
	default:
		int x, def;
		break;
	}
}
}c, sac);
	stderr.writeln("Unittest for LabelVarNameCheck passed.");
}
