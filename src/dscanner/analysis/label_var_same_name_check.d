// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.label_var_same_name_check;

import dparse.ast;
import dparse.lexer;
import dsymbol.scope_ : Scope;
import dscanner.analysis.base;
import dscanner.analysis.helpers;

/**
 * Checks for labels and variables that have the same name.
 */
final class LabelVarNameCheck : BaseAnalyzer
{
	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	mixin ScopedVisit!Module;
	mixin ScopedVisit!BlockStatement;
	mixin ScopedVisit!StructBody;
	mixin ScopedVisit!CaseStatement;
	mixin ScopedVisit!ForStatement;
	mixin ScopedVisit!IfStatement;
	mixin ScopedVisit!TemplateDeclaration;

	mixin AggregateVisit!ClassDeclaration;
	mixin AggregateVisit!StructDeclaration;
	mixin AggregateVisit!InterfaceDeclaration;
	mixin AggregateVisit!UnionDeclaration;

	override void visit(const VariableDeclaration var)
	{
		foreach (dec; var.declarators)
			duplicateCheck(dec.name, false, conditionalDepth > 0);
	}

	override void visit(const LabeledStatement labeledStatement)
	{
		duplicateCheck(labeledStatement.identifier, true, conditionalDepth > 0);
		if (labeledStatement.declarationOrStatement !is null)
			labeledStatement.declarationOrStatement.accept(this);
	}

	override void visit(const ConditionalDeclaration condition)
	{
		if (condition.falseDeclarations.length > 0)
			++conditionalDepth;
		condition.accept(this);
		if (condition.falseDeclarations.length > 0)
			--conditionalDepth;
	}

	override void visit(const VersionCondition condition)
	{
		++conditionalDepth;
		condition.accept(this);
		--conditionalDepth;
	}

	alias visit = BaseAnalyzer.visit;

private:

	Thing[string][] stack;

	template AggregateVisit(NodeType)
	{
		override void visit(const NodeType n)
		{
			pushAggregateName(n.name);
			n.accept(this);
			popAggregateName();
		}
	}

	template ScopedVisit(NodeType)
	{
		override void visit(const NodeType n)
		{
			pushScope();
			n.accept(this);
			popScope();
		}
	}

	void duplicateCheck(const Token name, bool fromLabel, bool isConditional)
	{
		import std.conv : to;
		import std.range : retro;

		size_t i;
		foreach (s; retro(stack))
		{
			string fqn = parentAggregateText ~ name.text;
			const(Thing)* thing = fqn in s;
			if (thing is null)
				currentScope[fqn] = Thing(fqn, name.line, name.column, !fromLabel /+, isConditional+/ );
			else if (i != 0 || !isConditional)
			{
				immutable thisKind = fromLabel ? "Label" : "Variable";
				immutable otherKind = thing.isVar ? "variable" : "label";
				addErrorMessage(name.line, name.column, "dscanner.suspicious.label_var_same_name",
						thisKind ~ " \"" ~ fqn ~ "\" has the same name as a "
						~ otherKind ~ " defined on line " ~ to!string(thing.line) ~ ".");
			}
			++i;
		}
	}

	static struct Thing
	{
		string name;
		size_t line;
		size_t column;
		bool isVar;
		//bool isConditional;
	}

	ref currentScope() @property
	{
		return stack[$ - 1];
	}

	void pushScope()
	{
		stack.length++;
	}

	void popScope()
	{
		stack.length--;
	}

	int conditionalDepth;

	void pushAggregateName(Token name)
	{
		parentAggregates ~= name;
		updateAggregateText();
	}

	void popAggregateName()
	{
		parentAggregates.length -= 1;
		updateAggregateText();
	}

	void updateAggregateText()
	{
		import std.algorithm : map;
		import std.array : join;

		if (parentAggregates.length)
			parentAggregateText = parentAggregates.map!(a => a.text).join(".") ~ ".";
		else
			parentAggregateText = "";
	}

	Token[] parentAggregates;
	string parentAggregateText;
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.label_var_same_name_check = Check.enabled;
	assertAnalyzerWarnings(q{
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

}c, sac);
	stderr.writeln("Unittest for LabelVarNameCheck passed.");
}
