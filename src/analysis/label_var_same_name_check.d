// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.label_var_same_name_check;

import std.d.ast;
import std.d.lexer;

import analysis.base;
import analysis.helpers;

/**
 * Checks for labels and variables that have the same name.
 */
class LabelVarNameCheck : BaseAnalyzer
{
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

	override void visit(const BlockStatement block)
	{
		pushScope();
		block.accept(this);
		popScope();
	}

	override void visit(const StructBody structBody)
	{
		pushScope();
		structBody.accept(this);
		popScope();
	}

	override void visit(const VariableDeclaration var)
	{
		foreach (dec; var.declarators)
			duplicateCheck(dec.name, false);
	}

	override void visit(const LabeledStatement labeledStatement)
	{
		duplicateCheck(labeledStatement.identifier, true);
		if (labeledStatement.declarationOrStatement !is null)
			labeledStatement.declarationOrStatement.accept(this);
	}

	alias visit = BaseAnalyzer.visit;

private:

	Thing[string][] stack;

	void duplicateCheck(const Token name, bool fromLabel)
	{
		import std.conv : to;
		const(Thing)* thing = name.text in currentScope;
		if (thing is null)
			currentScope[name.text] = Thing(name.text, name.line, name.column, false);
		else
		{
			immutable thisKind = fromLabel ? "Label" : "Variable";
			immutable otherKind = thing.isVar ? "variable" : "label";
			addErrorMessage(name.line, name.column, "dscanner.suspicious.label_var_same_name",
				thisKind ~ " \"" ~ name.text ~ "\" has the same name as a "
				~ otherKind ~ " defined on line " ~ to!string(thing.line) ~ ".");
		}
	}

	static struct Thing
	{
		string name;
		size_t line;
		size_t column;
		bool isVar;
	}

	ref currentScope() @property
	{
		return stack[$-1];
	}

	void pushScope()
	{
		stack.length++;
	}

	void popScope()
	{
		stack.length--;
	}
}

unittest
{
	import analysis.config : StaticAnalysisConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac;
	sac.label_var_same_name_check = true;
	assertAnalyzerWarnings(q{
unittest
{
blah:
	int blah; // [warn]: Variable "blah" has the same name as a label defined on line 4.
}
int blah;
	}c, sac);
	stderr.writeln("Unittest for LabelVarNameCheck passed.");
}
