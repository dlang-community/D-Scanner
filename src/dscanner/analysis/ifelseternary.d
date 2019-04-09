//          Copyright Robert burner Schadek 2019.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.ifelseternary;

import std.stdio;
import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import dscanner.analysis.helpers;
import dsymbol.scope_ : Scope;

/**
 * Checks if IfElse Statement can be rewritten as a ternary expression
 */
final class IfElseTernary : BaseAnalyzer
{
	import std.array : back, empty, popBack;

	private enum IfOrElse
	{
		If,
		Else
	}

	alias visit = BaseAnalyzer.visit;

	private IfOrElse[] inIfElse;
	private bool[] isAssign;
	private string[] thenIdentifier;
	private string[] elseIdentifier;
	private int unaryCnt;

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const IfStatement ifStatement)
	{
		import std.format : format;

		// we only need to look at the cases where we have a else
		if (ifStatement.elseStatement is null)
		{
			return;
		}

		this.inIfElse ~= IfOrElse.If;
		ifStatement.thenStatement.accept(this);
		this.inIfElse.popBack();

		this.inIfElse ~= IfOrElse.Else;
		ifStatement.elseStatement.accept(this);
		this.inIfElse.popBack();

		const shouldCheck = !this.thenIdentifier.empty
				&& this.thenIdentifier.length == this.elseIdentifier.length;
		const areEqual = shouldCheck
			? this.thenIdentifier.back == this.elseIdentifier.back
			: false;

		if (areEqual)
		{
			addErrorMessage(ifStatement.line, ifStatement.column,
					"dscanner.bugs.if_else_ternary",
					format("If Else expression of '%s' could be rewritten as ternary statement.",
						this.thenIdentifier.back));
		}

		while (this.thenIdentifier.length > this.elseIdentifier.length)
		{
			this.thenIdentifier.popBack();
		}

		while (this.elseIdentifier.length > this.thenIdentifier.length)
		{
			this.elseIdentifier.popBack();
		}
	}

	override void visit(const AssignExpression assignExpression)
	{
		if (assignExpression.expression is null || str(assignExpression.operator) != "=")
		{
			return;
		}

		this.unaryCnt = 0;

		this.isAssign ~= true;
		assignExpression.accept(this);
		this.isAssign.popBack();
	}

	override void visit(const UnaryExpression u)
	{
		// TODO there must be a better way to get the left side of an AssignExpression
		if (this.unaryCnt > 1)
		{
			return;
		}

		++this.unaryCnt;

		u.accept(this);
	}

	override void visit(const IdentifierOrTemplateInstance ioti)
	{
		if (this.inIfElse.empty || this.isAssign.empty || !this.isAssign.back)
		{
			return;
		}

		if (this.inIfElse.back == IfOrElse.If)
		{
			this.thenIdentifier ~= ioti.identifier.text;
		}
		else
		{
			this.elseIdentifier ~= ioti.identifier.text;
		}
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.if_else_ternary = Check.enabled;

	assertAnalyzerWarnings(q{
		void testSizeT()
		{
			string person = "unknown";
			if (person == "unknown") // [warn]: If Else expression of 'person' could be rewritten as ternary statement.
				person = "foo";
			else
				person = "bar";
		}
	}c, sac);

	assertAnalyzerWarnings(q{
		void testSizeT()
		{
			string person = "unknown";
			string hello;
			if (person == "unknown")
				hello = "foo";
			else
				person = "bar";
		}
	}c, sac);

	assertAnalyzerWarnings(q{
		void testSizeT()
		{
			string person = "unknown";
			string hello;
			if (person == "unknown") // [warn]: If Else expression of 'person' could be rewritten as ternary statement.
			{
				if (hello.empty)
				{
					hello = "foo";
				}
				person = "text";
			}
			else
				person = "bar";
		}
	}c, sac);

	assertAnalyzerWarnings(q{
		void testSizeT()
		{
			string person = "unknown";
			string hello;
			if (person == "unknown") // [warn]: If Else expression of 'person' could be rewritten as ternary statement.
			{
				if (hello.empty) // [warn]: If Else expression of 'hello' could be rewritten as ternary statement.

				{
					hello = "empty";
				}
				else
				{
					hello = "not empty";
				}
				person = "text";
			}
			else
				person = "bar";
		}
	}c, sac);

	assertAnalyzerWarnings(q{
		void fun(string[] args)
		{
		    int a;
		    if (a == 100) // [warn]: If Else expression of 'a' could be rewritten as ternary statement.
		        a = to!int(args[1]);
		    else
		        a = 100;
		}
	}c, sac);

	stderr.writeln("Unittest for IfElseTenaryCheck passed.");
}
