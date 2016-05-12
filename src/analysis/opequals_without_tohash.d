// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.opequals_without_tohash;

import std.stdio;
import dparse.ast;
import dparse.lexer;
import analysis.base;
import analysis.helpers;
import dsymbol.scope_ : Scope;

/**
 * Checks for when a class/struct has the method opEquals without toHash, or
 * toHash without opEquals.
 */
class OpEqualsWithoutToHashCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const ClassDeclaration node)
	{
		actualCheck(node.name, node.structBody);
		node.accept(this);
	}

	override void visit(const StructDeclaration node)
	{
		actualCheck(node.name, node.structBody);
		node.accept(this);
	}

	private void actualCheck(const Token name, const StructBody structBody)
	{
		bool hasOpEquals = false;
		bool hasToHash = false;
		bool hasOpCmp = false;

		// Just return if missing children
		if (!structBody || !structBody.declarations || name is Token.init)
			return;

		// Check all the function declarations
		foreach (declaration; structBody.declarations)
		{
			// Skip if not a function declaration
			if (!declaration || !declaration.functionDeclaration)
				continue;

			// Check if opEquals or toHash
			immutable string methodName = declaration.functionDeclaration.name.text;
			if (methodName == "opEquals")
				hasOpEquals = true;
			else if (methodName == "toHash")
				hasToHash = true;
			else if (methodName == "opCmp")
				hasOpCmp = true;
		}

		// Warn if has opEquals, but not toHash
		if (hasOpEquals && !hasToHash)
		{
			string message = "'" ~ name.text ~ "' has method 'opEquals', but not 'toHash'.";
			addErrorMessage(name.line, name.column, KEY, message);
		}
		// Warn if has toHash, but not opEquals
		else if (!hasOpEquals && hasToHash)
		{
			string message = "'" ~ name.text ~ "' has method 'toHash', but not 'opEquals'.";
			addErrorMessage(name.line, name.column, KEY, message);
		}

		if (hasOpCmp && !hasOpEquals)
		{
			addErrorMessage(name.line, name.column, KEY,
					"'" ~ name.text ~ "' has method 'opCmp', but not 'opEquals'.");
		}
	}

	enum string KEY = "dscanner.suspicious.incomplete_operator_overloading";
}

unittest
{
	import analysis.config : StaticAnalysisConfig, Check;

	StaticAnalysisConfig sac;
	sac.opequals_tohash_check = Check.enabled;
	assertAnalyzerWarnings(q{
		// Success because it has opEquals and toHash
		class Chimp
		{
			const bool opEquals(Object a, Object b)
			{
				return true;
			}

			const override hash_t toHash()
			{
				return 0;
			}
		}

		// Fail on class opEquals
		class Rabbit // [warn]: 'Rabbit' has method 'opEquals', but not 'toHash'.
		{
			const bool opEquals(Object a, Object b)
			{
				return true;
			}
		}

		// Fail on class toHash
		class Kangaroo // [warn]: 'Kangaroo' has method 'toHash', but not 'opEquals'.
		{
			override const hash_t toHash()
			{
				return 0;
			}
		}

		// Fail on struct opEquals
		struct Tarantula // [warn]: 'Tarantula' has method 'opEquals', but not 'toHash'.
		{
			const bool opEquals(Object a, Object b)
			{
				return true;
			}
		}

		// Fail on struct toHash
		struct Puma // [warn]: 'Puma' has method 'toHash', but not 'opEquals'.
		{
			const nothrow @safe hash_t toHash()
			{
				return 0;
			}
		}
	}c, sac);

	stderr.writeln("Unittest for OpEqualsWithoutToHashCheck passed.");
}
