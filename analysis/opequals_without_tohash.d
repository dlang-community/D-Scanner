// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.opequals_without_tohash;

import std.stdio;
import std.d.ast;
import std.d.lexer;
import analysis.base;
import analysis.helpers;

/**
 * Checks for when a class/struct has the method opEquals without toHash, or
 * toHash without opEquals.
 */
class OpEqualsWithoutToHashCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
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

		// Just return if missing children
		if (!structBody
			|| !structBody.declarations
			|| name is Token.init)
			return;

		// Check all the function declarations
		foreach (declaration; structBody.declarations)
		{
			// Skip if not a function declaration
			if (!declaration
				|| !declaration.functionDeclaration)
				continue;

			// Check if opEquals or toHash
			string methodName = declaration.functionDeclaration.name.text;
			if (methodName == "opEquals")
				hasOpEquals = true;
			else if (methodName == "toHash")
				hasToHash = true;
		}

		// Warn if has opEquals, but not toHash
		if (hasOpEquals && !hasToHash)
		{
			string message = "Has method opEquals, but not toHash";
			addErrorMessage(name.line, name.column, message);
		}
		// Warn if has toHash, but not opEquals
		else if (!hasOpEquals && hasToHash)
		{
			string message = "Has method toHash, but not opEquals";
			addErrorMessage(name.line, name.column, message);
		}
	}
}

unittest
{
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
		class Rabbit // [warn]: Has method opEquals, but not toHash
		{
			const bool opEquals(Object a, Object b)
			{
				return true;
			}
		}

		// Fail on class toHash
		class Kangaroo // [warn]: Has method toHash, but not opEquals
		{
			override const hash_t toHash()
			{
				return 0;
			}
		}

		// Fail on struct opEquals
		struct Tarantula // [warn]: Has method opEquals, but not toHash
		{
			const bool opEquals(Object a, Object b)
			{
				return true;
			}
		}

		// Fail on struct toHash
		struct Puma // [warn]: Has method toHash, but not opEquals
		{
			const nothrow @safe hash_t toHash()
			{
				return 0;
			}
		}
	}c, analysis.run.AnalyzerCheck.opequals_tohash_check);

	stderr.writeln("Unittest for OpEqualsWithoutToHashCheck passed.");
}

