// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.opequals_without_tohash;

import std.stdio;
import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import dscanner.analysis.helpers;
import dsymbol.scope_ : Scope;

/**
 * Checks for when a class/struct has the method opEquals without toHash, or
 * toHash without opEquals.
 */
final class OpEqualsWithoutToHashCheck : BaseAnalyzer
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
		bool hasOpEquals;
		bool hasToHash;
		bool hasOpCmp;

		// Just return if missing children
		if (!structBody || !structBody.declarations || name is Token.init)
			return;

		// Check all the function declarations
		foreach (declaration; structBody.declarations)
		{
			// Skip if not a function declaration
			if (!declaration || !declaration.functionDeclaration)
				continue;

			bool containsDisable(A)(const A[] attribs)
			{
				import std.algorithm.searching : canFind;
				return attribs.canFind!(a => a.atAttribute !is null &&
					a.atAttribute.identifier.text == "disable");
			}

			const isDeclationDisabled = containsDisable(declaration.attributes) ||
				containsDisable(declaration.functionDeclaration.memberFunctionAttributes);

			if (isDeclationDisabled)
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
	}

	enum string KEY = "dscanner.suspicious.incomplete_operator_overloading";
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
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

		// AA would use default equal and default toHash
		struct Bee
		{
			int opCmp(Bee) const
			{
				return true;
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

		// issue #659, do not warn if one miss and the other is not callable
		struct Fox {const nothrow @safe hash_t toHash() @disable;}
		struct Bat {@disable const nothrow @safe hash_t toHash();}
		struct Rat {const bool opEquals(Object a, Object b) @disable;}
		struct Cat {@disable const bool opEquals(Object a, Object b);}

	}c, sac);

	stderr.writeln("Unittest for OpEqualsWithoutToHashCheck passed.");
}
