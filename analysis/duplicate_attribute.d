// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.duplicate_attribute;

import std.stdio;
import std.string;
import std.d.ast;
import std.d.lexer;
import analysis.base;
import analysis.helpers;

// FIXME: Make it work with @safe, @trusted, @system
// FIXME: Make it work with pure, nothrow

/**
 * Checks for duplicate attributes such as @property
 */
class DuplicateAttributeCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const Declaration node)
	{
		checkAttributes(node);
		node.accept(this);
	}

	void checkAttributes(const Declaration node)
	{
		bool hasProperty = false;

		// Check the attributes
		foreach (attribute; node.attributes)
		{
			// Just skip if missing child nodes
			if (!attribute
				|| !attribute.storageClass
				|| !attribute.storageClass.atAttribute
				|| attribute.storageClass.atAttribute.identifier is Token.init)
				continue;

			// Is a property
			auto iden = attribute.storageClass.atAttribute.identifier;
			checkProperty(iden, hasProperty);
		}

		// Just return if missing function nodes
		if (!node.functionDeclaration
			|| !node.functionDeclaration.memberFunctionAttributes)
			return;

		// Check the functions
		foreach (memberFunctionAttribute; node.functionDeclaration.memberFunctionAttributes)
		{
			// Just skip if missing child nodes
			if (!memberFunctionAttribute
				|| !memberFunctionAttribute.atAttribute
				|| memberFunctionAttribute.atAttribute.identifier is Token.init)
				continue;

			// Is a property
			auto iden = memberFunctionAttribute.atAttribute.identifier;
			checkProperty(iden, hasProperty);
		}
	}

	void checkProperty(const Token iden, ref bool hasProperty)
	{
		// Just return if not a property
		if (!isProperty(iden))
			return;

		// Already has a property
		if (hasProperty)
		{
			string message = "The attribute '%s' is duplicated.".format(iden.text);
			addErrorMessage(iden.line, iden.column, message);
		}

		// Mark it as a property
		hasProperty = true;
	}
}

bool isProperty(const Token token) pure
{
	return token.type == tok!"identifier" && token.text == "property";
}

unittest
{
	assertAnalyzerWarnings(q{
		class CAllocator
		{
			@property bool xxx() // ok
			{
				return false;
			}

			@property @property bool aaa() // [warn]: The attribute 'property' is duplicated.
			{
				return false;
			}

			bool bbb() @property @property // [warn]: The attribute 'property' is duplicated.
			{
				return false;
			}

			@property bool ccc() @property // [warn]: The attribute 'property' is duplicated.
			{
				return false;
			}
		}
	}c, analysis.run.AnalyzerCheck.duplicate_attribute);

	stderr.writeln("Unittest for DuplicateAttributeCheck passed.");
}

