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

// FIXME: Make it work with pure, nothrow

/**
 * Checks for duplicate attributes such as @property, @safe, @trusted, @system
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
		bool hasSafe = false;
		bool hasTrusted = false;
		bool hasSystem = false;

		// Check the attributes
		foreach (attribute; node.attributes)
		{
			// Just skip if missing child nodes
			if (!attribute
				|| !attribute.storageClass
				|| !attribute.storageClass.atAttribute
				|| attribute.storageClass.atAttribute.identifier is Token.init)
				continue;

			// Check for the attributes
			auto iden = attribute.storageClass.atAttribute.identifier;
			checkDuplicateAttribute(iden, "property", hasProperty);
			checkDuplicateAttribute(iden, "safe", hasSafe);
			checkDuplicateAttribute(iden, "trusted", hasTrusted);
			checkDuplicateAttribute(iden, "system", hasSystem);
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

			// Check for the attributes
			auto iden = memberFunctionAttribute.atAttribute.identifier;
			checkDuplicateAttribute(iden, "property", hasProperty);
			checkDuplicateAttribute(iden, "safe", hasSafe);
			checkDuplicateAttribute(iden, "trusted", hasTrusted);
			checkDuplicateAttribute(iden, "system", hasSystem);
		}
	}

	void checkDuplicateAttribute(const Token token, const string attributeName, ref bool hasAttribute)
	{
		// Just return if not an attribute
		if (token.type != tok!"identifier"
			|| token.text != attributeName)
			return;

		// Already has that attribute
		if (hasAttribute)
		{
			string message = "The attribute '%s' is duplicated.".format(token.text);
			addErrorMessage(token.line, token.column, message);
		}

		// Mark it as having that attribute
		hasAttribute = true;
	}
}

unittest
{
	assertAnalyzerWarnings(q{
		class ExampleAttributes
		{
			@property @safe bool xxx() // ok
			{
				return false;
			}

			// Duplicate before
			@property @property bool aaa() // [warn]: The attribute 'property' is duplicated.
			{
				return false;
			}

			// Duplicate after
			bool bbb() @safe @safe // [warn]: The attribute 'safe' is duplicated.
			{
				return false;
			}

			// Duplicate before and after
			@system bool ccc() @system // [warn]: The attribute 'system' is duplicated.
			{
				return false;
			}

			// Duplicate before and after
			@trusted bool ddd() @trusted // [warn]: The attribute 'trusted' is duplicated.
			{
				return false;
			}
		}
	}c, analysis.run.AnalyzerCheck.duplicate_attribute);

	stderr.writeln("Unittest for DuplicateAttributeCheck passed.");
}

