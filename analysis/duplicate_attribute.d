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


/**
 * Checks for duplicate attributes such as @property, @safe, 
 * @trusted, @system, pure, and nothrow
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
		bool hasPure = false;
		bool hasNoThrow = false;

		// Check the attributes
		foreach (attribute; node.attributes)
		{
			size_t line, column;
			string attributeName = getAttributeName(attribute, line, column);
			if (!attributeName || line==0 || column==0)
				return;

			// Check for the attributes
			checkDuplicateAttribute(attributeName, "property", line, column, hasProperty);
			checkDuplicateAttribute(attributeName, "safe", line, column, hasSafe);
			checkDuplicateAttribute(attributeName, "trusted", line, column, hasTrusted);
			checkDuplicateAttribute(attributeName, "system", line, column, hasSystem);
			checkDuplicateAttribute(attributeName, "pure", line, column, hasPure);
			checkDuplicateAttribute(attributeName, "nothrow", line, column, hasNoThrow);
		}

		// Just return if missing function nodes
		if (!node.functionDeclaration
			|| !node.functionDeclaration.memberFunctionAttributes)
			return;

		// Check the functions
		foreach (memberFunctionAttribute; node.functionDeclaration.memberFunctionAttributes)
		{
			size_t line, column;
			string attributeName = getAttributeName(memberFunctionAttribute, line, column);
			if (!attributeName || line==0 || column==0)
				return;

			// Check for the attributes
			checkDuplicateAttribute(attributeName, "property", line, column, hasProperty);
			checkDuplicateAttribute(attributeName, "safe", line, column, hasSafe);
			checkDuplicateAttribute(attributeName, "trusted", line, column, hasTrusted);
			checkDuplicateAttribute(attributeName, "system", line, column, hasSystem);
			checkDuplicateAttribute(attributeName, "pure", line, column, hasPure);
			checkDuplicateAttribute(attributeName, "nothrow", line, column, hasNoThrow);
		}
	}

	void checkDuplicateAttribute(const string attributeName, const string attributeDesired, size_t line, size_t column, ref bool hasAttribute)
	{
		// Just return if not an attribute
		if (attributeName != attributeDesired)
			return;

		// Already has that attribute
		if (hasAttribute)
		{
			string message = "The attribute '%s' is duplicated.".format(attributeName);
			addErrorMessage(line, column, message);
		}

		// Mark it as having that attribute
		hasAttribute = true;
	}

	string getAttributeName(const Attribute attribute, ref size_t line, ref size_t column)
	{
		// Get the name from the attribute identifier
		if (attribute
			&& attribute.storageClass
			&& attribute.storageClass.atAttribute
			&& attribute.storageClass.atAttribute.identifier !is Token.init
			&& attribute.storageClass.atAttribute.identifier.text
			&& attribute.storageClass.atAttribute.identifier.text.length)
		{
			auto token = attribute.storageClass.atAttribute.identifier;
			line = token.line;
			column = token.column;
			return token.text;
		}

		// Get the attribute from the storage class token
		if (attribute
			&& attribute.storageClass
			&& attribute.storageClass.token !is Token.init)
		{
			auto token = attribute.storageClass.token;
			line = token.line;
			column = token.column;
			return token.type.str;
		}

		return null;
	}

	string getAttributeName(const MemberFunctionAttribute memberFunctionAttribute, ref size_t line, ref size_t column)
	{
		// Get the name from the tokenType
		if (memberFunctionAttribute
			&& memberFunctionAttribute.tokenType !is IdType.init
			&& memberFunctionAttribute.tokenType.str
			&& memberFunctionAttribute.tokenType.str.length)
		{
			// FIXME: How do we get the line/column number?
			return memberFunctionAttribute.tokenType.str;
		}

		// Get the name from the attribute identifier
		if (memberFunctionAttribute
			&& memberFunctionAttribute.atAttribute
			&& memberFunctionAttribute.atAttribute.identifier !is Token.init
			&& memberFunctionAttribute.atAttribute.identifier.type == tok!"identifier"
			&& memberFunctionAttribute.atAttribute.identifier.text
			&& memberFunctionAttribute.atAttribute.identifier.text.length)
		{
			auto iden = memberFunctionAttribute.atAttribute.identifier;
			line = iden.line;
			column = iden.column;
			return iden.text;
		}

		return null;
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

		class ExamplePureNoThrow
		{
			pure nothrow bool aaa() // ok
			{
				return false;
			}

			pure pure bool bbb() // [warn]: The attribute 'pure' is duplicated.
			{
				return false;
			}

			// FIXME: There is no way to get the line/column number of the attribute like this
			bool ccc() pure pure // FIXME: [warn]: The attribute 'pure' is duplicated.
			{
				return false;
			}

			nothrow nothrow bool ddd() // [warn]: The attribute 'nothrow' is duplicated.
			{
				return false;
			}

			// FIXME: There is no way to get the line/column number of the attribute like this
			bool eee() nothrow nothrow // FIXME: [warn]: The attribute 'nothrow' is duplicated.
			{
				return false;
			}
		}
	}c, analysis.run.AnalyzerCheck.duplicate_attribute);

	stderr.writeln("Unittest for DuplicateAttributeCheck passed.");
}

