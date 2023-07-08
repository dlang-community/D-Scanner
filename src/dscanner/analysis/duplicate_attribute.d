// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.duplicate_attribute;

import std.stdio;
import std.string;
import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import dscanner.analysis.helpers;
import dsymbol.scope_ : Scope;

/**
 * Checks for duplicate attributes such as @property, @safe,
 * @trusted, @system, pure, and nothrow
 */
final class DuplicateAttributeCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	mixin AnalyzerInfo!"duplicate_attribute";

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const Declaration node)
	{
		checkAttributes(node);
		node.accept(this);
	}

	void checkAttributes(const Declaration node)
	{
		bool hasProperty;
		bool hasSafe;
		bool hasTrusted;
		bool hasSystem;
		bool hasPure;
		bool hasNoThrow;

		// Check the attributes
		foreach (attribute; node.attributes)
		{
			const(Token)[] tokens;
			string attributeName = getAttributeName(attribute, tokens);
			if (!attributeName || !tokens.length)
				return;

			// Check for the attributes
			checkDuplicateAttribute(attributeName, "property", tokens, hasProperty);
			checkDuplicateAttribute(attributeName, "safe", tokens, hasSafe);
			checkDuplicateAttribute(attributeName, "trusted", tokens, hasTrusted);
			checkDuplicateAttribute(attributeName, "system", tokens, hasSystem);
			checkDuplicateAttribute(attributeName, "pure", tokens, hasPure);
			checkDuplicateAttribute(attributeName, "nothrow", tokens, hasNoThrow);
		}

		// Just return if missing function nodes
		if (!node.functionDeclaration || !node.functionDeclaration.memberFunctionAttributes)
			return;

		// Check the functions
		foreach (memberFunctionAttribute; node.functionDeclaration.memberFunctionAttributes)
		{
			const(Token)[] tokens;
			string attributeName = getAttributeName(memberFunctionAttribute, tokens);
			if (!attributeName || !tokens.length)
				return;

			// Check for the attributes
			checkDuplicateAttribute(attributeName, "property", tokens, hasProperty);
			checkDuplicateAttribute(attributeName, "safe", tokens, hasSafe);
			checkDuplicateAttribute(attributeName, "trusted", tokens, hasTrusted);
			checkDuplicateAttribute(attributeName, "system", tokens, hasSystem);
			checkDuplicateAttribute(attributeName, "pure", tokens, hasPure);
			checkDuplicateAttribute(attributeName, "nothrow", tokens, hasNoThrow);
		}
	}

	void checkDuplicateAttribute(const string attributeName,
			const string attributeDesired, const(Token)[] tokens, ref bool hasAttribute)
	{
		// Just return if not an attribute
		if (attributeName != attributeDesired)
			return;

		// Already has that attribute
		if (hasAttribute)
		{
			string message = "Attribute '%s' is duplicated.".format(attributeName);
			addErrorMessage(tokens, "dscanner.unnecessary.duplicate_attribute", message,
				[AutoFix.replacement(tokens, "", "Remove second attribute " ~ attributeName)]);
		}

		// Mark it as having that attribute
		hasAttribute = true;
	}

	string getAttributeName(const Attribute attribute, ref const(Token)[] outTokens)
	{
		// Get the name from the attribute identifier
		if (attribute && attribute.atAttribute && attribute.atAttribute.identifier !is Token.init
				&& attribute.atAttribute.identifier.text
				&& attribute.atAttribute.identifier.text.length)
		{
			auto token = attribute.atAttribute.identifier;
			outTokens = attribute.atAttribute.tokens;
			return token.text;
		}

		// Get the attribute from the storage class token
		if (attribute && attribute.attribute.type != tok!"")
		{
			outTokens = attribute.tokens;
			return attribute.attribute.type.str;
		}

		return null;
	}

	string getAttributeName(const MemberFunctionAttribute memberFunctionAttribute,
			ref const(Token)[] outTokens)
	{
		// Get the name from the tokenType
		if (memberFunctionAttribute && memberFunctionAttribute.tokenType !is IdType.init
				&& memberFunctionAttribute.tokenType.str
				&& memberFunctionAttribute.tokenType.str.length)
		{
			outTokens = memberFunctionAttribute.tokens;
			return memberFunctionAttribute.tokenType.str;
		}

		// Get the name from the attribute identifier
		if (memberFunctionAttribute && memberFunctionAttribute.atAttribute
				&& memberFunctionAttribute.atAttribute.identifier !is Token.init
				&& memberFunctionAttribute.atAttribute.identifier.type == tok!"identifier"
				&& memberFunctionAttribute.atAttribute.identifier.text
				&& memberFunctionAttribute.atAttribute.identifier.text.length)
		{
			auto iden = memberFunctionAttribute.atAttribute.identifier;
			outTokens = memberFunctionAttribute.atAttribute.tokens;
			return iden.text;
		}

		return null;
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.duplicate_attribute = Check.enabled;
	assertAnalyzerWarnings(q{
		class ExampleAttributes
		{
			@property @safe bool xxx() // ok
			{
				return false;
			}

			// Duplicate before
			@property @property bool aaa() /+
			          ^^^^^^^^^ [warn]: Attribute 'property' is duplicated. +/
			{
				return false;
			}

			// Duplicate after
			bool bbb() @safe @safe /+
			                 ^^^^^ [warn]: Attribute 'safe' is duplicated. +/
			{
				return false;
			}

			// Duplicate before and after
			@system bool ccc() @system /+
			                   ^^^^^^^ [warn]: Attribute 'system' is duplicated. +/
			{
				return false;
			}

			// Duplicate before and after
			@trusted bool ddd() @trusted /+
			                    ^^^^^^^^ [warn]: Attribute 'trusted' is duplicated. +/
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

			pure pure bool bbb() /+
			     ^^^^ [warn]: Attribute 'pure' is duplicated. +/
			{
				return false;
			}

			bool ccc() pure pure /+
			                ^^^^ [warn]: Attribute 'pure' is duplicated. +/
			{
				return false;
			}

			nothrow nothrow bool ddd() /+
			        ^^^^^^^ [warn]: Attribute 'nothrow' is duplicated. +/
			{
				return false;
			}

			bool eee() nothrow nothrow /+
			                   ^^^^^^^ [warn]: Attribute 'nothrow' is duplicated. +/
			{
				return false;
			}
		}
	}c, sac);


	assertAutoFix(q{
		class ExampleAttributes
		{
			@property @property bool aaa() {} // fix
			bool bbb() @safe @safe {} // fix
			@system bool ccc() @system {} // fix
			@trusted bool ddd() @trusted {} // fix
		}

		class ExamplePureNoThrow
		{
			pure pure bool bbb() {} // fix
			bool ccc() pure pure {} // fix
			nothrow nothrow bool ddd() {} // fix
			bool eee() nothrow nothrow {} // fix
		}
	}c, q{
		class ExampleAttributes
		{
			@property bool aaa() {} // fix
			bool bbb() @safe {} // fix
			@system bool ccc() {} // fix
			@trusted bool ddd() {} // fix
		}

		class ExamplePureNoThrow
		{
			pure bool bbb() {} // fix
			bool ccc() pure {} // fix
			nothrow bool ddd() {} // fix
			bool eee() nothrow {} // fix
		}
	}c, sac);

	stderr.writeln("Unittest for DuplicateAttributeCheck passed.");
}
