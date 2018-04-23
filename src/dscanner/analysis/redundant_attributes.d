// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.redundant_attributes;

import dparse.ast;
import dparse.lexer;
import dsymbol.scope_ : Scope;
import dscanner.analysis.base;
import dscanner.analysis.helpers;

import std.algorithm;
import std.conv : to, text;
import std.range : empty, front, walkLength;

/**
 * Checks for redundant attributes. At the moment only visibility attributes.
 */
final class RedundantAttributesCheck : BaseAnalyzer
{
	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
		stack.length = 0;
	}

	override void visit(const Declaration decl)
	{

		// labels, e.g. private:
		if (auto attr = decl.attributeDeclaration)
		{
			if (filterAttributes(attr.attribute))
			{
				addAttribute(attr.attribute);
			}
		}

		auto attributes = decl.attributes.filter!(a => filterAttributes(a));
		if (attributes.walkLength > 0) {

			// new scope: private { }
			if (decl.declarations.length > 0)
			{
				const prev = currentAttributes[];
				// append to current scope and reset once block is left
				foreach (attr; attributes)
					addAttribute(attr);

				scope(exit) currentAttributes = prev;
				decl.accept(this);
			} // declarations, e.g. private int ...
			else
			{
				foreach (attr; attributes)
					checkAttribute(attr);

				decl.accept(this);
			}
		}
		else
		{
			decl.accept(this);
		}
	}

	alias visit = BaseAnalyzer.visit;

	mixin ScopedVisit!Module;
	mixin ScopedVisit!BlockStatement;
	mixin ScopedVisit!StructBody;
	mixin ScopedVisit!CaseStatement;
	mixin ScopedVisit!ForStatement;
	mixin ScopedVisit!IfStatement;
	mixin ScopedVisit!TemplateDeclaration;
	mixin ScopedVisit!ConditionalDeclaration;

private:

	alias ConstAttribute = const Attribute;
	alias CurrentScope = ConstAttribute[];
	ref CurrentScope currentAttributes()
	{
		return stack[$ - 1];
	}

	CurrentScope[] stack;

	void addAttribute(const Attribute attr)
	{
		removeOverwrite(attr);
		if (checkAttribute(attr))
		{
			currentAttributes ~= attr;
		}
	}

	bool checkAttribute(const Attribute attr)
	{
		auto match = currentAttributes.find!(a => a.attribute.type == attr.attribute.type);
		if (!match.empty)
		{
			auto token = attr.attribute;
			addErrorMessage(token.line, token.column, KEY,
					text("same visibility attribute used as defined on line ",
						match.front.attribute.line.to!string, "."));
			return false;
		}
		return true;
	}

	void removeOverwrite(const Attribute attr)
	{
		import std.array : array;
		const group = getAttributeGroup(attr);
		if (currentAttributes.filter!(a => getAttributeGroup(a) == group
					&& !isIdenticalAttribute(a, attr)).walkLength > 0)
		{
			currentAttributes = currentAttributes.filter!(a => getAttributeGroup(a) != group
					|| isIdenticalAttribute(a, attr)).array;
		}
	}

	bool filterAttributes(const Attribute attr)
	{
		return isAccessSpecifier(attr);
	}

	static int getAttributeGroup(const Attribute attr)
	{
		if (isAccessSpecifier(attr))
			return 1;

		// TODO: not implemented
		return attr.attribute.type;
	}

	static bool isAccessSpecifier(const Attribute attr)
	{
		auto type = attr.attribute.type;
		return type.among(tok!"private", tok!"protected", tok!"public", tok!"package", tok!"export") > 0;
	}

	static bool isIdenticalAttribute(const Attribute a, const Attribute b)
	{
		return a.attribute.type == b.attribute.type;
	}

	auto attributesString()
	{
		return currentAttributes.map!(a => a.attribute.type.str).joiner(",").to!string;
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

	void pushScope()
	{
		stack.length++;
	}

	void popScope()
	{
		stack.length--;
	}

	enum string KEY = "dscanner.suspicious.redundant_attributes";
}


version(unittest)
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;
}

unittest
{
	StaticAnalysisConfig sac = disabledConfig();
	sac.redundant_attributes_check = Check.enabled;

	// test labels vs. block attributes
	assertAnalyzerWarnings(q{
unittest
{
private:
	private int blah; // [warn]: same visibility attribute used as defined on line 4.
protected
{
	protected int blah; // [warn]: same visibility attribute used as defined on line 6.
}
	private int blah; // [warn]: same visibility attribute used as defined on line 4.
}}c, sac);

	// test labels vs. block attributes
	assertAnalyzerWarnings(q{
unittest
{
	private:
	private: // [warn]: same visibility attribute used as defined on line 4.
	public:
		private int a;
		public int b; // [warn]: same visibility attribute used as defined on line 6.
		public // [warn]: same visibility attribute used as defined on line 6.
		{
			int c;
		}
}}c, sac);

	// test scopes
	assertAnalyzerWarnings(q{
unittest
{
private:
	private int foo2; // [warn]: same visibility attribute used as defined on line 4.
	private void foo() // [warn]: same visibility attribute used as defined on line 4.
	{
		private int blah;
	}
}}c, sac);

	// check duplicated visibility attributes
	assertAnalyzerWarnings(q{
unittest
{
private:
	public int a;
private: // [warn]: same visibility attribute used as defined on line 4.
}}c, sac);

	// test conditional compilation
	assertAnalyzerWarnings(q{
unittest
{
version(unittest)
{
	private:
	private int foo; // [warn]: same visibility attribute used as defined on line 6.
}
private int foo2;
}}c, sac);

// test scopes
	assertAnalyzerWarnings(q{
unittest
{
public:
	if (1 == 1)
	{
		private int b;
	}
	else
	{
		public int b;
	}
	public int b; // [warn]: same visibility attribute used as defined on line 4.
}}c, sac);
}

// test other attribute (not yet implemented, thus shouldn't trigger warnings)
unittest
{
	StaticAnalysisConfig sac = disabledConfig();
	sac.redundant_attributes_check = Check.enabled;

	// test labels vs. block attributes
	assertAnalyzerWarnings(q{
unittest
{
@safe:
	@safe void foo();
@system
{
	@system void foo();
}
	@safe void foo();
}}c, sac);


	stderr.writeln("Unittest for RedundantAttributesCheck passed.");
}
