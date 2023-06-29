//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.undocumented;

import dscanner.analysis.base;
import dsymbol.scope_ : Scope;
import dparse.ast;
import dparse.lexer;

import std.regex : ctRegex, matchAll;
import std.stdio;

/**
 * Checks for undocumented public declarations. Ignores some operator overloads,
 * main functions, and functions whose name starts with "get" or "set".
 */
final class UndocumentedDeclarationCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	mixin AnalyzerInfo!"undocumented_declaration_check";

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const Module mod)
	{
		push(tok!"public");
		mod.accept(this);
	}

	override void visit(const Declaration dec)
	{
		if (dec.attributeDeclaration)
		{
			auto attr = dec.attributeDeclaration.attribute;
			if (isProtection(attr.attribute.type))
				set(attr.attribute.type);
			else if (attr.attribute == tok!"override")
				setOverride(true);
			else if (attr.deprecated_ !is null)
				setDeprecated(true);
			else if (attr.atAttribute !is null && attr.atAttribute.identifier.text == "disable")
				setDisabled(true);
		}

		immutable bool shouldPop = dec.attributeDeclaration is null;
		immutable bool prevOverride = getOverride();
		immutable bool prevDisabled = getDisabled();
		immutable bool prevDeprecated = getDeprecated();
		bool dis;
		bool dep;
		bool ovr;
		bool pushed;
		foreach (attribute; dec.attributes)
		{
			if (isProtection(attribute.attribute.type))
			{
				if (shouldPop)
				{
					pushed = true;
					push(attribute.attribute.type);
				}
				else
					set(attribute.attribute.type);
			}
			else if (attribute.attribute == tok!"override")
				ovr = true;
			else if (attribute.deprecated_ !is null)
				dep = true;
			else if (attribute.atAttribute !is null
					&& attribute.atAttribute.identifier.text == "disable")
				dis = true;
		}
		if (ovr)
			setOverride(true);
		if (dis)
			setDisabled(true);
		if (dep)
			setDeprecated(true);
		dec.accept(this);
		if (shouldPop && pushed)
			pop();
		if (ovr)
			setOverride(prevOverride);
		if (dis)
			setDisabled(prevDisabled);
		if (dep)
			setDeprecated(prevDeprecated);
	}

	override void visit(const VariableDeclaration variable)
	{
		if (!currentIsInteresting() || variable.comment.ptr !is null)
			return;
		if (variable.autoDeclaration !is null)
		{
			addMessage(variable.autoDeclaration.parts[0].identifier,
					variable.autoDeclaration.parts[0].identifier.text);
			return;
		}
		foreach (dec; variable.declarators)
		{
			if (dec.comment.ptr is null)
				addMessage(dec.name, dec.name.text);
			return;
		}
	}

	override void visit(const ConditionalDeclaration cond)
	{
		const VersionCondition ver = cond.compileCondition.versionCondition;
		if (ver is null || (ver.token != tok!"unittest" && ver.token.text != "none"))
			cond.accept(this);
		else if (cond.falseDeclarations.length > 0)
			foreach (f; cond.falseDeclarations)
				visit(f);
	}

	override void visit(const FunctionBody fb)
	{
	}

	override void visit(const Unittest u)
	{
	}

	override void visit(const TraitsExpression t)
	{
	}

	mixin V!AnonymousEnumMember;
	mixin V!ClassDeclaration;
	mixin V!EnumDeclaration;
	mixin V!InterfaceDeclaration;
	mixin V!StructDeclaration;
	mixin V!UnionDeclaration;
	mixin V!TemplateDeclaration;
	mixin V!FunctionDeclaration;
	mixin V!Constructor;

private:

	mixin template V(T)
	{
		override void visit(const T declaration)
		{
			import std.traits : hasMember;
			static if (hasMember!(T, "storageClasses"))
			{
				// stop at declarations with a deprecated in their storage classes
				foreach (sc; declaration.storageClasses)
					if (sc.deprecated_ !is null)
						return;
			}

			if (currentIsInteresting())
			{
				if (declaration.comment.ptr is null)
				{
					static if (hasMember!(T, "name"))
					{
						static if (is(T == FunctionDeclaration))
						{
							import std.algorithm : canFind;

							if (!(ignoredFunctionNames.canFind(declaration.name.text)
									|| isGetterOrSetter(declaration.name.text)
									|| isProperty(declaration)))
							{
								addMessage(declaration.name, declaration.name.text);
							}
						}
						else
						{
							if (declaration.name.type != tok!"")
								addMessage(declaration.name, declaration.name.text);
						}
					}
					else
					{
						import std.algorithm : countUntil;

						// like constructors
						auto tokens = declaration.tokens.findTokenForDisplay(tok!"this");
						auto earlyEnd = tokens.countUntil!(a => a == tok!"{" || a == tok!"(" || a == tok!";");
						if (earlyEnd != -1)
							tokens = tokens[0 .. earlyEnd];
						addMessage(tokens, null);
					}
				}
				static if (!(is(T == TemplateDeclaration) || is(T == FunctionDeclaration)))
				{
					declaration.accept(this);
				}
			}
		}
	}

	static bool isGetterOrSetter(string name)
	{
		return !matchAll(name, getSetRe).empty;
	}

	static bool isProperty(const FunctionDeclaration dec)
	{
		if (dec.memberFunctionAttributes is null)
			return false;
		foreach (attr; dec.memberFunctionAttributes)
		{
			if (attr.atAttribute && attr.atAttribute.identifier.text == "property")
				return true;
		}
		return false;
	}

	void addMessage(T)(T range, string name)
	{
		import std.string : format;

		addErrorMessage(range, "dscanner.style.undocumented_declaration", name is null
				? "Public declaration is undocumented."
				: format("Public declaration '%s' is undocumented.", name));
	}

	bool getOverride()
	{
		return stack[$ - 1].isOverride;
	}

	void setOverride(bool o = true)
	{
		stack[$ - 1].isOverride = o;
	}

	bool getDisabled()
	{
		return stack[$ - 1].isDisabled;
	}

	void setDisabled(bool d = true)
	{
		stack[$ - 1].isDisabled = d;
	}

	bool getDeprecated()
	{
		return stack[$ - 1].isDeprecated;
	}

	void setDeprecated(bool d = true)
	{
		stack[$ - 1].isDeprecated = d;
	}

	bool currentIsInteresting()
	{
		return stack[$ - 1].protection == tok!"public"
			&& !getOverride() && !getDisabled() && !getDeprecated();
	}

	void set(IdType p)
	in
	{
		assert(isProtection(p));
	}
	do
	{
		stack[$ - 1].protection = p;
	}

	void push(IdType p)
	in
	{
		assert(isProtection(p));
	}
	do
	{
		stack ~= ProtectionInfo(p, false);
	}

	void pop()
	{
		assert(stack.length > 1);
		stack = stack[0 .. $ - 1];
	}

	static struct ProtectionInfo
	{
		IdType protection;
		bool isOverride;
		bool isDeprecated;
		bool isDisabled;
	}

	ProtectionInfo[] stack;
}

// Ignore undocumented symbols with these names
private immutable string[] ignoredFunctionNames = [
	"opCmp", "opEquals", "toString", "toHash", "main"
];

private enum getSetRe = ctRegex!`^(?:get|set)(?:\p{Lu}|_).*`;

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.undocumented_declaration_check = Check.enabled;

	assertAnalyzerWarnings(q{
		class C{} /+
		      ^ [warn]: Public declaration 'C' is undocumented. +/
		interface I{} /+
		          ^ [warn]: Public declaration 'I' is undocumented. +/
		enum e = 0; /+
		     ^ [warn]: Public declaration 'e' is undocumented. +/
		void f(){} /+
		     ^ [warn]: Public declaration 'f' is undocumented. +/
		struct S{} /+
		       ^ [warn]: Public declaration 'S' is undocumented. +/
		template T(){} /+
		         ^ [warn]: Public declaration 'T' is undocumented. +/
		union U{} /+
		      ^ [warn]: Public declaration 'U' is undocumented. +/
	}, sac);

	assertAnalyzerWarnings(q{
		/// C
		class C{}
		/// I
		interface I{}
		/// e
		enum e = 0;
		/// f
		void f(){}
		/// S
		struct S{}
		/// T
		template T(){}
		/// U
		union U{}
	}, sac);

	// https://github.com/dlang-community/D-Scanner/issues/760
	assertAnalyzerWarnings(q{
		deprecated auto func(){}
		deprecated auto func()(){}
	}, sac);

	assertAnalyzerWarnings(q{
		class C{} /// a
		interface I{} /// b
		enum e = 0; /// c
		void f(){} /// d
		struct S{} /// e
		template T(){} /// f
		union U{} /// g
	}, sac);

	stderr.writeln("Unittest for UndocumentedDeclarationCheck passed.");
}
