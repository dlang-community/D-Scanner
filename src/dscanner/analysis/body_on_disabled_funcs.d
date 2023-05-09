module dscanner.analysis.body_on_disabled_funcs;

import dscanner.analysis.base;
import dparse.ast;
import dparse.lexer;
import dsymbol.scope_;
import std.meta : AliasSeq;

final class BodyOnDisabledFuncsCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	mixin AnalyzerInfo!"body_on_disabled_func_check";

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	static foreach (AggregateType; AliasSeq!(InterfaceDeclaration, ClassDeclaration,
		StructDeclaration, UnionDeclaration, FunctionDeclaration))
		override void visit(const AggregateType t)
		{
			scope wasDisabled = isDisabled;
			isDisabled = false;
			t.accept(this);
			isDisabled = wasDisabled;
		}

	override void visit(const Declaration dec)
	{
		foreach (attr; dec.attributes)
		{
			if (attr.atAttribute !is null && attr.atAttribute.identifier.text == "disable") {
				// found attr block w. disable: dec.constructor
				scope wasDisabled = isDisabled;
				isDisabled = true;
				visitDeclarationInner(dec);
				dec.accept(this);
				isDisabled = wasDisabled;
				return;
			}
		}

		visitDeclarationInner(dec);
		scope wasDisabled = isDisabled;
		dec.accept(this);
		isDisabled = wasDisabled;
	}

private:
	bool isDisabled = false;

	bool isDeclDisabled(T)(const T dec)
	{
		// `@disable { ... }`
		if (isDisabled)
			return true;

		static if (__traits(hasMember, T, "storageClasses"))
		{
			// `@disable doThing() {}`
			if (hasDisabledStorageclass(dec.storageClasses))
				return true;
		}

		// `void doThing() @disable {}`
		return hasDisabledMemberFunctionAttribute(dec.memberFunctionAttributes);
	}

	void visitDeclarationInner(const Declaration dec)
	{
		if (dec.attributeDeclaration !is null
			&& dec.attributeDeclaration.attribute
			&& dec.attributeDeclaration.attribute.atAttribute
			&& dec.attributeDeclaration.attribute.atAttribute.identifier.text == "disable")
		{
			// found `@disable:`, so all code in this block is now disabled
			isDisabled = true;
		}
		else if (dec.functionDeclaration !is null
					&& isDeclDisabled(dec.functionDeclaration)
					&& dec.functionDeclaration.functionBody !is null
					&& dec.functionDeclaration.functionBody.missingFunctionBody is null)
		{
			addErrorMessage(dec.functionDeclaration.name.line, dec.functionDeclaration.name.column,
					KEY, "Function marked with '@disabled' should not have a body");
		}
		else if (dec.constructor !is null
					&& isDeclDisabled(dec.constructor)
					&& dec.constructor.functionBody !is null
					&& dec.constructor.functionBody.missingFunctionBody is null)
		{
			addErrorMessage(dec.constructor.line, dec.constructor.column,
					KEY, "Constructor marked with '@disabled' should not have a body");
		}
		else if (dec.destructor !is null
					&& isDeclDisabled(dec.destructor)
					&& dec.destructor.functionBody !is null
					&& dec.destructor.functionBody.missingFunctionBody is null)
		{
			addErrorMessage(dec.destructor.line, dec.destructor.column,
					KEY, "Destructor marked with '@disabled' should not have a body");
		}
	}

	bool hasDisabledStorageclass(const(StorageClass[]) storageClasses)
	{
		foreach (sc; storageClasses)
		{
			if (sc.atAttribute !is null && sc.atAttribute.identifier.text == "disable")
				return true;
		}
		return false;
	}

	bool hasDisabledMemberFunctionAttribute(const(MemberFunctionAttribute[]) memberFunctionAttributes)
	{
		foreach (attr; memberFunctionAttributes)
		{
			if (attr.atAttribute !is null && attr.atAttribute.identifier.text == "disable")
				return true;
		}
		return false;
	}

	enum string KEY = "dscanner.confusing.disabled_function_with_body";
}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.body_on_disabled_func_check = Check.enabled;

	assertAnalyzerWarnings(q{
	class C1
	{
		this() {}
		void doThing() {}
		~this() {}

		this();
		void doThing();
		~this();

	@disable:
		@disable
		{
			class UnaffectedSubClass
			{
				this() {}
				void doThing() {}
				~this() {}
			}
		}

		this() {} // [warn]: Constructor marked with '@disabled' should not have a body
		void doThing() {} // [warn]: Function marked with '@disabled' should not have a body
		~this() {} // [warn]: Destructor marked with '@disabled' should not have a body

		this();
		void doThing();
		~this();
	}

	class C2
	{
		@disable this() {} // [warn]: Constructor marked with '@disabled' should not have a body
		@disable { this() {} } // [warn]: Constructor marked with '@disabled' should not have a body
		this() @disable {} // [warn]: Constructor marked with '@disabled' should not have a body

		@disable void doThing() {} // [warn]: Function marked with '@disabled' should not have a body
		@disable doThing() {} // [warn]: Function marked with '@disabled' should not have a body
		@disable { void doThing() {} } // [warn]: Function marked with '@disabled' should not have a body
		void doThing() @disable {} // [warn]: Function marked with '@disabled' should not have a body

		@disable ~this() {} // [warn]: Destructor marked with '@disabled' should not have a body
		@disable { ~this() {} } // [warn]: Destructor marked with '@disabled' should not have a body
		~this() @disable {} // [warn]: Destructor marked with '@disabled' should not have a body

		@disable this();
		@disable { this(); }
		this() @disable;

		@disable void doThing();
		// @disable doThing(); // this is invalid grammar!
		@disable { void doThing(); }
		void doThing() @disable;

		@disable ~this();
		@disable { ~this(); }
		~this() @disable;
	}
	}c, sac);

	stderr.writeln("Unittest for BodyOnDisabledFuncsCheck passed.");
}
