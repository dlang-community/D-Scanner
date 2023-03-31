module dscanner.analysis.body_on_disabled_funcs;

import dscanner.analysis.base;
import dparse.ast;
import dparse.lexer;
import std.stdio;
import dsymbol.scope_;

final class BodyOnDisabledFuncsCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	mixin AnalyzerInfo!"body_on_disabled_func_check";

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const Declaration dec)
	{
		foreach (attr; dec.attributes)
		{
			if (attr.atAttribute !is null && attr.atAttribute.identifier.text == "disable") {
				writeln("found attr block w. disable: ", dec.constructor);
				isDisabled = true;
				visitDeclarationInner(dec);
				dec.accept(this);
				isDisabled = false;
				return;
			}
		}

		visitDeclarationInner(dec);
		dec.accept(this);
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
		if (dec.attributeDeclaration !is null)
		{
			writeln("found attrdecl: ", dec.attributeDeclaration);
		}
		else if (dec.functionDeclaration !is null
					&& isDeclDisabled(dec.functionDeclaration)
					&& dec.functionDeclaration.functionBody !is null)
		{
			addErrorMessage(dec.functionDeclaration.name.line, dec.functionDeclaration.name.column,
					KEY, "Function marked with '@disabled' should not have a body");
		}
		else if (dec.constructor !is null
					&& isDeclDisabled(dec.constructor)
					&& dec.constructor.functionBody !is null)
		{
			addErrorMessage(dec.constructor.line, dec.constructor.column,
					KEY, "Constructor marked with '@disabled' should not have a body");
		}
		else if (dec.destructor !is null
					&& isDeclDisabled(dec.destructor)
					&& dec.destructor.functionBody !is null)
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