//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.function_attributes;

import std.d.ast;
import std.d.lexer;
import analysis.base;

import std.stdio;

/**
 * Prefer
 * ---
 * int getStuff() const {}
 * ---
 * to
 * ---
 * const int getStuff() {}
 * ---
 */
class FunctionAttributeCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const InterfaceDeclaration dec)
	{
		auto t = inInterface;
		inInterface = true;
		dec.accept(this);
		inInterface = t;
	}

	override void visit(const ClassDeclaration dec)
	{
		auto t = inInterface;
		inInterface = false;
		dec.accept(this);
		inInterface = t;
	}

	override void visit(const AttributeDeclaration dec)
	{
		if (inInterface > 0 && dec.attribute.storageClass !is null
			&& dec.attribute.storageClass.token == tok!"abstract")
		{
			addErrorMessage(dec.attribute.storageClass.token.line,
				dec.attribute.storageClass.token.column, KEY, ABSTRACT_MESSAGE);
		}
	}

	override void visit(const Declaration dec)
	{
		if (dec.attributes.length == 0)
			goto end;
		foreach (attr; dec.attributes)
		{
			if (attr.storageClass is null)
				continue;
			if (attr.storageClass.token == tok!"abstract" && inInterface)
			{
				addErrorMessage(attr.storageClass.token.line,
					attr.storageClass.token.column, KEY, ABSTRACT_MESSAGE);
				continue;
			}
			if (dec.functionDeclaration !is null
				&& (attr.storageClass.token == tok!"const"
				|| attr.storageClass.token == tok!"inout"
				|| attr.storageClass.token == tok!"immutable"))
			{
				import std.string : format;
				immutable string attrString = str(attr.storageClass.token.type);
				addErrorMessage(dec.functionDeclaration.name.line,
					dec.functionDeclaration.name.column, KEY,
					format("'%s' is not an attribute of the return type."
					~ " Place it after the parameter list to clarify.", attrString));
			}
		}
	end:
		dec.accept(this);
	}

	bool inInterface;

	private enum ABSTRACT_MESSAGE = "'abstract' attribute is redundant in interface declarations";
	private enum KEY = "dscanner.confusing.function_attributes";
}
