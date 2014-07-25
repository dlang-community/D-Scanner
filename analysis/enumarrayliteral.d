//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.enumarrayliteral;

import std.d.ast;
import std.d.lexer;
import analysis.base;

void doNothing(string, size_t, size_t, string, bool) {}

class EnumArrayLiteralCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	bool looking = false;

	mixin visitTemplate!ClassDeclaration;
	mixin visitTemplate!InterfaceDeclaration;
	mixin visitTemplate!UnionDeclaration;
	mixin visitTemplate!StructDeclaration;

	override void visit(const Declaration dec)
	{
		if (inAggregate) foreach (attr; dec.attributes)
		{
			if (attr.storageClass !is null &&
				attr.storageClass.token == tok!"enum")
			{
				looking = true;
			}
		}
		dec.accept(this);
		looking = false;
	}

	override void visit(const AutoDeclaration autoDec)
	{
		if (looking)
		{
			foreach (i, initializer; autoDec.initializers)
			{
				if (initializer is null) continue;
				if (initializer.nonVoidInitializer is null) continue;
				if (initializer.nonVoidInitializer.arrayInitializer is null) continue;
				addErrorMessage(autoDec.identifiers[i].line,
					autoDec.identifiers[i].column, "This enum may lead to "
					~ "unnecessary allocation at run-time. Use 'static immutable "
					~ autoDec.identifiers[i].text ~ " = [ ...' instead.");
			}
		}
		autoDec.accept(this);
	}
}

