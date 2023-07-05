//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.enumarrayliteral;

import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import std.algorithm : find, map;
import dsymbol.scope_ : Scope;

void doNothing(string, size_t, size_t, string, bool)
{
}

final class EnumArrayLiteralCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	mixin AnalyzerInfo!"enum_array_literal_check";

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	bool looking;

	mixin visitTemplate!ClassDeclaration;
	mixin visitTemplate!InterfaceDeclaration;
	mixin visitTemplate!UnionDeclaration;
	mixin visitTemplate!StructDeclaration;

	override void visit(const AutoDeclaration autoDec)
	{
		auto enumToken = autoDec.storageClasses.find!(a => a.token == tok!"enum");
		if (enumToken.length)
		{
			foreach (part; autoDec.parts)
			{
				if (part.initializer is null)
					continue;
				if (part.initializer.nonVoidInitializer is null)
					continue;
				if (part.initializer.nonVoidInitializer.arrayInitializer is null)
					continue;
				addErrorMessage(part.initializer.nonVoidInitializer,
						"dscanner.performance.enum_array_literal",
						"This enum may lead to unnecessary allocation at run-time."
						~ " Use 'static immutable "
						~ part.identifier.text ~ " = [ ...' instead.",
						[
							AutoFix.replacement(enumToken[0].token, "static immutable")
						]);
			}
		}
		autoDec.accept(this);
	}
}
