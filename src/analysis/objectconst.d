//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.objectconst;

import std.stdio;
import std.regex;
import dparse.ast;
import dparse.lexer;
import analysis.base;
import analysis.helpers;
import dsymbol.scope_ : Scope;

/**
 * Checks that opEquals, opCmp, toHash, 'opCast', and toString are either const,
 * immutable, or inout.
 */
class ObjectConstCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	///
	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	mixin visitTemplate!ClassDeclaration;
	mixin visitTemplate!InterfaceDeclaration;
	mixin visitTemplate!UnionDeclaration;
	mixin visitTemplate!StructDeclaration;

	override void visit(const AttributeDeclaration d)
	{
		if (d.attribute.attribute == tok!"const" && inAggregate)
		{
			constColon = true;
		}
		d.accept(this);
	}

	override void visit(const Declaration d)
	{
		import std.algorithm : any;
		bool setConstBlock;
		if (inAggregate && d.attributes && d.attributes.any!(a => a.attribute == tok!"const"))
		{
			setConstBlock = true;
			constBlock = true;
		}

		if (inAggregate && d.functionDeclaration !is null && !constColon && !constBlock
				&& isInteresting(d.functionDeclaration.name.text)
				&& !hasConst(d.functionDeclaration.memberFunctionAttributes))
		{
			addErrorMessage(d.functionDeclaration.name.line,
					d.functionDeclaration.name.column, "dscanner.suspicious.object_const",
					"Methods 'opCmp', 'toHash', 'opEquals', 'opCast', and/or 'toString' are non-const.");
		}

		d.accept(this);

		if (!inAggregate)
			constColon = false;
		if (setConstBlock)
			constBlock = false;
	}

	private static bool hasConst(const MemberFunctionAttribute[] attributes)
	{
		import std.algorithm : any;

		return attributes.any!(a => a.tokenType == tok!"const"
				|| a.tokenType == tok!"immutable" || a.tokenType == tok!"inout");
	}

	private static bool isInteresting(string name)
	{
		return name == "opCmp" || name == "toHash" || name == "opEquals"
			|| name == "toString" || name == "opCast";
	}

	private bool constBlock;
	private bool constColon;

}

unittest
{
	import analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.object_const_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void testConsts()
		{
			// Will be ok because all are declared const/immutable
			class Cat
			{
				const bool opEquals(Object a, Object b) // ok
				{
					return true;
				}

				const int opCmp(Object o) // ok
				{
					return 1;
				}

				const hash_t toHash() // ok
				{
					return 0;
				}

				const string toString() // ok
				{
					return "Cat";
				}
			}

			class Bat
			{
          		const: override string toString() { return "foo"; } // ok
			}

			class Fox
			{
          		const{ override string toString() { return "foo"; }} // ok
			}

			// Will warn, because none are const
			class Dog
			{
				bool opEquals(Object a, Object b) // [warn]: Methods 'opCmp', 'toHash', 'opEquals', 'opCast', and/or 'toString' are non-const.
				{
					return true;
				}

				int opCmp(Object o) // [warn]: Methods 'opCmp', 'toHash', 'opEquals', 'opCast', and/or 'toString' are non-const.
				{
					return 1;
				}

				hash_t toHash() // [warn]: Methods 'opCmp', 'toHash', 'opEquals', 'opCast', and/or 'toString' are non-const.
				{
					return 0;
				}

				string toString() // [warn]: Methods 'opCmp', 'toHash', 'opEquals', 'opCast', and/or 'toString' are non-const.
				{
					return "Dog";
				}
			}
		}
	}c, sac);

	stderr.writeln("Unittest for ObjectConstCheck passed.");
}
