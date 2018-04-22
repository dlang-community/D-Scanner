//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.builtin_property_names;

import std.stdio;
import std.regex;
import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import dscanner.analysis.helpers;
import dsymbol.scope_;
import std.algorithm : map;

/**
 * The following code should be killed with fire:
 * ---
 * class SomeClass
 * {
 * 	void init();
 * 	int init;
 * 	string mangleof = "LOL";
 * 	auto init = 10;
 * 	enum sizeof = 10;
 * }
 * ---
 */
final class BuiltinPropertyNameCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const FunctionDeclaration fd)
	{
		if (depth > 0 && isBuiltinProperty(fd.name.text))
		{
			addErrorMessage(fd.name.line, fd.name.column, KEY, generateErrorMessage(fd.name.text));
		}
		fd.accept(this);
	}

	override void visit(const FunctionBody functionBody)
	{
		immutable int d = depth;
		scope (exit)
			depth = d;
		depth = 0;
		functionBody.accept(this);
	}

	override void visit(const AutoDeclaration ad)
	{
		if (depth > 0)
			foreach (i; ad.parts.map!(a => a.identifier))
			{
				if (isBuiltinProperty(i.text))
					addErrorMessage(i.line, i.column, KEY, generateErrorMessage(i.text));
			}
	}

	override void visit(const Declarator d)
	{
		if (depth > 0 && isBuiltinProperty(d.name.text))
			addErrorMessage(d.name.line, d.name.column, KEY, generateErrorMessage(d.name.text));
	}

	override void visit(const StructBody sb)
	{
		depth++;
		sb.accept(this);
		depth--;
	}

private:

	enum string KEY = "dscanner.confusing.builtin_property_names";

	string generateErrorMessage(string name)
	{
		import std.string : format;

		return format("Avoid naming members '%s'. This can"
				~ " confuse code that depends on the '.%s' property of a type.", name, name);
	}

	bool isBuiltinProperty(string name)
	{
		import std.algorithm : canFind;

		return BuiltinProperties.canFind(name);
	}

	enum string[] BuiltinProperties = ["init", "sizeof", "mangleof", "alignof", "stringof"];
	int depth;
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.builtin_property_names_check = Check.enabled;
	assertAnalyzerWarnings(q{
class SomeClass
{
	void init(); // [warn]: Avoid naming members 'init'. This can confuse code that depends on the '.init' property of a type.
	int init; // [warn]: Avoid naming members 'init'. This can confuse code that depends on the '.init' property of a type.
	auto init = 10; // [warn]: Avoid naming members 'init'. This can confuse code that depends on the '.init' property of a type.
}
	}c, sac);

	stderr.writeln("Unittest for NumberStyleCheck passed.");
}
