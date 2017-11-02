//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.style;

import std.stdio;
import dparse.ast;
import dparse.lexer;
import std.regex;
import std.array;
import std.conv;
import std.format;
import dscanner.analysis.helpers;
import dscanner.analysis.base;
import dsymbol.scope_ : Scope;

final class StyleChecker : BaseAnalyzer
{
	alias visit = ASTVisitor.visit;

	enum string varFunNameRegex = `^([\p{Ll}_][_\w\d]*|[\p{Lu}\d_]+)$`;
	enum string aggregateNameRegex = `^\p{Lu}[\w\d]*$`;
	enum string moduleNameRegex = `^[\p{Ll}_\d]+$`;
	enum string KEY = "dscanner.style.phobos_naming_convention";

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const ModuleDeclaration dec)
	{
		foreach (part; dec.moduleName.identifiers)
		{
			if (part.text.matchFirst(moduleNameRegex).length == 0)
				addErrorMessage(part.line, part.column, KEY,
						"Module/package name '" ~ part.text ~ "' does not match style guidelines.");
		}
	}

	// "extern (Windows) {}" : push visit pop
	override void visit(const Declaration dec)
	{
		bool p;
		if (dec.attributes)
			foreach (attrib; dec.attributes)
				if (const LinkageAttribute la = attrib.linkageAttribute)
		{
			p = true;
			pushWinStyle(la.identifier.text.length && la.identifier.text == "Windows");
		}

		dec.accept(this);

		if (p)
			popWinStyle;
	}

	// "extern (Windows) :" : overwrite current
	override void visit(const AttributeDeclaration dec)
	{
		if (dec.attribute && dec.attribute.linkageAttribute)
		{
			const LinkageAttribute la = dec.attribute.linkageAttribute;
			_winStyles[$-1] = la.identifier.text.length && la.identifier.text == "Windows";
		}
	}

	override void visit(const VariableDeclaration vd)
	{
		vd.accept(this);
	}

	override void visit(const Declarator dec)
	{
		checkLowercaseName("Variable", dec.name);
	}

	override void visit(const FunctionDeclaration dec)
	{
		// "extern(Windows) Name();" push visit pop
		bool p;
		if (dec.attributes)
			foreach (attrib; dec.attributes)
				if (const LinkageAttribute la = attrib.linkageAttribute)
		{
			p = true;
			pushWinStyle(la.identifier.text.length && la.identifier.text == "Windows");
		}

		if (dec.functionBody || (!dec.functionBody && !winStyle()))
			checkLowercaseName("Function", dec.name);

		if (p)
			popWinStyle;
	}

	void checkLowercaseName(string type, ref const Token name)
	{
		if (name.text.length > 0 && name.text.matchFirst(varFunNameRegex).length == 0)
			addErrorMessage(name.line, name.column, KEY,
					type ~ " name '" ~ name.text ~ "' does not match style guidelines.");
	}

	override void visit(const ClassDeclaration dec)
	{
		checkAggregateName("Class", dec.name);
		dec.accept(this);
	}

	override void visit(const InterfaceDeclaration dec)
	{
		checkAggregateName("Interface", dec.name);
		dec.accept(this);
	}

	override void visit(const EnumDeclaration dec)
	{
		if (dec.name.text is null || dec.name.text.length == 0)
			return;
		checkAggregateName("Enum", dec.name);
		dec.accept(this);
	}

	override void visit(const StructDeclaration dec)
	{
		checkAggregateName("Struct", dec.name);
		dec.accept(this);
	}

	void checkAggregateName(string aggregateType, ref const Token name)
	{
		if (name.text.length > 0 && name.text.matchFirst(aggregateNameRegex).length == 0)
			addErrorMessage(name.line, name.column, KEY,
					aggregateType ~ " name '" ~ name.text ~ "' does not match style guidelines.");
	}

	bool[] _winStyles = [false];

	bool winStyle()
	{
		return _winStyles[$-1];
	}

	void pushWinStyle(const bool value)
	{
		_winStyles.length += 1;
		_winStyles[$-1] = value;
	}

	void popWinStyle()
	{
		_winStyles.length -= 1;
	}
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.style_check = Check.enabled;

	assertAnalyzerWarnings(q{
		module AMODULE; // [warn]: Module/package name 'AMODULE' does not match style guidelines.

		bool A_VARIABLE; // FIXME:
		bool a_variable; // ok
		bool aVariable; // ok

		void A_FUNCTION() {} // FIXME:
		class cat {} // [warn]: Class name 'cat' does not match style guidelines.
		interface puma {} // [warn]: Interface name 'puma' does not match style guidelines.
		struct dog {} // [warn]: Struct name 'dog' does not match style guidelines.
		enum racoon { a } // [warn]: Enum name 'racoon' does not match style guidelines.
		enum bool something = false;
		enum bool someThing = false;
		enum Cat { fritz, }
		enum Cat = Cat.fritz;
	}c, sac);

	assertAnalyzerWarnings(q{
		extern(Windows)
		{
			bool Fun0();
			extern(Windows) bool Fun1();
		}
	}c, sac);

	assertAnalyzerWarnings(q{
		extern(Windows)
		{
			extern(D) bool Fun2(); // [warn]: Function name 'Fun2' does not match style guidelines.
			bool Fun3();
		}
	}c, sac);

	assertAnalyzerWarnings(q{
		extern(Windows)
		{
			extern(C):
				extern(D) bool Fun4(); // [warn]: Function name 'Fun4' does not match style guidelines.
				bool Fun5(); // [warn]: Function name 'Fun5' does not match style guidelines.
		}
	}c, sac);

	assertAnalyzerWarnings(q{
		extern(Windows):
			bool Fun6();
			bool Fun7();
		extern(D):
			void okOkay();
			void NotReallyOkay(); // [warn]: Function name 'NotReallyOkay' does not match style guidelines.
	}c, sac);

	assertAnalyzerWarnings(q{
		extern(Windows):
			bool WinButWithBody(){} // [warn]: Function name 'WinButWithBody' does not match style guidelines.
	}c, sac);

	stderr.writeln("Unittest for StyleChecker passed.");
}
