module dscanner.analysis.ignored_unittest;

import dparse.lexer;
import dparse.ast;
import dscanner.analysis.base;

import std.stdio;
import std.meta : AliasSeq;

private string scoped(string v, string val)
{
	if (__ctfe)
	{
		return "auto _old_" ~ v ~ " = " ~ v ~ "; " ~ v ~ " = " ~ val ~ "; scope (exit) " ~ v ~ " = _old_" ~ v ~ ";";
	}
	else
	{
		return null;
	}
}

/**
 * unittests in methods are never run, unittests in templates may only serve for documentation
 */
final class IgnoredUnittestCheck : BaseAnalyzer
{
	enum string KEY_IGNORED = "dscanner.bugs.ignored_unittest";
	enum string MESSAGE_IGNORED = "unittest blocks can only be run in global scope or in types";
	enum string KEY_TEMPLATED = "dscanner.suspicious.templated_unittest";
	enum string MESSAGE_TEMPLATED = "unittest blocks in templates may not be run and can only serve for documentation purpose. Document with a ddoc-comment or move unittest block out of template.";
	mixin AnalyzerInfo!"ignored_unittest";

	private bool inValidScope = true;
	private bool inUndocumentable;
	private bool inTemplate;

	///
	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	static foreach (T; AliasSeq!(
		ClassDeclaration,
		InterfaceDeclaration,
		StructDeclaration,
		UnionDeclaration
	))
		override void visit(const T b)
		{
			mixin(scoped("inValidScope", "true"));
			mixin(scoped("inTemplate", "inTemplate || b.templateParameters !is null"));
			b.accept(this);
		}

	override void visit(const MixinTemplateDeclaration m)
	{
		// ignore mixin templates as they might be used for proper unittests
		if (m.templateDeclaration)
		{
			mixin(scoped("inValidScope", "true"));
			mixin(scoped("inTemplate", "false"));
			m.templateDeclaration.accept(this);
		}
	}

	override void visit(const FunctionDeclaration b)
	{
		mixin(scoped("inTemplate", "inTemplate || b.templateParameters !is null"));
		b.accept(this);
	}

	override void visit(const FunctionBody b)
	{
		mixin(scoped("inValidScope", "false"));
		mixin(scoped("inUndocumentable", "true"));
		b.accept(this);
	}

	override void visit(const TemplateDeclaration decl)
	{
		mixin(scoped("inTemplate", "true"));
		decl.accept(this);
	}

	override void visit(const Unittest test)
	{
		if (!inValidScope)
		{
			addErrorMessage(test.line, test.column, KEY_IGNORED, MESSAGE_IGNORED);
		}
		else if (inTemplate && inUndocumentable)
		{
			addErrorMessage(test.line, test.column, KEY_IGNORED, MESSAGE_IGNORED);
		}
		else if (inTemplate)
		{
			if (test.comment is null)
				addErrorMessage(test.line, test.column, KEY_TEMPLATED, MESSAGE_TEMPLATED);
		}

		if (!skipTests)
		{
			mixin(scoped("inValidScope", "false"));
			mixin(scoped("inUndocumentable", "true"));
			test.accept(this);
		}
	}

	alias visit = BaseAnalyzer.visit;
}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.ignored_unittest = Check.enabled;

	assertAnalyzerWarnings(q{
		unittest {}

		void foo() {
			unittest {} // [warn]: %s
		}

		void bar()() {
			unittest {} // [warn]: %s
		}

		class T1() {
			unittest {} // [warn]: %s
		}

		class T2() {
			///
			unittest {}
		}

		class C {
			unittest {}
		}

		unittest {
			unittest {} // [warn]: %s
		}

		void test1() {
			struct S {
				unittest {} // surprisingly, this gets called
			}
		}

		void test2()() {
			struct S {
				unittest {} // [warn]: %s
			}
		}

		void test2() {
			struct S() {
				unittest {} // [warn]: %s
			}
		}

		mixin template MT()
		{
			unittest { /* ok */ }
		}

		struct MixedStruct
		{
			mixin MT;
		}
	}c.format(
		IgnoredUnittestCheck.MESSAGE_IGNORED,
		IgnoredUnittestCheck.MESSAGE_IGNORED,
		IgnoredUnittestCheck.MESSAGE_TEMPLATED,
		IgnoredUnittestCheck.MESSAGE_IGNORED,
		IgnoredUnittestCheck.MESSAGE_IGNORED,
		IgnoredUnittestCheck.MESSAGE_IGNORED,
	), sac);

	stderr.writeln("Unittest for IgnoredUnittestCheck passed.");
}
