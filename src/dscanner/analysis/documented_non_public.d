// Copyright (c) 2018, dlang-community
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.documented_non_public;

import std.stdio;
import std.string;
import dparse.ast;
import dparse.lexer;
import dscanner.analysis.base;
import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
import dscanner.analysis.helpers;
import dscanner.analysis.common;

import dsymbol.scope_ : Scope;

/**
 * Checks for documented symbols that aren't public
 */
class DocumentedNonPublicCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;
	enum string NON_PUBLIC_MESSAGE = "`%s` has a Ddoc documentation, but isn't public";

	private bool isLastSeenVisibilityLabelPublic;

	///
	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	override void visit(const Module mod)
	{
		isLastSeenVisibilityLabelPublic = true;
		mod.accept(this);
	}

	override void visit(const Declaration decl)
	{
		import std.algorithm.searching : any;
		import std.algorithm.iteration : map;

		// skip private symbols
		enum tokPrivate = tok!"private",
			 tokProtected = tok!"protected",
			 tokPackage = tok!"package",
			 tokPublic = tok!"public";

		if (decl.attributes.length > 0)
		{
			const bool isPublic = !decl.attributes.map!`a.attribute`.any!(x => x == tokPrivate ||
																			   x == tokProtected ||
																			   x == tokPackage);
			// recognize label blocks
			if (isLabel(decl))
				isLastSeenVisibilityLabelPublic = isPublic;

			if (!isPublic)
				return;
		}

		if (isLastSeenVisibilityLabelPublic || decl.attributes.map!`a.attribute`.any!(x => x == tokPublic))
		{
			auto comment = hasComment(decl);
			if (comment.hasComment)
			{
				string message = NON_PUBLIC_MESSAGE.format(comment.name);
				addErrorMessage(comment.line, comment.column, "dscanner.analysis.documented_non_public", message);
			}
		}

		decl.accept(this);
	}

	auto hasComment(const Declaration decl)
	{
		import std.meta : AliasSeq;
		import std.typecons : Tuple;

		Tuple!(size_t, "line", size_t, "column", string, "name", bool, "hasComment") result;

		alias properties = AliasSeq!(
			//"aliasDeclaration",
		 	"classDeclaration",
			 //"constructor",
			 //"destructor",
		 	"enumDeclaration",
		 	"eponymousTemplateDeclaration",
		 	"functionDeclaration",
		 	"interfaceDeclaration",
			 //"invariant_",
			 //"sharedStaticConstructor",
			 //"sharedStaticDestructor",
			 //"staticConstructor",
			 //"staticDestructor",
		 	"structDeclaration",
		 	"templateDeclaration",
		 	"unionDeclaration",
			 //"unittest_",
		 	"variableDeclaration",
		);
		if (decl.declarations !is null)
			return result;

		static foreach (property; properties)
		{{
			mixin("auto d = decl." ~ property ~ ";");
			if (d !is null && d.comment.ptr !is null) {
				static if (__traits(compiles, d.name)) {
					const t = d.name;
				} else {
					const t = d.declarators[0].name;
				}
				result.name = t.text;
				result.line = t.line;
				result.column = t.column;
				result.hasComment = true;
				return result;
			}
		}}

		return result;
	}
}

unittest
{
	StaticAnalysisConfig sac = disabledConfig();
	sac.documented_non_public = Check.enabled;

	// https://github.com/dlang-community/D-Scanner/issues/493
	assertAnalyzerWarnings(q{
///
private int foo(){} // [warn]: %s

///
protected int foo(){} // [warn]: %s

///
package int foo(){} // [warn]: %s

///
int foo(){}

///
public int foo(){}
	}c.format(
		DocumentedNonPublicCheck.NON_PUBLIC_MESSAGE.format("foo"),
		DocumentedNonPublicCheck.NON_PUBLIC_MESSAGE.format("foo"),
		DocumentedNonPublicCheck.NON_PUBLIC_MESSAGE.format("foo"),
	), sac);
}

unittest
{
	StaticAnalysisConfig sac = disabledConfig();
	sac.documented_non_public = Check.enabled;

	assertAnalyzerWarnings(q{
protected:
///
int foo(){} // [warn]: %s
private:
///
int foo(){} // [warn]: %s
int bar(){}

	}c.format(
		DocumentedNonPublicCheck.NON_PUBLIC_MESSAGE.format("foo"),
		DocumentedNonPublicCheck.NON_PUBLIC_MESSAGE.format("foo"),
	), sac);

	assertAnalyzerWarnings(q{
protected {
	///
	int foo(){} // [warn]: %s

}
int bar(){}
private {
	///
	int foo(){} // [warn]: %s
}
int bar(){}

	}c.format(
		DocumentedNonPublicCheck.NON_PUBLIC_MESSAGE.format("foo"),
		DocumentedNonPublicCheck.NON_PUBLIC_MESSAGE.format("foo"),
	), sac);

	assertAnalyzerWarnings(q{

struct Foo {
	int bar(){}
	protected {
		///
		int foo(){} // [warn]: %s
	}
	int bar(){}
	private:
	///
	int foo(){} // [warn]: %s
	public:
	int bar(){}
}
	}c.format(
		DocumentedNonPublicCheck.NON_PUBLIC_MESSAGE.format("foo"),
		DocumentedNonPublicCheck.NON_PUBLIC_MESSAGE.format("foo"),
	), sac);

	stderr.writeln("Unittest for DocumentedNonPublicCheck passed.");
}
