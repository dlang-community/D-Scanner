// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module analysis.has_public_example;

import analysis.base;
import dsymbol.scope_ : Scope;
import dparse.ast;
import dparse.lexer;

import std.algorithm;
import std.stdio;

/**
 * Checks for public declarations without a documented unittests.
 * For now, variable and enum declarations aren't checked.
 */
class HasPublicExampleCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const Module mod)
	{
		// the last seen declaration is memorized
		Declaration lastDecl;

		// keep track of ddoced unittests after visiting lastDecl
		bool hasNoDdocUnittest;

		// on lastDecl reset we check for seen ddoced unittests since lastDecl was observed
		void checkLastDecl()
		{
			if (lastDecl !is null && hasNoDdocUnittest)
				triggerError(lastDecl);
			lastDecl = null;
		}

		// check all public top-level declarations
		foreach (decl; mod.declarations)
		{
			if (!isPublic(decl.attributes))
			{
				checkLastDecl();
				continue;
			}

			const bool hasDdocHeader = hasDdocHeader(decl);

			// check the documentation of a unittest declaration
			if (decl.unittest_ !is null)
			{
				if (hasDdocHeader)
					hasNoDdocUnittest = false;
			}
			// add all declarations that could be publicly documented to the lastDecl "stack"
			else if (hasDittableDecl(decl))
			{
				// ignore dittoed declarations
				if (hasDittos(decl))
					continue;

				// new public symbol -> check the previous decl
				checkLastDecl;

				lastDecl = hasDdocHeader ? cast(Declaration) decl : null;
				hasNoDdocUnittest = true;
			}
			else
			// ran into variableDeclaration or something else -> reset & validate current lastDecl "stack"
				checkLastDecl;
		}
		checkLastDecl;
	}

private:

	bool hasDitto(Decl)(const Decl decl)
	{
		import ddoc.comments : parseComment;
		if (decl is null || decl.comment is null)
			return false;

		return parseComment(decl.comment, null).isDitto;
	}

	bool hasDittos(Decl)(const Decl decl)
	{
		foreach (property; possibleDeclarations)
			if (mixin("hasDitto(decl." ~ property ~ ")"))
				return true;
		return false;
	}

	bool hasDittableDecl(Decl)(const Decl decl)
	{
		foreach (property; possibleDeclarations)
			if (mixin("decl." ~ property ~ " !is null"))
				return true;
		return false;
	}

	import std.meta : AliasSeq;
	alias possibleDeclarations = AliasSeq!(
		"classDeclaration",
		"enumDeclaration",
		"functionDeclaration",
		"interfaceDeclaration",
		"structDeclaration",
		"templateDeclaration",
		"unionDeclaration",
		//"variableDeclaration",
	);

	bool hasDdocHeader(const Declaration decl)
	{
		if (decl.declarations !is null)
			return false;

		// unittest can have ddoc headers as well, but don't have a name
		if (decl.unittest_ !is null && decl.unittest_.comment.ptr !is null)
			return true;

		foreach (property; possibleDeclarations)
			if (mixin("decl." ~ property ~ " !is null && decl." ~ property ~ ".comment.ptr !is null"))
				return true;

		return false;
	}

	bool isPublic(const Attribute[] attrs)
	{
		import dparse.lexer : tok;

		enum tokPrivate = tok!"private", tokProtected = tok!"protected", tokPackage = tok!"package";

		if (attrs.map!`a.attribute`.any!(x => x == tokPrivate || x == tokProtected || x == tokPackage))
			return false;

		return true;
	}

	void triggerError(const Declaration decl)
	{
		foreach (property; possibleDeclarations)
			if (auto fn = mixin("decl." ~ property))
				addMessage(fn.name.line, fn.name.column, fn.name.text);
	}

	void addMessage(size_t line, size_t column, string name)
	{
		import std.string : format;

		addErrorMessage(line, column, "dscanner.style.has_public_example", name is null
				? "Public declaration has no documented example."
				: format("Public declaration '%s' has no documented example.", name));
	}
}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import analysis.helpers : assertAnalyzerWarnings;

	StaticAnalysisConfig sac = disabledConfig();
	sac.has_public_example = Check.enabled;

	assertAnalyzerWarnings(q{
		/// C
		class C{}
		///
		unittest {}

		/// I
		interface I{}
		///
		unittest {}

		/// e
		enum e = 0;
		///
		unittest {}

		/// f
		void f(){}
		///
		unittest {}

		/// S
		struct S{}
		///
		unittest {}

		/// T
		template T(){}
		///
		unittest {}

		/// U
		union U{}
		///
		unittest {}
	}, sac);

	// enums or variables don't need to have public unittest
	assertAnalyzerWarnings(q{
		/// C
		class C{} // [warn]: Public declaration 'C' has no documented example.
		unittest {}

		/// I
		interface I{} // [warn]: Public declaration 'I' has no documented example.
		unittest {}

		/// f
		void f(){} // [warn]: Public declaration 'f' has no documented example.
		unittest {}

		/// S
		struct S{} // [warn]: Public declaration 'S' has no documented example.
		unittest {}

		/// T
		template T(){} // [warn]: Public declaration 'T' has no documented example.
		unittest {}

		/// U
		union U{} // [warn]: Public declaration 'U' has no documented example.
		unittest {}
	}, sac);

	// test module header unittest
	assertAnalyzerWarnings(q{
		unittest {}
		/// C
		class C{} // [warn]: Public declaration 'C' has no documented example.
	}, sac);

	// test documented module header unittest
	assertAnalyzerWarnings(q{
		///
		unittest {}
		/// C
		class C{} // [warn]: Public declaration 'C' has no documented example.
	}, sac);

	// test multiple unittest blocks
	assertAnalyzerWarnings(q{
		/// C
		class C{} // [warn]: Public declaration 'C' has no documented example.
		unittest {}
		unittest {}
		unittest {}

		/// U
		union U{}
		unittest {}
		///
		unittest {}
		unittest {}
	}, sac);

	/// check private
	assertAnalyzerWarnings(q{
		/// C
		private class C{}

		/// I
		protected interface I{}

		/// e
		package enum e = 0;

		/// f
		package(std) void f(){}

		/// S
		extern(C) struct S{}
		///
		unittest {}
	}, sac);

	// check intermediate private declarations
	// removed for issue #500
	/*assertAnalyzerWarnings(q{
		/// C
		class C{}
		private void foo(){}
		///
		unittest {}
	}, sac);*/

	// check intermediate ditto-ed declarations
	assertAnalyzerWarnings(q{
		/// I
		interface I{}
		/// ditto
		void f(){}
		///
		unittest {}
	}, sac);

	// test reset on private symbols (#500)
	assertAnalyzerWarnings(q{
		///
		void dirName(C)(C[] path) {} // [warn]: Public declaration 'dirName' has no documented example.
		private void _dirName(R)(R path) {}
		///
		unittest {}
	}, sac);

	stderr.writeln("Unittest for HasPublicExampleCheck passed.");
}

