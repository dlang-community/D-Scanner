// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.properly_documented_public_functions;

import dparse.lexer;
import dparse.ast;
import analysis.base : BaseAnalyzer;

import std.format : format;
import std.range.primitives;
import std.stdio;

/**
 * Requires each public function to contain the following ddoc sections
	- PARAMS:
		- if the function has at least one parameter
		- every parameter must have a ddoc params entry (applies for template paramters too)
		- Ddoc params entries without a parameter trigger warnings as well
	- RETURNS: (except if it's void, only functions)
 */
class ProperlyDocumentedPublicFunctions : BaseAnalyzer
{
	enum string MISSING_PARAMS_KEY = "dscanner.style.doc_missing_params";
	enum string MISSING_PARAMS_MESSAGE = "Parameter %s isn't documented in the `Params` section.";
	enum string MISSING_TEMPLATE_PARAMS_MESSAGE
		= "Template parameters %s isn't documented in the `Params` section.";

	enum string NON_EXISTENT_PARAMS_KEY = "dscanner.style.doc_non_existing_params";
	enum string NON_EXISTENT_PARAMS_MESSAGE = "Documented parameter %s isn't a function parameter.";

	enum string MISSING_RETURNS_KEY = "dscanner.style.doc_missing_returns";
	enum string MISSING_RETURNS_MESSAGE = "A public function needs to contain a `Returns` section.";

	///
	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
	}

	override void visit(const Module mod)
	{
		islastSeenVisibilityLabelPublic = true;
		mod.accept(this);
		postCheckSeenDdocParams();
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
			if (!hasDeclaration(decl))
				islastSeenVisibilityLabelPublic = isPublic;

			if (!isPublic)
				return;
		}

		if (islastSeenVisibilityLabelPublic || decl.attributes.map!`a.attribute`.any!(x => x == tokPublic))
		{
			if (decl.functionDeclaration !is null ||
				decl.templateDeclaration !is null ||
				decl.mixinTemplateDeclaration !is null ||
				decl.classDeclaration !is null ||
				decl.structDeclaration !is null)
					decl.accept(this);
		}
	}

	override void visit(const TemplateDeclaration decl)
	{
		setLastDdocParams(decl.name.line, decl.name.column, decl.comment);
		checkDdocParams(decl.name.line, decl.name.column, decl.templateParameters);

		withinTemplate = true;
		scope(exit) withinTemplate = false;
		decl.accept(this);
	}

	override void visit(const MixinTemplateDeclaration decl)
	{
		decl.accept(this);
	}

	override void visit(const StructDeclaration decl)
	{
		setLastDdocParams(decl.name.line, decl.name.column, decl.comment);
		checkDdocParams(decl.name.line, decl.name.column, decl.templateParameters);
		decl.accept(this);
	}

	override void visit(const ClassDeclaration decl)
	{
		setLastDdocParams(decl.name.line, decl.name.column, decl.comment);
		checkDdocParams(decl.name.line, decl.name.column, decl.templateParameters);
		decl.accept(this);
	}

	override void visit(const FunctionDeclaration decl)
	{
		import std.algorithm.searching : any;

		// ignore header declaration for now
		if (decl.functionBody is null)
			return;

		auto comment = setLastDdocParams(decl.name.line, decl.name.column, decl.comment);
		checkDdocParams(decl.name.line, decl.name.column, decl.parameters);
		checkDdocParams(decl.name.line, decl.name.column, decl.templateParameters);

		enum voidType = tok!"void";

		if (decl.returnType is null || decl.returnType.type2.builtinType != voidType)
			if (!(comment.isDitto || withinTemplate || comment.sections.any!(s => s.name == "Returns")))
				addErrorMessage(decl.name.line, decl.name.column, MISSING_RETURNS_KEY, MISSING_RETURNS_MESSAGE);
	}

	alias visit = BaseAnalyzer.visit;

private:
	bool islastSeenVisibilityLabelPublic;
	bool withinTemplate;

	static struct Function
	{
		bool active;
		size_t line, column;
		const(string)[] ddocParams;
		bool[string] params;
	}
	Function lastSeenFun;

	// find invalid ddoc parameters (i.e. they don't occur in a function declaration)
	void postCheckSeenDdocParams()
	{
		import std.format : format;

		if (lastSeenFun.active)
		foreach (p; lastSeenFun.ddocParams)
			if (p !in lastSeenFun.params)
				addErrorMessage(lastSeenFun.line, lastSeenFun.column, NON_EXISTENT_PARAMS_KEY,
					NON_EXISTENT_PARAMS_MESSAGE.format(p));

		lastSeenFun.active = false;
	}

	auto setLastDdocParams(size_t line, size_t column, string commentText)
	{
		import ddoc.comments : parseComment;
		import std.algorithm.searching : find;
		import std.algorithm.iteration : map;
		import std.array : array;

		const comment = parseComment(commentText, null);
		if (!comment.isDitto && !withinTemplate)
		{
			// check old function for invalid ddoc params
			if (lastSeenFun.active)
				postCheckSeenDdocParams();

			const paramSection = comment.sections.find!(s => s.name == "Params");
			if (paramSection.empty)
			{
				lastSeenFun = Function(true, line, column, null);
			}
			else
			{
				auto ddocParams = paramSection[0].mapping.map!(a => a[0]).array;
				lastSeenFun = Function(true, line, column, ddocParams);
			}
		}

		return comment;
	}

	void checkDdocParams(size_t line, size_t column, const Parameters params)
	{
		import std.algorithm.searching : canFind;

		if (lastSeenFun.active && params !is null)
			foreach (p; params.parameters)
			{
				if (!lastSeenFun.ddocParams.canFind(p.name.text))
					addErrorMessage(line, column, MISSING_PARAMS_KEY,
						format(MISSING_PARAMS_MESSAGE, p.name.text));
				else
					lastSeenFun.params[p.name.text] = true;
			}
	}

	void checkDdocParams(size_t line, size_t column, const TemplateParameters templateParams)
	{
		import std.algorithm.searching : canFind;

		if (lastSeenFun.active && templateParams !is null && templateParams.templateParameterList !is null)
			foreach (p; templateParams.templateParameterList.items)
			{
				auto name = templateParamName(p);
				assert(name, "Invalid template parameter name."); // this shouldn't happen
				if (!lastSeenFun.ddocParams.canFind(name))
					addErrorMessage(line, column, MISSING_PARAMS_KEY,
						format(MISSING_TEMPLATE_PARAMS_MESSAGE, name));
				else
					lastSeenFun.params[name] = true;
			}
	}

	static string templateParamName(const TemplateParameter p)
	{
		if (p.templateTypeParameter)
			return p.templateTypeParameter.identifier.text;
   		if (p.templateValueParameter)
			return p.templateValueParameter.identifier.text;
   		if (p.templateAliasParameter)
			return p.templateAliasParameter.identifier.text;
   		if (p.templateTupleParameter)
			return p.templateTupleParameter.identifier.text;
   		if (p.templateThisParameter)
			return p.templateThisParameter.templateTypeParameter.identifier.text;

		return null;
	}

	bool hasDeclaration(const Declaration decl)
	{
		import std.meta : AliasSeq;
		alias properties = AliasSeq!(
			"aliasDeclaration",
		 	"aliasThisDeclaration",
		 	"anonymousEnumDeclaration",
		 	"attributeDeclaration",
		 	"classDeclaration",
		 	"conditionalDeclaration",
		 	"constructor",
		 	"debugSpecification",
		 	"destructor",
		 	"enumDeclaration",
		 	"eponymousTemplateDeclaration",
		 	"functionDeclaration",
		 	"importDeclaration",
		 	"interfaceDeclaration",
		 	"invariant_",
		 	"mixinDeclaration",
		 	"mixinTemplateDeclaration",
		 	"postblit",
		 	"pragmaDeclaration",
		 	"sharedStaticConstructor",
		 	"sharedStaticDestructor",
		 	"staticAssertDeclaration",
		 	"staticConstructor",
		 	"staticDestructor",
		 	"structDeclaration",
		 	"templateDeclaration",
		 	"unionDeclaration",
		 	"unittest_",
		 	"variableDeclaration",
		 	"versionSpecification",
		);
		if (decl.declarations !is null)
			return false;

		auto isNull = true;
		foreach (property; properties)
			if (mixin("decl." ~ property ~ " !is null"))
				isNull = false;

		return !isNull;
	}
}

version(unittest)
{
	import std.stdio : stderr;
	import std.format : format;
	import analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import analysis.helpers : assertAnalyzerWarnings;
}

// missing params
unittest
{
	StaticAnalysisConfig sac = disabledConfig;
	sac.properly_documented_public_functions = Check.enabled;

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		void foo(int k){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_PARAMS_MESSAGE.format("k")
	), sac);

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		void foo(int K)(){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_TEMPLATE_PARAMS_MESSAGE.format("K")
	), sac);

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		struct Foo(Bar){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_TEMPLATE_PARAMS_MESSAGE.format("Bar")
	), sac);

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		class Foo(Bar){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_TEMPLATE_PARAMS_MESSAGE.format("Bar")
	), sac);

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		template Foo(Bar){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_TEMPLATE_PARAMS_MESSAGE.format("Bar")
	), sac);


	// test no parameters
	assertAnalyzerWarnings(q{
		/** Some text */
		void foo(){}
	}c, sac);

	assertAnalyzerWarnings(q{
		/** Some text */
		struct Foo(){}
	}c, sac);

	assertAnalyzerWarnings(q{
		/** Some text */
		class Foo(){}
	}c, sac);

	assertAnalyzerWarnings(q{
		/** Some text */
		template Foo(){}
	}c, sac);

}

// missing returns (only functions)
unittest
{
	StaticAnalysisConfig sac = disabledConfig;
	sac.properly_documented_public_functions = Check.enabled;

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		int foo(){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_RETURNS_MESSAGE,
	), sac);

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		auto foo(){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_RETURNS_MESSAGE,
	), sac);
}

// ignore private
unittest
{
	StaticAnalysisConfig sac = disabledConfig;
	sac.properly_documented_public_functions = Check.enabled;

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		private void foo(int k){}
	}c, sac);

	// with block
	assertAnalyzerWarnings(q{
	private:
		/**
		Some text
		*/
		private void foo(int k){}
		public int bar(){} // [warn]: %s
	public:
		int foobar(){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_RETURNS_MESSAGE,
		ProperlyDocumentedPublicFunctions.MISSING_RETURNS_MESSAGE,
	), sac);

	// with block (template)
	assertAnalyzerWarnings(q{
	private:
		/**
		Some text
		*/
		private template foo(int k){}
		public template bar(T){} // [warn]: %s
	public:
		template foobar(T){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_TEMPLATE_PARAMS_MESSAGE.format("T"),
		ProperlyDocumentedPublicFunctions.MISSING_TEMPLATE_PARAMS_MESSAGE.format("T"),
	), sac);

	// with block (struct)
	assertAnalyzerWarnings(q{
	private:
		/**
		Some text
		*/
		private struct foo(int k){}
		public struct bar(T){} // [warn]: %s
	public:
		struct foobar(T){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_TEMPLATE_PARAMS_MESSAGE.format("T"),
		ProperlyDocumentedPublicFunctions.MISSING_TEMPLATE_PARAMS_MESSAGE.format("T"),
	), sac);
}

// test parameter names
unittest
{
	StaticAnalysisConfig sac = disabledConfig;
	sac.properly_documented_public_functions = Check.enabled;

	assertAnalyzerWarnings(q{
/**
 * Description.
 *
 * Params:
 *
 * Returns:
 * A long description.
 */
int foo(int k){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_PARAMS_MESSAGE.format("k")
	), sac);

	assertAnalyzerWarnings(q{
/**
Description.

Params:
val =  A stupid parameter
k = A stupid parameter

Returns:
A long description.
*/
int foo(int k){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.NON_EXISTENT_PARAMS_MESSAGE.format("val")
	), sac);

	assertAnalyzerWarnings(q{
/**
Description.

Params:

Returns:
A long description.
*/
int foo(int k){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_PARAMS_MESSAGE.format("k")
	), sac);

	assertAnalyzerWarnings(q{
/**
Description.

Params:
foo =  A stupid parameter
bad =  A stupid parameter (does not exist)
foobar  = A stupid parameter

Returns:
A long description.
*/
int foo(int foo, int foobar){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.NON_EXISTENT_PARAMS_MESSAGE.format("bad")
	), sac);

	assertAnalyzerWarnings(q{
/**
Description.

Params:
foo =  A stupid parameter
bad =  A stupid parameter (does not exist)
foobar  = A stupid parameter

Returns:
A long description.
*/
struct foo(int foo, int foobar){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.NON_EXISTENT_PARAMS_MESSAGE.format("bad")
	), sac);

	// properly documented
	assertAnalyzerWarnings(q{
/**
Description.

Params:
foo =  A stupid parameter
bar  = A stupid parameter

Returns:
A long description.
*/
int foo(int foo, int bar){}
	}c, sac);

	assertAnalyzerWarnings(q{
/**
Description.

Params:
foo =  A stupid parameter
bar  = A stupid parameter

Returns:
A long description.
*/
struct foo(int foo, int bar){}
	}c, sac);
}

// support ditto
unittest
{
	StaticAnalysisConfig sac = disabledConfig;
	sac.properly_documented_public_functions = Check.enabled;

	assertAnalyzerWarnings(q{
/**
 * Description.
 *
 * Params:
 * k =  A stupid parameter
 *
 * Returns:
 * A long description.
 */
int foo(int k){}

/// ditto
int bar(int k){}
	}c, sac);

	assertAnalyzerWarnings(q{
/**
 * Description.
 *
 * Params:
 * k =  A stupid parameter
 * K =  A stupid parameter
 *
 * Returns:
 * A long description.
 */
int foo(int k){}

/// ditto
struct Bar(K){}
	}c, sac);

	assertAnalyzerWarnings(q{
/**
 * Description.
 *
 * Params:
 * k =  A stupid parameter
 * f =  A stupid parameter
 *
 * Returns:
 * A long description.
 */
int foo(int k){}

/// ditto
int bar(int f){}
	}c, sac);

	assertAnalyzerWarnings(q{
/**
 * Description.
 *
 * Params:
 * k =  A stupid parameter
 *
 * Returns:
 * A long description.
 */
int foo(int k){}

/// ditto
int bar(int bar){} // [warn]: %s
	}c.format(
		ProperlyDocumentedPublicFunctions.MISSING_PARAMS_MESSAGE.format("bar")
	), sac);

	assertAnalyzerWarnings(q{
/**
 * Description.
 *
 * Params:
 * k =  A stupid parameter
 * bar =  A stupid parameter
 * f =  A stupid parameter
 *
 * Returns:
 * A long description.
 * See_Also:
 *	$(REF takeExactly, std,range)
 */
int foo(int k){} // [warn]: %s

/// ditto
int bar(int bar){}
	}c.format(
		ProperlyDocumentedPublicFunctions.NON_EXISTENT_PARAMS_MESSAGE.format("f")
	), sac);
}

 // check correct ddoc headers
unittest
{
	StaticAnalysisConfig sac = disabledConfig;
	sac.properly_documented_public_functions = Check.enabled;

	assertAnalyzerWarnings(q{
/++
    Counts elements in the given
    $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
    until the given predicate is true for one of the given $(D needles).

    Params:
		val  =  A stupid parameter

    Returns: Awesome values.
  +/
string bar(string val){}
	}c, sac);

	assertAnalyzerWarnings(q{
/++
    Counts elements in the given
    $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
    until the given predicate is true for one of the given $(D needles).

    Params:
		val  =  A stupid parameter

    Returns: Awesome values.
  +/
template bar(string val){}
	}c, sac);

	stderr.writeln("Unittest for ProperlyDocumentedPublicFunctions passed.");
}

unittest
{
	StaticAnalysisConfig sac = disabledConfig;
	sac.properly_documented_public_functions = Check.enabled;

	assertAnalyzerWarnings(q{
/**
 * Ddoc for the inner function appears here.
 * This function is declared this way to allow for multiple variable-length
 * template argument lists.
 * ---
 * abcde!("a", "b", "c")(100, x, y, z);
 * ---
 * Params:
 *    Args = foo
 *    U = bar
 *    T = barr
 *    varargs = foobar
 *    t = foo
 * Returns: bar
 */
template abcde(Args ...) {
    auto abcde(T, U...)(T t, U varargs) {
        /// ....
    }
}
	}c, sac);
}
