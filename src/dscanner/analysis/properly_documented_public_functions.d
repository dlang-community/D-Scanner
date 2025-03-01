// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.properly_documented_public_functions;

import dscanner.analysis.base;
import std.format : format;
import std.range.primitives;
import std.conv : to;
import std.algorithm.searching : canFind, any, find;
import dmd.astcodegen;

/**
 * Requires each public function to contain the following ddoc sections
	- PARAMS:
		- if the function has at least one parameter
		- every parameter must have a ddoc params entry (applies for template paramters too)
		- Ddoc params entries without a parameter trigger warnings as well
	- RETURNS: (except if it's void, only functions)
 */
extern(C++) class ProperlyDocumentedPublicFunctions(AST) : BaseAnalyzerDmd
{
	enum string MISSING_PARAMS_KEY = "dscanner.style.doc_missing_params";
	enum string MISSING_PARAMS_MESSAGE = "Parameter %s isn't documented in the `Params` section.";
	enum string MISSING_TEMPLATE_PARAMS_MESSAGE
		= "Template parameters %s isn't documented in the `Params` section.";

	enum string NON_EXISTENT_PARAMS_KEY = "dscanner.style.doc_non_existing_params";
	enum string NON_EXISTENT_PARAMS_MESSAGE = "Documented parameter %s isn't a function parameter.";

	enum string MISSING_RETURNS_KEY = "dscanner.style.doc_missing_returns";
	enum string MISSING_RETURNS_MESSAGE = "A public function needs to contain a `Returns` section.";

	enum string MISSING_THROW_KEY = "dscanner.style.doc_missing_throw";
	enum string MISSING_THROW_MESSAGE = "An instance of `%s` is thrown but not documented in the `Throws` section";

	mixin AnalyzerInfo!"properly_documented_public_functions";
	alias visit = BaseAnalyzerDmd.visit;

	extern(D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.Module m)
	{
		super.visit(m);
		postCheckSeenDdocParams();
	}

	override void visit(AST.Catch c)
	{
		import std.algorithm.iteration : filter;
		import std.array : array;

		thrown = thrown.filter!(a => a != to!string(c.type.toChars())).array;
		super.visit(c);
	}


	override void visit(AST.ThrowStatement t)
	{
		AST.NewExp ne = t.exp.isNewExp();
		if (ne)
			thrown ~= to!string(ne.newtype.toChars());
	
		super.visit(t);
	}

	override void visit(AST.FuncDeclaration d)
	{
		nestedFunc++;
		scope (exit)
			nestedFunc--;

		import std.stdio : writeln, writefln;
		import std.conv : to;
		import std.algorithm.searching : canFind, any, find;
		import dmd.dsymbol : Visibility;
		import dmd.mtype : Type;
		import ddoc.comments : parseComment;
		import std.algorithm.iteration : map;
		import std.array : array;

		if (d.comment is null || d.fbody is null || d.visibility.kind != Visibility.Kind.public_)
		{
			super.visit(d);
			return;
		}

		if (nestedFunc == 1)
		{
			thrown.length = 0;
			string[] params;

			if (d.parameters) foreach (p; *d.parameters)
				params ~= to!string(p.ident.toString());

			auto comment = setLastDdocParams(d.loc.linnum, d.loc.charnum, to!string(d.comment));
			checkDdocParams(d.loc.linnum, d.loc.charnum, params, null);

			auto tf = d.type.isTypeFunction();
			if (tf && tf.next != Type.tvoid && d.comment
				&& !comment.isDitto && !comment.sections.any!(s => s.name == "Returns"))
					addErrorMessage(cast(ulong) d.loc.linnum, cast(ulong) d.loc.charnum,
								MISSING_RETURNS_KEY, MISSING_RETURNS_MESSAGE);
		}
		
		super.visit(d);
		if (nestedFunc == 1)
			foreach (t; thrown)
				if (!hasThrowSection(to!string(d.comment)))
					addErrorMessage(cast(ulong) d.loc.linnum, cast(ulong) d.loc.charnum,
									MISSING_THROW_KEY, MISSING_THROW_MESSAGE.format(t));
	}

	override void visit(AST.TemplateDeclaration d)
	{
		import dmd.dsymbol : Visibility;
		import ddoc.comments : parseComment;
		import std.algorithm.iteration : map, filter;
		import std.algorithm.searching : find, canFind;
		import std.array : array;

		if (d.comment is null)
			return;

		// A `template` inside another public `template` declaration will have visibility undefined
		// Check that as well as it's part of the public template
		if ((d.visibility.kind != Visibility.Kind.public_)
			&& !(d.visibility.kind == Visibility.Kind.undefined && withinTemplate))
				return;

		if (d.visibility.kind == Visibility.Kind.public_)
		{
			setLastDdocParams(d.loc.linnum, d.loc.charnum, to!string(d.comment));
			withinTemplate = true;
			funcParams.length = 0;
			templateParams.length = 0;
		}

		foreach (p; *d.origParameters)
			if (!canFind(templateParams, to!string(p.ident.toString())))
				templateParams ~= to!string(p.ident.toString());

		super.visit(d);

		if (d.visibility.kind == Visibility.Kind.public_)
		{
			withinTemplate = false;
			checkDdocParams(d.loc.linnum, d.loc.charnum, funcParams, templateParams);
		}
	}

	/** 
	 * Look for: foo(T)(T x)
	 * In that case, T does not have to be documented, because x must be.
	 */
	override bool visitEponymousMember(AST.TemplateDeclaration d)
    {
		import ddoc.comments : parseComment;
		import std.algorithm.searching : canFind, any, find;
		import std.algorithm.iteration : map, filter;
		import std.array : array;

        if (!d.members || d.members.length != 1)
            return false;
        AST.Dsymbol onemember = (*d.members)[0];
        if (onemember.ident != d.ident)
            return false;

        if (AST.FuncDeclaration fd = onemember.isFuncDeclaration())
        {
			const comment = parseComment(to!string(d.comment), null);
			const paramSection = comment.sections.find!(s => s.name == "Params");
			auto tf = fd.type.isTypeFunction();

			if (tf)
				foreach (idx, p; tf.parameterList)
				{

					if (!paramSection.empty &&
						!canFind(paramSection[0].mapping.map!(a => a[0]).array, to!string(p.ident.toString())) &&
						!canFind(funcParams, to!string(p.ident.toString())))
							funcParams ~= to!string(p.ident.toString());

					lastSeenFun.params[to!string(p.ident.toString())] = true;

					auto ti = p.type.isTypeIdentifier();
					if (ti is null)
						continue;

					templateParams = templateParams.filter!(a => a != to!string(ti.ident.toString())).array;
					lastSeenFun.params[to!string(ti.ident.toString())] = true;
				}
            return true;
        }

        if (AST.AggregateDeclaration ad = onemember.isAggregateDeclaration())
            return true;

        if (AST.VarDeclaration vd = onemember.isVarDeclaration())
        {
            if (d.constraint)
                return false;
            
			if (vd._init)
                return true;
        }

        return false;
    }

	extern(D) auto setLastDdocParams(size_t line, size_t column, string commentText)
	{
		import ddoc.comments : parseComment;
		import std.algorithm.searching : find;
		import std.algorithm.iteration : map;
		import std.array : array;

		const comment = parseComment(commentText, null);
		if (withinTemplate)
		{
			const paramSection = comment.sections.find!(s => s.name == "Params");
			if (!paramSection.empty)
				lastSeenFun.ddocParams ~= paramSection[0].mapping.map!(a => a[0]).array;
		}
		else if (!comment.isDitto)
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

	/** 
	 * 
	 * Params:
	 *   line = Line of the public declaration verified
	 *   column = Column of the public declaration verified
	 *   params = Funcion parameters that must be documented
	 *   templateParams = Template parameters that must be documented.
	 *			Can be null if we are looking at a regular FuncDeclaration
	 */
	extern(D) void checkDdocParams(size_t line, size_t column, string[] params, string[] templateParams)
	{
		import std.array : array;
		import std.algorithm.searching : canFind, countUntil;
		import std.algorithm.iteration : map;
		import std.algorithm.mutation : remove;
		import std.range : indexed, iota;

		if (lastSeenFun.active && !params.empty)
			foreach (p; params)
			{

				if (!lastSeenFun.ddocParams.canFind(p))
					addErrorMessage(line, column, MISSING_PARAMS_KEY,
						format(MISSING_PARAMS_MESSAGE, p));
				else
					lastSeenFun.params[p] = true;
			}

		checkDdocParams(line, column, templateParams);
	}

	extern(D) void checkDdocParams(size_t line, size_t column, string[] templateParams)
	{
		import std.algorithm.searching : canFind;
		foreach (p; templateParams)
		{
			if (!lastSeenFun.ddocParams.canFind(p))
				addErrorMessage(line, column, MISSING_PARAMS_KEY,
					format(MISSING_TEMPLATE_PARAMS_MESSAGE, p));
			else
				lastSeenFun.params[p] = true;
		}
	}

	extern(D) bool hasThrowSection(string commentText)
	{
		import std.algorithm.searching : canFind;
		import ddoc.comments : parseComment;

		const comment = parseComment(commentText, null);
		return comment.isDitto || comment.sections.canFind!(s => s.name == "Throws");
	}
	
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

	private enum KEY = "dscanner.performance.enum_array_literal";
	int nestedFunc;
	int withinTemplate;

	extern(D) string[] funcParams;
	extern(D) string[] templateParams;
	extern(D) string[] thrown;

	static struct Function
	{
		bool active;
		size_t line, column;
		// All params documented
		const(string)[] ddocParams;
		// Stores actual function params that are also documented
		bool[string] params;
	}
	Function lastSeenFun;
}

version(unittest)
{
	import std.stdio : stderr;
	import std.format : format;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarnings = assertAnalyzerWarningsDMD;
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
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_PARAMS_MESSAGE.format("k")
	), sac, true);

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		void foo(int K)(){} // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_TEMPLATE_PARAMS_MESSAGE.format("K")
	), sac, true);

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		struct Foo(Bar){} // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_TEMPLATE_PARAMS_MESSAGE.format("Bar")
	), sac, true);

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		class Foo(Bar){} // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_TEMPLATE_PARAMS_MESSAGE.format("Bar")
	), sac, true);

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		template Foo(Bar){} // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_TEMPLATE_PARAMS_MESSAGE.format("Bar")
	), sac, true);


	// test no parameters
	assertAnalyzerWarnings(q{
		/** Some text */
		void foo(){}
	}c, sac, true);

	assertAnalyzerWarnings(q{
		/** Some text */
		struct Foo(){}
	}c, sac, true);

	assertAnalyzerWarnings(q{
		/** Some text */
		class Foo(){}
	}c, sac, true);

	assertAnalyzerWarnings(q{
		/** Some text */
		template Foo(){}
	}c, sac, true);

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
		int foo(){ return 0; } // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_RETURNS_MESSAGE,
	), sac, true);

	assertAnalyzerWarnings(q{
		/**
		Some text
		*/
		auto foo(){ return 0; } // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_RETURNS_MESSAGE,
	), sac, true);
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
	}c, sac, true);

	// with block
	assertAnalyzerWarnings(q{
	private:
		/**
		Some text
		*/
		private void foo(int k){}
		///
		public int bar(){ return 0; } // [warn]: %s
	public:
		///
		int foobar(){ return 0; } // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_RETURNS_MESSAGE,
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_RETURNS_MESSAGE,
	), sac, true);

	// with block (template)
	assertAnalyzerWarnings(q{
	private:
		/**
		Some text
		*/
		private template foo(int k){}
		///
		public template bar(T){} // [warn]: %s
	public:
		///
		template foobar(T){} // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_TEMPLATE_PARAMS_MESSAGE.format("T"),
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_TEMPLATE_PARAMS_MESSAGE.format("T"),
	), sac, true);

	// with block (struct)
	assertAnalyzerWarnings(q{
	private:
		/**
		Some text
		*/
		private struct foo(int k){}
		///
		public struct bar(T){} // [warn]: %s
	public:
		///
		struct foobar(T){} // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_TEMPLATE_PARAMS_MESSAGE.format("T"),
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_TEMPLATE_PARAMS_MESSAGE.format("T"),
	), sac, true);
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
int foo(int k){ return 0; } // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_PARAMS_MESSAGE.format("k")
	), sac, true);

	assertAnalyzerWarnings(q{
/**
 * Description.
 *
 * Params:
 *
 * Returns:
 * A long description.
 */
int foo(int k) => k; // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_PARAMS_MESSAGE.format("k")
	), sac, true);

	assertAnalyzerWarnings(q{
/**
Description.

Params:
val =  A stupid parameter
k = A stupid parameter

Returns:
A long description.
*/
int foo(int k){ return 0; } // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).NON_EXISTENT_PARAMS_MESSAGE.format("val")
	), sac, true);

	assertAnalyzerWarnings(q{
/**
Description.

Params:

Returns:
A long description.
*/
int foo(int k){ return 0; } // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_PARAMS_MESSAGE.format("k")
	), sac, true);

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
int foo(int foo, int foobar){ return 0; } // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).NON_EXISTENT_PARAMS_MESSAGE.format("bad")
	), sac, true);

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
		(ProperlyDocumentedPublicFunctions!ASTCodegen).NON_EXISTENT_PARAMS_MESSAGE.format("bad")
	), sac, true);

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
int foo(int foo, int bar){ return 0; }
	}c, sac, true);

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
	}c, sac, true);
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
int foo(int k){ return 0; }

/// ditto
int bar(int k){ return 0; }
	}c, sac, true);

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
int foo(int k){ return 0; }

/// ditto
struct Bar(K){}
	}c, sac, true);

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
int foo(int k){ return 0; }

/// ditto
int bar(int f){ return 0; }
	}c, sac, true);

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
int foo(int k){ return 0; }

/// ditto
int bar(int bar){ return 0; } // [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_PARAMS_MESSAGE.format("bar")
	), sac, true);

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
int foo(int k){ return 0; } // [warn]: %s

/// ditto
int bar(int bar){ return 0; }
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).NON_EXISTENT_PARAMS_MESSAGE.format("f")
	), sac, true);
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
string bar(string val){ return ""; }
	}c, sac, true);

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
	}c, sac, true);

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
	///
	auto abcde(T, U...)(T t, U varargs) {
		/// ....
	}
}
	}c, sac, true);
}

// Don't force the documentation of the template parameter if it's a used type in the parameter list
unittest
{
	StaticAnalysisConfig sac = disabledConfig;
	sac.properly_documented_public_functions = Check.enabled;

	assertAnalyzerWarnings(q{
/++
An awesome description.

Params:
	r =  an input range.

Returns: Awesome values.
+/
string bar(R)(R r){}
	}c, sac, true);

	assertAnalyzerWarnings(q{
/++
An awesome description.

Params:
	r =  an input range.

Returns: Awesome values.
+/
string bar(P, R)(R r){}// [warn]: %s
	}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_TEMPLATE_PARAMS_MESSAGE.format("P")
	), sac, true);
}

// https://github.com/dlang-community/D-Scanner/issues/601
unittest
{
	StaticAnalysisConfig sac = disabledConfig;
	sac.properly_documented_public_functions = Check.enabled;

	assertAnalyzerWarnings(q{
	void put(Range)(Range items) if (canPutConstRange!Range)
	{
		alias p = put!(Unqual!Range);
		p(items);
	}
	}, sac, true);
}

unittest
{
	StaticAnalysisConfig sac = disabledConfig;
	sac.properly_documented_public_functions = Check.enabled;

	assertAnalyzerWarnings(q{
    /++
    An awesome description.

    Params:
	    items = things to put.

    Returns: Awesome values.
    +/
	void put(Range)(const(Range) items) if (canPutConstRange!Range)
	{}
	}, sac, true);
}

unittest
{
	StaticAnalysisConfig sac = disabledConfig;
	sac.properly_documented_public_functions = Check.enabled;

	assertAnalyzerWarnings(q{
class AssertError : Error
{
    this(string msg) { super(msg); }
}

/++
Throw but likely catched.
+/
void bar1(){
	try{throw new Exception("bla");throw new Error("bla");}
	catch(Exception){} catch(Error){}}

/++
Simple case
+/
	void bar2(){throw new Exception("bla");} // [warn]: %s

/++
Supposed to be documented

Throws: Exception if...
+/
void bar3(){throw new Exception("bla");}

/++
rethrow
+/
void bar4(){try throw new Exception("bla"); catch(Exception) throw new Error("bla");} // [warn]: %s

/++
trust nothrow before everything
+/
void bar5() nothrow {try throw new Exception("bla"); catch(Exception) assert(0);}

/++
case of throw in nested func
+/
void bar6() // [warn]: %s
{
	void foo(){throw new AssertError("bla");}
	foo();
}

/++
case of throw in nested func but caught
+/
void bar7()
{
	void foo(){throw new AssertError("bla");}
	try foo();
	catch (AssertError){}
}

/++
case of double throw in nested func but only 1 caught
+/
void bar8() // [warn]: %s
{
	void foo(){throw new AssertError("bla");throw new Error("bla");}
	try foo();
	catch (Error){}
}}c.format(
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_THROW_MESSAGE.format("object.Exception"),
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_THROW_MESSAGE.format("object.Error"),
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_THROW_MESSAGE
			.format("properly_documented_public_functions.AssertError"),
		(ProperlyDocumentedPublicFunctions!ASTCodegen).MISSING_THROW_MESSAGE
			.format("properly_documented_public_functions.AssertError")
	), sac, true);
}

// https://github.com/dlang-community/D-Scanner/issues/583
unittest
{
	StaticAnalysisConfig sac = disabledConfig;
	sac.properly_documented_public_functions = Check.enabled;

	assertAnalyzerWarnings(q{
	/++
	Implements the homonym function (also known as `accumulate`)
	Returns:
		the accumulated `result`
	Params:
		fun = one or more functions
	+/
	template reduce(fun...)
	if (fun.length >= 1)
	{
		/++
		No-seed version. The first element of `r` is used as the seed's value.
		Params:
			r = an iterable value as defined by `isIterable`
		Returns:
			the final result of the accumulator applied to the iterable
		+/
		auto reduce(R)(R r){}
	}
	}c.format(
	), sac, true);

	stderr.writeln("Unittest for ProperlyDocumentedPublicFunctions passed.");
}
