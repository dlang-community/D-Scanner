//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.pokemon;

import std.stdio;
import dscanner.analysis.base;
import dscanner.analysis.helpers;

/**
 * Checks for Pokémon exception handling, i.e. "gotta' catch 'em all".
 *
 * ---
 * try {
 *    choose(pikachu);
 * } catch (Throwable e) {
 *    ...
 * }
 * ---
 */
extern(C++) class PokemonExceptionCheck(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"exception_check";
	alias visit = BaseAnalyzerDmd.visit;

	extern(D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.Catch c)
	{
		if (c.type.isTypeIdentifier().ident.toString() == "Error" || 
			c.type.isTypeIdentifier().ident.toString() == "Throwable")
				addErrorMessage(cast(ulong) c.loc.linnum, cast(ulong) c.loc.charnum,
								KEY, MESSAGE);
	}

private:
	enum MESSAGE = "Catching Error or Throwable is almost always a bad idea.";
	enum string KEY = "dscanner.suspicious.catch_em_all";
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.exception_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{
		void testCatch()
		{
			try
			{
				// ...
			}
			catch (AssertError err) //ok
			{

			}
			catch (Exception err) // ok
			{

			}
			catch (shared(Exception) err) // ok
			{

			}
			catch (Error err) // [warn]: Catching Error or Throwable is almost always a bad idea.
			{

			}
			catch (Throwable err) // [warn]: Catching Error or Throwable is almost always a bad idea.
			{

			}
			catch (shared(Error) err) // [warn]: Catching Error or Throwable is almost always a bad idea.
			{

			}
		}
	}c, sac);

	stderr.writeln("Unittest for PokemonExceptionCheck passed.");
}