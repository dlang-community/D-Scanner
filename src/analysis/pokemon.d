//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.pokemon;

import std.stdio;
import dparse.ast;
import dparse.lexer;
import analysis.base;
import analysis.helpers;
import dsymbol.scope_ : Scope;

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
class PokemonExceptionCheck : BaseAnalyzer
{
	enum MESSAGE = "Catching Error or Throwable is almost always a bad idea.";
	enum string KEY = "dscanner.suspicious.catch_em_all";

	alias visit = BaseAnalyzer.visit;

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const LastCatch lc)
	{
		addErrorMessage(lc.line, lc.column, KEY, MESSAGE);
		lc.accept(this);
	}

	bool ignoreType = true;

	override void visit(const Catch c)
	{
		ignoreType = false;
		c.type.accept(this);
		ignoreType = true;

		c.accept(this);
	}

	override void visit(const Type2 type2)
	{
		if (ignoreType)
			return;

		if (type2.type !is null)
		{
			type2.type.accept(this);
			return;
		}

		if (type2.symbol.identifierOrTemplateChain.identifiersOrTemplateInstances.length != 1)
		{
			return;
		}
		const identOrTemplate = type2.symbol.identifierOrTemplateChain
			.identifiersOrTemplateInstances[0];
		if (identOrTemplate.templateInstance !is null)
		{
			return;
		}
		if (identOrTemplate.identifier.text == "Throwable"
				|| identOrTemplate.identifier.text == "Error")
		{
			immutable column = identOrTemplate.identifier.column;
			immutable line = identOrTemplate.identifier.line;
			addErrorMessage(line, column, KEY, MESSAGE);
		}
	}
}

unittest
{
	import analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.exception_check = Check.enabled;
	assertAnalyzerWarnings(q{
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
			catch // [warn]: Catching Error or Throwable is almost always a bad idea.
			{

			}
		}
	}c, sac);

	stderr.writeln("Unittest for PokemonExceptionCheck passed.");
}
