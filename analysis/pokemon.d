//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.pokemon;

import std.d.ast;
import std.d.lexer;
import analysis.base;

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
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const Catch c)
	{
		if (c.type.type2.symbol.identifierOrTemplateChain.identifiersOrTemplateInstances.length != 1)
		{
			c.accept(this);
			return;
		}
		auto identOrTemplate = c.type.type2.symbol.identifierOrTemplateChain.identifiersOrTemplateInstances[0];
		if (identOrTemplate.templateInstance !is null)
		{
			c.accept(this);
			return;
		}
		if (identOrTemplate.identifier.text == "Throwable"
			|| identOrTemplate.identifier.text == "Error")
		{
			immutable column = identOrTemplate.identifier.column;
			immutable line = identOrTemplate.identifier.line;
			addErrorMessage(line, column, "Catching Error or Throwable is a really bad idea.");
		}
		c.accept(this);
	}
}
