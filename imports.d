//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module imports;

import std.d.ast;
import std.stdio;

class ImportPrinter : ASTVisitor
{
	override void visit(SingleImport singleImport)
	{
		ignore = false;
		singleImport.accept(this);
		writeln();
		ignore = true;
	}

	override void visit(IdentifierChain identifierChain)
	{
		if (ignore) return;
		bool first = true;
		foreach (ident; identifierChain.identifiers)
		{
			if (!first)
				write(".");
			write(ident.value);
			first = false;
		}
	}

	alias ASTVisitor.visit visit;

	bool ignore = true;
}
