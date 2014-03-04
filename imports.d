//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module imports;

import stdx.d.ast;
import std.stdio;

class ImportPrinter : ASTVisitor
{
	override void visit(const SingleImport singleImport)
	{
		ignore = false;
		singleImport.accept(this);
		writeln();
		ignore = true;
	}

	override void visit(const IdentifierChain identifierChain)
	{
		if (ignore) return;
		bool first = true;
		foreach (ident; identifierChain.identifiers)
		{
			if (!first)
				write(".");
			write(ident.text);
			first = false;
		}
	}

	alias visit = ASTVisitor.visit;

	bool ignore = true;
}
