//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module imports;

import stdx.d.ast;
import std.stdio;
import std.container;

class ImportPrinter : ASTVisitor
{
	this()
	{
		imports = new RedBlackTree!string;
	}

	override void visit(const SingleImport singleImport)
	{
		ignore = false;
		singleImport.accept(this);
		ignore = true;
	}

	override void visit(const IdentifierChain identifierChain)
	{
		if (ignore) return;
		bool first = true;
		string s;
		foreach (ident; identifierChain.identifiers)
		{
			if (!first)
				s ~= ".";
			s ~= ident.text;
			first = false;
		}
		imports.insert(s);
	}

	RedBlackTree!string imports;

	alias visit = ASTVisitor.visit;

	bool ignore = true;
}
