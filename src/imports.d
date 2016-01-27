//          Copyright Brian Schott (Hackerpilot) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module imports;

import dparse.ast;
import std.stdio;
import std.container;

/**
 * AST visitor that collects modules imported to an R-B tree.
 */
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
		if (ignore)
			return;
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

	alias visit = ASTVisitor.visit;

	/// Collected imports
	RedBlackTree!string imports;

private:
	bool ignore = true;
}
