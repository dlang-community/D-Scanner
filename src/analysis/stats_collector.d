//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.stats_collector;

import dparse.ast;
import dparse.lexer;
import analysis.base;

class StatsCollector : BaseAnalyzer
{
	alias visit = ASTVisitor.visit;

	this(string fileName)
	{
		super(fileName, null);
	}

	override void visit(const Statement statement)
	{
		statementCount++;
		statement.accept(this);
	}

	override void visit(const ClassDeclaration classDeclaration)
	{
		classCount++;
		classDeclaration.accept(this);
	}

	override void visit(const InterfaceDeclaration interfaceDeclaration)
	{
		interfaceCount++;
		interfaceDeclaration.accept(this);
	}

	override void visit(const FunctionDeclaration functionDeclaration)
	{
		functionCount++;
		functionDeclaration.accept(this);
	}

	override void visit(const StructDeclaration structDeclaration)
	{
		structCount++;
		structDeclaration.accept(this);
	}

	override void visit(const TemplateDeclaration templateDeclaration)
	{
		templateCount++;
		templateDeclaration.accept(this);
	}

	uint interfaceCount;
	uint classCount;
	uint functionCount;
	uint templateCount;
	uint structCount;
	uint statementCount;
	uint lineOfCodeCount;
	uint undocumentedPublicSymbols;
}
