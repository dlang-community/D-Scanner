//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module analysis.if_statements;

import dparse.ast;
import dparse.lexer;
import dparse.formatter;
import analysis.base;
import dsymbol.scope_ : Scope;

class IfStatementCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;
	this(string fileName, const(Scope)* sc)
	{
		super(fileName, sc);
	}

	override void visit(const IfStatement ifStatement)
	{
		import std.string : format;
		import std.algorithm : sort, countUntil;
		import std.array : appender;

		++depth;

		if (ifStatement.expression.items.length == 1
				&& (cast(AndAndExpression) ifStatement.expression.items[0]) is null)
		{
			redundancyCheck(ifStatement.expression,
					ifStatement.expression.line, ifStatement.expression.column);
		}
		inIfExpresson = true;
		ifStatement.expression.accept(this);
		inIfExpresson = false;
		ifStatement.thenStatement.accept(this);
		if (expressions.length)
			expressions = expressions[0 .. expressions.countUntil!(a => a.depth + 1 >= depth)];
		if (ifStatement.elseStatement)
			ifStatement.elseStatement.accept(this);
		--depth;
	}

	override void visit(const AndAndExpression andAndExpression)
	{
		if (inIfExpresson)
		{
			redundancyCheck(andAndExpression, andAndExpression.line, andAndExpression.column);
			redundancyCheck(andAndExpression.left, andAndExpression.line, andAndExpression.column);
			redundancyCheck(andAndExpression.right, andAndExpression.line,
					andAndExpression.column);
		}
		andAndExpression.accept(this);
	}

	override void visit(const OrOrExpression orOrExpression)
	{
		// intentionally does nothing
	}

private:
	invariant
	{
		assert(depth >= 0);
	}

	void redundancyCheck(const ExpressionNode expression, size_t line, size_t column)
	{
		import std.string : format;
		import std.array : appender;
		import std.algorithm : sort;

		if (expression is null)
			return;
		auto app = appender!string();
		dparse.formatter.format(app, expression);
		immutable size_t prevLocation = alreadyChecked(app.data, line, column);
		if (prevLocation != size_t.max)
		{
			addErrorMessage(line, column, KEY, "Expression %s is true: already checked on line %d.".format(
					expressions[prevLocation].formatted, expressions[prevLocation].line));
		}
		else
		{
			expressions ~= ExpressionInfo(app.data, line, column, depth);
			sort(expressions);
		}
	}

	size_t alreadyChecked(string expressionText, size_t line, size_t column)
	{
		foreach (i, ref info; expressions)
		{
			if (info.line == line && info.column == column)
				continue;
			if (info.formatted == expressionText)
				return i;
		}
		return size_t.max;
	}

	bool inIfExpresson;
	int depth;
	ExpressionInfo[] expressions;
	enum string KEY = "dscanner.if_statement";
}

private struct ExpressionInfo
{
	int opCmp(ref const ExpressionInfo other) const nothrow
	{
		if (line < other.line || (line == other.line && column < other.column))
			return 1;
		if (line == other.line && column == other.column)
			return 0;
		return -1;
	}

	string formatted;
	size_t line;
	size_t column;
	int depth;
}
