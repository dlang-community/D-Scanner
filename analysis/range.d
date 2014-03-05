//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.range;

import stdx.d.ast;
import stdx.d.lexer;
import analysis.base;

/**
 * Checks for .. expressions where the left side is larger than the right. This
 * is almost always a mistake.
 */
class BackwardsRangeCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	bool hasLeft;
	bool hasRight;
	long left;
	long right;
	size_t column;
	size_t line;
	enum State { ignore, left, right }
	State state = State.ignore;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const ForeachStatement foreachStatement)
	{
		if (foreachStatement.low !is null && foreachStatement.high !is null)
		{
			import std.string;
			state = State.left;
			foreachStatement.low.accept(this);
			state = State.right;
			foreachStatement.high.accept(this);
			state = State.ignore;
			if (hasLeft && hasRight && left > right)
			{
				string message = format(
					"%d is larger than %d. Did you mean to use 'foreach_reverse( ... ; %d .. %d)'?",
					left, right, right, left);
				addErrorMessage(line, this.column, message);
			}
			hasLeft = false;
			hasRight = false;
		}
		foreachStatement.accept(this);
	}

	override void visit(const UnaryExpression unary)
	{
		if (state != State.ignore && unary.primaryExpression is null)
			return;
		else
			unary.accept(this);
	}

	override void visit(const PrimaryExpression primary)
	{
		import std.conv;
		import std.string;
		if (state == State.ignore || !isNumberLiteral(primary.primary.type))
			return;
		if (state == State.left)
		{
			line = primary.primary.line;
			this.column = primary.primary.column;
			left = to!long(primary.primary.text.removechars("_uUlL"));
			hasLeft = true;
		}
		else
		{
			right = to!long(primary.primary.text.removechars("_uUlL"));
			hasRight = true;
		}
	}

	override void visit(const SliceExpression sliceExpression)
	{
		if (sliceExpression.lower !is null && sliceExpression.upper !is null)
		{
			state = State.left;
			sliceExpression.lower.accept(this);
			state = State.right;
			sliceExpression.upper.accept(this);
			state = State.ignore;
			if (hasLeft && hasRight && left > right)
			{
				import std.string;
				string message = format(
					"%d is larger than %d. This slice is likely incorrect.",
					left, right);
				addErrorMessage(line, this.column, message);
			}
			hasLeft = false;
			hasRight = false;
		}
		sliceExpression.accept(this);
	}
}
