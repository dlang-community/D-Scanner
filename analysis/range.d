//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.range;

import std.stdio;
import std.d.ast;
import std.d.lexer;
import analysis.base;
import analysis.helpers;

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
	enum string KEY = "dscanner.bugs.backwards_slices";
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
				addErrorMessage(line, this.column, KEY, message);
			}
			hasLeft = false;
			hasRight = false;
		}
		foreachStatement.accept(this);
	}

	override void visit(const AddExpression add)
	{
		auto s = state;
		state = State.ignore;
		add.accept(this);
		state = s;
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

		if (state == State.ignore || !isNumberLiteral(primary.primary.type))
			return;
		if (state == State.left)
		{
			line = primary.primary.line;
			this.column = primary.primary.column;

			try left = parseNumber(primary.primary.text);
			catch (ConvException e) return;
			hasLeft = true;
		}
		else
		{
			try right = parseNumber(primary.primary.text);
			catch (ConvException e) return;
			hasRight = true;
		}
	}

	long parseNumber(string te)
	{
		import std.conv;
		import std.string;
		string t = te.removechars("_uUlL");
		if (t.length > 2)
		{
			if (t[1] == 'x' || t[1] == 'X')
				return to!long(t[2..$], 16);
			if (t[1] == 'b' || t[1] == 'B')
				return to!long(t[2..$], 2);
		}
		return to!long(t);
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
				addErrorMessage(line, this.column, KEY, message);
			}
			hasLeft = false;
			hasRight = false;
		}
		sliceExpression.accept(this);
	}
}

unittest
{
	import analysis.config;
	StaticAnalysisConfig sac;
	sac.backwards_range_check = true;
	assertAnalyzerWarnings(q{
		void testRange()
		{
			int[] data = [1, 2, 3, 4, 5];

			data = data[1 .. 3]; // ok
			data = data[3 .. 1]; // [warn]: 3 is larger than 1. This slice is likely incorrect.

			foreach (n; 1 .. 3) { } // ok
			foreach (n; 3 .. 1) { } // [warn]: 3 is larger than 1. Did you mean to use 'foreach_reverse( ... ; 1 .. 3)'?
		}
	}c, sac);

	stderr.writeln("Unittest for BackwardsRangeCheck passed.");
}

