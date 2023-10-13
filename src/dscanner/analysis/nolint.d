module dscanner.analysis.nolint;

@safe:

import dparse.ast;
import dparse.lexer;

import std.algorithm : canFind;
import std.regex : matchAll, regex;
import std.string : lastIndexOf, strip;
import std.typecons;

struct NoLint
{
	bool containsCheck(scope const(char)[] check) const
	{
		while (true)
		{
			if (disabledChecks.get((() @trusted => cast(string) check)(), 0) > 0)
				return true;

			auto dot = check.lastIndexOf('.');
			if (dot == -1)
				break;
			check = check[0 .. dot];
		}
		return false;
	}

	// automatic pop when returned value goes out of scope
	Poppable push(in Nullable!NoLint other) scope
	{
		if (other.isNull)
			return Poppable(null);

		foreach (key, value; other.get.getDisabledChecks)
			this.disabledChecks[key] += value;

		return Poppable(() => this.pop(other));
	}

package:
	const(int[string]) getDisabledChecks() const
	{
		return this.disabledChecks;
	}

	void pushCheck(in string check)
	{
		disabledChecks[check]++;
	}

	void merge(in Nullable!NoLint other)
	{
		if (other.isNull)
			return;

		foreach (key, value; other.get.getDisabledChecks)
			this.disabledChecks[key] += value;
	}

private:
	void pop(in Nullable!NoLint other)
	{
		if (other.isNull)
			return;

		foreach (key, value; other.get.getDisabledChecks)
		{
			assert(this.disabledChecks.get(key, 0) >= value);

			this.disabledChecks[key] -= value;
		}
	}

	static struct Poppable
	{
		~this()
		{
			if (onPop)
				onPop();
			onPop = null;
		}

	private:
		void delegate() onPop;
	}

	int[string] disabledChecks;
}

struct NoLintFactory
{
	static Nullable!NoLint fromModuleDeclaration(in ModuleDeclaration moduleDeclaration)
	{
		NoLint noLint;

		foreach (atAttribute; moduleDeclaration.atAttributes)
			noLint.merge(NoLintFactory.fromAtAttribute(atAttribute));

		if (!noLint.getDisabledChecks.length)
			return nullNoLint;

		return noLint.nullable;
	}

	static Nullable!NoLint fromDeclaration(in Declaration declaration)
	{
		NoLint noLint;
		foreach (attribute; declaration.attributes)
			noLint.merge(NoLintFactory.fromAttribute(attribute));

		if (!noLint.getDisabledChecks.length)
			return nullNoLint;

		return noLint.nullable;
	}

private:
	static Nullable!NoLint fromAttribute(const(Attribute) attribute)
	{
		if (attribute is null)
			return nullNoLint;

		return NoLintFactory.fromAtAttribute(attribute.atAttribute);

	}

	static Nullable!NoLint fromAtAttribute(const(AtAttribute) atAttribute)
	{
		if (atAttribute is null)
			return nullNoLint;

		auto ident = atAttribute.identifier;
		auto argumentList = atAttribute.argumentList;

		if (argumentList !is null)
		{
			if (ident.text.length)
				return NoLintFactory.fromStructUda(ident, argumentList);
			else
				return NoLintFactory.fromStringUda(argumentList);

		}
		else
			return nullNoLint;
	}

	// @nolint("..")
	static Nullable!NoLint fromStructUda(in Token ident, in ArgumentList argumentList)
	in (ident.text.length && argumentList !is null)
	{
		if (ident.text != "nolint")
			return nullNoLint;

		NoLint noLint;

		foreach (nodeExpr; argumentList.items)
		{
			if (auto unaryExpr = cast(const UnaryExpression) nodeExpr)
			{
				auto primaryExpression = unaryExpr.primaryExpression;
				if (primaryExpression is null)
					continue;

				if (primaryExpression.primary != tok!"stringLiteral")
					continue;

				noLint.pushCheck(primaryExpression.primary.text.strip("\""));
			}
		}

		if (!noLint.getDisabledChecks().length)
			return nullNoLint;

		return noLint.nullable;
	}

	// @("nolint(..)")
	static Nullable!NoLint fromStringUda(in ArgumentList argumentList)
	in (argumentList !is null)
	{
		NoLint noLint;

		foreach (nodeExpr; argumentList.items)
		{
			if (auto unaryExpr = cast(const UnaryExpression) nodeExpr)
			{
				auto primaryExpression = unaryExpr.primaryExpression;
				if (primaryExpression is null)
					continue;

				if (primaryExpression.primary != tok!"stringLiteral")
					continue;

				auto str = primaryExpression.primary.text.strip("\"");
				Nullable!NoLint currNoLint = NoLintFactory.fromString(str);
				noLint.merge(currNoLint);
			}
		}

		if (!noLint.getDisabledChecks().length)
			return nullNoLint;

		return noLint.nullable;

	}

	// Transform a string with form "nolint(abc, efg)"
	// into a NoLint struct
	static Nullable!NoLint fromString(in string str)
	{
		static immutable re = regex(`[\w-_.]+`, "g");
		auto matches = matchAll(str, re);

		if (!matches)
			return nullNoLint;

		const udaName = matches.hit;
		if (udaName != "nolint")
			return nullNoLint;

		matches.popFront;

		NoLint noLint;

		while (matches)
		{
			noLint.pushCheck(matches.hit);
			matches.popFront;
		}

		if (!noLint.getDisabledChecks.length)
			return nullNoLint;

		return noLint.nullable;
	}

	static nullNoLint = Nullable!NoLint.init;
}

unittest
{
	const s1 = "nolint(abc)";
	const s2 = "nolint(abc, efg, hij)";
	const s3 = "    nolint (   abc ,  efg  )    ";
	const s4 = "nolint(dscanner.style.abc_efg-ijh)";
	const s5 = "OtherUda(abc)";
	const s6 = "nolint(dscanner)";

	assert(NoLintFactory.fromString(s1).get.containsCheck("abc"));

	assert(NoLintFactory.fromString(s2).get.containsCheck("abc"));
	assert(NoLintFactory.fromString(s2).get.containsCheck("efg"));
	assert(NoLintFactory.fromString(s2).get.containsCheck("hij"));

	assert(NoLintFactory.fromString(s3).get.containsCheck("abc"));
	assert(NoLintFactory.fromString(s3).get.containsCheck("efg"));

	assert(NoLintFactory.fromString(s4).get.containsCheck("dscanner.style.abc_efg-ijh"));

	assert(NoLintFactory.fromString(s5).isNull);

	assert(NoLintFactory.fromString(s6).get.containsCheck("dscanner"));
	assert(!NoLintFactory.fromString(s6).get.containsCheck("dscanner2"));
	assert(NoLintFactory.fromString(s6).get.containsCheck("dscanner.foo"));

	import std.stdio : stderr, writeln;

	(() @trusted => stderr.writeln("Unittest for NoLint passed."))();
}
