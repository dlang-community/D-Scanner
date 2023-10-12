module dscanner.analysis.nolint;

import dparse.ast;
import dparse.lexer;

import std.algorithm: canFind;
import std.regex: regex, matchAll;
import std.string: strip;
import std.typecons;


struct NoLint
{
	bool containsCheck(in string check) const
	{
		return (check in disabledChecks) !is null &&
				disabledChecks[check] > 0;
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

	void push(in Nullable!NoLint other)
	{
		if(other.isNull)
			return;

		foreach(item; other.get.getDisabledChecks.byKeyValue)
			this.disabledChecks[item.key] += item.value;
	}

	void pop(in Nullable!NoLint other)
	{
		if(other.isNull)
			return;

		foreach(item; other.get.getDisabledChecks.byKeyValue)
		{
			assert((item.key in disabledChecks) !is null &&
					this.disabledChecks[item.key] >= item.value);

			this.disabledChecks[item.key] -= item.value;
		}
	}


private:
		int[string] disabledChecks;
}

struct NoLintFactory
{
	static Nullable!NoLint fromModuleDeclaration(in ModuleDeclaration moduleDeclaration)
	{
		NoLint noLint;

		foreach(atAttribute; moduleDeclaration.atAttributes)
			noLint.push(NoLintFactory.fromAtAttribute(atAttribute));

		if(!noLint.getDisabledChecks.length)
			return nullNoLint;

		return noLint.nullable;
	}

	static Nullable!NoLint fromDeclaration(in Declaration declaration)
	{
		NoLint noLint;
		foreach(attribute; declaration.attributes)
			noLint.push(NoLintFactory.fromAttribute(attribute));

		if(!noLint.getDisabledChecks.length)
			return nullNoLint;

		return noLint.nullable;
	}


private:
	static Nullable!NoLint fromAttribute(const(Attribute) attribute)
	{
		if(attribute is null)
			return nullNoLint;

		return NoLintFactory.fromAtAttribute(attribute.atAttribute);

	}

	static Nullable!NoLint fromAtAttribute(const(AtAttribute) atAttribute)
	{
		if(atAttribute is null)
			return nullNoLint;

		auto ident = atAttribute.identifier;
		auto argumentList = atAttribute.argumentList;

		if(argumentList !is null)
		{
			if(ident.text.length)
				return NoLintFactory.fromStructUda(ident, argumentList);
			else
				return NoLintFactory.fromStringUda(argumentList);

		}
		else
			return nullNoLint;
	}

	// @nolint("..")
	static Nullable!NoLint fromStructUda(in Token ident, in ArgumentList argumentList)
	in(ident.text.length && argumentList !is null)
	{
		if(ident.text != "nolint")
			return nullNoLint;

		NoLint noLint;

		foreach(nodeExpr; argumentList.items)
		{
			if(auto unaryExpr = cast(UnaryExpression) nodeExpr)
			{
				auto primaryExpression = unaryExpr.primaryExpression;
				if(primaryExpression is null)
					continue;

				if(primaryExpression.primary != tok!"stringLiteral")
					continue;

				noLint.pushCheck(primaryExpression.primary.text.strip("\""));
			}
		}

		if(!noLint.getDisabledChecks().length)
			return nullNoLint;

		return noLint.nullable;
	}

	// @("nolint(..)")
	static Nullable!NoLint fromStringUda(in ArgumentList argumentList)
	in(argumentList !is null)
	{
		NoLint noLint;

		foreach(nodeExpr; argumentList.items)
		{
			if(auto unaryExpr = cast(UnaryExpression) nodeExpr)
			{
				auto primaryExpression = unaryExpr.primaryExpression;
				if(primaryExpression is null)
					continue;

				if(primaryExpression.primary != tok!"stringLiteral")
					continue;

				auto str = primaryExpression.primary.text.strip("\"");
				Nullable!NoLint currNoLint = NoLintFactory.fromString(str);
				noLint.push(currNoLint);
			}
		}

		if(!noLint.getDisabledChecks().length)
			return nullNoLint;

		return noLint.nullable;

	}

	// Transform a string with form "nolint(abc, efg)"
	// into a NoLint struct
	static Nullable!NoLint fromString(in string str)
	{
		auto re = regex(`[\w-_.]+`, "g");
		auto matches = matchAll(str, re);

		if(!matches)
			return nullNoLint;

		const udaName = matches.hit;
		if(udaName != "nolint")
			return nullNoLint;

		matches.popFront;

		NoLint noLint;

		while(matches)
		{
			noLint.pushCheck(matches.hit);
			matches.popFront;
		}

		if(!noLint.getDisabledChecks.length)
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

	assert(NoLintFactory.fromString(s1).get.containsCheck("abc"));

	assert(NoLintFactory.fromString(s2).get.containsCheck("abc"));
	assert(NoLintFactory.fromString(s2).get.containsCheck("efg"));
	assert(NoLintFactory.fromString(s2).get.containsCheck("hij"));

	assert(NoLintFactory.fromString(s3).get.containsCheck("abc"));
	assert(NoLintFactory.fromString(s3).get.containsCheck("efg"));

	assert(NoLintFactory.fromString(s4).get.containsCheck("dscanner.style.abc_efg-ijh"));

	assert(NoLintFactory.fromString(s5).isNull);

	import std.stdio: stderr, writeln;
	stderr.writeln("Unittest for NoLint passed.");
}
