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
		return disabledChecks.canFind(check);
	}

package:
	const(string[]) getDisabledChecks() const
	{
		return this.disabledChecks;
	}

	void addCheck(in string check)
	{
		disabledChecks ~= check;
	}

	void merge(in Nullable!NoLint other)
	{
		if(!other.isNull)
			this.disabledChecks ~= other.get.getDisabledChecks();
	}

private:
		string[] disabledChecks;
}

struct NoLintFactory
{
	static Nullable!NoLint fromDeclaration(in Declaration declaration)
	{
		NoLint noLint;
		foreach(attribute; declaration.attributes)
			noLint.merge(NoLintFactory.fromAttribute(attribute));

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

				noLint.addCheck(primaryExpression.primary.text.strip("\""));
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
				noLint.merge(currNoLint);
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
		auto re = regex(`\w+`, "g");
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
			noLint.addCheck(matches.hit);
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
	const s4 = "OtherUda(abc)";

	assert(NoLintFactory.fromString(s1).get == NoLint(["abc"]));
	assert(NoLintFactory.fromString(s2).get == NoLint(["abc", "efg", "hij"]));
	assert(NoLintFactory.fromString(s3).get == NoLint(["abc", "efg"]));
	assert(NoLintFactory.fromString(s4).isNull);
}
