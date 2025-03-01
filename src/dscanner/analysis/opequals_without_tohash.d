// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.opequals_without_tohash;

import dscanner.analysis.base;
import dscanner.analysis.helpers;

/**
 * Checks for when a class/struct has the method opEquals without toHash, or
 * toHash without opEquals.
 */
extern(C++) class OpEqualsWithoutToHashCheck(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"opequals_tohash_check";
	alias visit = BaseAnalyzerDmd.visit;

	extern(D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.ClassDeclaration cd)
	{
		visitBaseClasses(cd);
		visitAggregate(cd);
	}

	override void visit(AST.StructDeclaration sd)
	{
		visitAggregate(sd);
	}

	private void isInteresting(AST.FuncDeclaration fd, ref bool hasOpEquals, ref bool hasToHash)
	{
		import dmd.astenums : STC;

		if (!(fd.storage_class & STC.disable) && fd.ident.toString() == "opEquals")
			hasOpEquals = true;

		if (!(fd.storage_class & STC.disable) && fd.ident.toString() == "toHash")
			hasToHash = true;
	}

	private void visitAggregate(AST.AggregateDeclaration ad)
	{
		bool hasOpEquals, hasToHash;

		if (!ad.members)
			return;
	
		foreach(member; *ad.members)
		{
			if (auto fd = member.isFuncDeclaration())
			{
				isInteresting(fd, hasOpEquals, hasToHash);
				member.accept(this);
			}
			else if (auto scd = member.isStorageClassDeclaration())
			{
				foreach (smember; *scd.decl)
				{
					if (auto fd2 = smember.isFuncDeclaration())
					{
						isInteresting(fd2, hasOpEquals, hasToHash);
						smember.accept(this);
					}
					else
						smember.accept(this);
				}
			}
			else
				member.accept(this);
		}

		if (hasOpEquals && !hasToHash)
		{
			string message = ad.ident.toString().dup;
			message = "'" ~ message ~ "' has method 'opEquals', but not 'toHash'.";
			addErrorMessage(cast(ulong) ad.loc.linnum, cast(ulong) ad.loc.charnum, KEY, message);
		}
		else if (!hasOpEquals && hasToHash)
		{
			string message = ad.ident.toString().dup;
			message = "'" ~ message ~ "' has method 'toHash', but not 'opEquals'.";
			addErrorMessage(cast(ulong) ad.loc.linnum, cast(ulong) ad.loc.charnum, KEY, message);
		}
	}

	private enum KEY = "dscanner.suspicious.incomplete_operator_overloading";
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.opequals_tohash_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{
		// Success because it has opEquals and toHash
		class Chimp
		{
			const bool opEquals(Object a, Object b)
			{
				return true;
			}

			const override hash_t toHash()
			{
				return 0;
			}
		}

		// AA would use default equal and default toHash
		struct Bee
		{
			int opCmp(Bee) const
			{
				return true;
			}
		}

		// Fail on class opEquals
		class Rabbit // [warn]: 'Rabbit' has method 'opEquals', but not 'toHash'.
		{
			const bool opEquals(Object a, Object b)
			{
				return true;
			}
		}

		// Fail on class toHash
		class Kangaroo // [warn]: 'Kangaroo' has method 'toHash', but not 'opEquals'.
		{
			override const hash_t toHash()
			{
				return 0;
			}
		}

		// Fail on struct opEquals
		struct Tarantula // [warn]: 'Tarantula' has method 'opEquals', but not 'toHash'.
		{
			const bool opEquals(Object a, Object b)
			{
				return true;
			}
		}

		// Fail on struct toHash
		struct Puma // [warn]: 'Puma' has method 'toHash', but not 'opEquals'.
		{
			const nothrow @safe hash_t toHash()
			{
				return 0;
			}
		}

		// issue #659, do not warn if one miss and the other is not callable
		struct Fox {const nothrow @safe hash_t toHash() @disable;}
		struct Bat {@disable const nothrow @safe hash_t toHash();}
		struct Rat {const bool opEquals(Object a, Object b) @disable;}
		struct Cat {@disable const bool opEquals(Object a, Object b);}

	}c, sac);

	stderr.writeln("Unittest for OpEqualsWithoutToHashCheck passed.");
}