//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.objectconst;

import dscanner.analysis.base;
import dscanner.analysis.helpers;
import std.stdio;

extern(C++) class ObjectConstCheck(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"object_const_check";
	alias visit = BaseAnalyzerDmd.visit;

	extern(D) this(string fileName)
	{
		super(fileName);
	}

	void visitAggregate(AST.AggregateDeclaration ad)
	{
		import dmd.astenums : MODFlags, STC;

		if (!ad.members)
			return;

		foreach(member; *ad.members)
		{
			if (auto fd = member.isFuncDeclaration())
			{
				if (isInteresting(fd.ident.toString()) && !isConstFunc(fd) &&
					!(fd.storage_class & STC.disable))
						addErrorMessage(cast(ulong) fd.loc.linnum, cast(ulong) fd.loc.charnum, KEY,
							"Methods 'opCmp', 'toHash', 'opEquals', 'opCast', and/or 'toString' are non-const.");
				
				member.accept(this);
			}
			else if (auto scd = member.isStorageClassDeclaration())
			{
				foreach (smember; *scd.decl)
				{
					if (auto fd2 = smember.isFuncDeclaration())
					{
						if (isInteresting(fd2.ident.toString()) && !isConstFunc(fd2, scd) &&
							!(fd2.storage_class & STC.disable))
								addErrorMessage(cast(ulong) fd2.loc.linnum, cast(ulong) fd2.loc.charnum, KEY,
									"Methods 'opCmp', 'toHash', 'opEquals', 'opCast', and/or 'toString' are non-const.");
						
						smember.accept(this);
					}
					else
						smember.accept(this);
				}
			}
			else
				member.accept(this);
		}
	}

	override void visit(AST.ClassDeclaration cd)
	{
		visitAggregate(cd);	
	}

	override void visit(AST.StructDeclaration sd)
	{
		visitAggregate(sd);
	}

	override void visit(AST.InterfaceDeclaration id)
	{
		visitAggregate(id);
	}

	override void visit(AST.UnionDeclaration ud)
	{
		visitAggregate(ud);
	}

	extern(D) private static bool isInteresting(const char[] name)
	{
		return name == "opCmp" || name == "toHash" || name == "opEquals"
			|| name == "toString" || name == "opCast";
	}

	/**
	 * Checks if a function has either one of attributes `const`, `immutable`, `inout`
	 */
	private bool isConstFunc(AST.FuncDeclaration fd, AST.StorageClassDeclaration scd = null)
	{
		import dmd.astenums : MODFlags, STC;
		import std.stdio : writeln;

		if (scd && (scd.stc & STC.const_ || scd.stc & STC.immutable_ || scd.stc & STC.wild))
			return true;

		if(fd.type && (fd.type.mod == MODFlags.const_ ||
			fd.type.mod == MODFlags.immutable_ || fd.type.mod == MODFlags.wild))
				return true;

		return false; 
	}

	private enum KEY = "dscanner.suspicious.object_const";
	
	AST.AggregateDeclaration deleteme;
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.object_const_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{
		void testConsts()
		{
			// Will be ok because all are declared const/immutable
			class Cat
			{
				const bool opEquals(Object a, Object b) // ok
				{
					return true;
				}

				const int opCmp(Object o) // ok
				{
					return 1;
				}

				immutable hash_t toHash() // ok
				{
					return 0;
				}

				const string toString() // ok
				{
					return "Cat";
				}
			}

			class Bat
			{
				const: override string toString() { return "foo"; } // ok
			}

			class Fox
			{
				inout { override string toString() { return "foo"; } } // ok
			}

			class Rat
			{
				bool opEquals(Object a, Object b) @disable; // ok
			}

			class Ant
			{
				@disable bool opEquals(Object a, Object b); // ok
			}

			// Will warn, because none are const
			class Dog
			{
				bool opEquals(Object a, Object b) // [warn]: Methods 'opCmp', 'toHash', 'opEquals', 'opCast', and/or 'toString' are non-const.
				{
					return true;
				}

				int opCmp(Object o) // [warn]: Methods 'opCmp', 'toHash', 'opEquals', 'opCast', and/or 'toString' are non-const.
				{
					return 1;
				}

				hash_t toHash() // [warn]: Methods 'opCmp', 'toHash', 'opEquals', 'opCast', and/or 'toString' are non-const.
				{
					return 0;
				}

				string toString() // [warn]: Methods 'opCmp', 'toHash', 'opEquals', 'opCast', and/or 'toString' are non-const.
				{
					return "Dog";
				}
			}
		}
	}c, sac);

	stderr.writeln("Unittest for ObjectConstCheck passed.");
}
