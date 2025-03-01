module dscanner.analysis.constructors;

import std.stdio;
import dscanner.analysis.base;
import dscanner.analysis.helpers;

extern(C++) class ConstructorCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"constructor_check";

	extern(D) this(string fileName)
	{
		super(fileName);
	}

	override void visit(AST.ClassDeclaration d)
	{
		bool hasDefaultArgConstructor = false;
		bool hasNoArgConstructor = false;

		if (d.members)
		{
			foreach (s; *d.members)
			{
				if (auto cd = s.isCtorDeclaration())
				{
					auto tf = cd.type.isTypeFunction();

					if (tf)
					{
						if (tf.parameterList.parameters.length == 0)
							hasNoArgConstructor = true;
						else
						{
							// Check if all parameters have a default value
							hasDefaultArgConstructor = true;

							foreach (param; *tf.parameterList.parameters)
								if (param.defaultArg is null)
									hasDefaultArgConstructor = false;
						}
					}
				}

				s.accept(this);
			}
		}

		if (hasNoArgConstructor && hasDefaultArgConstructor)
		{
			addErrorMessage(cast(ulong) d.loc.linnum,
					cast(ulong) d.loc.charnum, KEY, MESSAGE);
		}
	}
	
private:
	enum MESSAGE = "This class has a zero-argument constructor as well as a"
					~ " constructor with default arguments. This can be confusing.";
	enum KEY = "dscanner.confusing.constructor_args";
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.constructor_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{
		class Cat // [warn]: This class has a zero-argument constructor as well as a constructor with default arguments. This can be confusing.
		{
			this() {}
			this(string name = "kittie") {}
		}

		class Cat // [warn]: This class has a zero-argument constructor as well as a constructor with default arguments. This can be confusing.
		{
			this() {}
			this(string name = "kittie", int x = 2) {}
		}

		class Cat
		{
			this() {}
			this(string name = "kittie", int x) {}
		}
	}c, sac);

	stderr.writeln("Unittest for ConstructorCheck passed.");
}
