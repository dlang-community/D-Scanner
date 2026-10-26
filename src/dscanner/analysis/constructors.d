module dscanner.analysis.constructors;

import std.stdio;
import dscanner.analysis.base;
import dscanner.analysis.source_range : extentOf;
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
		AST.CtorDeclaration noArgConstructor, defaultArgConstructor;

		if (d.members)
		{
			foreach (s; *d.members)
			{
				if (auto cd = s.isCtorDeclaration())
				{
					auto tf = cd.type.isTypeFunction();

					if (tf)
					{
						import std.algorithm : all;

						if (tf.parameterList.parameters.length == 0)
							noArgConstructor = cd;
						else if ((*tf.parameterList.parameters)[].all!(param => param.defaultArg !is null))
							defaultArgConstructor = cd;
					}
				}

				s.accept(this);
			}
		}

		if (noArgConstructor && defaultArgConstructor)
		{
			addErrorMessage(nameRange(d), KEY, MESSAGE, [
				diagnosticAt(constructorRange(noArgConstructor), "zero-argument constructor defined here"),
				diagnosticAt(constructorRange(defaultArgConstructor), "default argument constructor defined here"),
			]);
		}
	}

	extern (D) private size_t[2] constructorRange(AST.CtorDeclaration constructor)
	{
		return [constructor.loc.fileOffset, constructor.endloc.fileOffset + 1];
	}

	override void visit(AST.StructDeclaration d)
	{
		import std.string : stripRight;

		if (d.members)
		{
			foreach (s; *d.members)
			{
				auto cd = s.isCtorDeclaration();
				auto tf = cd ? cd.type.isTypeFunction() : null;
				if (tf && tf.parameterList.parameters.length == 1)
				{
					auto defaultArg = (*tf.parameterList.parameters)[0].defaultArg;
					if (defaultArg)
					{
						// the range starts at the `=` before the default value
						const valueRange = extentOf(source, defaultArg);
						const assignOffset = source[0 .. valueRange[0]].stripRight.length - 1;
						addErrorMessage([assignOffset, valueRange[1]], STRUCT_KEY, STRUCT_MESSAGE);
					}
				}

				s.accept(this);
			}
		}
	}

private:
	enum STRUCT_MESSAGE = "This struct constructor can never be called with its default argument.";
	enum STRUCT_KEY = "dscanner.confusing.struct_constructor_default_args";
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
		class Cat /+
		      ^^^ [warn]: This class has a zero-argument constructor as well as a constructor with default arguments. This can be confusing. +/
		{
			this() {}
			this(string name = "kittie") {}
		}

		class Cat /+
		      ^^^ [warn]: This class has a zero-argument constructor as well as a constructor with default arguments. This can be confusing. +/
		{
			this() {}
			this(string name = "kittie", int x = 2) {}
		}

		class Cat
		{
			this() {}
			this(string name = "kittie", int x) {}
		}

		struct Dog
		{
			this() {}
			this(string name = "doggie") {} /+
			                 ^^^^^^^^^^ [warn]: This struct constructor can never be called with its default argument. +/
		}
	}c, sac);

	stderr.writeln("Unittest for ConstructorCheck passed.");
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.rundmd : analyzeDmd, parseDmdModule;
	import dscanner.utils : getModuleName;
	import std.algorithm : map;
	import std.array : array;
	import std.file : remove, write;
	import std.typecons : tuple;

	StaticAnalysisConfig sac = disabledConfig();
	sac.constructor_check = Check.enabled;

	enum fileName = "constructor_supplemental_test.d";
	enum code = "class Cat\n{\n\tthis() {}\n\tthis(string name = \"kittie\") {}\n}\n";
	write(fileName, code);
	scope (exit)
		remove(fileName);

	auto m = parseDmdModule(fileName, code);
	auto messages = analyzeDmd(fileName, m, getModuleName(m.md), sac)[].array;

	assert(messages.length == 1);
	assert(messages[0].supplemental.map!(s => tuple(s.startLine, s.message)).array == [
		tuple(3, "zero-argument constructor defined here"),
		tuple(4, "default argument constructor defined here"),
	]);
}
