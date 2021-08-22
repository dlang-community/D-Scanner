module dscanner.analysis.constructors;

import dparse.ast;
import dparse.lexer;
import std.stdio;
import dscanner.analysis.base;
import dscanner.analysis.helpers;
import dsymbol.scope_ : Scope;

final class ConstructorCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	mixin AnalyzerInfo!"constructor_check";

	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
	}

	override void visit(const ClassDeclaration classDeclaration)
	{
		immutable bool oldHasDefault = hasDefaultArgConstructor;
		immutable bool oldHasNoArg = hasNoArgConstructor;
		hasNoArgConstructor = false;
		hasDefaultArgConstructor = false;
		immutable State prev = state;
		state = State.inClass;
		classDeclaration.accept(this);
		if (hasNoArgConstructor && hasDefaultArgConstructor)
		{
			addErrorMessage(classDeclaration.name.line,
					classDeclaration.name.column, "dscanner.confusing.constructor_args",
					"This class has a zero-argument constructor as well as a"
					~ " constructor with one default argument. This can be confusing.");
		}
		hasDefaultArgConstructor = oldHasDefault;
		hasNoArgConstructor = oldHasNoArg;
		state = prev;
	}

	override void visit(const StructDeclaration structDeclaration)
	{
		immutable State prev = state;
		state = State.inStruct;
		structDeclaration.accept(this);
		state = prev;
	}

	override void visit(const Constructor constructor)
	{
		final switch (state)
		{
		case State.inStruct:
			if (constructor.parameters.parameters.length == 1
					&& constructor.parameters.parameters[0].default_ !is null)
			{
				addErrorMessage(constructor.line, constructor.column,
						"dscanner.confusing.struct_constructor_default_args",
						"This struct constructor can never be called with its "
						~ "default argument.");
			}
			break;
		case State.inClass:
			if (constructor.parameters.parameters.length == 1
					&& constructor.parameters.parameters[0].default_ !is null)
			{
				hasDefaultArgConstructor = true;
			}
			else if (constructor.parameters.parameters.length == 0)
				hasNoArgConstructor = true;
			break;
		case State.ignoring:
			break;
		}
	}

private:

	enum State : ubyte
	{
		ignoring,
		inClass,
		inStruct
	}

	State state;

	bool hasNoArgConstructor;
	bool hasDefaultArgConstructor;
}

unittest
{
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;

	StaticAnalysisConfig sac = disabledConfig();
	sac.constructor_check = Check.enabled;
	assertAnalyzerWarnings(q{
		class Cat // [warn]: This class has a zero-argument constructor as well as a constructor with one default argument. This can be confusing.
		{
			this() {}
			this(string name = "kittie") {}
		}

		struct Dog
		{
			this() {}
			this(string name = "doggie") {} // [warn]: This struct constructor can never be called with its default argument.
		}
	}c, sac);

	stderr.writeln("Unittest for ConstructorCheck passed.");
}
