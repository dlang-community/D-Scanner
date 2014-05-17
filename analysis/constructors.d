module analysis.constructors;

import std.d.ast;
import std.d.lexer;
import std.stdio;
import analysis.base;
import analysis.helpers;


class ConstructorCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const ClassDeclaration classDeclaration)
	{
		bool oldHasDefault = hasDefaultArgConstructor;
		bool oldHasNoArg = hasNoArgConstructor;
		hasNoArgConstructor = false;
		hasDefaultArgConstructor = false;
		State prev = state;
		state = State.inClass;
		classDeclaration.accept(this);
		if (hasNoArgConstructor && hasDefaultArgConstructor)
		{
			addErrorMessage(classDeclaration.name.line,
				classDeclaration.name.column, "This class has a zero-argument"
				~ " constructor as well as a constructor with one default"
				~ " argument. This can be confusing");
		}
		hasDefaultArgConstructor = oldHasDefault;
		hasNoArgConstructor = oldHasNoArg;
		state = prev;
	}

	override void visit(const StructDeclaration structDeclaration)
	{
		State prev = state;
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

	enum State: ubyte
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
	assertAnalyzerWarnings(q{
		class Cat // [warn]: This class has a zero-argument constructor as well as a constructor with one default argument. This can be confusing
		{
			this() {}
			this(string name = "kittie") {}
		}

		struct Dog
		{
			this() {}
			this(string name = "doggie") {} // [warn]: This struct constructor can never be called with its default argument.
		}
	}c, analysis.run.AnalyzerCheck.constructor_check);

	stderr.writeln("Unittest for ConstructorCheck passed.");
}

