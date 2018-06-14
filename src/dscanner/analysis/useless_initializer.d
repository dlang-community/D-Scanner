//          Copyright Basile Burg 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
module dscanner.analysis.useless_initializer;

import dscanner.analysis.base;
import dscanner.utils : safeAccess;
import containers.dynamicarray;
import containers.hashmap;
import dparse.ast;
import dparse.lexer;
import std.algorithm;
import std.range : empty;
import std.stdio;

/*
Limitations:
	- Stuff s = Stuff.init does not work with type with postfixes`*` `[]`.
	- Stuff s = Stuff.init is only detected for struct within the module.
	- BasicType b = BasicType(v), default init used in BasicType ctor, e.g int(8).
*/

/**
 * Check that detects the initializers that are
 * not different from the implcit initializer.
 */
final class UselessInitializerChecker : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

private:

	enum key = "dscanner.useless-initializer";

	version(unittest)
	{
		enum msg = "X";
	}
	else
	{
		enum msg = "Variable `%s` initializer is useless because it does not differ from the default value";
	}

	static immutable intDefs = ["0", "0L", "0UL", "0uL", "0U", "0x0", "0b0"];

	HashMap!(string, bool) _structCanBeInit;
	DynamicArray!(string) _structStack;
	DynamicArray!(bool) _inStruct;
	DynamicArray!(bool) _atDisabled;
	bool _inTest;

public:

	///
	this(string fileName, bool skipTests = false)
	{
		super(fileName, null, skipTests);
		_inStruct.insert(false);
	}

	override void visit(const(Unittest) test)
	{
		if (skipTests)
			return;
		_inTest = true;
		test.accept(this);
		_inTest = false;
	}

	override void visit(const(StructDeclaration) decl)
	{
		if (_inTest)
			return;

		assert(_inStruct.length > 1);

		const string structName = _inStruct[$-2] ?
			_structStack.back() ~ "." ~ decl.name.text :
		  decl.name.text;

		_structStack.insert(structName);
		_structCanBeInit[structName] = false;
		_atDisabled.insert(false);
		decl.accept(this);
		_structStack.removeBack();
		_atDisabled.removeBack();
	}

	override void visit(const(Declaration) decl)
	{
		_inStruct.insert(decl.structDeclaration !is null);
		decl.accept(this);
		if (_inStruct.length > 1 && _inStruct[$-2] && decl.constructor &&
			((decl.constructor.parameters && decl.constructor.parameters.parameters.length == 0) ||
			!decl.constructor.parameters))
		{
			_atDisabled[$-1] = decl.attributes
				.canFind!(a => a.atAttribute !is null && a.atAttribute.identifier.text == "disable");
		}
		_inStruct.removeBack();
	}

	override void visit(const(Constructor) decl)
	{
		if (_inStruct.length > 1 && _inStruct[$-2] &&
			((decl.parameters && decl.parameters.parameters.length == 0) || !decl.parameters))
		{
			const bool canBeInit = !_atDisabled[$-1];
			_structCanBeInit[_structStack.back()] = canBeInit;
			if (!canBeInit)
				_structCanBeInit[_structStack.back()] = !decl.memberFunctionAttributes
					.canFind!(a => a.atAttribute !is null && a.atAttribute.identifier.text == "disable");
		}
		decl.accept(this);
	}

	// issue 473, prevent to visit delegates that contain duck type checkers.
	override void visit(const(TypeofExpression)) {}

	// issue 473, prevent to check expressions in __traits(compiles, ...)
	override void visit(const(TraitsExpression) e)
	{
		if (e.identifier.text == "compiles")
		{
			return;
		}
		else
		{
			e.accept(this);
		}
	}

	override void visit(const(VariableDeclaration) decl)
	{
		if (!decl.type || !decl.type.type2 ||
			// initializer has to appear clearly in generated ddoc
			decl.comment !is null ||
			// issue 474, manifest constants HAVE to be initialized.
			decl.storageClasses.canFind!(a => a.token == tok!"enum"))
		{
			return;
		}

		foreach (declarator; decl.declarators)
		{
			if (!declarator.initializer ||
				!declarator.initializer.nonVoidInitializer ||
				declarator.comment !is null)
			{
				continue;
			}

			version(unittest)
			{
				enum warn = q{addErrorMessage(declarator.name.line,
					declarator.name.column, key, msg);};
			}
			else
			{
				import std.format : format;
				enum warn = q{addErrorMessage(declarator.name.line,
				declarator.name.column, key, msg.format(declarator.name.text));};
			}

			// ---  Info about the declaration type --- //
			const bool isPtr = decl.type.typeSuffixes && decl.type.typeSuffixes
				.canFind!(a => a.star != tok!"");
			const bool isArr = decl.type.typeSuffixes && decl.type.typeSuffixes
				.canFind!(a => a.array);

			bool isStr, isSzInt;
			Token customType;

			if (const TypeIdentifierPart tip = safeAccess(decl).type.type2.typeIdentifierPart)
			{
				if (!tip.typeIdentifierPart)
				{
					customType = tip.identifierOrTemplateInstance.identifier;
					isStr = customType.text.among("string", "wstring", "dstring") != 0;
					isSzInt = customType.text.among("size_t", "ptrdiff_t") != 0;
				}
			}

			// --- 'BasicType/Symbol AssignExpression' ---//
			const NonVoidInitializer nvi = declarator.initializer.nonVoidInitializer;
			const UnaryExpression ue = cast(UnaryExpression) nvi.assignExpression;
			if (ue && ue.primaryExpression)
			{
				const Token value = ue.primaryExpression.primary;

				if (!isPtr && !isArr && !isStr && decl.type.type2.builtinType != tok!"")
				{
					switch(decl.type.type2.builtinType)
					{
					// check for common cases of default values
					case tok!"byte",    tok!"ubyte":
					case tok!"short",   tok!"ushort":
					case tok!"int",     tok!"uint":
					case tok!"long",    tok!"ulong":
					case tok!"cent",    tok!"ucent":
					case tok!"bool":
						if (intDefs.canFind(value.text) || value == tok!"false")
							mixin(warn);
						goto default;
					default:
					// check for BasicType.init
						if (ue.primaryExpression.basicType.type == decl.type.type2.builtinType &&
							ue.primaryExpression.primary.text == "init" &&
							!ue.primaryExpression.expression)
							mixin(warn);
					}
				}
				else if (isSzInt)
				{
					if (intDefs.canFind(value.text))
						mixin(warn);
				}
				else if (isPtr || isStr)
				{
					if (str(value.type) == "null")
						mixin(warn);
				}
				else if (isArr)
				{
					if (str(value.type) == "null")
						mixin(warn);
					else if (nvi.arrayInitializer && nvi.arrayInitializer.arrayMemberInitializations.length == 0)
						mixin(warn);
				}
			}

			else if (const IdentifierOrTemplateInstance iot = safeAccess(ue)
				.unaryExpression.primaryExpression.identifierOrTemplateInstance)
			{
				// Symbol s = Symbol.init
				if (ue && customType != tok!"" && iot.identifier == customType &&
					ue.identifierOrTemplateInstance && ue.identifierOrTemplateInstance.identifier.text == "init")
				{
					if (customType.text in _structCanBeInit)
					{
						if  (!_structCanBeInit[customType.text])
							mixin(warn);
					}
				}
			}

			// 'Symbol ArrayInitializer' : assumes Symbol is an array b/c of the Init
			else if (nvi.arrayInitializer && (isArr || isStr))
			{
				if (nvi.arrayInitializer.arrayMemberInitializations.length == 0)
					mixin(warn);
			}
		}

		decl.accept(this);
	}
}

@system unittest
{
	import dscanner.analysis.config : Check, disabledConfig, StaticAnalysisConfig;
	import dscanner.analysis.helpers: assertAnalyzerWarnings;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig;
	sac.useless_initializer = Check.enabled;

	// fails
	assertAnalyzerWarnings(q{
		struct S {}
		ubyte a = 0x0;      // [warn]: X
		int a = 0;          // [warn]: X
		ulong a = 0;        // [warn]: X
		int* a = null;      // [warn]: X
		Foo* a = null;      // [warn]: X
		int[] a = null;     // [warn]: X
		int[] a = [];       // [warn]: X
		string a = null;    // [warn]: X
		string a = null;    // [warn]: X
		wstring a = null;   // [warn]: X
		dstring a = null;   // [warn]: X
		size_t a = 0;       // [warn]: X
		ptrdiff_t a = 0;    // [warn]: X
		string a = [];      // [warn]: X
		char[] a = null;    // [warn]: X
		int a = int.init;   // [warn]: X
		char a = char.init; // [warn]: X
		S s = S.init;       // [warn]: X
		bool a = false;     // [warn]: X
	}, sac);

	// passes
	assertAnalyzerWarnings(q{
		struct D {@disable this();}
		struct E {this() @disable;}
		ubyte a = 0xFE;
		int a = 1;
		ulong a = 1;
		int* a = &a;
		Foo* a = &a;
		int[] a = &a;
		int[] a = [0];
		string a = "sdf";
		string a = "sdg"c;
		wstring a = "sdg"w;
		dstring a = "fgh"d;
		string a = q{int a;};
		size_t a = 1;
		ptrdiff_t a;
		ubyte a;
		int a;
		ulong a;
		int* a;
		Foo* a;
		int[] a;
		string a;
		wstring a;
		dstring a;
		string a = ['a'];
		string a = "";
		string a = ""c;
		wstring a = ""w;
		dstring a = ""d;
		string a = q{};
		char[] a = "ze";
		S s = S(0,1);
		S s = s.call();
		enum {a}
		enum ubyte a = 0;
		static assert(is(typeof((){T t = T.init;})));
		void foo(){__traits(compiles, (){int a = 0;}).writeln;}
		bool a;
		D d = D.init;
		E e = E.init;
		NotKnown nk = NotKnown.init;
	}, sac);

	stderr.writeln("Unittest for UselessInitializerChecker passed.");
}

