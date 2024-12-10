//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.function_attributes;

import dscanner.analysis.base;
import dmd.astenums : STC, MOD, MODFlags;
import dmd.tokens : Token, TOK;
import std.string : format;

/**
 * Prefer
 * ---
 * int getStuff() const {}
 * ---
 * to
 * ---
 * const int getStuff() {}
 * ---
 */
extern (C++) class FunctionAttributeCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"function_attribute_check";

	private enum KEY = "dscanner.confusing.function_attributes";
	private enum CONST_MSG = "Zero-parameter '@property' function should be marked 'const', 'inout', or 'immutable'.";
	private enum ABSTRACT_MSG = "'abstract' attribute is redundant in interface declarations";
	private enum RETURN_MSG = "'%s' is not an attribute of the return type. Place it after the parameter list to clarify.";

	private bool inInterface = false;
	private bool inAggregate = false;
	private Token[] tokens;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
		getTokens();
	}

	private void getTokens()
	{
		import dscanner.utils : readFile;
		import dmd.errorsink : ErrorSinkNull;
		import dmd.globals : global;
		import dmd.lexer : Lexer;

		auto bytes = readFile(fileName) ~ '\0';
		__gshared ErrorSinkNull errorSinkNull;
		if (!errorSinkNull)
			errorSinkNull = new ErrorSinkNull;

		scope lexer = new Lexer(null, cast(char*) bytes, 0, bytes.length, 0, 0, errorSinkNull, &global.compileEnv);
		while (lexer.nextToken() != TOK.endOfFile)
			tokens ~= lexer.token;
	}

	mixin visitAggregate!(AST.InterfaceDeclaration, true);
	mixin visitAggregate!(AST.ClassDeclaration);
	mixin visitAggregate!(AST.StructDeclaration);
	mixin visitAggregate!(AST.UnionDeclaration);

	private template visitAggregate(NodeType, bool isInterface = false)
	{
		override void visit(NodeType node)
		{
			immutable bool oldInAggregate = inAggregate;
			immutable bool oldInInterface = inInterface;

			inAggregate = !isStaticAggregate(node.loc.linnum, node.loc.charnum);
			inInterface = isInterface;
			super.visit(node);

			inAggregate = oldInAggregate;
			inInterface = oldInInterface;
		}
	}

	private bool isStaticAggregate(uint lineNum, uint charNum)
	{
		import std.algorithm : any, filter;

		return tokens.filter!(token => token.loc.linnum == lineNum && token.loc.charnum <= charNum)
			.filter!(token => token.value >= TOK.struct_ && token.value <= TOK.immutable_)
			.any!(token => token.value == TOK.static_);
	}

	override void visit(AST.FuncDeclaration fd)
	{
		import std.algorithm : canFind, find, filter, until;
		import std.array : array;
		import std.range : retro;

		super.visit(fd);

		if (fd.type is null)
			return;

		string funcName = fd.ident is null ? "" : cast(string) fd.ident.toString();
		ulong fileOffset = cast(ulong) fd.loc.fileOffset;
		ulong lineNum = cast(ulong) fd.loc.linnum;
		ulong charNum = cast(ulong) fd.loc.charnum;
		ulong[2] index = [fileOffset, fileOffset + funcName.length];
		ulong[2] lines = [lineNum, lineNum];
		ulong[2] columns = [charNum, charNum + funcName.length];

		if (inInterface)
		{
			immutable bool isAbstract = (fd.storage_class & STC.abstract_) > 0;
			if (isAbstract)
			{
				auto offset = tokens.filter!(t => t.loc.linnum >= fd.loc.linnum)
					.until!(t => t.value == TOK.leftCurly)
					.array
					.retro()
					.find!(t => t.value == TOK.abstract_)
					.front.loc.fileOffset;

				addErrorMessage(
					index, lines, columns, KEY, ABSTRACT_MSG,
					[AutoFix.replacement(offset, offset + 8, "", "Remove `abstract` attribute")]
				);

				return;
			}
		}

		auto tf = fd.type.isTypeFunction();

		if (inAggregate && tf)
		{
			string storageTok = getConstLikeStorage(tf.mod);
			auto bodyStartToken = TOK.leftCurly;
			if (fd.fbody is null)
				bodyStartToken = TOK.semicolon;

			Token[] funcTokens = tokens.filter!(t => t.loc.fileOffset > fd.loc.fileOffset)
				.until!(t => t.value == TOK.leftCurly || t.value == bodyStartToken)
				.array;

			if (storageTok is null)
			{
				bool isStatic = (fd.storage_class & STC.static_) > 0;
				bool isZeroParamProperty = tf.isProperty() && tf.parameterList.parameters.length == 0;
				auto propertyRange = funcTokens.retro()
					.until!(t => t.value == TOK.rightParenthesis)
					.find!(t => t.ident.toString() == "property")
					.find!(t => t.value == TOK.at);

				if (!isStatic && isZeroParamProperty && !propertyRange.empty)
					addErrorMessage(
						index, lines, columns, KEY, CONST_MSG,
						[
							AutoFix.insertionAt(propertyRange.front.loc.fileOffset, "const "),
							AutoFix.insertionAt(propertyRange.front.loc.fileOffset, "inout "),
							AutoFix.insertionAt(propertyRange.front.loc.fileOffset, "immutable "),
						]
					);
			}
			else
			{
				bool hasConstLikeAttribute = funcTokens.retro()
					.canFind!(t => t.value == TOK.const_ || t.value == TOK.immutable_ || t.value == TOK.inout_);

				if (!hasConstLikeAttribute)
				{
					auto funcRange = tokens.filter!(t => t.loc.linnum >= fd.loc.linnum)
						.until!(t => t.value == TOK.leftCurly || t.value == TOK.semicolon);
					auto parensToken = funcRange.until!(t => t.value == TOK.leftParenthesis)
						.array
						.retro()
						.front;
					auto funcEndToken = funcRange.array
						.retro()
						.find!(t => t.value == TOK.rightParenthesis)
						.front;
					auto constLikeToken = funcRange
						.find!(t => t.value == TOK.const_ || t.value == TOK.inout_ || t.value == TOK.immutable_)
						.front;

					string modifier;
					if (constLikeToken.value == TOK.const_)
						modifier = " const";
					else if (constLikeToken.value == TOK.inout_)
						modifier = " inout";
					else
						modifier = " immutable";

					AutoFix fix1 = AutoFix
						.replacement(constLikeToken.loc.fileOffset, constLikeToken.loc.fileOffset + modifier.length,
							"", "Move" ~ modifier ~ " after parameter list")
						.concat(AutoFix.insertionAt(funcEndToken.loc.fileOffset + 1, modifier));

					AutoFix fix2 = AutoFix.replacement(constLikeToken.loc.fileOffset + modifier.length - 1,
							constLikeToken.loc.fileOffset + modifier.length, "(", "Make return type" ~ modifier)
						.concat(AutoFix.insertionAt(parensToken.loc.fileOffset - 1, ")"));

					addErrorMessage(index, lines, columns, KEY, RETURN_MSG.format(storageTok), [fix1, fix2]);
				}
			}
		}
	}

	private extern (D) string getConstLikeStorage(MOD mod)
	{
		if (mod & MODFlags.const_)
			return "const";

		if (mod & MODFlags.immutable_)
			return "immutable";

		if (mod & MODFlags.wild)
			return "inout";

		return null;
	}
}

unittest
{
	import dscanner.analysis.config : Check, disabledConfig, StaticAnalysisConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD, assertAutoFix;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.function_attribute_check = Check.enabled;

	assertAnalyzerWarningsDMD(q{
		int foo() @property { return 0; }

		class ClassName {
			const int confusingConst() { return 0; } // [warn]: 'const' is not an attribute of the return type. Place it after the parameter list to clarify.
			int bar() @property { return 0; } // [warn]: Zero-parameter '@property' function should be marked 'const', 'inout', or 'immutable'.
			static int barStatic() @property { return 0; }
			int barConst() const @property { return 0; }
		}

		struct StructName {
			int bar() @property { return 0; } // [warn]: Zero-parameter '@property' function should be marked 'const', 'inout', or 'immutable'.
			static int barStatic() @property { return 0; }
			int barConst() const @property { return 0; }
		}

		union UnionName {
			int bar() @property { return 0; } // [warn]: Zero-parameter '@property' function should be marked 'const', 'inout', or 'immutable'.
			static int barStatic() @property { return 0; }
			int barConst() const @property { return 0; }
		}

		interface InterfaceName {
			int bar() @property; // [warn]: Zero-parameter '@property' function should be marked 'const', 'inout', or 'immutable'.
			static int barStatic() @property { return 0; }
			int barConst() const @property;
			abstract int method(); // [warn]: 'abstract' attribute is redundant in interface declarations
		}
	}c, sac);

	// Test taken from phobos / utf.d, shouldn't warn
	assertAnalyzerWarningsDMD(q{
		static struct R
		{
			@safe pure @nogc nothrow:
			this(string s) { this.s = s; }
			@property bool empty() { return idx == s.length; }
			@property char front() { return s[idx]; }
			void popFront() { ++idx; }
			size_t idx;
			string s;
		}
	}c, sac);

	assertAutoFix(q{
		int foo() @property { return 0; }

		class ClassName {
			const int confusingConst() { return 0; } // fix:0
			const int confusingConst() { return 0; } // fix:1

			int bar() @property { return 0; } // fix:0
			int bar() @property { return 0; } // fix:1
			int bar() @property { return 0; } // fix:2
		}

		struct StructName {
			int bar() @property { return 0; } // fix:0
		}

		union UnionName {
			int bar() @property { return 0; } // fix:0
		}

		interface InterfaceName {
			int bar() @property; // fix:0

			abstract int method(); // fix
		}
	}c, q{
		int foo() @property { return 0; }

		class ClassName {
			int confusingConst() const { return 0; } // fix:0
			const(int) confusingConst() { return 0; } // fix:1

			int bar() const @property { return 0; } // fix:0
			int bar() inout @property { return 0; } // fix:1
			int bar() immutable @property { return 0; } // fix:2
		}

		struct StructName {
			int bar() const @property { return 0; } // fix:0
		}

		union UnionName {
			int bar() const @property { return 0; } // fix:0
		}

		interface InterfaceName {
			int bar() const @property; // fix:0

			int method(); // fix
		}
	}c, sac);

	stderr.writeln("Unittest for ObjectConstCheck passed.");
}
