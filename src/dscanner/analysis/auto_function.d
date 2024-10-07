//          Copyright Basile Burg 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.auto_function;

import dscanner.analysis.base;
import std.conv : to;
import std.algorithm.searching : canFind;

/**
 * Checks for auto functions without return statement.
 *
 * Auto function without return statement can be an omission and are not
 * detected by the compiler. However sometimes they can be used as a trick
 * to infer attributes.
 */
extern (C++) class AutoFunctionChecker(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"auto_function_check";

	private bool foundReturn;
	private bool foundFalseAssert;
	private bool inMixin;
	private bool foundReturnLiteral;
	private string[] literalsWithReturn;

	private enum string KEY = "dscanner.suspicious.missing_return";
	private enum string MESSAGE = "Auto function without return statement, prefer replacing auto with void";
	private enum string MESSAGE_INSERT = "Auto function without return statement, prefer inserting void to be explicit";

	///
	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.FuncDeclaration d)
	{
		import dmd.astenums : STC, STMT;

		if (d.storage_class & STC.disable || d.fbody is null || (d.fbody && d.fbody.isReturnStatement()))
			return;

		ulong lineNum = cast(ulong) d.loc.linnum;
		ulong charNum = cast(ulong) d.loc.charnum;

		auto oldFoundReturn = foundReturn;
		auto oldFoundFalseAssert = foundFalseAssert;

		foundReturn = false;
		foundFalseAssert = false;
		super.visitFuncBody(d);

		if (!foundReturn && !foundFalseAssert)
		{
			if (d.storage_class & STC.auto_)
			{
				auto voidStart = extractVoidStartLocation(d);

				addErrorMessage(
					lineNum, charNum, KEY, MESSAGE,
					[
						AutoFix.replacement(voidStart + 1, voidStart + 6, "", "Replace `auto` with `void`")
							.concat(AutoFix.insertionAt(d.loc.fileOffset, "void "))
					]
				);
			}
			else if (auto returnType = cast(AST.TypeFunction) d.type)
			{
				if (returnType.next is null)
				{
					addErrorMessage(
						lineNum, charNum, KEY, MESSAGE_INSERT,
						[AutoFix.insertionAt(d.loc.fileOffset, "void ")]
					);
				}
			}
		}

		foundReturn = oldFoundReturn;
		foundFalseAssert = oldFoundFalseAssert;
	}

	private auto extractVoidStartLocation(AST.FuncDeclaration d)
	{
		import dmd.common.outbuffer : OutBuffer;
		import dmd.hdrgen : toCBuffer, HdrGenState;
		import std.string : indexOf;

		OutBuffer buf;
		HdrGenState hgs;
		toCBuffer(d, buf, hgs);
		string funcStr = cast(string) buf.extractSlice();
		string funcName = cast(string) d.ident.toString();
		auto funcNameStart = funcStr.indexOf(funcName);
		auto voidTokenStart = funcStr.indexOf("void");
		auto voidOffset = funcNameStart - voidTokenStart;
		return d.loc.fileOffset - voidOffset;
	}

	override void visit(AST.ReturnStatement s)
	{
		foundReturn = true;
	}

	override void visit(AST.AssertExp assertExpr)
	{
		auto ie = assertExpr.e1.isIntegerExp();
		if (ie && ie.getInteger() == 0)
			foundFalseAssert = true;
	}

	override void visit(AST.MixinStatement mixinStatement)
	{
		auto oldInMixin = inMixin;
		inMixin = true;
		super.visit(mixinStatement);
		inMixin = oldInMixin;
	}

	override void visit(AST.StringExp stringExpr)
	{
		foundReturnLiteral = foundReturnLiteral || canFind(stringExpr.toStringz(), "return");

		if (inMixin)
			foundReturn = foundReturn || foundReturnLiteral;
	}

	override void visit(AST.IdentifierExp ie)
	{
		if (inMixin)
			foundReturn = foundReturn || canFind(literalsWithReturn, to!string(ie.ident.toString()));

		super.visit(ie);
	}

	override void visit(AST.VarDeclaration vd)
	{
		auto oldFoundReturnLiteral = foundReturnLiteral;
		foundFalseAssert = false;
		super.visit(vd);

		if (foundReturnLiteral)
			literalsWithReturn ~= to!string(vd.ident.toString());

		foundReturnLiteral = oldFoundReturnLiteral;
	}
}

unittest
{
	import std.stdio : stderr;
	import std.format : format;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD, assertAutoFix;

	StaticAnalysisConfig sac = disabledConfig();
	sac.auto_function_check = Check.enabled;

	string MESSAGE = "Auto function without return statement, prefer replacing auto with void";
	string MESSAGE_INSERT = "Auto function without return statement, prefer inserting void to be explicit";

	assertAnalyzerWarningsDMD(q{
		auto ref doStuff(){} // [warn]: %s
		auto doStuff(){} // [warn]: %s
		@Custom
		auto doStuff(){} // [warn]: %s
		int doStuff(){auto doStuff(){}} // [warn]: %s
		auto doStuff(){return 0;}
		int doStuff(){/*error but not the aim*/}
	}c.format(MESSAGE, MESSAGE, MESSAGE, MESSAGE), sac);

	assertAnalyzerWarningsDMD(q{
		auto doStuff(){assert(true);} // [warn]: %s
		auto doStuff(){assert(false);}
	}c.format(MESSAGE), sac);

	assertAnalyzerWarningsDMD(q{
		auto doStuff(){assert(1);} // [warn]: %s
		auto doStuff(){assert(0);}
	}c.format(MESSAGE), sac);

	assertAnalyzerWarningsDMD(q{
		auto doStuff(){mixin("0+0");} // [warn]: %s
		auto doStuff(){mixin("return 0;");}
	}c.format(MESSAGE), sac);

	assertAnalyzerWarningsDMD(q{
		auto doStuff(){mixin("0+0");} // [warn]: %s
		auto doStuff(){mixin("static if (true)" ~ "  return " ~ 0.stringof ~ ";");}
	}c.format(MESSAGE), sac);

	assertAnalyzerWarningsDMD(q{
		auto doStuff(){} // [warn]: %s
		extern(C) auto doStuff();
	}c.format(MESSAGE), sac);

	assertAnalyzerWarningsDMD(q{
		auto doStuff(){} // [warn]: %s
		@disable auto doStuff();
	}c.format(MESSAGE), sac);

	assertAnalyzerWarningsDMD(q{
		@property doStuff(){} // [warn]: %s
		@safe doStuff(){} // [warn]: %s
		@disable doStuff();
		@safe void doStuff();
	}c.format(MESSAGE_INSERT, MESSAGE_INSERT), sac);

	assertAnalyzerWarningsDMD(q{
		enum _genSave = "return true;";
		auto doStuff(){ mixin(_genSave);}
	}, sac);

	assertAutoFix(q{
		auto ref doStuff(){} // fix
		auto doStuff(){} // fix
		@property doStuff(){} // fix
		@safe doStuff(){} // fix
		@Custom
		auto doStuff(){} // fix
	}c, q{
		ref void doStuff(){} // fix
		void doStuff(){} // fix
		@property void doStuff(){} // fix
		@safe void doStuff(){} // fix
		@Custom
		void doStuff(){} // fix
	}c, sac, true);

	stderr.writeln("Unittest for AutoFunctionChecker passed.");
}
