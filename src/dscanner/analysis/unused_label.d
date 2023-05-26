// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.unused_label;

import dscanner.analysis.base;
import dmd.tokens;

/**
 * Checks for labels that are never used.
 */
extern (C++) class UnusedLabelCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"unused_label_check";

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.Module m)
	{
		pushScope();
		super.visit(m);
		popScope();
	}

	override void visit(AST.LabelStatement ls)
	{
		Label* label = ls.ident.toString() in current;

		if (label is null)
		{
			current[ls.ident.toString()] = Label(ls.ident.toString(),
					ls.loc.linnum, ls.loc.charnum, false);
		}
		else
		{
			label.line = ls.loc.linnum;
			label.column = ls.loc.charnum;
		}

		super.visit(ls);
	}

	override void visit(AST.GotoStatement gs)
	{
		if (gs.ident)
			labelUsed(gs.ident.toString());
	}

	override void visit(AST.BreakStatement bs)
	{
		if (bs.ident)
			labelUsed(bs.ident.toString());
	}

	override void visit(AST.StaticForeachStatement s)
	{
		if (s.sfe.aggrfe)
			super.visit(s.sfe.aggrfe);

		if (s.sfe.rangefe)
			super.visit(s.sfe.rangefe);
	}

	override void visit(AST.ContinueStatement cs)
	{
		if (cs.ident)
			labelUsed(cs.ident.toString());
	}

	override void visit(AST.FuncDeclaration fd)
	{
		pushScope();
		super.visit(fd);
		popScope();
	}

	override void visit(AST.FuncLiteralDeclaration fd)
	{
		pushScope();
		super.visit(fd);
		popScope();
	}

	override void visit(AST.AsmStatement as)
	{
		if (!as.tokens)
			return;

		// Look for jump instructions
		bool jmp;
		if (getFirstLetterOf(cast(char*) as.tokens[0].ptr) == 'j')
			jmp = true;

		// Last argument of the jmp instruction will be the label
		Token* label;
		for (label = as.tokens; label.next; label = label.next) {}

		if (jmp && label.ident)
			labelUsed(label.ident.toString());
	}

	private char getFirstLetterOf(char* str)
	{
		import std.ascii : isAlpha;

		if (str is null)
			return '\0';

		while (str && !isAlpha(*str))
			str++;

		return *str;
	}

private:

	static struct Label
	{
		const(char)[] name;
		size_t line;
		size_t column;
		bool used;
	}

	extern (D) Label[const(char)[]][] stack;

	extern (D) auto ref current()
	{
		return stack[$ - 1];
	}

	void pushScope()
	{
		stack.length++;
	}

	void popScope()
	{
		import std.conv : to;

		foreach (label; current.byValue())
		{
			if (label.line == size_t.max || label.column == size_t.max)
			{
				// TODO: handle unknown labels
			}
			else if (!label.used)
			{
				addErrorMessage(label.line, label.column, "dscanner.suspicious.unused_label",
						"Label \"" ~ to!string(label.name) ~ "\" is not used.");
			}
		}
		stack.length--;
	}

	extern (D) void labelUsed(const(char)[] name)
	{
		Label* entry = name in current;
		if (entry is null)
			current[name] = Label(name, size_t.max, size_t.max, true);
		else
			entry.used = true;
	}
}

unittest
{
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.unused_label_check = Check.enabled;
	assertAnalyzerWarningsDMD(q{
		int testUnusedLabel()
		{
		    int x = 0;
		A: // [warn]: Label "A" is not used.
			if (x) goto B;
			x++;
		B:
			goto C;
			void foo()
			{
			C: // [warn]: Label "C" is not used.
				return;
			}
		C:
			void bar()
			{
				goto D;
			D:
				return;
			}
		D: // [warn]: Label "D" is not used.
			goto E;
			() {
			E: // [warn]: Label "E" is not used.
				return;
			}();
		E:
			() {
				goto F;
			F:
				return;
			}();
		F: // [warn]: Label "F" is not used.
			return x;
		G: // [warn]: Label "G" is not used.
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		void testAsm()
		{
			asm { jmp lbl;}
			lbl:
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		void testAsm()
		{
			asm { mov RAX,1;}
			lbl: // [warn]: Label "lbl" is not used.
		}
	}c, sac);

	// from std.math
	assertAnalyzerWarningsDMD(q{
		real polyImpl() {
			asm {
				jecxz return_ST;
		    }
		}
	}c, sac);

	// a label might be hard to find, e.g. in a mixin
	assertAnalyzerWarningsDMD(q{
		real polyImpl() {
			mixin("return_ST: return 1;");
			asm {
				jecxz return_ST;
		    }
		}
	}c, sac);

	assertAnalyzerWarningsDMD(q{
		void testAsm()
		{
			asm nothrow @nogc
            {
                "movgr2fcsr $r0,%0" :
                : "r" (newState & (roundingMask | allExceptions));
            }
		}
	}c, sac);

	stderr.writeln("Unittest for UnusedLabelCheck passed.");
}
