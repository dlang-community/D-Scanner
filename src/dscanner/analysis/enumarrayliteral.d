//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.enumarrayliteral;

import dscanner.analysis.base;

extern(C++) class EnumArrayVisitor(AST) : BaseAnalyzerDmd
{
	mixin AnalyzerInfo!"enum_array_literal_check";
	alias visit = BaseAnalyzerDmd.visit;

	extern(D) this(string fileName)
	{
		super(fileName);
	}

	override void visit(AST.VarDeclaration vd)
    {
		import dmd.astenums : STC, InitKind;
		import std.string : toStringz;

		string message = "This enum may lead to unnecessary allocation at run-time."
						~ " Use 'static immutable "
						~ vd.ident.toString().idup() ~ " = [ ...' instead.";

		if (!vd.type && vd._init.kind == InitKind.array && vd.storage_class & STC.manifest)
			addErrorMessage(cast(ulong) vd.loc.linnum,
				cast(ulong) vd.loc.charnum, KEY,
				message);
		super.visit(vd);
	}

	private enum KEY = "dscanner.performance.enum_array_literal";
}