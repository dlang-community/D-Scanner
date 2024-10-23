module dscanner.analysis.mismatched_args;

import dscanner.analysis.base;
import std.format : format;
import dmd.arraytypes : Identifiers;

/// Checks for mismatched argument and parameter names
extern (C++) class MismatchedArgumentCheck(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;
	mixin AnalyzerInfo!"mismatched_args_check";

	private enum string KEY = "dscanner.confusing.argument_parameter_mismatch";
	private enum string MSG = "Argument %d is named '%s', but this is the name of parameter %d";

	private string[][string] funcsWithParams;
	private bool inFunction;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.Module moduleNode)
	{
		import dmd.astcodegen : ASTCodegen;

		auto argVisitor = new ParameterVisitor!(ASTCodegen)(fileName, skipTests);
		argVisitor.visit(moduleNode);
		funcsWithParams = argVisitor.funcsWithParams;

		super.visit(moduleNode);
	}

	override void visit(AST.CallExp callExpr)
	{
		super.visit(callExpr);

		auto funcIdent = callExpr.e1.isIdentifierExp();
		if (callExpr.arguments is null || funcIdent is null || funcIdent.ident is null)
			return;

		string funcName = cast(string) funcIdent.ident.toString();
		if ((funcName in funcsWithParams) is null || (*callExpr.arguments).length != funcsWithParams[funcName].length)
			return;

		auto namedArgsPositions = getNamedArgsPositions(callExpr.names, funcName);
		string[] args;

		foreach (int idx, arg; *callExpr.arguments)
		{
			if (auto identifier = arg.isIdentifierExp())
			{
				if (identifier.ident)
				{
					if ((idx in namedArgsPositions) is null)
						args ~= cast(string) identifier.ident.toString();
					else
						args ~= "";
				}
				else
				{
					return;
				}
			}
			else
			{
				args ~= "";
			}
		}

		foreach_reverse (argIdx, arg; args)
		{
			foreach_reverse (paramIdx, param; funcsWithParams[funcName])
			{
				if (arg == param)
				{
					if (argIdx == paramIdx)
						break;

					addErrorMessage(callExpr.loc.linnum, callExpr.loc.charnum,KEY,
						MSG.format(argIdx + 1, arg, paramIdx + 1));

					return;
				}
			}
		}
	}

	private extern (D) bool[int] getNamedArgsPositions(Identifiers* names, string funcName)
	{
		bool[int] namedArgsPositions;

		if (names is null || (funcName in funcsWithParams) is null)
			return namedArgsPositions;

		auto funcParams = funcsWithParams[funcName];

		foreach (name; *names)
		{
			if (name is null)
				continue;

			string argName = cast(string) name.toString();
			int idx;
			for (idx = 0; idx < funcParams.length; idx++)
				if (funcParams[idx] == argName)
					break;

			namedArgsPositions[idx] = true;
		}

		return namedArgsPositions;
	}
}

extern (C++) class ParameterVisitor(AST) : BaseAnalyzerDmd
{
	alias visit = BaseAnalyzerDmd.visit;

	public string[][string] funcsWithParams;
	private string currentFunc;
	private bool ignoreCurrentFunc;
	private bool inFunction;

	extern (D) this(string fileName, bool skipTests = false)
	{
		super(fileName, skipTests);
	}

	override void visit(AST.TemplateDeclaration templateDecl)
	{
		if (inFunction)
			return;

		inFunction = true;
		super.visit(templateDecl);
	}

	override void visit(AST.FuncDeclaration funcDecl)
	{
		if (inFunction)
			return;

		inFunction = true;
		string lastFunc = currentFunc;
		currentFunc = cast(string) funcDecl.ident.toString();
		funcsWithParams[currentFunc] = [];

		bool ignoreLast = ignoreCurrentFunc;
		ignoreCurrentFunc = false;

		super.visit(funcDecl);

		if (ignoreCurrentFunc)
			funcsWithParams.remove(currentFunc);

		ignoreCurrentFunc = ignoreLast;
		currentFunc = lastFunc;
	}

	override void visit(AST.Parameter parameter)
	{
		if (parameter.ident is null)
		{
			ignoreCurrentFunc = true;
			return;
		}

		funcsWithParams[currentFunc] ~= cast(string) parameter.ident.toString();
	}
}

unittest
{
	import dscanner.analysis.helpers : assertAnalyzerWarningsDMD;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.mismatched_args_check = Check.enabled;

	assertAnalyzerWarningsDMD(q{
		void foo(int x, int y)
		{
		}

		void bar()
		{
			int x = 1;
			int y = 2;
			foo(y, x); // [warn]: Argument 2 is named 'x', but this is the name of parameter 1
			foo(y + 1, x); // [warn]: Argument 2 is named 'x', but this is the name of parameter 1
			foo(y + 1, f(x));
			foo(x: y, y: x);
			foo(y, 0); // [warn]: Argument 1 is named 'y', but this is the name of parameter 2

			// foo(y: y, x); // TODO: this shouldn't error
			foo(x, y: x); // TODO: this should error
			foo(y, y: x); // [warn]: Argument 1 is named 'y', but this is the name of parameter 2
		}
	}c, sac);

	stderr.writeln("Unittest for MismatchedArgumentCheck passed.");
}
