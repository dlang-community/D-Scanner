module dscanner.analysis.mismatched_args;

import dscanner.analysis.base;
import dscanner.utils : safeAccess;
import dsymbol.scope_;
import dsymbol.symbol;
import dparse.ast;
import dparse.lexer : tok, Token;
import dsymbol.builtin.names;

/// Checks for mismatched argument and parameter names
final class MismatchedArgumentCheck : BaseAnalyzer
{
	mixin AnalyzerInfo!"mismatched_args_check";

	///
	this(BaseAnalyzerArguments args)
	{
		super(args);
	}

	override void visit(const FunctionCallExpression fce)
	{
		import std.typecons : scoped;
		import std.algorithm.iteration : each, map;
		import std.array : array;

		if (fce.arguments is null)
			return;
		auto argVisitor = scoped!ArgVisitor;
		argVisitor.visit(fce.arguments);
		const istring[] args = argVisitor.args;

		auto identVisitor = scoped!IdentVisitor;
		if (fce.unaryExpression !is null)
			identVisitor.visit(fce.unaryExpression);
		else if (fce.type !is null)
			identVisitor.visit(fce.type);

		const(DSymbol)*[] symbols = resolveSymbol(sc, identVisitor.names.length > 0
				? identVisitor.names : [CONSTRUCTOR_SYMBOL_NAME]);

		static struct ErrorMessage
		{
			const(Token)[] range;
			string message;
		}

		ErrorMessage[] messages;
		bool matched;

		foreach (sym; symbols)
		{
			// The cast is a hack because .array() confuses the compiler's overload
			// resolution code.
			const(istring)[] params = sym is null ? [] : sym.functionParameters.map!(a => a.name).array();
			const ArgMismatch[] mismatches = compareArgsToParams(params, args);
			if (mismatches.length == 0)
				matched = true;
			else
			{
				foreach (ref const mm; mismatches)
				{
					messages ~= ErrorMessage(argVisitor.tokens[mm.argIndex], createWarningFromMismatch(mm));
				}
			}
		}

		if (!matched)
			foreach (m; messages)
				addErrorMessage(m.range, KEY, m.message);
	}

	alias visit = ASTVisitor.visit;

private:

	enum string KEY = "dscanner.confusing.argument_parameter_mismatch";
}

final class IdentVisitor : ASTVisitor
{
	override void visit(const IdentifierOrTemplateInstance ioti)
	{
		import dsymbol.string_interning : internString;

		if (ioti.identifier != tok!"")
			names ~= internString(ioti.identifier.text);
		else
			names ~= internString(ioti.templateInstance.identifier.text);
	}

	override void visit(const Arguments)
	{
	}

	override void visit(const IndexExpression ie)
	{
		if (ie.unaryExpression !is null)
			visit(ie.unaryExpression);
	}

	alias visit = ASTVisitor.visit;

	istring[] names;
}

final class ArgVisitor : ASTVisitor
{
	override void visit(const NamedArgumentList al)
	{
		foreach (a; al.items)
		{
			auto u = cast(UnaryExpression) a.assignExpression;
			size_t prevArgs = args.length;
			if (u !is null && !a.name.text.length)
				visit(u);

			if (args.length == prevArgs)
			{
				// if we didn't get an identifier in the unary expression,
				// assume it's a good argument
				args ~= istring.init;
				tokens ~= a.tokens;
			}
		}
	}

	override void visit(const UnaryExpression unary)
	{
		import dsymbol.string_interning : internString;

		if (auto iot = unary.safeAccess.primaryExpression.identifierOrTemplateInstance.unwrap)
		{
			if (iot.identifier == tok!"")
				return;
			immutable t = iot.identifier;
			tokens ~= [t];
			args ~= internString(t.text);
		}
	}

	alias visit = ASTVisitor.visit;

	const(Token[])[] tokens;
	istring[] args;
}

const(DSymbol)*[] resolveSymbol(const Scope* sc, const istring[] symbolChain)
{
	import std.array : empty;

	const(DSymbol)*[] matchingSymbols = sc.getSymbolsByName(symbolChain[0]);
	if (matchingSymbols.empty)
		return null;

	foreach (ref symbol; matchingSymbols)
	{
		inner: foreach (i; 1 .. symbolChain.length)
		{
			if (symbol.kind == CompletionKind.variableName
					|| symbol.kind == CompletionKind.memberVariableName
					|| symbol.kind == CompletionKind.functionName
					|| symbol.kind == CompletionKind.aliasName)
				symbol = symbol.type;
			if (symbol is null)
			{
				symbol = null;
				break inner;
			}
			auto p = symbol.getPartsByName(symbolChain[i]);
			if (p.empty)
			{
				symbol = null;
				break inner;
			}
			symbol = p[0];
		}
	}
	return matchingSymbols;
}

struct ArgMismatch
{
	size_t argIndex;
	size_t paramIndex;
	string name;
}

immutable(ArgMismatch[]) compareArgsToParams(const istring[] params, const istring[] args) pure
{
	import std.exception : assumeUnique;

	if (args.length != params.length)
		return [];
	ArgMismatch[] retVal;
	foreach (i, arg; args)
	{
		if (arg is null || arg == params[i])
			continue;
		foreach (j, param; params)
			if (param == arg)
				retVal ~= ArgMismatch(i, j, arg);
	}
	return assumeUnique(retVal);
}

string createWarningFromMismatch(const ArgMismatch mismatch) pure
{
	import std.format : format;

	return "Argument %d is named '%s', but this is the name of parameter %d".format(
			mismatch.argIndex + 1, mismatch.name, mismatch.paramIndex + 1);
}

unittest
{
	import dsymbol.string_interning : internString;
	import std.algorithm.iteration : map;
	import std.array : array;
	import std.conv : to;

	{
		istring[] args = ["a", "b", "c"].map!internString().array();
		istring[] params = ["a", "b", "c"].map!internString().array();
		immutable res = compareArgsToParams(params, args);
		assert(res == []);
	}

	{
		istring[] args = ["a", "c", "b"].map!internString().array();
		istring[] params = ["a", "b", "c"].map!internString().array();
		immutable res = compareArgsToParams(params, args);
		assert(res == [ArgMismatch(1, 2, "c"), ArgMismatch(2, 1, "b")], to!string(res));
	}

	{
		istring[] args = ["a", "c", "b"].map!internString().array();
		istring[] params = ["alpha", "bravo", "c"].map!internString().array();
		immutable res = compareArgsToParams(params, args);
		assert(res == [ArgMismatch(1, 2, "c")]);
	}

	{
		istring[] args = ["a", "b"].map!internString().array();
		istring[] params = [null, "b"].map!internString().array();
		immutable res = compareArgsToParams(params, args);
		assert(res == []);
	}
}

unittest
{
	import dscanner.analysis.helpers : assertAnalyzerWarnings;
	import dscanner.analysis.config : StaticAnalysisConfig, Check, disabledConfig;
	import std.stdio : stderr;

	StaticAnalysisConfig sac = disabledConfig();
	sac.mismatched_args_check = Check.enabled;
	assertAnalyzerWarnings(q{
		void foo(int x, int y)
		{
		}

		void bar()
		{
			int x = 1;
			int y = 2;
			foo(y, x); /+
			       ^ [warn]: Argument 2 is named 'x', but this is the name of parameter 1 +/
			foo(y + 1, x); /+
			           ^ [warn]: Argument 2 is named 'x', but this is the name of parameter 1 +/
			foo(y + 1, f(x));
			foo(x: y, y: x);
			foo(y, 0); /+
			    ^ [warn]: Argument 1 is named 'y', but this is the name of parameter 2 +/

			// foo(y: y, x); // TODO: this shouldn't error
			foo(x, y: x); // TODO: this should error
			foo(y, y: x); /+
			    ^ [warn]: Argument 1 is named 'y', but this is the name of parameter 2 +/
		}
	}c, sac);
	stderr.writeln("Unittest for MismatchedArgumentCheck passed.");
}
