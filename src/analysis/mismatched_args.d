module analysis.mismatched_args;

import analysis.base : BaseAnalyzer;
import dsymbol.scope_;
import dsymbol.symbol;
import dparse.ast;
import dparse.lexer : tok;
import dsymbol.builtin.names;

/// Checks for mismatched argument and parameter names
final class MismatchedArgumentCheck : BaseAnalyzer
{
	///
	this(string fileName, const(Scope)* sc, bool skipTests = false)
	{
		super(fileName, sc, skipTests);
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
			size_t line;
			size_t column;
			string message;
		}

		ErrorMessage[] messages;
		bool matched;

		foreach (sym; symbols)
		{
			// The cast is a hack because .array() confuses the compiler's overload
			// resolution code.
			const(istring)[] params = sym is null ? [] : sym.argNames[].map!(a => cast() a).array();
			const ArgMismatch[] mismatches = compareArgsToParams(params, args);
			if (mismatches.length == 0)
				matched = true;
			else
			{
				foreach (size_t i, ref const mm; mismatches)
				{
					messages ~= ErrorMessage(argVisitor.lines[i],
							argVisitor.columns[i], createWarningFromMismatch(mm));
				}
			}
		}

		if (!matched)
			foreach (m; messages)
				addErrorMessage(m.line, m.column, KEY, m.message);
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
	override void visit(const ArgumentList al)
	{
		foreach (a; al.items)
		{
			auto u = cast(UnaryExpression) a;
			if (u !is null)
				visit(u);
			else
			{
				args ~= istring.init;
				lines ~= size_t.max;
				columns ~= size_t.max;
			}
		}
	}

	override void visit(const UnaryExpression unary)
	{
		import dsymbol.string_interning : internString;

		if (unary.primaryExpression is null)
			return;
		if (unary.primaryExpression.identifierOrTemplateInstance is null)
			return;
		if (unary.primaryExpression.identifierOrTemplateInstance.identifier == tok!"")
			return;
		immutable t = unary.primaryExpression.identifierOrTemplateInstance.identifier;
		lines ~= t.line;
		columns ~= t.column;
		args ~= internString(t.text);
	}

	alias visit = ASTVisitor.visit;

	size_t[] lines;
	size_t[] columns;
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
					|| symbol.kind == CompletionKind.functionName)
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
