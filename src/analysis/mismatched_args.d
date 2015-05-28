module analysis.mismatched_args;

import analysis.base : BaseAnalyzer;
import dsymbol.scope_;
import dsymbol.symbol;
import std.d.ast;
import std.d.lexer : tok;

/// Checks for mismatched argument and parameter names
final class MismatchedArgumentCheck : BaseAnalyzer
{
	///
    this(string fileName, const Scope* sc)
    {
        super(fileName, sc);
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
		identVisitor.visit(fce.unaryExpression);

		DSymbol* sym = resolveSymbol(sc, identVisitor.names);
		const istring[] params = sym is null ? [] : sym.parts[].map!(a => a.name).array();
		const ArgMismatch[] mismatches = compareArgsToParams(params, args);
		foreach(size_t i, ref const mm; mismatches)
			addErrorMessage(argVisitor.lines[i], argVisitor.columns[i], KEY,
				createWarningFromMismatch(mm));
	}

	alias visit = ASTVisitor.visit;

private:

	enum KEY = "dscanner.confusing.argument_parameter_mismatch";
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

DSymbol* resolveSymbol(const Scope* sc, const istring[] symbolChain)
{
	import std.array:empty;

	DSymbol*[] s = sc.getSymbolsByName(symbolChain[0]);
	if (s.empty)
		return null;

	DSymbol* sym = s[0];
	foreach (i; 1 .. symbolChain.length)
	{
		if (sym.kind == CompletionKind.variableName || sym.kind == CompletionKind.memberVariableName
				|| sym.kind == CompletionKind.functionName)
			sym = sym.type;
		if (sym is null)
			return null;
		auto p = sym.getPartsByName(symbolChain[i]);
		if (p.empty)
			return null;
		sym = p[0];
	}
	return sym;
}

struct ArgMismatch
{
    size_t argIndex;
    size_t paramIndex;
	string name;
}

const(ArgMismatch[]) compareArgsToParams(const istring[] params, const istring[] args) pure
{
	if(args.length != params.length)
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
    return retVal;
}

string createWarningFromMismatch(const ArgMismatch mismatch) pure
{
    import std.format : format;

    return "Argument %d is named '%s', but this is the name of parameter %d"
        .format(mismatch.argIndex + 1, mismatch.name, mismatch.paramIndex + 1);
}

unittest
{
    {
        string[] args = ["a", "b", "c"];
        string[] params = ["a", "b", "c"];
        immutable res = compareArgsToParams(params, args);
        assert(res == []);
    }

    {
        string[] args = ["a", "c", "b"];
        string[] params = ["a", "b", "c"];
        immutable res = compareArgsToParams(params, args);
        assert(res == [ArgMismatch(1, 2), ArgMismatch(2, 1)]);
    }

    {
        string[] args = ["a", "c", "b"];
        string[] params = ["alpha", "bravo", "c"];
        immutable res = compareArgsToParams(params, args);
        assert(res == [ArgMismatch(1, 2)]);
    }

    {
        string[] args = ["a", "b"];
        string[] params = [null, "b"];
        immutable res = compareArgsToParams(params, args);
        assert(res == []);
    }
}
