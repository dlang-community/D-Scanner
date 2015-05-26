module analysis.mismatched_args;

struct ArgMismatch
{
	size_t argIndex;
	size_t paramIndex;
}

ArgMismatch[] compareArgsToParams(const string[] params, const string[] args) pure
in
{
	assert(args.length == params.length);
}
body
{
	ArgMismatch[] retVal;
	foreach (i, arg; args)
	{
		if (arg is null || arg == params[i])
			continue;
		foreach (j, param; params)
			if (param == arg)
				retVal ~= ArgMismatch(i, j);
	}
	return retVal;
}

string createWarningFromMismatch(ref const ArgMismatch mismatch, const string commonName) pure
{
	import std.format : format;

	return "Argument %d is named '%s', but this is the name of parameter %d".format(
		mismatch.argIndex + 1, commonName, mismatch.paramIndex + 1);
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
}
