module readers;

import std.array : appender, uninitializedArray;
import std.stdio : stdin, stderr, File;
import std.conv : to;
import std.file : exists;

ubyte[] readStdin()
{
	auto sourceCode = appender!(ubyte[])();
	ubyte[4096] buf;
	while (true)
	{
		auto b = stdin.rawRead(buf);
		if (b.length == 0)
			break;
		sourceCode.put(b);
	}
	return sourceCode.data;
}

ubyte[] readFile(string fileName)
{
	if (fileName == "stdin")
		return readStdin();
	if (!exists(fileName))
	{
		stderr.writefln("%s does not exist", fileName);
		return [];
	}
	File f = File(fileName);
	if (f.size == 0)
		return [];
	ubyte[] sourceCode = uninitializedArray!(ubyte[])(to!size_t(f.size));
	f.rawRead(sourceCode);
	return sourceCode;
}

string[] expandArgs(string[] args)
{
	import std.file : isFile, FileException, dirEntries, SpanMode;
	import std.algorithm.iteration : map;
	import std.algorithm.searching : endsWith;

	// isFile can throw if it's a broken symlink.
	bool isFileSafe(T)(T a)
	{
		try
			return isFile(a);
		catch (FileException)
			return false;
	}

	string[] rVal;
	if (args.length == 1)
		args ~= ".";
	foreach (arg; args[1 .. $])
	{
		if (arg == "stdin" || isFileSafe(arg))
			rVal ~= arg;
		else
			foreach (item; dirEntries(arg, SpanMode.breadth).map!(a => a.name))
			{
				if (isFileSafe(item) && (item.endsWith(`.d`) || item.endsWith(`.di`)))
					rVal ~= item;
				else
					continue;
			}
	}
	return rVal;
}
