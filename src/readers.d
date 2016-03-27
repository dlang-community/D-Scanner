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
