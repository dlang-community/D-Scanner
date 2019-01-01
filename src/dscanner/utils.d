module dscanner.utils;

import std.array : appender, uninitializedArray;
import std.stdio : stdin, stderr, File;
import std.conv : to;
import std.encoding : BOM, BOMSeq, EncodingException, getBOM;
import std.format : format;
import std.file : exists, read;

private void processBOM(ref ubyte[] sourceCode, string fname)
{
	enum spec = "D-Scanner does not support %s-encoded files (%s)";
	const BOMSeq bs = sourceCode.getBOM;
	with(BOM) switch (bs.schema)
	{
	case none, utf8:
		break;
	default:
		throw new EncodingException(spec.format(bs.schema, fname));
	}
	sourceCode = sourceCode[bs.sequence.length .. $];
}

unittest
{
	ubyte[] data = [0xEF, 0xBB, 0xBF, 'h', 'i', '!'];
	data.processBOM("unittest data");
	assert(data[] == cast(ubyte[]) "hi!");
}

unittest
{
	import std.exception : assertThrown, assertNotThrown;
	import std.encoding : bomTable;
	import std.traits : EnumMembers;

	foreach(m ; EnumMembers!BOM)
	{
		auto sc = bomTable[m].sequence.dup;
		if (m != BOM.none && m != BOM.utf8)
		{
			assertThrown!(EncodingException)(processBOM(sc, ""));
		}
		else
		{
			assertNotThrown!(EncodingException)(processBOM(sc, ""));
		}
	}
}

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
	auto data = sourceCode.data;
	data.processBOM("stdin");
	return data;
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
	ubyte[] sourceCode;
	sourceCode = cast(ubyte[]) fileName.read();
	sourceCode.processBOM(fileName);
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

/**
 * Allows to build access chains of class members as done with the $(D ?.) operator
 * in other languages. In the chain, any $(D null) member that is a class instance
 * or that returns one, has for effect to shortcut the complete evaluation.
 *
 * This function is copied from https://github.com/BBasile/iz to avoid a new submodule.
 * Any change made to this copy should also be applied to the origin.
 *
 * Params:
 *      M = The class type of the chain entry point.
 *
 * Bugs:
 *      Assigning a member only works with $(D unwrap).
 *
 */
struct SafeAccess(M)
if (is(M == class))
{
	M m;

	@disable this();

	/**
	 * Instantiate.
	 *
	 * Params:
	 *      m = An instance of the entry point type. It is usually only
	 *      $(D null) when the constructor is used internally, to build
	 *      the chain.
	 */
	this(M m)
	{
		this.m = m;
	}

	alias m this;
	/// Unprotect the class instance.
	alias unwrap = m;

	/// Allows cast to interfaces and classes inside the chain.
	auto ref as(A)() @trusted
	if (!__traits(hasMember, M, "as") && (is(A == class) || is(A == interface)))
	{
		return SafeAccess!(A)(cast(A) m);
	}

	/// Handles safe access.
	auto ref opDispatch(string member, A...)(auto ref A a)
	{
		import std.traits : ReturnType;
		alias T = typeof(__traits(getMember, m, member));
		static if (is(T == class))
		{
			return (!m || !__traits(getMember, m, member))
				? SafeAccess!T(null)
				: SafeAccess!T(__traits(getMember, m, member));
		}
		else
		{
			import std.traits : ReturnType, Parameters, isFunction;
			static if (isFunction!T)
			{
				// otherwise there's a missing return statement.
				alias R = ReturnType!T;
				static if (!is(R == void) &&
					!(is(R == class) && Parameters!T.length == 0))
						pragma(msg, __FILE__ ~ "(" ~ __LINE__.stringof ~ "): error, " ~
						"only `void function`s or `class` getters can be called without unwrap");

				static if (is(R == class))
				{
					return (m is null)
						? SafeAccess!R(null)
						: SafeAccess!R(__traits(getMember, m, member)(a));
				}
				else
				{
					if (m)
						__traits(getMember, m, member)(a);
				}
			}
			else
			{
				if (m)
					__traits(getMember, m, member) = a;
			}
		}
	}
}
/// General usage
@safe unittest
{
	class LongLineOfIdent3{int foo; void setFoo(int v) @safe{foo = v;}}
	class LongLineOfIdent2{LongLineOfIdent3 longLineOfIdent3;}
	class LongLineOfIdent1{LongLineOfIdent2 longLineOfIdent2;}
	class Root {LongLineOfIdent1 longLineOfIdent1;}

	SafeAccess!Root sar = SafeAccess!Root(new Root);
	// without the SafeAccess we would receive a SIGSEGV here
	sar.longLineOfIdent1.longLineOfIdent2.longLineOfIdent3.setFoo(0xDEADBEEF);

	bool notAccessed = true;
	// the same with `&&` whould be much longer
	if (LongLineOfIdent3 a = sar.longLineOfIdent1.longLineOfIdent2.longLineOfIdent3)
	{
		notAccessed = false;
	}
	assert(notAccessed);

	// checks that forwarding actually works
	sar.m.longLineOfIdent1 = new LongLineOfIdent1;
	sar.m.longLineOfIdent1.longLineOfIdent2 = new LongLineOfIdent2;
	sar.m.longLineOfIdent1.longLineOfIdent2.longLineOfIdent3 = new LongLineOfIdent3;

	sar.longLineOfIdent1.longLineOfIdent2.longLineOfIdent3.setFoo(42);
	assert(sar.longLineOfIdent1.longLineOfIdent2.longLineOfIdent3.unwrap.foo == 42);
}

/**
 * IFTI helper for $(D SafeAccess).
 *
 * Returns:
 *      $(D m) with the ability to safely access its members that are class
 *      instances.
 */
auto ref safeAccess(M)(M m)
{
	return SafeAccess!M(m);
}
