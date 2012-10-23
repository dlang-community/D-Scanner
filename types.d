
//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module types;

import std.stdio;
import std.array;
import std.range;
import std.algorithm;
import std.typecons;
import std.string;

/**
 * Returns: s with any quote characters backslash-escaped
 */
string escapeJSON(string s)
{
	return s.replace("\"", "\\\"");
}

unittest { assert(escapeJSON("abc\"def") == "abc\\\"def"); }

/**
 * Writes a string in JSON fromat to the given file
 * Params:
 *     f = the file to write to
 *     name = the name of the json attribute
 *     value = the value of the json attribute
 *     indent = the indent level
 */
void writeJSONString(File f, const string name, const string value, uint indent = 0)
{
	f.write(std.array.replicate("  ", indent), "\"", name, "\" : \"", escapeJSON(value), "\"");
}

/**
 * Writes a string array in JSON format to the given file
 *     f = the file to write to
 *     name = the name of the json attribute
 *     values = the strings that should be written
 *     indent = the indent level
 */
void writeJSONString(File f, const string name, const string[] values, uint indent = 0)
{
	f.writeln(std.array.replicate("  ", indent), "\"", name, "\" : [");
	foreach(i, v; values)
	{
		f.write(std.array.replicate("  ", indent + 1), "\"", escapeJSON(v), "\"");
		if (i + 1 < values.length)
			f.writeln(",");
		else
			f.writeln();
	}
	f.write(std.array.replicate("  ", indent), "]");
}

/**
 * Attributes common to everything interesting
 */
abstract class Base
{
public:

	/// Sybol name
	string name;

	/// Line number of declaration
	uint line;

	/// Attributes such as "ref", "const", etc.
	string[] attributes;

	/// Protection level such as "public", protected, etc.
	string protection;

	/// See_also: writeJSONString
	void writeJSONTo(File f, uint indent) const
	{
		f.writeln(std.array.replicate("  ", indent + 1), "{");
		printMembers(f, indent + 2);
		f.write("\n", std.array.replicate("  ", indent + 1), "}");
	}

protected:

	void printMembers(File f, uint indent = 0) const
	{
		writeJSONString(f, "name", name, indent);
		f.writeln(",");
		f.write(std.array.replicate("  ", indent), "\"line\" : ", line);
		f.writeln(",");
		writeJSONString(f, "protection", protection, indent);
		f.writeln(",");
		writeJSONString(f, "attributes", attributes, indent);
	}
}

/**
 * Varible declaration
 */
class Variable : Base
{
public:

	/// Variable type
	string type;

protected:

	override void printMembers(File f, uint indent = 0) const
	{
		super.printMembers(f, indent);
		f.writeln(",");
		writeJSONString(f, "type", type, indent);
	}
}

/**
 * Base class for any type that can be a template
 */
abstract class Templateable : Base
{
public:

	/// Template constraint, which may be null
	string constraint;

	/// Template parameters, may be empty
	string[] templateParameters;

protected:

	override void printMembers(File f, uint indent = 0) const
	{
		super.printMembers(f, indent);
		f.writeln(",");
		writeJSONString(f, "constraint", constraint, indent);
		f.writeln(",");
		writeJSONString(f, "templateParameters", templateParameters, indent);
	}
}

/**
 * Stuff common to struct, interface, and class.
 */
class Struct : Templateable
{
public:

	/// List of methods
	Function[] functions;

	/// List of member variables; may be empty
	Variable[] variables;

	/// Source code character position of the beginning of the struct body
	size_t bodyStart;

	/// Source code character position of the end of the struct body
	size_t bodyEnd;

	string getMemberType(string name) const
	{
		foreach (f; functions)
			if (f.name == name)
				return f.returnType;
		foreach (v; variables)
			if (v.name == name)
				return v.type;
		return null;
	}

	string[] getFunctionDocs(string functionName)
	{
		auto app = appender!(string[])();
		foreach (fun; functions)
		{
			if (fun.name != functionName)
				continue;
			app.put(fun.documentString());
		}
		return app.data;
		// TODO: Try this again with a newer DMD
		//return array(map!(a => a.documentString())(
		//	filter!(a => a.name == functionName)(functions)));
	}

	string[] getCtags(string fileName)
	{
		auto app = appender!(string[])();
		app.put(format("%s\t%s\t%d;\"\ts", name, fileName, line));
		foreach (Function f; functions)
		{
			app.put(format("%s\t%s\t%d;\"\tf\tarity:%d\tstruct:", f.name, fileName,
				f.line, f.parameters.length, name));
		}
		foreach (Variable v; variables)
		{
			app.put(format("%s\t%s\t%d;\"\tm\tstruct:%s", v.name, fileName,
				v.line, name));
		}
		return app.data();
	}

protected:

	override void printMembers(File f, uint indent = 0) const
	{
		super.printMembers(f, indent);
		f.writeln(",\n", std.array.replicate("  ", indent), "\"functions\" : [");
		foreach(i, fun; functions)
		{
			fun.writeJSONTo(f, indent);
			if (i + 1 < functions.length)
				f.writeln(",");
			else
				f.writeln();
		}
		f.writeln(std.array.replicate("  ", indent), "],\n", std.array.replicate("  ", indent), "\"variables\" : [");
		foreach(i, var; variables)
		{
			var.writeJSONTo(f, indent);
			if (i + 1 < variables.length)
				f.writeln(",");
			else
				f.writeln();
		}
		f.write(std.array.replicate("  ", indent), "]");
	}
}

/**
 * Functions and delegates
 */
class Function : Templateable
{
public:

	/// Function return type
	string returnType;

	/// Parameter list; may be empty
	Variable[] parameters;

	string documentString()
	{
		string r = returnType ~ " " ~ name ~ "(";
		foreach (i, param; parameters)
		{
			r ~= param.type ~ " " ~ param.name;
			if (i + 1 < parameters.length)
				r ~= ",\\n\\t";
		}
		r ~= ")";
		return r;
	}

protected:
	override void printMembers(File f, uint indent) const
	{
		super.printMembers(f, indent);
		f.write(",\n");
		f.writeln(std.array.replicate("  ", indent), "\"parameters\" : [");
		foreach(i, params; parameters)
		{
			params.writeJSONTo(f, indent);
			if (i + 1 < parameters.length)
				f.writeln(",");
			else
				f.writeln();
		}

		f.write(std.array.replicate("  ", indent), "],\n");
		writeJSONString(f, "returnType", returnType, indent);
	}
}

/**
 * class and interface
 */
class Inherits : Struct
{
public:

	/**
	 * List of interfaces and classes that this inherits or implements; may
	 * be empty
	 */
	string[] baseClasses;

	override string[] getCtags(string fileName)
	{
		auto app = appender!(string[])();
		app.put(format("%s\t%s\t%d;\"\tc\tinherits:%s", name, fileName, line,
			array(baseClasses.joiner(","))));
		foreach (Function f; functions)
		{
			app.put(format("%s\t%s\t%d;\"\tf\tarity:%d\tstruct:", f.name, fileName,
				f.line, f.parameters.length, name));
		}
		foreach (Variable v; variables)
		{
			app.put(format("%s\t%s\t%d;\"\tm\tstruct:%s", v.name, fileName,
				v.line, name));
		}
		return app.data();
	}

protected:

	override void printMembers(File f, uint indent = 0) const
	{
		super.printMembers(f, indent);
		f.writeln(",");
		writeJSONString(f, "baseClasses", baseClasses, indent);
	}
}

/**
 * enum member
 */
struct EnumMember
{
	uint line;
	string name;
}

/**
 * enum
 */
class Enum : Base
{
public:

	/// Base type for this enum
	string type;

	/// Enum members; may be empty
	EnumMember[] members;

	string[] getCtags(string fileName) const
	{
		auto app = appender!(string[])();
		app.put(format("%s\t%s\t%d;\"\tg", name, fileName, line));
		foreach (EnumMember member; members)
		{
			app.put(format("%s\t%s\t%d;\"\te\tenum:%s", member.name, fileName, member.line, name));
		}
		return app.data;
	}

protected:

	override void printMembers(File f, uint indent = 0) const
	{
		super.printMembers(f, indent);
		f.writeln(",");
		writeJSONString(f, "type", type, indent);
		f.writeln(",\n", std.array.replicate("  ", indent), "\"members\" : [");
		foreach(i, member; members)
		{
			f.writeln(std.array.replicate("  ", indent + 1), "{");
			writeJSONString(f, "name", member.name, indent + 2);
			f.writeln(",");
			f.writeln(std.array.replicate("  ", indent + 2), "\"line\" : ", member.line);
			f.write(std.array.replicate("  ", indent + 1), "}");
			if (i + 1 < members.length)
				f.writeln(",");
			else
				f.writeln();
		}
		f.write(std.array.replicate("  ", indent), "]");
	}

}

class HasDeclarations
{
public:
	/// List of interfaces declared in this module
	Inherits[] interfaces;

	/// List of classes declared in this module
	Inherits[] classes;

	/// List of functions declared in this module
	Function[] functions;

	/// List of unions declared in this module
	Struct[] unions;

	/// List of variables declared in this module
	Variable[] variables;

	/// List of structs declared in this module
	Struct[] structs;

	/// List of enums declared in this module
	Enum[] enums;
}

/**
 * Module is a container class for the other classes
 */
class Module : HasDeclarations
{
public:

	/// Module name. Will be blank if there is no module statement
	string name;

	/// List of other modules that are imported by this one
	string[] imports;

	/// Combine this module with another one
	void merge(Module other)
	{
		interfaces.insertInPlace(interfaces.length, other.interfaces);
		classes.insertInPlace(classes.length, other.classes);
		functions.insertInPlace(functions.length, other.functions);
		unions.insertInPlace(unions.length, other.unions);
		variables.insertInPlace(variables.length, other.variables);
		structs.insertInPlace(structs.length, other.structs);
		enums.insertInPlace(enums.length, other.enums);
		imports.insertInPlace(imports.length, other.imports);
	}

	/**
	 * Prints a JSON representation of this module to the given file
	 */
	void writeJSONTo(File f) const
	{
		uint indent = 0;
		f.writeln("{");
		writeJSONString(f, "name", name, indent + 1);
		f.writeln(",");
		writeJSONString(f, "imports", imports, indent + 1);
		f.writeln(",\n  \"interfaces\" : [");
		foreach(i, inter; interfaces)
		{
			inter.writeJSONTo(f, indent + 1);
			if (i + 1 < interfaces.length)
				f.writeln(",");
			else
				f.writeln();
		}
		f.writeln("  ],\n  \"classes\" : [");
		foreach(i, cl; classes)
		{
			cl.writeJSONTo(f, indent + 1);
			if (i + 1 < classes.length)
				f.writeln(",");
			else
				f.writeln();
		}
		f.writeln("  ],\n  \"structs\" : [");
		foreach(i, str; structs)
		{
			str.writeJSONTo(f, indent + 1);
			if (i + 1 < structs.length)
				f.writeln(",");
			else
				f.writeln();
		}
		f.writeln("  ],\n  \"unions\" : [");
		foreach(i, un; unions)
		{
			un.writeJSONTo(f, indent + 1);
			if (i + 1 < unions.length)
				f.writeln(",");
			else
				f.writeln();
		}
		f.writeln("  ],\n  \"functions\" : [");
		foreach(i, fun; functions)
		{
			fun.writeJSONTo(f, indent + 1);
			if (i + 1 < functions.length)
				f.writeln(",");
			else
				f.writeln();
		}
		f.writeln("  ],\n  \"variables\" : [");
		foreach(i, var; variables)
		{
			var.writeJSONTo(f, indent + 1);
			if (i + 1 < variables.length)
				f.writeln(",");
			else
				f.writeln();
		}
		f.writeln("  ],\n  \"enums\" : [");
		foreach(i, en; enums)
		{
			en.writeJSONTo(f, indent + 1);
			if (i + 1 < enums.length)
				f.writeln(",");
			else
				f.writeln();
		}
		f.writeln("  ]\n}");
	}

	/**
	 * Standards: http://ctags.sourceforge.net/FORMAT
	 */
	void writeCtagsTo(File file, string fileName)
	{
		string[] tags;
		foreach (Enum e; enums)
		{
			tags ~= e.getCtags(fileName);
		}

		foreach (Variable v; variables)
		{
			tags ~= format("%s\t%s\t%d;\"\tv", v.name, fileName, v.line);
		}

		foreach (Function f; functions)
		{
			tags ~= format("%s\t%s\t%d;\"\tf\tarity:%d", f.name, fileName,
				f.line, f.parameters.length);
		}
		foreach (Inherits c; classes)
		{
			tags ~= c.getCtags(fileName);
		}
		foreach (Inherits i; interfaces)
		{
			tags ~= i.getCtags(fileName);
		}
		foreach (Struct s; structs)
		{
			tags ~= s.getCtags(fileName);
		}

		sort(tags);
		file.writeln("!_TAG_FILE_FORMAT 2");
		file.writeln("!_TAG_FILE_SORTED 1");
		file.writeln("!_TAG_PROGRAM_URL https://github.com/Hackerpilot/Dscanner/");
		foreach (tag; tags)
		{
			file.writeln(tag);
		}
	}
}

immutable(string[][string]) typeProperties;
immutable(string[]) floatProperties;
immutable(string[]) integralProperties;
immutable(string[]) commonProperties;
immutable(string[]) arrayProperties;

static this()
{
	floatProperties = ["alignof", "dig", "epsilon", "im", "infinity", "init",
		"mangleof", "mant_dig", "max", "max_10_exp", ".max_­exp", "min_10_­exp",
		"min_­exp", "min_nor­mal", "nan",	"re", "sizeof"
	];

	integralProperties = ["alignof", "init", "mangleof", "max",
		"min", "sizeof", "stringof"
	];

	commonProperties = [
		"alignof",
		"init",
		"mangleof",
		"stringof"
	];

	arrayProperties = [
		"alignof",
		"init",
		"length",
		"mangleof",
		"ptr",
		"stringof",
	];

	typeProperties = [
		"bool" : commonProperties,
		"byte" : integralProperties,
		"ubyte" : integralProperties,
		"short" : integralProperties,
		"ushort" : integralProperties,
		"int" : integralProperties,
		"uint" : integralProperties,
		"long" : integralProperties,
		"ulong" : integralProperties,
		"cent" : integralProperties,
		"ucent" : integralProperties,
		"float" : floatProperties,
		"double" : floatProperties,
		"real" : floatProperties,
		"ifloat" : floatProperties,
		"idouble" : floatProperties,
		"ireal" : floatProperties,
		"cfloat" : floatProperties,
		"cdouble" : floatProperties,
		"creal" : floatProperties,
		"char" : commonProperties,
		"wchar" : commonProperties,
		"dchar" : commonProperties,
		"ptrdiff_t" : integralProperties,
		"size_t" : integralProperties,
		"string" : arrayProperties,
		"wstring" : arrayProperties,
		"dstring" : arrayProperties
	];
}

class CompletionContext
{
public:

	this(Module mod, bool extendedFunctionTypes = false)
	{
		this.currentModule = mod;
		this.extendedFunctionTypes = extendedFunctionTypes;
	}

	Tuple!(string, string)[string] getMembersOfType(string name)
	{
		foreach (m; chain(modules, [currentModule]))
		{
			foreach (inherits; chain(m.interfaces, m.classes))
			{
				if (inherits.name != name)
					continue;
				Tuple!(string, string)[string] typeMap;
				foreach (var; inherits.variables)
					typeMap[var.name] = Tuple!(string, string)(var.type, "m");
				foreach (fun; inherits.functions)
					typeMap[fun.name] = Tuple!(string, string)(fun.returnType, getFunctionType(fun));
				foreach (parent; inherits.baseClasses)
				{
					foreach (k, v; getMembersOfType(parent))
					{
						typeMap[k] = v;
					}
				}
				return typeMap;
			}

			foreach (s; chain(m.structs, m.unions))
			{
				if (s.name != name)
					continue;
				Tuple!(string, string)[string] typeMap;
				foreach (var; s.variables)
					typeMap[var.name] = Tuple!(string, string)(var.type, "m");
				foreach (fun; s.functions)
					typeMap[fun.name] = Tuple!(string, string)(fun.returnType, getFunctionType(fun));
				return typeMap;
			}
			foreach (Enum e; m.enums)
			{
				if (e.name != name)
					continue;
				Tuple!(string, string)[string] typeMap;
				foreach (member; e.members)
					typeMap[member.name] = Tuple!(string, string)(e.type, "e");
				return typeMap;
			}
		}
		return null;
	}

	Struct[] getStructsContaining(size_t cursorPosition)
	{
		auto app = appender!(Struct[])();
		foreach(s; chain(currentModule.structs, currentModule.interfaces,
			currentModule.classes, currentModule.unions))
		{
			if (s.bodyStart <= cursorPosition && s.bodyEnd >= cursorPosition)
				app.put(s);
		}
		return app.data();
	}



	string[] getCallTipsFor(string container, string functionName,
		size_t cursorPosition)
	{
		if (container == null || container.length == 0 || container == "void")
		{
			// Try member functions first if the cursor is inside of a class
			// or structure definiton
			Struct[] structs = getStructsContaining(cursorPosition);
			foreach (s; structs)
			{
				auto docs = s.getFunctionDocs(functionName);
				if (docs.length > 0)
					return docs;
			}
			// Try global functions if the above failed.
			return getCallTipsFor(functionName);
		}

		foreach (m; chain(modules, [currentModule]))
		{
			foreach (s; chain(m.structs, m.interfaces, m.classes, m.unions))
			{
				if (s.name != container)
					continue;
				return s.getFunctionDocs(functionName);
			}
		}
		return [];
	}

	void addModule(Module mod)
	{
		modules ~= mod;
	}

	Module currentModule;
	Module[] modules;
	string[] importDirectories;
	bool extendedFunctionTypes;

private:

	string[] getCallTipsFor(string functionName)
	{
		stderr.writeln("Getting call tips for ", functionName);
		auto app = appender!(string[])();
		foreach (m; chain(modules, [currentModule]))
		{
			foreach (fun; m.functions)
			{
				if (fun.name == functionName)
					app.put(fun.documentString());
			}
		}
		return app.data;
	}

	string getFunctionType(Function f) {
		string result = "f";
		if (extendedFunctionTypes) {
			result ~= " : ";
			result ~= "[#" ~ f.returnType ~ "#]";
			result ~= f.name ~ "(";
			bool first = true;
			foreach (param; f.parameters) {
				if (first) {
					first = false;
				}
				else {
					result ~= ",";
				}
				result ~= "<#" ~ param.type ~ " " ~ param.name ~ "#>";
			}
			result ~= ")";
		}
		return result;
	}
}
