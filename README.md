# Overview
Dscanner is a tool used to analyze D source code.

### Options
* **--dotComplete** _sourceFile_ _cursorPosition_ - Provide autocompletion for the
insertion of the dot operator. The cursor position is the character position in
the **file**, not the position in the line.
* **--sloc** _sourceFiles_ - count the number of logical lines of code in the given
source files.
* **--json** _sourceFile_ - Generate a JSON summary of the given source file
* **--parenComplete** _sourceFile_ _cursorPosition_ - Provides a listing of function
parameters or pre-defined version identifiers at the cursor position. The cursor
position is the character position in the **file**, not the line.
* **--highlight** _sourceFile_ - Syntax-highlight the given source file. The
resulting HTML will be written to standard output.
* **-I** _includePath_ - Include _includePath_ in the list of paths used to search
for imports. By default dscanner will search in the current working directory as
well as any paths specified in /etc/dmd.conf.
* **--ctags** _sourceFile_ - Generates ctags information from the given source
code file.

# Dot Completion
This is currently under development.

# Paren Completion
This is currently under development.

# JSON output
Generates a JSON summary of the input file.

### Example
The given D code:

	module example;

	import std.stdio;

	interface Iface {
		double interfaceMethod();
	}

	class SomeClass(T) if (isSomeString!T) : IFace {
	public:
		this() {}
		void doStuff(T);
		override double interfaceMethod() {}
	private:
		T theTee;
	}

	int freeFunction(int x) { return x + x; }

	void main(string[] args) {

	}

is transformed into the following JSON markup:

	{
	  "name" : "example",
	  "imports" : [
		"std.stdio"
	  ],
	  "interfaces" : [
		{
		  "name" : "Iface",
		  "line" : 5,
		  "protection" : "public",
		  "attributes" : [
		  ],
		  "constraint" : "",
		  "templateParameters" : [
		  ],
		  "functions" : [
			{
			  "name" : "interfaceMethod",
			  "line" : 6,
			  "protection" : "",
			  "attributes" : [
			  ],
			  "constraint" : "",
			  "templateParameters" : [
			  ],
			  "parameters" : [
			  ],
			  "returnType" : "double"
			}
		  ],
		  "variables" : [
		  ],
		  "baseClasses" : [
		  ]
		}
	  ],
	  "classes" : [
		{
		  "name" : "SomeClass",
		  "line" : 9,
		  "protection" : "public",
		  "attributes" : [
		  ],
		  "constraint" : "if (isSomeString!T)",
		  "templateParameters" : [
			"T"
		  ],
		  "functions" : [
			{
			  "name" : "this",
			  "line" : 11,
			  "protection" : "",
			  "attributes" : [
			  ],
			  "constraint" : "",
			  "templateParameters" : [
			  ],
			  "parameters" : [
			  ],
			  "returnType" : ""
			},
			{
			  "name" : "doStuff",
			  "line" : 12,
			  "protection" : "",
			  "attributes" : [
			  ],
			  "constraint" : "",
			  "templateParameters" : [
			  ],
			  "parameters" : [
				{
				  "name" : "",
				  "line" : 0,
				  "protection" : "",
				  "attributes" : [
				  ],
				  "type" : "T"
				}
			  ],
			  "returnType" : "void"
			},
			{
			  "name" : "interfaceMethod",
			  "line" : 13,
			  "protection" : "",
			  "attributes" : [
				"override"
			  ],
			  "constraint" : "",
			  "templateParameters" : [
			  ],
			  "parameters" : [
			  ],
			  "returnType" : "double"
			}
		  ],
		  "variables" : [
			{
			  "name" : "theTee",
			  "line" : 15,
			  "protection" : "private",
			  "attributes" : [
			  ],
			  "type" : "T"
			}
		  ],
		  "baseClasses" : [
			"IFace"
		  ]
		}
	  ],
	  "structs" : [
	  ],
	  "structs" : [
	  ],
	  "functions" : [
		{
		  "name" : "freeFunction",
		  "line" : 18,
		  "protection" : "",
		  "attributes" : [
		  ],
		  "constraint" : "",
		  "templateParameters" : [
		  ],
		  "parameters" : [
			{
			  "name" : "x",
			  "line" : 18,
			  "protection" : "",
			  "attributes" : [
			  ],
			  "type" : "int"
			}
		  ],
		  "returnType" : "int"
		},
		{
		  "name" : "main",
		  "line" : 20,
		  "protection" : "",
		  "attributes" : [
		  ],
		  "constraint" : "",
		  "templateParameters" : [
		  ],
		  "parameters" : [
			{
			  "name" : "args",
			  "line" : 20,
			  "protection" : "",
			  "attributes" : [
			  ],
			  "type" : "string[]"
			}
		  ],
		  "returnType" : "void"
		}
	  ],
	  "variables" : [
	  ],
	  "enums" : [
	  ]
	}

# Ctags output
Dscanner can create a tags file from the specified file. Output is formatted as
specified at http://ctags.sourceforge.net/FORMAT. The result of generating ctags
on the same file used in the JSON example will produce this output:

	{!_TAG_FILE_FORMAT 2}
	{!_TAG_FILE_SORTED 1}
	Iface	tmp.d	3;"	c	inherits:
	SomeClass	tmp.d	7;"	c	inherits:IFace
	doStuff	tmp.d	10;"	f	arity:1	struct:SomeClass
	freeFunction	tmp.d	16;"	f	arity:1
	interfaceMethod	tmp.d	11;"	f	arity:0	struct:SomeClass
	interfaceMethod	tmp.d	4;"	f	arity:0	struct:Iface
	main	tmp.d	18;"	f	arity:1
	theTee	tmp.d	13;"	m	struct:SomeClass
	this	tmp.d	9;"	f	arity:0	struct:SomeClass

# Line of Code count
This option counts the logical lines of code in the given source files, not
simply the physical lines. More specifically, it counts the number of
semicolons, **if**, **while**, **case**, **foreach**, and **for** tokens in the
given files.

# Highlighting
Syntax highlights the given file in HTML format. Output is written to _stdout_.
The CSS styling information is currently hard-coded.

