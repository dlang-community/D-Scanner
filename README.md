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
well as any paths specified in /etc/dmd.conf. This is only used for the
--parenComplete and --dotComplete options
* **--ctags** _sourceFile_ - Generates ctags information from the given source
code file.
* **--recursive** **-R** **-r** _directory_ - When used with --ctags, dscanner
will produce ctags output for all .d and .di files contained within _directory_
and its sub-directories.

# Dot Completion
This is currently under development.
### Output format
The output of the --dotComplete option is a list of valid completions at the
given cursor position. The completions are printed one per line. Lines are ended
by a single line feed character (0x0a). Each line consists of the symbol name
followed by a single space character (0x20), followed by one character indicating
what the symbol is. Symbol definitions are taken from the list of recommended
"kind" values from the CTAGS standard unless there was no relevant recommendaton
present.

##### Example output:
	foo v
	bar f

##### Supported kinds
* c -- class names
* i -- interface names
* s -- structure names
* v -- variable
* m -- member variable
* k -- keyword, built-in version, scope statement
* f -- function or method
* g -- enum name
* P -- package
* M -- module


# Paren Completion
Provides either a call tip for a function call or a list of pre-defined version
identifiers for a version() statement, or a list of scope identifiers for a
scope() statement. Anyone integrating dscanner into a text editor needs to look
at the first line of the output to determine whether to display an autocomplete
list or a call tip. (In the case of Scintilla, these are different)
### Call tips
Outputs the word "calltips" followed by a newline, followed by the call tips for
the function before the cursor. One overload of the function is printed per
line. The call tip may have newlines and tabs escaped in the common "\n" and
"\t" format. These should be un-escaped for display.
##### Example output
	calltips
	Token[] tokenize(S inputString,\n\tIterationStyle iterationStyle)
### Completions
Outputs the word "completions" followed by a newline, followed by a completion
list. See the documentation on the --dotComplete option for details
##### Example output
	completions
	exit k
	failure k
	success k

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

	!_TAG_FILE_FORMAT 2
	!_TAG_FILE_SORTED 1
	!_TAG_PROGRAM_URL https://github.com/Hackerpilot/Dscanner/
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

