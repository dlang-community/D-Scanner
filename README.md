# Overview
DScanner is a tool for analyzing D source code

### Building and installing
To build DScanner, run the build.sh script (or the build.bat file on Windows).
The build time can be rather long with the -inline flag (over 2 minutes on an
i7 processor), so you may wish to remove it from the build script. To install,
simply place the generated binary somewhere on your $PATH.

# Usage
The following examples assume that we are analyzing a simple file called helloworld.d

	import std.stdio;
	void main(string[] args)
	{
		writeln("Hello World");
	}

### Token Count
The "--tokenCount" or "-t" option prints the number of tokens in the given file

	$ dscanner --tokenCount helloworld.d
	20

### Import Listing
The "--imports" or "-i" option prints a listing of modules imported by the given
source file.

	$ dscanner --imports helloworld.d
	std.stdio

### Syntax Check
The "--syntaxCheck" option prints a listing of any errors or warnings found
while lexing or parsing the given source file. It does not do any semantic
analysis and it does not compile the code.

### Style Check
The "--styleCheck" option runs some basic static analysis checks against the
given source files.

#### Implemented checks
* Old alias syntax (i.e "alias a b;" should be replaced with "alias b = a;").
* Implicit concatenation of string literals.
* Complex number literals (e.g. "1.23i").
* Empty declarations (i.e. random ";" characters)
* enum array literals in struct/class bodies
* Avoid Pokémon exception handling
* opCmp or opEquals, or toHash not declared "const".
* Format numbers for readability.
* *delete* keyword is deprecated.
* "fish operators" (floating point operators) are deprecated.
* Left side of a *foreach* or *foreach\_reverse* range expression is larger than the right.
* Left side of a slice expression is larger than the right
* Variable, struct, class, union, module, package, and interface names that do not comply with Phobos style guidelines
* Struct constructors that have a single parameter that has a default argument.
* Assign expressions where the left side of the '=' operator is the same as the right
* 'if' statements where the 'else' block is the same as the 'if' block.

#### Wishlish
* Assigning to foreach variables that are not "ref".
* Unused variables.
* Unused imports.
* Unused parameters (check is skipped if function is marked "override")
* Variables that are never modified and not declared immutable.
* Public declarations not documented
* Declaring opEquals without toHash
* Assignment in conditionals

### Line of Code Count
The "--sloc" or "-l" option prints the number of lines of code in the file.
Instead of simply printing the number of line breaks, this counts the number of
semicolon, while, if, do, else, switch, for, foreach, foreach\_reverse, default,
and case tokens in the file.

	$ ./dscanner --sloc helloworld.d
	2

### Syntax Highlighting
The "--highlight" option prints the given source file as syntax-highlighted HTML
to the standard output. The CSS styling is currently hard-coded to use the
[Solarized](http://ethanschoonover.com/solarized) color scheme.

	No example. It would take up too much space

### CTAGS output
The "--ctags" or "-c" option generates CTAGS information and writes it to the
standard output. When used with the "--recursive", "-R", or "-r" option, CTAGS
information will be generated for a specified directory and all of its
sub-directories.

	$ dscanner --ctags helloworld.d
	!_TAG_FILE_FORMAT	2
	!_TAG_FILE_SORTED	1
	!_TAG_FILE_AUTHOR	Brian Schott
	!_TAG_PROGRAM_URL	https://github.com/Hackerpilot/Dscanner/
	main	helloworld.d	3;"	f	arity:1

CTAGS output uses the following tag kinds:

* g -- enum declarataion
* e -- enum member
* v -- variable declaration
* i -- interface declaration
* c -- class declaration
* s -- struct declaration
* f -- function declaration
* u -- union declaration
* T -- template declaration

More information on the CTAGS format can be found [here](http://ctags.sourceforge.net/FORMAT).

### Outline
The "--outline" option parses the given D source file and writes an simple
outline of the file's declarations to stdout.

### AST Dump
The "--ast" or "--xml" options will dump the complete abstract syntax tree of
the given source file to standard output in XML format. JSON output is planned
but not yet implemented.

	$ dscanner --ast helloworld.d
	<module>
	<declaration>
	<importDeclaration>
	<singleImport>
	<identifierChain>
	<identifier>std</identifier>
	<identifier>stdio</identifier>
	</identifierChain>
	</singleImport>
	</importDeclaration>
	</declaration>
	<declaration>
	<functionDeclaration line="3">
	<name>main</name>
	<type pretty="void">
	<type2>
	void
	</type2>
	</type>
	<parameters>
	<parameter>
	<name>args</name>
	<type pretty="string[]">
	<type2>
	<symbol>
	<identifierOrTemplateChain>
	<identifierOrTemplateInstance>
	<identifier>string</identifier>
	</identifierOrTemplateInstance>
	</identifierOrTemplateChain>
	</symbol>
	</type2>
	<typeSuffix type="[]"/>
	</type>
	<identifier>args</identifier>
	</parameter>
	</parameters>
	<functionBody>
	<blockStatement>
	<declarationsAndStatements>
	<declarationOrStatement>
	<statement>
	<statementNoCaseNoDefault>
	<expressionStatement>
	<expression>
	<assignExpression>
	<functionCallExpression>
	<unaryExpression>
	<primaryExpression>
	<identifierOrTemplateInstance>
	<identifier>writeln</identifier>
	</identifierOrTemplateInstance>
	</primaryExpression>
	</unaryExpression>
	<arguments>
	<argumentList>
	<assignExpression>
	<primaryExpression>
	<stringLiteral>Hello World</stringLiteral>
	</primaryExpression>
	</assignExpression>
	</argumentList>
	</arguments>
	</functionCallExpression>
	</assignExpression>
	</expression>
	</expressionStatement>
	</statementNoCaseNoDefault>
	</statement>
	</declarationOrStatement>
	</declarationsAndStatements>
	</blockStatement>
	</functionBody>
	</functionDeclaration>
	</declaration>
	</module>

# Useful code
The source code for DScanner has a complete lexer, parser, and abstact syntax
tree library for D code under the std/d/ directory. It is intended that these
modules eventually end up in Phobos, so feel free to use them for your own D
tools.
