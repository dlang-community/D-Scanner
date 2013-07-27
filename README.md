# Overview
DScanner is a tool for analyzing D source code

# Usage
The following examples assume that we are analyzing a simple file called helloworld.d

	import std.stdio;
	void main(string[] args)
	{
		writeln("Hello World");
	}

### Token Count
The "--tokenCount" or "-t" option prints the number of tokens in the given file

	$ dscanner --tokencount helloworld.d
	20

### Import Listing
The "--imports" or "-i" option prints a listing of modules imported by the given
source file.

	$ dscanner --imports helloworld.d
	std.stdio

### Line of Code Count
The "--sloc" or "-l" option prints the number of lines of code in the file.
Instead of simply printing the number of line breaks, this counts the number of
semicolon, while, if, do, else, switch, for, foreach, foreach\_reverse, default,
and case tokens in the file.

	$ ./dscanner --sloc helloworld.d
	2

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
