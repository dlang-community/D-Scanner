# D-Scanner [![CI status](https://travis-ci.org/Hackerpilot/Dscanner.svg?branch=master)](https://travis-ci.org/Hackerpilot/Dscanner/)
D-Scanner is a tool for analyzing D source code

### Building and installing
First make sure that you have all the source code. Run ```git submodule update --init --recursive```
after cloning the project.

To build D-Scanner, run ```make``` (or the build.bat file on Windows).
The build time can be rather long with the -inline flag on front-end versions
older than 2.066, so you may wish to remove it from the build script. The
makefile has "ldc" and "gdc" targets if you'd prefer to compile with one of these
compilers instead of DMD. To install, simply place the generated binary (in the
"bin" folder) somewhere on your $PATH.

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
The "--syntaxCheck" or "-s" option prints a listing of any errors or warnings found
while lexing or parsing the given source file. It does not do any semantic
analysis and it does not compile the code.

### Style Check
The "--styleCheck" or "-S" option runs some basic static analysis checks against
the given source files.

#### Configuration
By default all checks are enabled. Individual checks can be enabled or disabled
by using a configuration file. Running ```dscanner --defaultConfig``` will
generate a default configuration file and print the file's location. The
"--config" option will allow you to specify the path to a configuration file if
you do not want to use the one created by the "--defaultConfig" option.

#### Implemented checks
* Old alias syntax (i.e "alias a b;" should be replaced with "alias b = a;").
* Implicit concatenation of string literals.
* Complex number literals (e.g. "1.23i").
* Empty declarations (i.e. random ";" characters).
* enum array literals in struct/class bodies.
* Avoid Pokémon exception handling.
* opCmp or opEquals, or toHash not declared "const".
* Format numbers for readability.
* *delete* keyword is deprecated.
* "fish operators" (floating point operators) are deprecated.
* Left side of a *foreach* or *foreach\_reverse* range expression is larger than the right.
* Left side of a slice expression is larger than the right.
* Variable, struct, class, union, module, package, and interface names that do not comply with Phobos style guidelines.
* Struct constructors that have a single parameter that has a default argument.
* Assign expressions where the left side of the '=' operator is the same as the right.
* 'if' statements where the 'else' block is the same as the 'if' block.
* ||, &&, and == expressions where the left and right sides of the operator are identical.
* && and || expressions where the order of operations is confusing.
* Unused variables.
* Unused parameters (check is skipped if function is marked "override").
* Duplicate attributes.
* Declaring opEquals without toHash.
* Undocumented public declarations.
* Subtraction from .length properties. (These may be unsigned and could lead to integer underflow)
* Class, struct, and union member variables whose names conflict with built-in type properties.
* Confusing asm syntax.
* Placement of const, immutable, or inout before a function return type instead of after the parameters.
* Functions in interface declarations redundantly marked 'abstract'.
* Declaring a variable with the same name as a label.
* Variables that could have been declared const or immutable (experimental)
* Redundant parenthesis.
* Unused labels.
* Lines longer than 120 characters.
* Incorrect infinite range definitions.
* Some assertions that check conditions that will always be true.

#### Wishlist

[See this list of open issues](https://github.com/Hackerpilot/Dscanner/issues?q=is%3Aopen+is%3Aissue+label%3Aenhancement) for the wishlist.

### Reports
The "--report" option writes a JSON report on the static analysis checks
document above to standard output. This file is usually used by the D plugin for
SonarQube located [here](https://github.com/economicmodeling/sonar-d-plugin).

### Find Declaration
Ack, grep, and The Silver Searcher are useful for finding usages of symbols, but
their signal to noise ratio is not very good when searching for a symbol's
declaration. The "--declaration" or "-d" options allow you to search for a
symbols declaration. For example:

	$ dscanner -d TokenStructure
	./libdparse/src/std/lexer.d(248:8)

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

### CTAGS Output
The "--ctags" or "-c" option generates CTAGS information and writes it to the
standard output. Directory arguments are scanned recursively for `.d` and `.di`
files.

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
* a -- alias declarataion

More information on the CTAGS format can be found [here](http://ctags.sourceforge.net/FORMAT).

### Etags Output
The `--etags`, `-e`, and `--etagsAll` options are similar to `--ctags` except
that an Emacs-compatible tags file is generated. The `--etagsAll` option
generates tags for private and package declarations in addition to what
`--etags` and `-e` generate.

### Outline
The "--outline" option parses the given D source file and writes an simple
outline of the file's declarations to stdout.

### Configuration

By default Dscanner uses the configuration file given in `$HOME/.config/dscanner/dscanner.ini`.
Run `--defaultConfig` to regenerate it.
The `--config` option allows one to use a custom configuration file.

### AST Dump
The "--ast" or "--xml" options will dump the complete abstract syntax tree of
the given source file to standard output in XML format.

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

For more readable output, pipe the command through [xmllint](http://xmlsoft.org/xmllint.html)
using its formatting switch.

	$ dscanner --ast helloworld.d | xmllint --format -
