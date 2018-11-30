# D-Scanner

[![CI status](https://travis-ci.org/dlang-community/D-Scanner.svg?branch=master)](https://travis-ci.org/dlang-community/D-Scanner/)
[![Latest version](https://img.shields.io/dub/v/dscanner.svg)](http://code.dlang.org/packages/dscanner)
[![License](https://img.shields.io/dub/l/dscanner.svg)](http://code.dlang.org/packages/dscanner)

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

### Testing
Testing does not work with DUB.
Under linux or OSX run the tests with `make test`.
Under Windows run the tests with `build.bat test`.

### Installing with DUB

```sh
> dub fetch dscanner && dub run dscanner
```

## Installing with Docker

With Docker no installation is required:

```sh
docker run --rm -v $(pwd):/src dlangcommunity/dscanner
```

# Usage
The following examples assume that we are analyzing a simple file called helloworld.d

```d
import std.stdio;
void main(string[] args)
{
	writeln("Hello World");
}
```

### Token Count
The "--tokenCount" or "-t" option prints the number of tokens in the given file

	$ dscanner --tokenCount helloworld.d
	20

### Import Listing
The "--imports" or "-i" option prints a listing of modules imported by the given
source file.

	$ dscanner --imports helloworld.d
	std.stdio

Passing "-I" arguments (import locations) will cause D-Scanner to also attempt
to resolve the locations of the imported modules.

    $ dscanner --imports helloworld.d -I ~/.dvm/compilers/dmd-2.071.1-b2/src/phobos/ -I ~/.dvm/compilers/dmd-2.071.1-b2/src/druntime/src/
	/home/brian/.dvm/compilers/dmd-2.071.1-b2/src/phobos/std/stdio.d

Remember to pass map the import locations when you use Docker:

	docker run --rm -v $(pwd):/src -v /usr/include/dlang/dmd:/d dlangcommunity/dscanner --imports helloworld.d -I/d
	/d/std/stdio.d

The "--recursiveImports" option is similar to "--imports", except that it lists
imports of imports (and so on) recursively. The recursive import option requires
import paths to be specified in order to work correctly.

Limitations:
* The import listing feature DOES NOT IGNORE imports that may be unused to to `version` or `static if`.
* The import listing DOES NOT INCLUDE imports introduced by mixins.

### Syntax Check
The "--syntaxCheck" or "-s" option prints a listing of any errors or warnings found
while lexing or parsing the given source file. It does not do any semantic
analysis and it does not compile the code.

### Style Check
The "--styleCheck" or "-S" option runs some basic static analysis checks against
the given source files, the sources contained in the given folders, or the sources contained in the current working directory (when nothing is supplied).

#### Skip style checks in the tests
Static checks in the unit tests can produce irrelevant warnings. For example,
it's legit to declare a variable that's not used if the goal is to verify that
a templatized function can be instantiated by inference of the type of this variable.
To avoid these cases, it's possible to pass the "--skipTests" option.

#### Configuration
By default all checks are enabled. Individual checks can be enabled or disabled
by using a configuration file. Such a file can be placed, for example, is the root directory of your project.
Running ```dscanner --defaultConfig``` will generate a default configuration file and print the file's location.
You can also specify the path to a configuration file by using the "--config" option if
you want to override the default or the local settings.

For each check, three values are possible:
* `"disabled"`: the check is not performed.
* `"enabled"`: the check is performed.
* `"skip-unittest"`: the check is performed but not in the unit tests.

Any other value deactivates a check.

Note that the "--skipTests" option is the equivalent of changing each
`"enabled"` check by a `"skip-unittest"` check.

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
* Auto functions without return statement. The compiler doesn't see an omission and it infers 'void' as return type.
* `final` attribute is used but in this context it's a noop.
* Check for properly documented public functions ("Returns" and "Params" sections). Initially implemented to lint Phobos. By default disabled.
* Check for explicitly annotated unittests (_@system_ or _@safe_). Initially implemented to lint Phobos. By default disabled.
* Check for that imports are sorted. Initially implemented to lint Phobos. By default disabled.
* Virtual calls inside classes constructors.
* Useless initializers.
* Allman brace style
* Redundant visibility attributes
* Public declarations without a documented unittest. By default disabled.
* Asserts without an explanatory message. By default disabled.
* Indentation of if constraints
* Check that `@trusted` is not applied to a whole scope. Trusting a whole scope can be a problem when new declarations are added and if they are not verified manually to be trustable.
* Redundant storage class attributes

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
If a `dscanner.ini` file is locate in the working directory or any of it's parents, it overrides any other configuration files.

### AST Dump
The "--ast" or "--xml" options will dump the complete abstract syntax tree of
the given source file to standard output in XML format.

```sh
$ dscanner --ast helloworld.d
```

```xml
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
```

For more readable output, pipe the command through [xmllint](http://xmlsoft.org/xmllint.html)
using its formatting switch.

	$ dscanner --ast helloworld.d | xmllint --format -

Selecting modules for a specific check
--------------------------------------

It is possible to create a new section `analysis.config.ModuleFilters` in the `.dscanner.ini`.
In this optional section a comma-separated list of inclusion and exclusion selectors can
be specified for every check on which selective filtering should be applied.
These given selectors match on the module name and partial matches (`std.` or `.foo.`) are possible.
Moreover, every selectors must begin with either `+` (inclusion) or `-` (exclusion).
Exclusion selectors take precedence over all inclusion operators.
Of course, for every check a different selector set can given:

```ini
[analysis.config.ModuleFilters]
final_attribute_check = "+std.foo,+std.bar"
useless_initializer = "-std."
```

A few examples:

- `+std.`: Includes all modules matching `std.`
- `+std.bitmanip,+std.json`: Applies the check only for these two modules
- `-std.bitmanip,-std.json`: Applies the check for all modules, but these two
- `+.bar`: Includes all modules matching `.bar` (e.g. `foo.bar`, `a.b.c.barros`)
- `-etc.`: Excludes all modules from `.etc`
- `+std,-std.internal`: Includes entire `std`, except for the internal modules
