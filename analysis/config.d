//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.config;

import inifiled;

StaticAnalysisConfig defaultStaticAnalysisConfig()
{
	StaticAnalysisConfig config;
	foreach (mem; __traits(allMembers, StaticAnalysisConfig))
		mixin ("config." ~ mem ~ " = true;");
	return config;
}

@INI("Configurue which static analysis checks are enabled")
struct StaticAnalysisConfig
{
	@INI("Check variable, class, struct, interface, union, and function names against the Phobos style guide")
	bool style_check;

	@INI("Check for array literals that cause unnecessary allocation")
	bool enum_array_literal_check;

	@INI("Check for poor exception handling practices")
	bool exception_check;

	@INI("Check for use of the deprecated 'delete' keyword")
	bool delete_check;

	@INI("Check for use of the deprecated floating point operators")
	bool float_operator_check;

	@INI("Check number literals for readability")
	bool number_style_check;

	@INI("Checks that opEquals, opCmp, toHash, and toString are either const, immutable, or inout.")
	bool object_const_check;

	@INI("Checks for .. expressions where the left side is larger than the right.")
	bool backwards_range_check;

	@INI("Checks for if statements whose 'then' block is the same as the 'else' block")
	bool if_else_same_check;

	@INI("Checks for some problems with constructors")
	bool constructor_check;

	@INI("Checks for unused variables and function parameters")
	bool unused_variable_check;

	@INI("Checks for duplicate attributes")
	bool duplicate_attribute;

	@INI("Checks that opEquals and toHash are both defined or neither are defined")
	bool opequals_tohash_check;

	@INI("Checks for subtraction from .length properties")
	bool length_subtraction_check;

	@INI("Checks for methods or properties whose names conflict with built-in properties")
	bool builtin_property_names_check;

	@INI("Checks for confusing code in inline asm statements")
	bool asm_style_check;
}
