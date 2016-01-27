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
		mixin("config." ~ mem ~ " = true;");
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

	@INI("Checks for unused labels")
	bool unused_label_check;

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

	@INI("Checks for confusing logical operator precedence")
	bool logical_precedence_check;

	@INI("Checks for undocumented public declarations")
	bool undocumented_declaration_check;

	@INI("Checks for poor placement of function attributes")
	bool function_attribute_check;

	@INI("Checks for use of the comma operator")
	bool comma_expression_check;

	@INI("Checks for local imports that are too broad")
	bool local_import_check;

	@INI("Checks for variables that could be declared immutable")
	bool could_be_immutable_check = false; // disabled by default for now

	@INI("Checks for redundant expressions in if statements")
	bool redundant_if_check;

	@INI("Checks for redundant parenthesis")
	bool redundant_parens_check;

	@INI("Checks for mismatched argument and parameter names")
	bool mismatched_args_check;

	@INI("Checks for labels with the same name as variables")
	bool label_var_same_name_check;

	@INI("Checks for lines longer than 120 characters")
	bool long_line_check;

	@INI("Checks for assignment to auto-ref function parameters")
	bool auto_ref_assignment_check;

	@INI("Checks for incorrect infinite range definitions")
	bool incorrect_infinite_range_check;

	@INI("Checks for asserts that are always true")
	bool useless_assert_check;
}
