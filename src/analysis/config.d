//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.config;

import inifiled;

StaticAnalysisConfig defaultStaticAnalysisConfig()
{
	StaticAnalysisConfig config;
	config.fillConfig!(Check.enabled);
	return config;
}

enum Check: string
{
    disabled    = "disabled",
    enabled     = "enabled",
    skipTests   = "skip-unittest"
}

void fillConfig(string check)(ref StaticAnalysisConfig config)
{
    foreach (mem; __traits(allMembers, StaticAnalysisConfig))
    {
        static if (is(typeof(__traits(getMember, StaticAnalysisConfig, mem))))
            static if (is(typeof(__traits(getMember, config, mem)) == string))
                __traits(getMember, config, mem) = check;
    }
}

unittest
{
    StaticAnalysisConfig c;
    c.fillConfig!(Check.enabled);
    assert(c.enum_array_literal_check == Check.enabled);
    fillConfig!(Check.skipTests)(c);
    assert(c.alias_syntax_check == Check.skipTests);
}

@INI("Configure which static analysis checks are enabled")
struct StaticAnalysisConfig
{
	@INI("Check variable, class, struct, interface, union, and function names against the Phobos style guide")
	string style_check = Check.disabled;

	@INI("Check for array literals that cause unnecessary allocation")
	string enum_array_literal_check = Check.disabled;

	@INI("Check for poor exception handling practices")
	string exception_check = Check.disabled;

	@INI("Check for use of the deprecated 'delete' keyword")
	string delete_check = Check.disabled;

	@INI("Check for use of the deprecated floating point operators")
	string float_operator_check = Check.disabled;

	@INI("Check number literals for readability")
	string number_style_check = Check.disabled;

	@INI("Checks that opEquals, opCmp, toHash, and toString are either const, immutable, or inout.")
	string object_const_check = Check.disabled;

	@INI("Checks for .. expressions where the left side is larger than the right.")
	string backwards_range_check = Check.disabled;

	@INI("Checks for if statements whose 'then' block is the same as the 'else' block")
	string if_else_same_check = Check.disabled;

	@INI("Checks for some problems with constructors")
	string constructor_check = Check.disabled;

	@INI("Checks for unused variables and function parameters")
	string unused_variable_check = Check.disabled;

	@INI("Checks for unused labels")
	string unused_label_check = Check.disabled;

	@INI("Checks for duplicate attributes")
	string duplicate_attribute = Check.disabled;

	@INI("Checks that opEquals and toHash are both defined or neither are defined")
	string opequals_tohash_check = Check.disabled;

	@INI("Checks for subtraction from .length properties")
	string length_subtraction_check = Check.disabled;

	@INI("Checks for methods or properties whose names conflict with built-in properties")
	string builtin_property_names_check = Check.disabled;

	@INI("Checks for confusing code in inline asm statements")
	string asm_style_check = Check.disabled;

	@INI("Checks for confusing logical operator precedence")
	string logical_precedence_check = Check.disabled;

	@INI("Checks for undocumented public declarations")
	string undocumented_declaration_check = Check.disabled;

	@INI("Checks for poor placement of function attributes")
	string function_attribute_check = Check.disabled;

	@INI("Checks for use of the comma operator")
	string comma_expression_check = Check.disabled;

	@INI("Checks for local imports that are too broad")
	string local_import_check = Check.disabled;

	@INI("Checks for variables that could be declared immutable")
	string could_be_immutable_check = Check.disabled; // disabled by default for now

	@INI("Checks for redundant expressions in if statements")
	string redundant_if_check = Check.disabled;

	@INI("Checks for redundant parenthesis")
	string redundant_parens_check = Check.disabled;

	@INI("Checks for mismatched argument and parameter names")
	string mismatched_args_check = Check.disabled;

	@INI("Checks for labels with the same name as variables")
	string label_var_same_name_check = Check.disabled;

	@INI("Checks for lines longer than 120 characters")
	string long_line_check = Check.disabled;

	@INI("Checks for assignment to auto-ref function parameters")
	string auto_ref_assignment_check = Check.disabled;

	@INI("Checks for incorrect infinite range definitions")
	string incorrect_infinite_range_check = Check.disabled;

	@INI("Checks for asserts that are always true")
	string useless_assert_check = Check.disabled;

	@INI("Check for uses of the old-style alias syntax")
	string alias_syntax_check = Check.disabled;

	@INI("Checks for else if that should be else static if")
	string static_if_else_check = Check.disabled;

	@INI("Check for unclear lambda syntax")
	string lambda_return_check = Check.disabled;

	@INI("Check for auto function without return statement")
	string auto_function_check = Check.disabled;

	@INI("Check for sortedness of imports")
	string imports_sortedness = Check.disabled;

	@INI("Check for explicitly annotated unittests")
	string explicitly_annotated_unittests = Check.disabled;

    @INI("Check for useless usage of the final attribute")
    string final_attribute_check = Check.disabled;
}
