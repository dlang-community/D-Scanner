//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.config;

import inifiled;

/// Returns: A default configuration.
StaticAnalysisConfig defaultStaticAnalysisConfig()
{
	StaticAnalysisConfig config;
	return config;
}

/// Describes how a check is operated.
enum Check: string
{
	/// Check is disabled.
	disabled    = "disabled",
	/// Check is enabled.
	enabled     = "enabled",
	/// Check is enabled but not operated in the unittests.
	skipTests   = "skip-unittest"
}

/// Applies the --skipTests switch, allowing to call Dscanner without config
/// and less noise related to the unittests.
void enabled2SkipTests(ref StaticAnalysisConfig config)
{
	foreach (mem; __traits(allMembers, StaticAnalysisConfig))
	{
		static if (is(typeof(__traits(getMember, StaticAnalysisConfig, mem))))
			static if (is(typeof(__traits(getMember, config, mem)) == string))
		{
			if (__traits(getMember, config, mem) == Check.enabled)
				__traits(getMember, config, mem) = Check.skipTests;
		}
	}
}

/// Returns a config with all the checks disabled.
StaticAnalysisConfig disabledConfig()
{
	StaticAnalysisConfig config;
	foreach (mem; __traits(allMembers, StaticAnalysisConfig))
	{
		static if (is(typeof(__traits(getMember, StaticAnalysisConfig, mem))))
		static if (is(typeof(__traits(getMember, config, mem)) == string))
			__traits(getMember, config, mem) = Check.disabled;
	}
	return config;
}

@INI("Configure which static analysis checks are enabled", "analysis.config.StaticAnalysisConfig")
struct StaticAnalysisConfig
{
	@INI("Check variable, class, struct, interface, union, and function names against the Phobos style guide")
	string style_check = Check.enabled;

	@INI("Check for array literals that cause unnecessary allocation")
	string enum_array_literal_check = Check.enabled;

	@INI("Check for poor exception handling practices")
	string exception_check = Check.enabled;

	@INI("Check for use of the deprecated 'delete' keyword")
	string delete_check = Check.enabled;

	@INI("Check for use of the deprecated floating point operators")
	string float_operator_check = Check.enabled;

	@INI("Check number literals for readability")
	string number_style_check = Check.enabled;

	@INI("Checks that opEquals, opCmp, toHash, and toString are either const, immutable, or inout.")
	string object_const_check = Check.enabled;

	@INI("Checks for .. expressions where the left side is larger than the right.")
	string backwards_range_check = Check.enabled;

	@INI("Checks for if statements whose 'then' block is the same as the 'else' block")
	string if_else_same_check = Check.enabled;

	@INI("Checks for some problems with constructors")
	string constructor_check = Check.enabled;

	@INI("Checks for unused variables and function parameters")
	string unused_variable_check = Check.enabled;

	@INI("Checks for unused labels")
	string unused_label_check = Check.enabled;

	@INI("Checks for duplicate attributes")
	string duplicate_attribute = Check.enabled;

	@INI("Checks that opEquals and toHash are both defined or neither are defined")
	string opequals_tohash_check = Check.enabled;

	@INI("Checks for subtraction from .length properties")
	string length_subtraction_check = Check.enabled;

	@INI("Checks for methods or properties whose names conflict with built-in properties")
	string builtin_property_names_check = Check.enabled;

	@INI("Checks for confusing code in inline asm statements")
	string asm_style_check = Check.enabled;

	@INI("Checks for confusing logical operator precedence")
	string logical_precedence_check = Check.enabled;

	@INI("Checks for undocumented public declarations")
	string undocumented_declaration_check = Check.enabled;

	@INI("Checks for poor placement of function attributes")
	string function_attribute_check = Check.enabled;

	@INI("Checks for use of the comma operator")
	string comma_expression_check = Check.enabled;

	@INI("Checks for local imports that are too broad")
	string local_import_check = Check.enabled;

	@INI("Checks for variables that could be declared immutable")
	string could_be_immutable_check = Check.enabled;

	@INI("Checks for redundant expressions in if statements")
	string redundant_if_check = Check.enabled;

	@INI("Checks for redundant parenthesis")
	string redundant_parens_check = Check.enabled;

	@INI("Checks for mismatched argument and parameter names")
	string mismatched_args_check = Check.enabled;

	@INI("Checks for labels with the same name as variables")
	string label_var_same_name_check = Check.enabled;

	@INI("Checks for lines longer than 120 characters")
	string long_line_check = Check.enabled;

	@INI("Checks for assignment to auto-ref function parameters")
	string auto_ref_assignment_check = Check.enabled;

	@INI("Checks for incorrect infinite range definitions")
	string incorrect_infinite_range_check = Check.enabled;

	@INI("Checks for asserts that are always true")
	string useless_assert_check = Check.enabled;

	@INI("Check for uses of the old-style alias syntax")
	string alias_syntax_check = Check.enabled;

	@INI("Checks for else if that should be else static if")
	string static_if_else_check = Check.enabled;

	@INI("Check for unclear lambda syntax")
	string lambda_return_check = Check.enabled;

	@INI("Check for auto function without return statement")
	string auto_function_check = Check.enabled;

	@INI("Check for sortedness of imports")
	string imports_sortedness = Check.disabled;

	@INI("Check for explicitly annotated unittests")
	string explicitly_annotated_unittests = Check.disabled;

	@INI("Check for properly documented public functions (Returns, Params)")
	string properly_documented_public_functions = Check.disabled;

	@INI("Check for useless usage of the final attribute")
	string final_attribute_check = Check.enabled;

	@INI("Check for virtual calls in the class constructors")
	string vcall_in_ctor = Check.enabled;

	@INI("Check for useless user defined initializers")
	string useless_initializer = Check.disabled;

	@INI("Check allman brace style")
	string allman_braces_check = Check.disabled;

	@INI("Check for redundant attributes")
	string redundant_attributes_check = Check.enabled;

	@INI("Check public declarations without a documented unittest")
	string has_public_example = Check.disabled;

	@INI("Check for asserts without an explanatory message")
	string assert_without_msg = Check.disabled;

	@INI("Check indent of if constraints")
	string if_constraints_indent = Check.disabled;

	@INI("Check for @trusted applied to a bigger scope than a single function")
	string trust_too_much = Check.enabled;

	@INI("Check for redundant storage classes on variable declarations")
	string redundant_storage_classes = Check.enabled;

	@INI("Module-specific filters")
	ModuleFilters filters;
}

private template ModuleFiltersMixin(A)
{
	const string ModuleFiltersMixin = () {
		string s;
		foreach (mem; __traits(allMembers, StaticAnalysisConfig))
			static if (is(typeof(__traits(getMember, StaticAnalysisConfig, mem)) == string))
				s ~= `@INI("Exclude/Import modules") string[] ` ~ mem ~ ";\n";

		return s;
	}();
}

@INI("ModuleFilters for selectively enabling (+std) and disabling (-std.internal) individual checks", "analysis.config.ModuleFilters")
struct ModuleFilters
{
	mixin(ModuleFiltersMixin!int);
}
