module dscanner.analysis.rundmd;

import std.algorithm : any, canFind, filter, map;
import std.conv : to;

import dmd.astcodegen;
import dmd.dmodule : Module;
import dmd.frontend;

import dscanner.analysis.config : Check, StaticAnalysisConfig;
import dscanner.analysis.base : BaseAnalyzerDmd, MessageSet;

import dscanner.analysis.alias_syntax_check : AliasSyntaxCheck;
import dscanner.analysis.always_curly : AlwaysCurlyCheck;
import dscanner.analysis.asm_style : AsmStyleCheck;
import dscanner.analysis.assert_without_msg : AssertWithoutMessageCheck;
import dscanner.analysis.auto_function : AutoFunctionChecker;
import dscanner.analysis.auto_ref_assignment : AutoRefAssignmentCheck;
import dscanner.analysis.body_on_disabled_funcs : BodyOnDisabledFuncsCheck;
import dscanner.analysis.builtin_property_names : BuiltinPropertyNameCheck;
import dscanner.analysis.constructors : ConstructorCheck;
import dscanner.analysis.cyclomatic_complexity : CyclomaticComplexityCheck;
import dscanner.analysis.del : DeleteCheck;
import dscanner.analysis.enumarrayliteral : EnumArrayVisitor;
import dscanner.analysis.explicitly_annotated_unittests : ExplicitlyAnnotatedUnittestCheck;
import dscanner.analysis.final_attribute : FinalAttributeChecker;
import dscanner.analysis.has_public_example : HasPublicExampleCheck;
import dscanner.analysis.ifelsesame : IfElseSameCheck;
import dscanner.analysis.imports_sortedness : ImportSortednessCheck;
import dscanner.analysis.incorrect_infinite_range : IncorrectInfiniteRangeCheck;
import dscanner.analysis.label_var_same_name_check : LabelVarNameCheck;
import dscanner.analysis.lambda_return_check : LambdaReturnCheck;
import dscanner.analysis.length_subtraction : LengthSubtractionCheck;
import dscanner.analysis.line_length : LineLengthCheck;
import dscanner.analysis.local_imports : LocalImportCheck;
import dscanner.analysis.logic_precedence : LogicPrecedenceCheck;
import dscanner.analysis.numbers : NumberStyleCheck;
import dscanner.analysis.objectconst : ObjectConstCheck;
import dscanner.analysis.opequals_without_tohash : OpEqualsWithoutToHashCheck;
import dscanner.analysis.pokemon : PokemonExceptionCheck;
import dscanner.analysis.properly_documented_public_functions : ProperlyDocumentedPublicFunctions;
import dscanner.analysis.range : BackwardsRangeCheck;
import dscanner.analysis.redundant_attributes : RedundantAttributesCheck;
import dscanner.analysis.redundant_parens : RedundantParenCheck;
import dscanner.analysis.redundant_storage_class : RedundantStorageClassCheck;
import dscanner.analysis.static_if_else : StaticIfElse;
import dscanner.analysis.style : StyleChecker;
import dscanner.analysis.trust_too_much : TrustTooMuchCheck;
import dscanner.analysis.unmodified : UnmodifiedFinder;
import dscanner.analysis.unused_label : UnusedLabelCheck;
import dscanner.analysis.unused_parameter : UnusedParameterCheck;
import dscanner.analysis.unused_result : UnusedResultChecker;
import dscanner.analysis.unused_variable : UnusedVariableCheck;
import dscanner.analysis.useless_assert : UselessAssertCheck;
import dscanner.analysis.useless_initializer : UselessInitializerChecker;
import dscanner.analysis.vcall_in_ctor : VcallCtorChecker;

version (unittest)
	enum ut = true;
else
	enum ut = false;

Module parseDmdModule(string fileName, string sourceCode)
{
	setupDmd();

	auto code = sourceCode;
	if (code[$ - 1] != '\0')
		code ~= '\0';

	auto dmdModule = dmd.frontend.parseModule(cast(const(char)[]) fileName, cast(const (char)[]) code);
	return dmdModule.module_;
}

private void setupDmd()
{
	import std.path : dirName;
	import dmd.arraytypes : Strings;
	import dmd.globals : global;

	auto dmdParentDir = dirName(dirName(dirName(dirName(__FILE_FULL_PATH__))));
	auto dmdDirPath = dmdParentDir ~ "/dmd" ~ "\0";
	auto druntimeDirPath = dmdParentDir ~ "/dmd/druntime/src" ~ "\0";
	global.params.useUnitTests = true;
	global.path = Strings();
	global.path.push(dmdDirPath.ptr);
	global.path.push(druntimeDirPath.ptr);
	initDMD();
}

MessageSet analyzeDmd(string fileName, ASTCodegen.Module m, const char[] moduleName, const StaticAnalysisConfig config)
{
	MessageSet set = new MessageSet;
	BaseAnalyzerDmd[] visitors;

	if (moduleName.shouldRunDmd!(ObjectConstCheck!ASTCodegen)(config))
		visitors ~= new ObjectConstCheck!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(EnumArrayVisitor!ASTCodegen)(config))
		visitors ~= new EnumArrayVisitor!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(DeleteCheck!ASTCodegen)(config))
		visitors ~= new DeleteCheck!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(FinalAttributeChecker!ASTCodegen)(config))
		visitors ~= new FinalAttributeChecker!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(ImportSortednessCheck!ASTCodegen)(config))
		visitors ~= new ImportSortednessCheck!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(IncorrectInfiniteRangeCheck!ASTCodegen)(config))
		visitors ~= new IncorrectInfiniteRangeCheck!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(RedundantAttributesCheck!ASTCodegen)(config))
		visitors ~= new RedundantAttributesCheck!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(LengthSubtractionCheck!ASTCodegen)(config))
		visitors ~= new LengthSubtractionCheck!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(AliasSyntaxCheck!ASTCodegen)(config))
		visitors ~= new AliasSyntaxCheck!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(ExplicitlyAnnotatedUnittestCheck!ASTCodegen)(config))
		visitors ~= new ExplicitlyAnnotatedUnittestCheck!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(ConstructorCheck!ASTCodegen)(config))
		visitors ~= new ConstructorCheck!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(AssertWithoutMessageCheck!ASTCodegen)(config))
		visitors ~= new AssertWithoutMessageCheck!ASTCodegen(
			fileName,
			config.assert_without_msg == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(LocalImportCheck!ASTCodegen)(config))
		visitors ~= new LocalImportCheck!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(OpEqualsWithoutToHashCheck!ASTCodegen)(config))
		visitors ~= new OpEqualsWithoutToHashCheck!ASTCodegen(
			fileName,
			config.opequals_tohash_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(TrustTooMuchCheck!ASTCodegen)(config))
		visitors ~= new TrustTooMuchCheck!ASTCodegen(
			fileName,
			config.trust_too_much == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(AutoRefAssignmentCheck!ASTCodegen)(config))
		visitors ~= new AutoRefAssignmentCheck!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(LogicPrecedenceCheck!ASTCodegen)(config))
		visitors ~= new LogicPrecedenceCheck!ASTCodegen(
			fileName,
			config.logical_precedence_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(UnusedLabelCheck!ASTCodegen)(config))
		visitors ~= new UnusedLabelCheck!ASTCodegen(
			fileName,
			config.unused_label_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(BuiltinPropertyNameCheck!ASTCodegen)(config))
		visitors ~= new BuiltinPropertyNameCheck!ASTCodegen(fileName);

	if (moduleName.shouldRunDmd!(PokemonExceptionCheck!ASTCodegen)(config))
		visitors ~= new PokemonExceptionCheck!ASTCodegen(
			fileName,
			config.exception_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(BackwardsRangeCheck!ASTCodegen)(config))
		visitors ~= new BackwardsRangeCheck!ASTCodegen(
			fileName,
			config.backwards_range_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(ProperlyDocumentedPublicFunctions!ASTCodegen)(config))
		visitors ~= new ProperlyDocumentedPublicFunctions!ASTCodegen(
			fileName,
			config.properly_documented_public_functions == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(RedundantParenCheck!ASTCodegen)(config))
		visitors ~= new RedundantParenCheck!ASTCodegen(
			fileName,
			config.redundant_parens_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(StaticIfElse!ASTCodegen)(config))
		visitors ~= new StaticIfElse!ASTCodegen(
			fileName,
			config.static_if_else_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(UselessAssertCheck!ASTCodegen)(config))
		visitors ~= new UselessAssertCheck!ASTCodegen(
			fileName,
			config.useless_assert_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(AsmStyleCheck!ASTCodegen)(config))
		visitors ~= new AsmStyleCheck!ASTCodegen(
			fileName,
			config.asm_style_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(RedundantStorageClassCheck!ASTCodegen)(config))
		visitors ~= new RedundantStorageClassCheck!ASTCodegen(
			fileName,
			config.redundant_storage_classes == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(NumberStyleCheck!ASTCodegen)(config))
		visitors ~= new NumberStyleCheck!ASTCodegen(
			fileName,
			config.number_style_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(IfElseSameCheck!ASTCodegen)(config))
		visitors ~= new IfElseSameCheck!ASTCodegen(
			fileName,
			config.if_else_same_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(CyclomaticComplexityCheck!ASTCodegen)(config))
		visitors ~= new CyclomaticComplexityCheck!ASTCodegen(
			fileName,
			config.cyclomatic_complexity == Check.skipTests && !ut,
			config.max_cyclomatic_complexity.to!int
		);

	if (moduleName.shouldRunDmd!(LabelVarNameCheck!ASTCodegen)(config))
		visitors ~= new LabelVarNameCheck!ASTCodegen(
			fileName,
			config.label_var_same_name_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(LambdaReturnCheck!ASTCodegen)(config))
		visitors ~= new LambdaReturnCheck!ASTCodegen(
			fileName,
			config.lambda_return_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(AlwaysCurlyCheck!ASTCodegen)(config))
		visitors ~= new AlwaysCurlyCheck!ASTCodegen(
			fileName,
			config.always_curly_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(StyleChecker!ASTCodegen)(config))
		visitors ~= new StyleChecker!ASTCodegen(
			fileName,
			config.style_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(AutoFunctionChecker!ASTCodegen)(config))
		visitors ~= new AutoFunctionChecker!ASTCodegen(
			fileName,
			config.auto_function_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(UnusedParameterCheck!ASTCodegen)(config))
		visitors ~= new UnusedParameterCheck!ASTCodegen(
			fileName,
			config.unused_parameter_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(UnusedVariableCheck!ASTCodegen)(config))
		visitors ~= new UnusedVariableCheck!ASTCodegen(
			fileName,
			config.unused_variable_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(UnmodifiedFinder!ASTCodegen)(config))
		visitors ~= new UnmodifiedFinder!ASTCodegen(
			fileName,
			config.could_be_immutable_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(BodyOnDisabledFuncsCheck!ASTCodegen)(config))
		visitors ~= new BodyOnDisabledFuncsCheck!ASTCodegen(
			fileName,
			config.body_on_disabled_func_check == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(UselessInitializerChecker!ASTCodegen)(config))
		visitors ~= new UselessInitializerChecker!ASTCodegen(
			fileName,
			config.useless_initializer == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(HasPublicExampleCheck!ASTCodegen)(config))
		visitors ~= new HasPublicExampleCheck!ASTCodegen(
			fileName,
			config.has_public_example == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!LineLengthCheck(config))
		visitors ~= new LineLengthCheck(
			fileName,
			config.long_line_check == Check.skipTests && !ut,
			config.max_line_length
		);

	if (moduleName.shouldRunDmd!(UnusedResultChecker!ASTCodegen)(config))
		visitors ~= new UnusedResultChecker!ASTCodegen(
			fileName,
			config.unused_result == Check.skipTests && !ut
		);

	if (moduleName.shouldRunDmd!(VcallCtorChecker!ASTCodegen)(config))
		visitors ~= new VcallCtorChecker!ASTCodegen(
			fileName,
			config.vcall_in_ctor == Check.skipTests && !ut
		);

	foreach (visitor; visitors)
	{
		m.accept(visitor);

		foreach (message; visitor.messages)
			set.insert(message);
	}

	return set;
}

/**
 * Checks whether a module is part of a user-specified include/exclude list.
 *
 * The user can specify a comma-separated list of filters, everyone needs to start with
 * either a '+' (inclusion) or '-' (exclusion).
 *
 * If no includes are specified, all modules are included.
*/
private bool shouldRunDmd(check : BaseAnalyzerDmd)(const char[] moduleName, const ref StaticAnalysisConfig config)
{
	enum string a = check.name;

	if (mixin("config." ~ a) == Check.disabled)
		return false;

	// By default, run the check
	if (!moduleName.length)
		return true;

	auto filters = mixin("config.filters." ~ a);

	// Check if there are filters are defined
	// filters starting with a comma are invalid
	if (filters.length == 0 || filters[0].length == 0)
		return true;

	auto includers = filters.filter!(f => f[0] == '+').map!(f => f[1..$]);
	auto excluders = filters.filter!(f => f[0] == '-').map!(f => f[1..$]);

	// exclusion has preference over inclusion
	if (!excluders.empty && excluders.any!(s => moduleName.canFind(s)))
		return false;

	if (!includers.empty)
		return includers.any!(s => moduleName.canFind(s));

	// by default: include all modules
	return true;
}

///
unittest
{
	bool test(string moduleName, string filters)
	{
		import std.array : split;

		StaticAnalysisConfig config;
		// it doesn't matter which check we test here
		config.asm_style_check = Check.enabled;
		// this is done automatically by inifiled
		config.filters.asm_style_check = filters.split(",");
		return moduleName.shouldRunDmd!(AsmStyleCheck!ASTCodegen)(config);
	}

	// test inclusion
	assert(test("std.foo", "+std."));
	// partial matches are ok
	assert(test("std.foo", "+bar,+foo"));
	// full as well
	assert(test("std.foo", "+bar,+std.foo,+foo"));
	// mismatch
	assert(!test("std.foo", "+bar,+banana"));

	// test exclusion
	assert(!test("std.foo", "-std."));
	assert(!test("std.foo", "-bar,-std.foo"));
	assert(!test("std.foo", "-bar,-foo"));
	// mismatch
	assert(test("std.foo", "-bar,-banana"));

	// test combination (exclusion has precedence)
	assert(!test("std.foo", "+foo,-foo"));
	assert(test("std.foo", "+foo,-bar"));
	assert(test("std.bar.foo", "-barr,+bar"));
}
