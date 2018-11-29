//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.run;

import core.memory : GC;

import std.stdio;
import std.array;
import std.conv;
import std.algorithm;
import std.range;
import std.array;
import std.functional : toDelegate;
import dparse.lexer;
import dparse.parser;
import dparse.ast;
import dparse.rollback_allocator;
import std.typecons : scoped;

import stdx.allocator : CAllocatorImpl;
import stdx.allocator.mallocator : Mallocator;
import stdx.allocator.building_blocks.region : Region;
import stdx.allocator.building_blocks.allocator_list : AllocatorList;

import dscanner.analysis.config;
import dscanner.analysis.base;
import dscanner.analysis.style;
import dscanner.analysis.enumarrayliteral;
import dscanner.analysis.pokemon;
import dscanner.analysis.del;
import dscanner.analysis.fish;
import dscanner.analysis.numbers;
import dscanner.analysis.objectconst;
import dscanner.analysis.range;
import dscanner.analysis.ifelsesame;
import dscanner.analysis.constructors;
import dscanner.analysis.unused;
import dscanner.analysis.unused_label;
import dscanner.analysis.duplicate_attribute;
import dscanner.analysis.opequals_without_tohash;
import dscanner.analysis.length_subtraction;
import dscanner.analysis.builtin_property_names;
import dscanner.analysis.asm_style;
import dscanner.analysis.logic_precedence;
import dscanner.analysis.stats_collector;
import dscanner.analysis.undocumented;
import dscanner.analysis.comma_expression;
import dscanner.analysis.function_attributes;
import dscanner.analysis.local_imports;
import dscanner.analysis.unmodified;
import dscanner.analysis.if_statements;
import dscanner.analysis.redundant_parens;
import dscanner.analysis.mismatched_args;
import dscanner.analysis.label_var_same_name_check;
import dscanner.analysis.line_length;
import dscanner.analysis.auto_ref_assignment;
import dscanner.analysis.incorrect_infinite_range;
import dscanner.analysis.useless_assert;
import dscanner.analysis.alias_syntax_check;
import dscanner.analysis.static_if_else;
import dscanner.analysis.lambda_return_check;
import dscanner.analysis.auto_function;
import dscanner.analysis.imports_sortedness;
import dscanner.analysis.explicitly_annotated_unittests;
import dscanner.analysis.properly_documented_public_functions;
import dscanner.analysis.final_attribute;
import dscanner.analysis.vcall_in_ctor;
import dscanner.analysis.useless_initializer;
import dscanner.analysis.allman;
import dscanner.analysis.redundant_attributes;
import dscanner.analysis.has_public_example;
import dscanner.analysis.assert_without_msg;
import dscanner.analysis.if_constraints_indent;
import dscanner.analysis.trust_too_much;
import dscanner.analysis.redundant_storage_class;

import dsymbol.string_interning : internString;
import dsymbol.scope_;
import dsymbol.semantic;
import dsymbol.conversion;
import dsymbol.conversion.first;
import dsymbol.conversion.second;
import dsymbol.modulecache : ModuleCache;

import dscanner.utils;

bool first = true;

private alias ASTAllocator = CAllocatorImpl!(
		AllocatorList!(n => Region!Mallocator(1024 * 128), Mallocator));

immutable string defaultErrorFormat = "{filepath}({line}:{column})[{type}]: {message}";

void messageFunctionFormat(string format, string fileName, size_t line, size_t column, string message, bool isError)
{
	auto s = format;

	s = s.replace("{filepath}", fileName);
	s = s.replace("{line}", to!string(line));
	s = s.replace("{column}", to!string(column));
	s = s.replace("{type}", isError ? "error" : "warn");
	s = s.replace("{message}", message);

	writefln("%s", s);
}

void messageFunction(string fileName, size_t line, size_t column, string message, bool isError)
{
	messageFunctionFormat(defaultErrorFormat, fileName, line, column, message, isError);
}

void messageFunctionJSON(string fileName, size_t line, size_t column, string message, bool)
{
	writeJSON("dscanner.syntax", fileName, line, column, message);
}

void writeJSON(string key, string fileName, size_t line, size_t column, string message)
{
	if (!first)
		writeln(",");
	else
		first = false;
	writeln("    {");
	writeln(`      "key": "`, key, `",`);
	writeln(`      "fileName": "`, fileName.replace("\\", "\\\\").replace(`"`, `\"`), `",`);
	writeln(`      "line": `, line, `,`);
	writeln(`      "column": `, column, `,`);
	writeln(`      "message": "`, message.replace("\\", "\\\\").replace(`"`, `\"`), `"`);
	write("    }");
}

bool syntaxCheck(string[] fileNames, string errorFormat, ref StringCache stringCache, ref ModuleCache moduleCache)
{
	StaticAnalysisConfig config = defaultStaticAnalysisConfig();
	return analyze(fileNames, config, errorFormat, stringCache, moduleCache, false);
}

void generateReport(string[] fileNames, const StaticAnalysisConfig config,
		ref StringCache cache, ref ModuleCache moduleCache)
{
	writeln("{");
	writeln(`  "issues": [`);
	first = true;
	StatsCollector stats = new StatsCollector("");
	ulong lineOfCodeCount;
	foreach (fileName; fileNames)
	{
		auto code = readFile(fileName);
		// Skip files that could not be read and continue with the rest
		if (code.length == 0)
			continue;
		RollbackAllocator r;
		const(Token)[] tokens;
		const Module m = parseModule(fileName, code, &r, defaultErrorFormat, cache, true, tokens, &lineOfCodeCount);
		stats.visit(m);
		MessageSet results = analyze(fileName, m, config, moduleCache, tokens, true);
		foreach (result; results[])
		{
			writeJSON(result.key, result.fileName, result.line, result.column, result.message);
		}
	}
	writeln();
	writeln("  ],");
	writefln(`  "interfaceCount": %d,`, stats.interfaceCount);
	writefln(`  "classCount": %d,`, stats.classCount);
	writefln(`  "functionCount": %d,`, stats.functionCount);
	writefln(`  "templateCount": %d,`, stats.templateCount);
	writefln(`  "structCount": %d,`, stats.structCount);
	writefln(`  "statementCount": %d,`, stats.statementCount);
	writefln(`  "lineOfCodeCount": %d,`, lineOfCodeCount);
	writefln(`  "undocumentedPublicSymbols": %d`, stats.undocumentedPublicSymbols);
	writeln("}");
}

/**
 * For multiple files
 *
 * Returns: true if there were errors or if there were warnings and `staticAnalyze` was true.
 */
bool analyze(string[] fileNames, const StaticAnalysisConfig config, string errorFormat,
		ref StringCache cache, ref ModuleCache moduleCache, bool staticAnalyze = true)
{
	bool hasErrors;
	foreach (fileName; fileNames)
	{
		auto code = readFile(fileName);
		// Skip files that could not be read and continue with the rest
		if (code.length == 0)
			continue;
		RollbackAllocator r;
		uint errorCount;
		uint warningCount;
		const(Token)[] tokens;
		const Module m = parseModule(fileName, code, &r, errorFormat, cache, false, tokens,
				null, &errorCount, &warningCount);
		assert(m);
		if (errorCount > 0 || (staticAnalyze && warningCount > 0))
			hasErrors = true;
		MessageSet results = analyze(fileName, m, config, moduleCache, tokens, staticAnalyze);
		if (results is null)
			continue;
		foreach (result; results[])
		{
			hasErrors = true;
			messageFunctionFormat(errorFormat, result.fileName, result.line, result.column, result.message, false);
		}
	}
	return hasErrors;
}

const(Module) parseModule(string fileName, ubyte[] code, RollbackAllocator* p,
		string errorFormat, ref StringCache cache, bool report, ref const(Token)[] tokens,
		ulong* linesOfCode = null, uint* errorCount = null, uint* warningCount = null)
{
	import dscanner.stats : isLineOfCode;

	auto writeMessages = delegate(string fileName, size_t line, size_t column, string message, bool isError){
		return messageFunctionFormat(errorFormat, fileName, line, column, message, isError);
	};

	LexerConfig config;
	config.fileName = fileName;
	config.stringBehavior = StringBehavior.source;
	tokens = getTokensForParser(code, config, &cache);
	if (linesOfCode !is null)
		(*linesOfCode) += count!(a => isLineOfCode(a.type))(tokens);
	return dparse.parser.parseModule(tokens, fileName, p,
		report ? toDelegate(&messageFunctionJSON) : writeMessages,
		errorCount, warningCount);
}

/**
Checks whether a module is part of a user-specified include/exclude list.
The user can specify a comma-separated list of filters, everyone needs to start with
either a '+' (inclusion) or '-' (exclusion).
If no includes are specified, all modules are included.
*/
bool shouldRun(string a)(string moduleName, const ref StaticAnalysisConfig config)
{
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
		StaticAnalysisConfig config;
		// it doesn't matter which check we test here
		config.asm_style_check = Check.enabled;
		// this is done automatically by inifiled
		config.filters.asm_style_check = filters.split(",");
		return shouldRun!"asm_style_check"(moduleName, config);
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

MessageSet analyze(string fileName, const Module m, const StaticAnalysisConfig analysisConfig,
		ref ModuleCache moduleCache, const(Token)[] tokens, bool staticAnalyze = true)
{
	import dsymbol.symbol : DSymbol;

	if (!staticAnalyze)
		return null;

	auto symbolAllocator = scoped!ASTAllocator();
	version (unittest)
		enum ut = true;
	else
		enum ut = false;

	string moduleName;
	if (m !is null && m.moduleDeclaration !is null &&
		  m.moduleDeclaration.moduleName !is null &&
		  m.moduleDeclaration.moduleName.identifiers !is null)
		moduleName = m.moduleDeclaration.moduleName.identifiers.map!(e => e.text).join(".");

	auto first = scoped!FirstPass(m, internString(fileName), symbolAllocator,
			symbolAllocator, true, &moduleCache, null);
	first.run();

	secondPass(first.rootSymbol, first.moduleScope, moduleCache);
	auto moduleScope = first.moduleScope;
	scope(exit) typeid(DSymbol).destroy(first.rootSymbol.acSymbol);
	scope(exit) typeid(SemanticSymbol).destroy(first.rootSymbol);
	scope(exit) typeid(Scope).destroy(first.moduleScope);
	BaseAnalyzer[] checks;

	GC.disable;

	with(analysisConfig)
	if (moduleName.shouldRun!"asm_style_check"(analysisConfig))
		checks ~= new AsmStyleCheck(fileName, moduleScope,
		asm_style_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"backwards_range_check"(analysisConfig))
		checks ~= new BackwardsRangeCheck(fileName, moduleScope,
		analysisConfig.backwards_range_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"builtin_property_names_check"(analysisConfig))
		checks ~= new BuiltinPropertyNameCheck(fileName, moduleScope,
		analysisConfig.builtin_property_names_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"comma_expression_check"(analysisConfig))
		checks ~= new CommaExpressionCheck(fileName, moduleScope,
		analysisConfig.comma_expression_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"constructor_check"(analysisConfig))
		checks ~= new ConstructorCheck(fileName, moduleScope,
		analysisConfig.constructor_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"could_be_immutable_check"(analysisConfig))
		checks ~= new UnmodifiedFinder(fileName, moduleScope,
		analysisConfig.could_be_immutable_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"delete_check"(analysisConfig))
		checks ~= new DeleteCheck(fileName, moduleScope,
		analysisConfig.delete_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"duplicate_attribute"(analysisConfig))
		checks ~= new DuplicateAttributeCheck(fileName, moduleScope,
		analysisConfig.duplicate_attribute == Check.skipTests && !ut);

	if (moduleName.shouldRun!"enum_array_literal_check"(analysisConfig))
		checks ~= new EnumArrayLiteralCheck(fileName, moduleScope,
		analysisConfig.enum_array_literal_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"exception_check"(analysisConfig))
		checks ~= new PokemonExceptionCheck(fileName, moduleScope,
		analysisConfig.exception_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"float_operator_check"(analysisConfig))
		checks ~= new FloatOperatorCheck(fileName, moduleScope,
		analysisConfig.float_operator_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"function_attribute_check"(analysisConfig))
		checks ~= new FunctionAttributeCheck(fileName, moduleScope,
		analysisConfig.function_attribute_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"if_else_same_check"(analysisConfig))
		checks ~= new IfElseSameCheck(fileName, moduleScope,
		analysisConfig.if_else_same_check == Check.skipTests&& !ut);

	if (moduleName.shouldRun!"label_var_same_name_check"(analysisConfig))
		checks ~= new LabelVarNameCheck(fileName, moduleScope,
		analysisConfig.label_var_same_name_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"length_subtraction_check"(analysisConfig))
		checks ~= new LengthSubtractionCheck(fileName, moduleScope,
		analysisConfig.length_subtraction_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"local_import_check"(analysisConfig))
		checks ~= new LocalImportCheck(fileName, moduleScope,
		analysisConfig.local_import_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"logical_precedence_check"(analysisConfig))
		checks ~= new LogicPrecedenceCheck(fileName, moduleScope,
		analysisConfig.logical_precedence_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"mismatched_args_check"(analysisConfig))
		checks ~= new MismatchedArgumentCheck(fileName, moduleScope,
		analysisConfig.mismatched_args_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"number_style_check"(analysisConfig))
		checks ~= new NumberStyleCheck(fileName, moduleScope,
		analysisConfig.number_style_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"object_const_check"(analysisConfig))
		checks ~= new ObjectConstCheck(fileName, moduleScope,
		analysisConfig.object_const_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"opequals_tohash_check"(analysisConfig))
		checks ~= new OpEqualsWithoutToHashCheck(fileName, moduleScope,
		analysisConfig.opequals_tohash_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"redundant_parens_check"(analysisConfig))
		checks ~= new RedundantParenCheck(fileName, moduleScope,
		analysisConfig.redundant_parens_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"style_check"(analysisConfig))
		checks ~= new StyleChecker(fileName, moduleScope,
		analysisConfig.style_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"undocumented_declaration_check"(analysisConfig))
		checks ~= new UndocumentedDeclarationCheck(fileName, moduleScope,
		analysisConfig.undocumented_declaration_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"unused_label_check"(analysisConfig))
		checks ~= new UnusedLabelCheck(fileName, moduleScope,
		analysisConfig.unused_label_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"unused_variable_check"(analysisConfig))
		checks ~= new UnusedVariableCheck(fileName, moduleScope,
		analysisConfig.unused_variable_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"long_line_check"(analysisConfig))
		checks ~= new LineLengthCheck(fileName, tokens,
		analysisConfig.long_line_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"auto_ref_assignment_check"(analysisConfig))
		checks ~= new AutoRefAssignmentCheck(fileName,
		analysisConfig.auto_ref_assignment_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"incorrect_infinite_range_check"(analysisConfig))
		checks ~= new IncorrectInfiniteRangeCheck(fileName,
		analysisConfig.incorrect_infinite_range_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"useless_assert_check"(analysisConfig))
		checks ~= new UselessAssertCheck(fileName,
		analysisConfig.useless_assert_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"alias_syntax_check"(analysisConfig))
		checks ~= new AliasSyntaxCheck(fileName,
		analysisConfig.alias_syntax_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"static_if_else_check"(analysisConfig))
		checks ~= new StaticIfElse(fileName,
		analysisConfig.static_if_else_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"lambda_return_check"(analysisConfig))
		checks ~= new LambdaReturnCheck(fileName,
		analysisConfig.lambda_return_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"auto_function_check"(analysisConfig))
		checks ~= new AutoFunctionChecker(fileName,
		analysisConfig.auto_function_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"imports_sortedness"(analysisConfig))
		checks ~= new ImportSortednessCheck(fileName,
		analysisConfig.imports_sortedness == Check.skipTests && !ut);

	if (moduleName.shouldRun!"explicitly_annotated_unittests"(analysisConfig))
		checks ~= new ExplicitlyAnnotatedUnittestCheck(fileName,
		analysisConfig.explicitly_annotated_unittests == Check.skipTests && !ut);

	if (moduleName.shouldRun!"properly_documented_public_functions"(analysisConfig))
		checks ~= new ProperlyDocumentedPublicFunctions(fileName,
		analysisConfig.properly_documented_public_functions == Check.skipTests && !ut);

	if (moduleName.shouldRun!"final_attribute_check"(analysisConfig))
		checks ~= new FinalAttributeChecker(fileName,
		analysisConfig.final_attribute_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"vcall_in_ctor"(analysisConfig))
		checks ~= new VcallCtorChecker(fileName,
		analysisConfig.vcall_in_ctor == Check.skipTests && !ut);

	if (moduleName.shouldRun!"useless_initializer"(analysisConfig))
		checks ~= new UselessInitializerChecker(fileName,
		analysisConfig.useless_initializer == Check.skipTests && !ut);

	if (moduleName.shouldRun!"allman_braces_check"(analysisConfig))
		checks ~= new AllManCheck(fileName, tokens,
		analysisConfig.allman_braces_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"redundant_attributes_check"(analysisConfig))
		checks ~= new RedundantAttributesCheck(fileName, moduleScope,
		analysisConfig.redundant_attributes_check == Check.skipTests && !ut);

	if (moduleName.shouldRun!"has_public_example"(analysisConfig))
		checks ~= new HasPublicExampleCheck(fileName, moduleScope,
		analysisConfig.has_public_example == Check.skipTests && !ut);

	if (moduleName.shouldRun!"assert_without_msg"(analysisConfig))
		checks ~= new AssertWithoutMessageCheck(fileName, moduleScope,
		analysisConfig.assert_without_msg == Check.skipTests && !ut);

	if (moduleName.shouldRun!"if_constraints_indent"(analysisConfig))
		checks ~= new IfConstraintsIndentCheck(fileName, tokens,
		analysisConfig.if_constraints_indent == Check.skipTests && !ut);

	if (moduleName.shouldRun!"trust_too_much"(analysisConfig))
		checks ~= new TrustTooMuchCheck(fileName,
		analysisConfig.trust_too_much == Check.skipTests && !ut);

	if (moduleName.shouldRun!"redundant_storage_classes"(analysisConfig))
		checks ~= new RedundantStorageClassCheck(fileName,
		analysisConfig.redundant_storage_classes == Check.skipTests && !ut);

	version (none)
		if (moduleName.shouldRun!"redundant_if_check"(analysisConfig))
			checks ~= new IfStatementCheck(fileName, moduleScope,
			analysisConfig.redundant_if_check == Check.skipTests && !ut);

	foreach (check; checks)
	{
		check.visit(m);
	}

	MessageSet set = new MessageSet;
	foreach (check; checks)
		foreach (message; check.messages)
			set.insert(message);

	GC.enable;

	return set;
}

