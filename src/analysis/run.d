//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.run;

import std.stdio;
import std.array;
import std.conv;
import std.algorithm;
import std.range;
import std.array;
import dparse.lexer;
import dparse.parser;
import dparse.ast;
import dparse.rollback_allocator;
import std.typecons : scoped;

import std.experimental.allocator : CAllocatorImpl;
import std.experimental.allocator.mallocator : Mallocator;
import std.experimental.allocator.building_blocks.region : Region;
import std.experimental.allocator.building_blocks.allocator_list : AllocatorList;

import analysis.config;
import analysis.base;
import analysis.style;
import analysis.enumarrayliteral;
import analysis.pokemon;
import analysis.del;
import analysis.fish;
import analysis.numbers;
import analysis.objectconst;
import analysis.range;
import analysis.ifelsesame;
import analysis.constructors;
import analysis.unused;
import analysis.unused_label;
import analysis.duplicate_attribute;
import analysis.opequals_without_tohash;
import analysis.length_subtraction;
import analysis.builtin_property_names;
import analysis.asm_style;
import analysis.logic_precedence;
import analysis.stats_collector;
import analysis.undocumented;
import analysis.comma_expression;
import analysis.function_attributes;
import analysis.local_imports;
import analysis.unmodified;
import analysis.if_statements;
import analysis.redundant_parens;
import analysis.mismatched_args;
import analysis.label_var_same_name_check;
import analysis.line_length;
import analysis.auto_ref_assignment;
import analysis.incorrect_infinite_range;
import analysis.useless_assert;
import analysis.alias_syntax_check;
import analysis.static_if_else;
import analysis.lambda_return_check;
import analysis.auto_function;

import dsymbol.string_interning : internString;
import dsymbol.scope_;
import dsymbol.semantic;
import dsymbol.conversion;
import dsymbol.conversion.first;
import dsymbol.conversion.second;
import dsymbol.modulecache : ModuleCache;

import readers;

bool first = true;

private alias ASTAllocator = CAllocatorImpl!(
		AllocatorList!(n => Region!Mallocator(1024 * 128), Mallocator));

void messageFunction(string fileName, size_t line, size_t column, string message, bool isError)
{
	writefln("%s(%d:%d)[%s]: %s", fileName, line, column, isError ? "error" : "warn", message);
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
	writeln(`      "fileName": "`, fileName.replace(`"`, `\"`).replace("\\", "\\\\"), `",`);
	writeln(`      "line": `, line, `,`);
	writeln(`      "column": `, column, `,`);
	writeln(`      "message": "`, message.replace(`"`, `\"`).replace("\\", "\\\\"), `"`);
	write("    }");
}

bool syntaxCheck(string[] fileNames, ref StringCache stringCache, ref ModuleCache moduleCache)
{
	StaticAnalysisConfig config = defaultStaticAnalysisConfig();
	return analyze(fileNames, config, stringCache, moduleCache, false);
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
		auto code = fileName == "stdin" ? readStdin() : readFile(fileName);
		// Skip files that could not be read and continue with the rest
		if (code.length == 0)
			continue;
		RollbackAllocator r;
		const(Token)[] tokens;
		const Module m = parseModule(fileName, code, &r, cache, true, tokens, &lineOfCodeCount);
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
bool analyze(string[] fileNames, const StaticAnalysisConfig config,
		ref StringCache cache, ref ModuleCache moduleCache, bool staticAnalyze = true)
{
	bool hasErrors = false;
	foreach (fileName; fileNames)
	{
		auto code = fileName == "stdin" ? readStdin() : readFile(fileName);
		// Skip files that could not be read and continue with the rest
		if (code.length == 0)
			continue;
		RollbackAllocator r;
		uint errorCount = 0;
		uint warningCount = 0;
		const(Token)[] tokens;
		const Module m = parseModule(fileName, code, &r, cache, false, tokens,
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
			writefln("%s(%d:%d)[warn]: %s", result.fileName, result.line,
					result.column, result.message);
		}
	}
	return hasErrors;
}

const(Module) parseModule(string fileName, ubyte[] code, RollbackAllocator* p,
		ref StringCache cache, bool report, ref const(Token)[] tokens,
		ulong* linesOfCode = null, uint* errorCount = null, uint* warningCount = null)
{
	import stats : isLineOfCode;

	LexerConfig config;
	config.fileName = fileName;
	config.stringBehavior = StringBehavior.source;
	tokens = getTokensForParser(code, config, &cache);
	if (linesOfCode !is null)
		(*linesOfCode) += count!(a => isLineOfCode(a.type))(tokens);
	return dparse.parser.parseModule(tokens, fileName, p, report
			? &messageFunctionJSON : &messageFunction, errorCount, warningCount);
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


	auto first = scoped!FirstPass(m, internString(fileName), symbolAllocator,
			symbolAllocator, true, &moduleCache, null);
	first.run();

	secondPass(first.rootSymbol, first.moduleScope, moduleCache);
	auto moduleScope = first.moduleScope;
	scope(exit) typeid(DSymbol).destroy(first.rootSymbol.acSymbol);
	scope(exit) typeid(SemanticSymbol).destroy(first.rootSymbol);
	scope(exit) typeid(Scope).destroy(first.moduleScope);
	BaseAnalyzer[] checks;

	if (analysisConfig.asm_style_check != Check.disabled)
		checks ~= new AsmStyleCheck(fileName, moduleScope,
		analysisConfig.asm_style_check == Check.skipTests && !ut);

	if (analysisConfig.backwards_range_check != Check.disabled)
		checks ~= new BackwardsRangeCheck(fileName, moduleScope,
		analysisConfig.backwards_range_check == Check.skipTests && !ut);

	if (analysisConfig.builtin_property_names_check != Check.disabled)
		checks ~= new BuiltinPropertyNameCheck(fileName, moduleScope,
		analysisConfig.builtin_property_names_check == Check.skipTests && !ut);

	if (analysisConfig.comma_expression_check != Check.disabled)
		checks ~= new CommaExpressionCheck(fileName, moduleScope,
		analysisConfig.comma_expression_check == Check.skipTests && !ut);

	if (analysisConfig.constructor_check != Check.disabled)
		checks ~= new ConstructorCheck(fileName, moduleScope,
		analysisConfig.constructor_check == Check.skipTests && !ut);

	if (analysisConfig.could_be_immutable_check != Check.disabled)
		checks ~= new UnmodifiedFinder(fileName, moduleScope,
		analysisConfig.could_be_immutable_check == Check.skipTests && !ut);

	if (analysisConfig.delete_check != Check.disabled)
		checks ~= new DeleteCheck(fileName, moduleScope,
		analysisConfig.delete_check == Check.skipTests && !ut);

	if (analysisConfig.duplicate_attribute != Check.disabled)
		checks ~= new DuplicateAttributeCheck(fileName, moduleScope,
		analysisConfig.duplicate_attribute == Check.skipTests && !ut);

	if (analysisConfig.enum_array_literal_check != Check.disabled)
		checks ~= new EnumArrayLiteralCheck(fileName, moduleScope,
		analysisConfig.enum_array_literal_check == Check.skipTests && !ut);

	if (analysisConfig.exception_check != Check.disabled)
		checks ~= new PokemonExceptionCheck(fileName, moduleScope,
		analysisConfig.exception_check == Check.skipTests && !ut);

	if (analysisConfig.float_operator_check != Check.disabled)
		checks ~= new FloatOperatorCheck(fileName, moduleScope,
		analysisConfig.float_operator_check == Check.skipTests && !ut);

	if (analysisConfig.function_attribute_check != Check.disabled)
		checks ~= new FunctionAttributeCheck(fileName, moduleScope,
		analysisConfig.function_attribute_check == Check.skipTests && !ut);

	if (analysisConfig.if_else_same_check != Check.disabled)
		checks ~= new IfElseSameCheck(fileName, moduleScope,
		analysisConfig.if_else_same_check == Check.skipTests&& !ut);

	if (analysisConfig.label_var_same_name_check != Check.disabled)
		checks ~= new LabelVarNameCheck(fileName, moduleScope,
		analysisConfig.label_var_same_name_check == Check.skipTests && !ut);

	if (analysisConfig.length_subtraction_check != Check.disabled)
		checks ~= new LengthSubtractionCheck(fileName, moduleScope,
		analysisConfig.length_subtraction_check == Check.skipTests && !ut);

	if (analysisConfig.local_import_check != Check.disabled)
		checks ~= new LocalImportCheck(fileName, moduleScope,
		analysisConfig.local_import_check == Check.skipTests && !ut);

	if (analysisConfig.logical_precedence_check != Check.disabled)
		checks ~= new LogicPrecedenceCheck(fileName, moduleScope,
		analysisConfig.logical_precedence_check == Check.skipTests && !ut);

	if (analysisConfig.mismatched_args_check != Check.disabled)
		checks ~= new MismatchedArgumentCheck(fileName, moduleScope,
		analysisConfig.mismatched_args_check == Check.skipTests && !ut);

	if (analysisConfig.number_style_check != Check.disabled)
		checks ~= new NumberStyleCheck(fileName, moduleScope,
		analysisConfig.number_style_check == Check.skipTests && !ut);

	if (analysisConfig.object_const_check != Check.disabled)
		checks ~= new ObjectConstCheck(fileName, moduleScope,
		analysisConfig.object_const_check == Check.skipTests && !ut);

	if (analysisConfig.opequals_tohash_check != Check.disabled)
		checks ~= new OpEqualsWithoutToHashCheck(fileName, moduleScope,
		analysisConfig.opequals_tohash_check == Check.skipTests && !ut);

	if (analysisConfig.redundant_parens_check != Check.disabled)
		checks ~= new RedundantParenCheck(fileName, moduleScope,
		analysisConfig.redundant_parens_check == Check.skipTests && !ut);

	if (analysisConfig.style_check != Check.disabled)
		checks ~= new StyleChecker(fileName, moduleScope,
		analysisConfig.style_check == Check.skipTests && !ut);

	if (analysisConfig.undocumented_declaration_check != Check.disabled)
		checks ~= new UndocumentedDeclarationCheck(fileName, moduleScope,
		analysisConfig.undocumented_declaration_check == Check.skipTests && !ut);

	if (analysisConfig.unused_label_check != Check.disabled)
		checks ~= new UnusedLabelCheck(fileName, moduleScope,
		analysisConfig.unused_label_check == Check.skipTests && !ut);

	if (analysisConfig.unused_variable_check != Check.disabled)
		checks ~= new UnusedVariableCheck(fileName, moduleScope,
		analysisConfig.unused_variable_check == Check.skipTests && !ut);

	if (analysisConfig.long_line_check != Check.disabled)
		checks ~= new LineLengthCheck(fileName, tokens,
		analysisConfig.long_line_check == Check.skipTests && !ut);

	if (analysisConfig.auto_ref_assignment_check != Check.disabled)
		checks ~= new AutoRefAssignmentCheck(fileName,
		analysisConfig.auto_ref_assignment_check == Check.skipTests && !ut);

	if (analysisConfig.incorrect_infinite_range_check != Check.disabled)
		checks ~= new IncorrectInfiniteRangeCheck(fileName,
		analysisConfig.incorrect_infinite_range_check == Check.skipTests && !ut);

	if (analysisConfig.useless_assert_check != Check.disabled)
		checks ~= new UselessAssertCheck(fileName,
		analysisConfig.useless_assert_check == Check.skipTests && !ut);

	if (analysisConfig.alias_syntax_check != Check.disabled)
		checks ~= new AliasSyntaxCheck(fileName,
		analysisConfig.alias_syntax_check == Check.skipTests && !ut);

	if (analysisConfig.static_if_else_check != Check.disabled)
		checks ~= new StaticIfElse(fileName,
		analysisConfig.static_if_else_check == Check.skipTests && !ut);

	if (analysisConfig.lambda_return_check != Check.disabled)
		checks ~= new LambdaReturnCheck(fileName,
		analysisConfig.lambda_return_check == Check.skipTests && !ut);

	if (analysisConfig.auto_function_check != Check.disabled)
		checks ~= new AutoFunctionChecker(fileName,
		analysisConfig.auto_function_check == Check.skipTests && !ut);

	version (none)
		if (analysisConfig.redundant_if_check != Check.disabled)
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
	return set;
}

