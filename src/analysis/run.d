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

import dsymbol.string_interning : internString;
import dsymbol.scope_;
import dsymbol.semantic;
import dsymbol.conversion;
import dsymbol.conversion.first;
import dsymbol.conversion.second;
import dsymbol.modulecache : ModuleCache;

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
	writeln(`      "fileName": "`, fileName, `",`);
	writeln(`      "line": `, line, `,`);
	writeln(`      "column": `, column, `,`);
	writeln(`      "message": "`, message.replace(`"`, `\"`), `"`);
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
		File f = File(fileName);
		if (f.size == 0)
			continue;
		auto code = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(code);
		ParseAllocator p = new ParseAllocator;
		const(Token)[] tokens;
		const Module m = parseModule(fileName, code, p, cache, true, tokens, &lineOfCodeCount);
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
		File f = File(fileName);
		if (f.size == 0)
			continue;
		auto code = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(code);
		ParseAllocator p = new ParseAllocator;
		uint errorCount = 0;
		uint warningCount = 0;
		const(Token)[] tokens;
		const Module m = parseModule(fileName, code, p, cache, false, tokens,
				null, &errorCount, &warningCount);
		assert(m);
		if (errorCount > 0 || (staticAnalyze && warningCount > 0))
			hasErrors = true;
		MessageSet results = analyze(fileName, m, config, moduleCache, tokens, staticAnalyze);
		if (results is null)
			continue;
		foreach (result; results[])
			writefln("%s(%d:%d)[warn]: %s", result.fileName, result.line,
					result.column, result.message);
	}
	return hasErrors;
}

const(Module) parseModule(string fileName, ubyte[] code, ParseAllocator p,
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
	if (!staticAnalyze)
		return null;

	auto symbolAllocator = new ASTAllocator;
	auto first = scoped!FirstPass(m, internString(fileName), symbolAllocator,
			symbolAllocator, true, &moduleCache, null);
	first.run();

	secondPass(first.rootSymbol, first.moduleScope, moduleCache);
	typeid(SemanticSymbol).destroy(first.rootSymbol);
	const(Scope)* moduleScope = first.moduleScope;

	BaseAnalyzer[] checks;

	if (analysisConfig.asm_style_check)
		checks ~= new AsmStyleCheck(fileName, moduleScope);
	if (analysisConfig.backwards_range_check)
		checks ~= new BackwardsRangeCheck(fileName, moduleScope);
	if (analysisConfig.builtin_property_names_check)
		checks ~= new BuiltinPropertyNameCheck(fileName, moduleScope);
	if (analysisConfig.comma_expression_check)
		checks ~= new CommaExpressionCheck(fileName, moduleScope);
	if (analysisConfig.constructor_check)
		checks ~= new ConstructorCheck(fileName, moduleScope);
	if (analysisConfig.could_be_immutable_check)
		checks ~= new UnmodifiedFinder(fileName, moduleScope);
	if (analysisConfig.delete_check)
		checks ~= new DeleteCheck(fileName, moduleScope);
	if (analysisConfig.duplicate_attribute)
		checks ~= new DuplicateAttributeCheck(fileName, moduleScope);
	if (analysisConfig.enum_array_literal_check)
		checks ~= new EnumArrayLiteralCheck(fileName, moduleScope);
	if (analysisConfig.exception_check)
		checks ~= new PokemonExceptionCheck(fileName, moduleScope);
	if (analysisConfig.float_operator_check)
		checks ~= new FloatOperatorCheck(fileName, moduleScope);
	if (analysisConfig.function_attribute_check)
		checks ~= new FunctionAttributeCheck(fileName, moduleScope);
	if (analysisConfig.if_else_same_check)
		checks ~= new IfElseSameCheck(fileName, moduleScope);
	if (analysisConfig.label_var_same_name_check)
		checks ~= new LabelVarNameCheck(fileName, moduleScope);
	if (analysisConfig.length_subtraction_check)
		checks ~= new LengthSubtractionCheck(fileName, moduleScope);
	if (analysisConfig.local_import_check)
		checks ~= new LocalImportCheck(fileName, moduleScope);
	if (analysisConfig.logical_precedence_check)
		checks ~= new LogicPrecedenceCheck(fileName, moduleScope);
	if (analysisConfig.mismatched_args_check)
		checks ~= new MismatchedArgumentCheck(fileName, moduleScope);
	if (analysisConfig.number_style_check)
		checks ~= new NumberStyleCheck(fileName, moduleScope);
	if (analysisConfig.object_const_check)
		checks ~= new ObjectConstCheck(fileName, moduleScope);
	if (analysisConfig.opequals_tohash_check)
		checks ~= new OpEqualsWithoutToHashCheck(fileName, moduleScope);
	if (analysisConfig.redundant_parens_check)
		checks ~= new RedundantParenCheck(fileName, moduleScope);
	if (analysisConfig.style_check)
		checks ~= new StyleChecker(fileName, moduleScope);
	if (analysisConfig.undocumented_declaration_check)
		checks ~= new UndocumentedDeclarationCheck(fileName, moduleScope);
	if (analysisConfig.unused_label_check)
		checks ~= new UnusedLabelCheck(fileName, moduleScope);
	if (analysisConfig.unused_variable_check)
		checks ~= new UnusedVariableCheck(fileName, moduleScope);
	if (analysisConfig.long_line_check)
		checks ~= new LineLengthCheck(fileName, tokens);
	if (analysisConfig.auto_ref_assignment_check)
		checks ~= new AutoRefAssignmentCheck(fileName);
	if (analysisConfig.incorrect_infinite_range_check)
		checks ~= new IncorrectInfiniteRangeCheck(fileName);
	if (analysisConfig.useless_assert_check)
		checks ~= new UselessAssertCheck(fileName);
	version (none)
		if (analysisConfig.redundant_if_check)
			checks ~= new IfStatementCheck(fileName, moduleScope);

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
