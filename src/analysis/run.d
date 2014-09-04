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
import std.d.lexer;
import std.d.parser;
import std.d.ast;

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

bool first = true;

void messageFunction(string fileName, size_t line, size_t column, string message,
	bool isError)
{
	writefln("%s(%d:%d)[%s]: %s", fileName, line, column,
		isError ? "error" : "warn", message);
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
	write(  "    }");
}

void syntaxCheck(string[] fileNames)
{
	StaticAnalysisConfig config = defaultStaticAnalysisConfig();
	analyze(fileNames, config, false);
}

void generateReport(string[] fileNames, const StaticAnalysisConfig config)
{
	writeln("{");
	writeln(`  "issues": [`);
	first = true;
	StatsCollector stats = new StatsCollector("");
	ulong lineOfCodeCount;
	foreach (fileName; fileNames)
	{
		File f = File(fileName);
		if (f.size == 0) continue;
		auto code = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(code);
		ParseAllocator p = new ParseAllocator;
		StringCache cache = StringCache(StringCache.defaultBucketCount);
		const Module m = parseModule(fileName, code, p, cache, true, &lineOfCodeCount);
		stats.visit(m);
		MessageSet results = analyze(fileName, m, config, true);
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

// For multiple files
void analyze(string[] fileNames, const StaticAnalysisConfig config, bool staticAnalyze = true)
{
	foreach (fileName; fileNames)
	{
		File f = File(fileName);
		if (f.size == 0) continue;
		auto code = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(code);
		ParseAllocator p = new ParseAllocator;
		StringCache cache = StringCache(StringCache.defaultBucketCount);
		const Module m = parseModule(fileName, code, p, cache, false);
		MessageSet results = analyze(fileName, m, config, staticAnalyze);
		if (results is null)
			continue;
		foreach (result; results[])
			writefln("%s(%d:%d)[warn]: %s", result.fileName, result.line,
				result.column, result.message);
	}
}

const(Module) parseModule(string fileName, ubyte[] code, ParseAllocator p,
	ref StringCache cache, bool report, ulong* linesOfCode = null)
{
	import stats : isLineOfCode;
	auto lexer = byToken(code);
	LexerConfig config;
	config.fileName = fileName;
	config.stringBehavior = StringBehavior.source;
	const(Token)[] tokens = getTokensForParser(code, config, &cache);
	if (linesOfCode !is null)
		(*linesOfCode) += count!(a => isLineOfCode(a.type))(tokens);
	foreach (message; lexer.messages)
	{
		if (report)
			messageFunctionJSON(fileName, message.line, message.column, message.message,
				message.isError);
		else
			messageFunction(fileName, message.line, message.column, message.message,
				message.isError);
	}
	return std.d.parser.parseModule(tokens, fileName, p,
		report ? &messageFunctionJSON : &messageFunction);
}

MessageSet analyze(string fileName, const Module m,
	const StaticAnalysisConfig analysisConfig, bool staticAnalyze = true)
{
	import std.parallelism;

	if (!staticAnalyze)
		return null;

	BaseAnalyzer[] checks;

	if (analysisConfig.style_check) checks ~= new StyleChecker(fileName);
	if (analysisConfig.enum_array_literal_check) checks ~= new EnumArrayLiteralCheck(fileName);
	if (analysisConfig.exception_check) checks ~= new PokemonExceptionCheck(fileName);
	if (analysisConfig.delete_check) checks ~= new DeleteCheck(fileName);
	if (analysisConfig.float_operator_check) checks ~= new FloatOperatorCheck(fileName);
	if (analysisConfig.number_style_check) checks ~= new NumberStyleCheck(fileName);
	if (analysisConfig.object_const_check) checks ~= new ObjectConstCheck(fileName);
	if (analysisConfig.backwards_range_check) checks ~= new BackwardsRangeCheck(fileName);
	if (analysisConfig.if_else_same_check) checks ~= new IfElseSameCheck(fileName);
	if (analysisConfig.constructor_check) checks ~= new ConstructorCheck(fileName);
	if (analysisConfig.unused_variable_check) checks ~= new UnusedVariableCheck(fileName);
	if (analysisConfig.duplicate_attribute) checks ~= new DuplicateAttributeCheck(fileName);
	if (analysisConfig.opequals_tohash_check) checks ~= new OpEqualsWithoutToHashCheck(fileName);
	if (analysisConfig.length_subtraction_check) checks ~= new LengthSubtractionCheck(fileName);
	if (analysisConfig.builtin_property_names_check) checks ~= new BuiltinPropertyNameCheck(fileName);
	if (analysisConfig.asm_style_check) checks ~= new AsmStyleCheck(fileName);
	if (analysisConfig.logical_precedence_check) checks ~= new LogicPrecedenceCheck(fileName);
	if (analysisConfig.undocumented_declaration_check) checks ~= new UndocumentedDeclarationCheck(fileName);
	if (analysisConfig.function_attribute_check) checks ~= new FunctionAttributeCheck(fileName);
	if (analysisConfig.comma_expression_check) checks ~= new CommaExpressionCheck(fileName);

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

