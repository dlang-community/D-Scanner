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

void messageFunction(string fileName, size_t line, size_t column, string message,
	bool isError)
{
	writefln("%s(%d:%d)[%s]: %s", fileName, line, column,
		isError ? "error" : "warn", message);
}

void syntaxCheck(File output, string[] fileNames)
{
	StaticAnalysisConfig config = defaultStaticAnalysisConfig();
	analyze(output, fileNames, config, false);
}

// For multiple files
void analyze(File output, string[] fileNames, StaticAnalysisConfig config, bool staticAnalyze = true)
{
	foreach (fileName; fileNames)
	{
		File f = File(fileName);
		if (f.size == 0) continue;
		auto code = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(code);

		string[] results = analyze(fileName, code, config, staticAnalyze);
		if (results.length > 0)
			output.writeln(results.join("\n"));
	}
}

// For a string
string[] analyze(string fileName, ubyte[] code, StaticAnalysisConfig analysisConfig, bool staticAnalyze = true)
{
	import std.parallelism;

	auto lexer = byToken(code);
	LexerConfig config;
	config.fileName = fileName;
	config.stringBehavior = StringBehavior.source;
	StringCache cache = StringCache(StringCache.defaultBucketCount);
	const(Token)[] tokens = getTokensForParser(code, config, &cache);

	foreach (message; lexer.messages)
	{
		messageFunction(fileName, message.line, message.column, message.message,
			message.isError);
	}

	ParseAllocator p = new ParseAllocator;
	Module m = parseModule(tokens, fileName, p, &messageFunction);

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

	foreach (check; checks)
	{
		check.visit(m);
	}

	MessageSet set = new MessageSet;
	foreach (check; checks)
		foreach (message; check.messages)
			set.insert(message);

	string[] results;
	foreach (message; set[])
		results ~= "%s(%d:%d)[warn]: %s".format(message.fileName, message.line,
			message.column, message.message);
	p.deallocateAll();
	return results;
}

