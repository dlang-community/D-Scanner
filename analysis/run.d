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

enum AnalyzerCheck : int
{
	style_check = 0x1,
	enum_array_literal_check = 0x2,
	exception_check = 0x4,
	delete_check = 0x8,
	float_operator_check = 0x10,
	number_style_check = 0x20,
	object_const_check = 0x40,
	backwards_range_check = 0x80,
	if_else_same_check = 0x100,
	constructor_check = 0x200,
	unused_variable_check = 0x400,
	all = 0x7FF
}

void messageFunction(string fileName, size_t line, size_t column, string message,
	bool isError)
{
	writefln("%s(%d:%d)[%s]: %s", fileName, line, column,
		isError ? "error" : "warn", message);
}

void syntaxCheck(File output, string[] fileNames)
{
	analyze(output, fileNames, AnalyzerCheck.all, false);
}

// For multiple files
void analyze(File output, string[] fileNames, AnalyzerCheck analyzers, bool staticAnalyze = true)
{
	foreach (fileName; fileNames)
	{
		File f = File(fileName);
		auto code = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(code);

		string[] results = analyze(fileName, code, analyzers, staticAnalyze);
		if (results.length > 0)
			output.writeln(results.join("\n"));
	}
}

// For a string
string[] analyze(string fileName, ubyte[] code, AnalyzerCheck analyzers, bool staticAnalyze = true)
{
	import std.parallelism;

	auto lexer = byToken(code);
	auto app = appender!(typeof(lexer.front)[])();
	while (!lexer.empty)
	{
		app.put(lexer.front);
		lexer.popFront();
	}

	foreach (message; lexer.messages)
	{
		messageFunction(fileName, message.line, message.column, message.message,
			message.isError);
	}

	ParseAllocator p = new ParseAllocator;
	Module m = parseModule(app.data, fileName, p, &messageFunction);

	if (!staticAnalyze)
		return null;

	BaseAnalyzer[] checks;

	if (analyzers & AnalyzerCheck.style_check) checks ~= new StyleChecker(fileName);
	if (analyzers & AnalyzerCheck.enum_array_literal_check) checks ~= new EnumArrayLiteralCheck(fileName);
	if (analyzers & AnalyzerCheck.exception_check) checks ~= new PokemonExceptionCheck(fileName);
	if (analyzers & AnalyzerCheck.delete_check) checks ~= new DeleteCheck(fileName);
	if (analyzers & AnalyzerCheck.float_operator_check) checks ~= new FloatOperatorCheck(fileName);
	if (analyzers & AnalyzerCheck.number_style_check) checks ~= new NumberStyleCheck(fileName);
	if (analyzers & AnalyzerCheck.object_const_check) checks ~= new ObjectConstCheck(fileName);
	if (analyzers & AnalyzerCheck.backwards_range_check) checks ~= new BackwardsRangeCheck(fileName);
	if (analyzers & AnalyzerCheck.if_else_same_check) checks ~= new IfElseSameCheck(fileName);
	if (analyzers & AnalyzerCheck.constructor_check) checks ~= new ConstructorCheck(fileName);
	if (analyzers & AnalyzerCheck.unused_variable_check) checks ~= new UnusedVariableCheck(fileName);

	foreach (check; checks)
	{
		check.visit(m);
	}

	MessageSet set = new MessageSet;
	foreach(check; checks)
		foreach (message; check.messages)
			set.insert(message);

	string[] results;
	foreach (message; set[])
		results ~= "%s(%d:%d)[warn]: %s".format(message.fileName, message.line,
			message.column, message.message);
	p.deallocateAll();
	return results;
}

