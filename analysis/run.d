module analysis.run;

import std.stdio;
import std.array;
import std.conv;
import std.algorithm;
import std.range;
import std.array;

import stdx.d.lexer;
import stdx.d.parser;
import stdx.d.ast;

import analysis.base;
import analysis.style;
import analysis.enumarrayliteral;
import analysis.pokemon;
import analysis.del;
import analysis.fish;
import analysis.numbers;
import analysis.objectconst;
import analysis.range;
import analysis.constructors;
import analysis.ifelsesame;

void messageFunction(string fileName, size_t line, size_t column, string message,
	bool isError)
{
	writefln("%s(%d:%d)[%s]: %s", fileName, line, column,
		isError ? "error" : "warn", message);
}

void syntaxCheck(File output, string[] fileNames)
{
	analyze(output, fileNames, false);
}

void analyze(File output, string[] fileNames, bool staticAnalyze = true)
{
	import std.parallelism;
	foreach (fileName; fileNames)
	{
		File f = File(fileName);
		auto bytes = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(bytes);
		auto lexer = byToken(bytes);
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
			return;

		BaseAnalyzer[] checks;
		checks ~= new StyleChecker(fileName);
		checks ~= new EnumArrayLiteralCheck(fileName);
		checks ~= new PokemonExceptionCheck(fileName);
		checks ~= new DeleteCheck(fileName);
		checks ~= new FloatOperatorCheck(fileName);
		checks ~= new NumberStyleCheck(fileName);
		checks ~= new ObjectConstCheck(fileName);
		checks ~= new BackwardsRangeCheck(fileName);
		checks ~= new IfElseSameCheck(fileName);
		checks ~= new ConstructorCheck(fileName);

		foreach (check; checks)
		{
			check.visit(m);
		}

		MessageSet set = new MessageSet;
		foreach(check; checks)
			foreach (message; check.messages)
				set.insert(message);
		foreach (message; set[])
			writefln("%s(%d:%d)[warn]: %s", message.fileName, message.line,
				message.column, message.message);
		p.deallocateAll();
	}
}

