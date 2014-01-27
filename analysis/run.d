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

		Module m = parseModule(app.data, fileName, &messageFunction);

		if (!staticAnalyze)
			return;

		auto style = new StyleChecker(fileName);
		style.visit(m);

		auto enums = new EnumArrayLiteralCheck(fileName);
		enums.visit(m);

		auto pokemon = new PokemonExceptionCheck(fileName);
		pokemon.visit(m);

		auto del = new DeleteCheck(fileName);
		del.visit(m);

		auto fish = new FloatOperatorCheck(fileName);
		fish.visit(m);

		auto numbers = new NumberStyleCheck(fileName);
		numbers.visit(m);

		foreach (message; sort(chain(enums.messages, style.messages,
			pokemon.messages, del.messages, fish.messages, numbers.messages
			).array))
		{
			writeln(message);
		}
	}
}

