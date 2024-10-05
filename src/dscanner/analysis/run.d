//          Copyright Brian Schott (Hackerpilot) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.analysis.run;

import core.memory : GC;

import dparse.ast;
import dparse.lexer;
import dparse.parser;
import dparse.rollback_allocator;

import std.algorithm;
import std.array;
import std.conv;
import std.file : mkdirRecurse;
import std.functional : toDelegate;
import std.path : dirName;
import std.range;
import std.stdio;
import std.typecons : scoped;

import std.experimental.allocator : CAllocatorImpl;
import std.experimental.allocator.mallocator : Mallocator;
import std.experimental.allocator.building_blocks.region : Region;
import std.experimental.allocator.building_blocks.allocator_list : AllocatorList;

import dscanner.analysis.autofix : improveAutoFixWhitespace;
import dscanner.analysis.config;
import dscanner.analysis.base;
import dscanner.analysis.style;
import dscanner.analysis.enumarrayliteral;
import dscanner.analysis.pokemon;
import dscanner.analysis.del;
import dscanner.analysis.numbers;
import dscanner.analysis.objectconst;
import dscanner.analysis.range;
import dscanner.analysis.ifelsesame;
import dscanner.analysis.constructors;
import dscanner.analysis.unused_variable;
import dscanner.analysis.unused_label;
import dscanner.analysis.unused_parameter;
import dscanner.analysis.opequals_without_tohash;
import dscanner.analysis.length_subtraction;
import dscanner.analysis.builtin_property_names;
import dscanner.analysis.asm_style;
import dscanner.analysis.logic_precedence;
import dscanner.analysis.stats_collector;
import dscanner.analysis.undocumented;
import dscanner.analysis.function_attributes;
import dscanner.analysis.local_imports;
import dscanner.analysis.unmodified;
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
import dscanner.analysis.always_curly;
import dscanner.analysis.redundant_attributes;
import dscanner.analysis.has_public_example;
import dscanner.analysis.assert_without_msg;
import dscanner.analysis.if_constraints_indent;
import dscanner.analysis.trust_too_much;
import dscanner.analysis.redundant_storage_class;
import dscanner.analysis.unused_result;
import dscanner.analysis.cyclomatic_complexity;
import dscanner.analysis.body_on_disabled_funcs;

import dsymbol.string_interning : internString;
import dsymbol.scope_;
import dsymbol.semantic;
import dsymbol.conversion;
import dsymbol.conversion.first;
import dsymbol.conversion.second;
import dsymbol.modulecache : ModuleCache;

import dscanner.utils;
import dscanner.reports : DScannerJsonReporter, SonarQubeGenericIssueDataReporter;

import dmd.astbase : ASTBase;
import dmd.parse : Parser;

import dmd.frontend;
import dmd.astcodegen;

bool first = true;

version (unittest)
	enum ut = true;
else
	enum ut = false;

void doNothing(string, size_t, size_t, string, bool)
{
}

private alias ASTAllocator = CAllocatorImpl!(
		AllocatorList!(n => Region!Mallocator(1024 * 128), Mallocator));

immutable string defaultErrorFormat = "{filepath}({line}:{column})[{type}]: {message}";

string[string] errorFormatMap()
{
	static string[string] ret;
	if (ret is null)
		ret = [
			"github": "::{type2} file={filepath},line={line},endLine={endLine},col={column},endColumn={endColumn},title={Type2} ({name})::{message}",
			"pretty": "\x1B[1m{filepath}({line}:{column}): {Type2}: \x1B[0m{message} \x1B[2m({name})\x1B[0m{context}{supplemental}",
			"digitalmars": "{filepath}({line},{column}): {Type2}: {message}",
		];
	return ret;
}

private string formatBase(string format, Message.Diagnostic diagnostic, scope const(ubyte)[] code, bool color)
{
	auto s = format;
	s = s.replace("{filepath}", diagnostic.fileName);
	s = s.replace("{line}", to!string(diagnostic.startLine));
	s = s.replace("{column}", to!string(diagnostic.startColumn));
	s = s.replace("{startIndex}", to!string(diagnostic.startIndex));
	s = s.replace("{endLine}", to!string(diagnostic.endLine));
	s = s.replace("{endColumn}", to!string(diagnostic.endColumn));
	s = s.replace("{endIndex}", to!string(diagnostic.endIndex));
	s = s.replace("{message}", diagnostic.message);
	s = s.replace("{context}", diagnostic.formatContext(cast(const(char)[]) code, color));
	return s;
}

private string formatContext(Message.Diagnostic diagnostic, scope const(char)[] code, bool color)
{
	import std.string : indexOf, lastIndexOf;

	if (diagnostic.startIndex >= diagnostic.endIndex || diagnostic.endIndex > code.length
		|| diagnostic.startColumn >= diagnostic.endColumn || diagnostic.endColumn == 0
		|| diagnostic.startColumn == 0)
		return null;

	auto lineStart = code.lastIndexOf('\n', diagnostic.startIndex) + 1;
	auto lineEnd = code.indexOf('\n', diagnostic.endIndex);
	if (lineEnd == -1)
		lineEnd = code.length;

	auto ret = appender!string;
	ret.reserve((lineEnd - lineStart) + diagnostic.endColumn + (color ? 30 : 10));
	ret ~= '\n';
	if (color)
		ret ~= "\x1B[m"; // reset
	ret ~= code[lineStart .. lineEnd].replace('\t', ' ');
	ret ~= '\n';
	if (color)
		ret ~= "\x1B[0;33m"; // reset, yellow
	foreach (_; 0 .. diagnostic.startColumn - 1)
		ret ~= ' ';
	foreach (_; 0 .. diagnostic.endColumn - diagnostic.startColumn)
		ret ~= '^';
	if (color)
		ret ~= "\x1B[m"; // reset
	return ret.data;
}

version (Windows)
void enableColoredOutput()
{
	import core.sys.windows.windows : DWORD, ENABLE_VIRTUAL_TERMINAL_PROCESSING,
		GetConsoleMode, GetStdHandle, HANDLE, INVALID_HANDLE_VALUE,
		SetConsoleMode, STD_OUTPUT_HANDLE;

	static bool enabledColor = false;
	if (enabledColor)
		return;
	enabledColor = true;

	// Set output mode to handle virtual terminal sequences
	HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
	if (hOut == INVALID_HANDLE_VALUE)
		return;

	DWORD dwMode;
	if (!GetConsoleMode(hOut, &dwMode))
		return;

	dwMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
	if (!SetConsoleMode(hOut, dwMode))
		return;
}

void messageFunctionFormat(string format, Message message, bool isError, scope const(ubyte)[] code = null)
{
	bool color = format.canFind("\x1B[");
	if (color)
	{
		version (Windows)
			enableColoredOutput();
	}

	auto s = format.formatBase(message.diagnostic, code, color);

	string formatType(string s, string type, string colorCode)
	{
		import std.ascii : toUpper;
		import std.string : representation;

		string upperFirst(string s) { return s[0].toUpper ~ s[1 .. $]; }
		string upper(string s) { return s.representation.map!(a => toUpper(cast(char) a)).array; }

		string type2 = type;
		if (type2 == "warn")
			type2 = "warning";

		s = s.replace("{type}", color ? (colorCode ~ type ~ "\x1B[m") : type);
		s = s.replace("{Type}", color ? (colorCode ~ upperFirst(type) ~ "\x1B[m") : upperFirst(type));
		s = s.replace("{TYPE}", color ? (colorCode ~ upper(type) ~ "\x1B[m") : upper(type));
		s = s.replace("{type2}", color ? (colorCode ~ type2 ~ "\x1B[m") : type2);
		s = s.replace("{Type2}", color ? (colorCode ~ upperFirst(type2) ~ "\x1B[m") : upperFirst(type2));
		s = s.replace("{TYPE2}", color ? (colorCode ~ upper(type2) ~ "\x1B[m") : upper(type2));

		return s;
	}

	s = formatType(s, isError ? "error" : "warn", isError ? "\x1B[31m" : "\x1B[33m");
	s = s.replace("{name}", message.checkName);
	s = s.replace("{supplemental}", message.supplemental.map!(a => "\n\t"
		~ formatType(format.formatBase(a, code, color), "hint", "\x1B[35m")
			.replace("{name}", "").replace("{supplemental}", "")
			.replace("\n", "\n\t"))
		.join());

	writefln("%s", s);
}

void messageFunction(Message message, bool isError)
{
	messageFunctionFormat(defaultErrorFormat, message, isError);
}

void messageFunctionJSON(string fileName, size_t line, size_t column, string message, bool)
{
	writeJSON(Message(Message.Diagnostic.from(fileName, [0, 0], line, [column, column], message), "dscanner.syntax"));
}

void writeJSON(Message message)
{
	if (!first)
		writeln(",");
	else
		first = false;
	writeln("    {");
	writeln(`      "key": "`, message.key, `",`);
	if (message.checkName !is null)
	{
		writeln(`      "name": "`, message.checkName, `",`);
	}
	writeln(`      "fileName": "`, message.fileName.replace("\\", "\\\\").replace(`"`, `\"`), `",`);
	writeln(`      "line": `, message.startLine, `,`);
	writeln(`      "column": `, message.startColumn, `,`);
	writeln(`      "index": `, message.startIndex, `,`);
	writeln(`      "endLine": `, message.endLine, `,`);
	writeln(`      "endColumn": `, message.endColumn, `,`);
	writeln(`      "endIndex": `, message.endIndex, `,`);
	writeln(`      "message": "`, message.message.replace("\\", "\\\\").replace(`"`, `\"`), `",`);
	if (message.supplemental.length)
	{
		writeln(`      "supplemental": [`);
		foreach (i, suppl; message.supplemental)
		{
			if (i != 0)
				writeln(",");
			writeln(`        {`);
			if (message.fileName != suppl.fileName)
				writeln(`          "fileName": `, suppl.fileName, `,`);
			if (suppl.message.length)
				writeln(`          "message": `, suppl.message, `,`);
			writeln(`          "line": `, suppl.startLine, `,`);
			writeln(`          "column": `, suppl.startColumn, `,`);
			writeln(`          "endLine": `, suppl.endLine, `,`);
			writeln(`          "endColumn": `, suppl.endColumn);
			write(`        }`);
		}
		if (message.supplemental.length)
			writeln();
		writeln(`      ]`);
	}
	else
		writeln(`      "supplemental": []`);
	write("    }");
}

bool syntaxCheck(string[] fileNames, string errorFormat, ref StringCache stringCache, ref ModuleCache moduleCache)
{
	StaticAnalysisConfig config = defaultStaticAnalysisConfig();
	return analyze(fileNames, config, errorFormat, stringCache, moduleCache, false);
}

void generateReport(string[] fileNames, const StaticAnalysisConfig config,
		ref StringCache cache, ref ModuleCache moduleCache, string reportFile = "")
{
	auto reporter = new DScannerJsonReporter();

	auto writeMessages = delegate void(string fileName, size_t line, size_t column, string message, bool isError){
		// TODO: proper index and column ranges
		reporter.addMessage(
			Message(Message.Diagnostic.from(fileName, [0, 0], line, [column, column], message), "dscanner.syntax"),
			isError);
	};

	first = true;
	StatsCollector stats = new StatsCollector(BaseAnalyzerArguments.init);
	ulong lineOfCodeCount;
	foreach (fileName; fileNames)
	{
		auto code = readFile(fileName);
		// Skip files that could not be read and continue with the rest
		if (code.length == 0)
			continue;
		RollbackAllocator r;
		const(Token)[] tokens;
		const Module m = parseModule(fileName, code, &r, cache, tokens, writeMessages, &lineOfCodeCount, null, null);
		stats.visit(m);
		MessageSet messageSet = analyze(fileName, m, config, moduleCache, tokens, true);
		reporter.addMessageSet(messageSet);
	}

	string reportFileContent = reporter.getContent(stats, lineOfCodeCount);
	if (reportFile == "")
	{
		writeln(reportFileContent);
	}
	else
	{
		mkdirRecurse(reportFile.dirName);
		toFile(reportFileContent, reportFile);
	}
}

void generateSonarQubeGenericIssueDataReport(string[] fileNames, const StaticAnalysisConfig config,
		ref StringCache cache, ref ModuleCache moduleCache, string reportFile = "")
{
	auto reporter = new SonarQubeGenericIssueDataReporter();

	auto writeMessages = delegate void(string fileName, size_t line, size_t column, string message, bool isError){
		// TODO: proper index and column ranges
		reporter.addMessage(
			Message(Message.Diagnostic.from(fileName, [0, 0], line, [column, column], message), "dscanner.syntax"),
			isError);
	};

	foreach (fileName; fileNames)
	{
		auto code = readFile(fileName);
		// Skip files that could not be read and continue with the rest
		if (code.length == 0)
			continue;
		RollbackAllocator r;
		const(Token)[] tokens;
		const Module m = parseModule(fileName, code, &r, cache, tokens, writeMessages, null, null, null);
		MessageSet messageSet = analyze(fileName, m, config, moduleCache, tokens, true);
		reporter.addMessageSet(messageSet);
	}

	string reportFileContent = reporter.getContent();
	if (reportFile == "")
	{
		writeln(reportFileContent);
	}
	else
	{
		mkdirRecurse(reportFile.dirName);
		toFile(reportFileContent, reportFile);
	}
}

/**
 * For multiple files
 *
 * Returns: true if there were errors or if there were warnings and `staticAnalyze` was true.
 */
bool analyze(string[] fileNames, const StaticAnalysisConfig config, string errorFormat,
		ref StringCache cache, ref ModuleCache moduleCache, bool staticAnalyze = true)
{
	import std.string : toStringz;
	import dscanner.analysis.rundmd : parseDmdModule;

	import dscanner.analysis.rundmd : analyzeDmd;

	bool hasErrors;
	foreach (fileName; fileNames)
	{
		auto code = readFile(fileName);
		// Skip files that could not be read and continue with the rest
		if (code.length == 0)
			continue;

		auto dmdModule = parseDmdModule(fileName, cast(string) code);

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
		MessageSet resultsDmd = analyzeDmd(fileName, dmdModule, getModuleName(dmdModule.md), config);
		foreach (result; resultsDmd[])
		{
			results.insert(result);
		}
		if (results is null)
			continue;
		foreach (result; results[])
		{
			hasErrors = true;
			messageFunctionFormat(errorFormat, result, false, code);
		}
	}
	return hasErrors;
}

/**
 * Interactive automatic issue fixing for multiple files
 *
 * Returns: true if there were parse errors.
 */
bool autofix(string[] fileNames, const StaticAnalysisConfig config, string errorFormat,
		ref StringCache cache, ref ModuleCache moduleCache, bool autoApplySingle,
		const AutoFixFormatting overrideFormattingConfig = AutoFixFormatting.invalid)
{
	import std.format : format;

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
		if (errorCount > 0)
			hasErrors = true;
		MessageSet results = analyze(fileName, m, config, moduleCache, tokens, true, true, overrideFormattingConfig);
		if (results is null)
			continue;

		AutoFix.CodeReplacement[] changes;
		size_t index;
		auto numAutofixes = results[].filter!(a => a.autofixes.length > (autoApplySingle ? 1 : 0)).count;
		foreach (result; results[])
		{
			if (autoApplySingle && result.autofixes.length == 1)
			{
				changes ~= result.autofixes[0].expectReplacements;
			}
			else if (result.autofixes.length)
			{
				index++;
				string fileProgress = format!"[%d / %d] "(index, numAutofixes);
				messageFunctionFormat(fileProgress ~ errorFormat, result, false, code);

				UserSelect selector;
				selector.addSpecial(-1, "Skip", "0", "n", "s");
				auto item = selector.show(result.autofixes.map!"a.name");
				switch (item)
				{
				case -1:
					break; // skip
				default:
					changes ~= result.autofixes[item].expectReplacements;
					break;
				}
			}
		}
		if (changes.length)
		{
			changes.sort!"a.range[0] < b.range[0]";
			improveAutoFixWhitespace(cast(const(char)[]) code, changes);
			foreach_reverse (change; changes)
				code = code[0 .. change.range[0]]
					~ cast(const(ubyte)[])change.newText
					~ code[change.range[1] .. $];
			writeln("Writing changes to ", fileName);
			writeFileSafe(fileName, code);
		}
	}
	return hasErrors;
}

private struct UserSelect
{
	import std.string : strip;

	struct SpecialAction
	{
		int id;
		string title;
		string[] shorthands;
	}

	SpecialAction[] specialActions;

	void addSpecial(int id, string title, string[] shorthands...)
	{
		specialActions ~= SpecialAction(id, title, shorthands.dup);
	}

	/// Returns an integer in the range 0 - regularItems.length or a
	/// SpecialAction id or -1 when EOF or empty.
	int show(R)(R regularItems)
	{
		// TODO: implement interactive preview
		// TODO: implement apply/skip all occurrences (per file or globally)
		foreach (special; specialActions)
			writefln("%s) %s", special.shorthands[0], special.title);
		size_t i;
		foreach (autofix; regularItems)
			writefln("%d) %s", ++i, autofix);

		while (true)
		{
			try
			{
				write(" > ");
				stdout.flush();
				string input = readln().strip;
				if (!input.length)
				{
					writeln();
					return -1;
				}

				foreach (special; specialActions)
					if (special.shorthands.canFind(input))
						return special.id;

				int item = input.to!int - 1;
				if (item < 0 || item >= regularItems.length)
					throw new Exception("Selected option number out of range.");
				return item;
			}
			catch (Exception e)
			{
				writeln("Invalid selection, try again. ", e.message);
			}
		}
	}
}

const(Module) parseModule(string fileName, ubyte[] code, RollbackAllocator* p,
		ref StringCache cache, ref const(Token)[] tokens,
		MessageDelegate dlgMessage, ulong* linesOfCode = null,
		uint* errorCount = null, uint* warningCount = null)
{
	import dscanner.stats : isLineOfCode;

	LexerConfig config;
	config.fileName = fileName;
	config.stringBehavior = StringBehavior.source;
	tokens = getTokensForParser(code, config, &cache);
	if (linesOfCode !is null)
		(*linesOfCode) += count!(a => isLineOfCode(a.type))(tokens);

	return dparse.parser.parseModule(tokens, fileName, p, dlgMessage, errorCount, warningCount);
}

const(Module) parseModule(string fileName, ubyte[] code, RollbackAllocator* p,
		string errorFormat, ref StringCache cache, bool report, ref const(Token)[] tokens,
		ulong* linesOfCode = null, uint* errorCount = null, uint* warningCount = null)
{
	auto writeMessages = delegate(string fileName, size_t line, size_t column, string message, bool isError){
		// TODO: proper index and column ranges
		return messageFunctionFormat(errorFormat,
			Message(Message.Diagnostic.from(fileName, [0, 0], line, [column, column], message), "dscanner.syntax"),
			isError, code);
	};

	return parseModule(fileName, code, p, cache, tokens,
		report ? toDelegate(&messageFunctionJSON) : writeMessages,
		linesOfCode, errorCount, warningCount);
}

/**
Checks whether a module is part of a user-specified include/exclude list.
The user can specify a comma-separated list of filters, everyone needs to start with
either a '+' (inclusion) or '-' (exclusion).
If no includes are specified, all modules are included.
*/
bool shouldRun(check : BaseAnalyzer)(string moduleName, const ref StaticAnalysisConfig config)
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

BaseAnalyzer[] getAnalyzersForModuleAndConfig(string fileName,
	const(Token)[] tokens, const Module m,
	const StaticAnalysisConfig analysisConfig, const Scope* moduleScope)
{
	BaseAnalyzer[] checks;

	string moduleName;
	if (m !is null && m.moduleDeclaration !is null &&
		  m.moduleDeclaration.moduleName !is null &&
		  m.moduleDeclaration.moduleName.identifiers !is null)
		moduleName = m.moduleDeclaration.moduleName.identifiers.map!(e => e.text).join(".");

	BaseAnalyzerArguments args = BaseAnalyzerArguments(
		fileName,
		tokens,
		moduleScope
	);

	if (moduleName.shouldRun!FunctionAttributeCheck(analysisConfig))
		checks ~= new FunctionAttributeCheck(args.setSkipTests(
		analysisConfig.function_attribute_check == Check.skipTests && !ut));

	if (moduleName.shouldRun!MismatchedArgumentCheck(analysisConfig))
		checks ~= new MismatchedArgumentCheck(args.setSkipTests(
		analysisConfig.mismatched_args_check == Check.skipTests && !ut));

	if (moduleName.shouldRun!UndocumentedDeclarationCheck(analysisConfig))
		checks ~= new UndocumentedDeclarationCheck(args.setSkipTests(
		analysisConfig.undocumented_declaration_check == Check.skipTests && !ut));

	if (moduleName.shouldRun!AllManCheck(analysisConfig))
		checks ~= new AllManCheck(args.setSkipTests(
		analysisConfig.allman_braces_check == Check.skipTests && !ut));

	if (moduleName.shouldRun!IfConstraintsIndentCheck(analysisConfig))
		checks ~= new IfConstraintsIndentCheck(args.setSkipTests(
		analysisConfig.if_constraints_indent == Check.skipTests && !ut));

	return checks;
}

MessageSet analyze(string fileName, const Module m, const StaticAnalysisConfig analysisConfig,
		ref ModuleCache moduleCache, const(Token)[] tokens, bool staticAnalyze = true,
		bool resolveAutoFixes = false,
		const AutoFixFormatting overrideFormattingConfig = AutoFixFormatting.invalid)
{
	import dsymbol.symbol : DSymbol;
	import dscanner.analysis.autofix : resolveAutoFixFromCheck;

	if (!staticAnalyze)
		return null;

	const(AutoFixFormatting) formattingConfig =
		(resolveAutoFixes && overrideFormattingConfig is AutoFixFormatting.invalid)
			? analysisConfig.getAutoFixFormattingConfig()
			: overrideFormattingConfig;

	scope first = new FirstPass(m, internString(fileName), &moduleCache, null);
	first.run();

	secondPass(first.rootSymbol, first.moduleScope, moduleCache);
	auto moduleScope = first.moduleScope;
	scope(exit) typeid(DSymbol).destroy(first.rootSymbol.acSymbol);
	scope(exit) typeid(SemanticSymbol).destroy(first.rootSymbol);
	scope(exit) typeid(Scope).destroy(first.moduleScope);

	GC.disable;
	scope (exit)
		GC.enable;

	MessageSet set = new MessageSet;
	foreach (BaseAnalyzer check; getAnalyzersForModuleAndConfig(fileName, tokens, m, analysisConfig, moduleScope))
	{
		check.visit(m);
		foreach (message; check.messages)
		{
			if (resolveAutoFixes)
				foreach (ref autofix; message.autofixes)
					autofix.resolveAutoFixFromCheck(check, m, tokens, formattingConfig);
			set.insert(message);
		}
	}

	return set;
}

version (unittest)
{
	shared static this()
	{
		// mute dsymbol warnings in tests
		static if (__VERSION__ >= 2_101)
		{
			import std.logger : sharedLog, LogLevel;
			(cast()sharedLog).logLevel = LogLevel.error;
		}
		else
		{
			import std.experimental.logger : globalLogLevel, LogLevel;
			globalLogLevel = LogLevel.error;
		}
	}
}
