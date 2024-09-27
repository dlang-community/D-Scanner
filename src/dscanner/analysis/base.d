module dscanner.analysis.base;

import dparse.ast;
import dparse.lexer : IdType, str, Token, tok;
import dscanner.analysis.nolint;
import dsymbol.scope_ : Scope;
import std.array;
import std.container;
import std.meta : AliasSeq;
import std.string;
import std.sumtype;
import dmd.transitivevisitor;
import dmd.visitor;
import dmd.func;
import core.stdc.string;
import std.conv : to;

///
struct AutoFix
{
	///
	struct CodeReplacement
	{
		/// Byte index `[start, end)` within the file what text to replace.
		/// `start == end` if text is only getting inserted.
		size_t[2] range;
		/// The new text to put inside the range. (empty to delete text)
		string newText;
	}

	/// Context that the analyzer resolve method can use to generate the
	/// resolved `CodeReplacement` with.
	struct ResolveContext
	{
		/// Arbitrary analyzer-defined parameters. May grow in the future with
		/// more items.
		ulong[3] params;
		/// For dynamically sized data, may contain binary data.
		string extraInfo;
	}

	/// Display name for the UI.
	string name;
	/// Either code replacements, sorted by range start, never overlapping, or a
	/// context that can be passed to `BaseAnalyzer.resolveAutoFix` along with
	/// the message key from the parent `Message` object.
	///
	/// `CodeReplacement[]` should be applied to the code in reverse, otherwise
	/// an offset to the following start indices must be calculated and be kept
	/// track of.
	SumType!(CodeReplacement[], ResolveContext) replacements;

	invariant
	{
		replacements.match!(
			(const CodeReplacement[] replacement)
			{
				import std.algorithm : all, isSorted;

				assert(replacement.all!"a.range[0] <= a.range[1]");
				assert(replacement.isSorted!"a.range[0] < b.range[0]");
			},
			(_) {}
		);
	}

	static AutoFix resolveLater(string name, ulong[3] params, string extraInfo = null)
	{
		AutoFix ret;
		ret.name = name;
		ret.replacements = ResolveContext(params, extraInfo);
		return ret;
	}

	static AutoFix replacement(size_t tokenStart, size_t tokenEnd, string newText, string name)
	{
		AutoFix ret;
		ret.name = name;
		ret.replacements = [
			AutoFix.CodeReplacement([tokenStart, tokenEnd], newText)
		];
		return ret;
	}

	static AutoFix replacement(const Token token, string newText, string name = null)
	{
		if (!name.length)
		{
			auto text = token.text.length ? token.text : str(token.type);
			if (newText.length)
				name = "Replace `" ~ text ~ "` with `" ~ newText ~ "`";
			else
				name = "Remove `" ~ text ~ "`";
		}
		return replacement([token], newText, name);
	}

	static AutoFix replacement(const BaseNode node, string newText, string name)
	{
		return replacement(node.tokens, newText, name);
	}

	static AutoFix replacement(const Token[] tokens, string newText, string name)
	in(tokens.length > 0, "must provide at least one token")
	{
		auto end = tokens[$ - 1].text.length ? tokens[$ - 1].text : str(tokens[$ - 1].type);
		return replacement([tokens[0].index, tokens[$ - 1].index + end.length], newText, name);
	}

	static AutoFix replacement(size_t[2] range, string newText, string name)
	{
		AutoFix ret;
		ret.name = name;
		ret.replacements = [
			AutoFix.CodeReplacement(range, newText)
		];
		return ret;
	}

	static AutoFix insertionBefore(const Token token, string content, string name = null)
	{
		return insertionAt(token.index, content, name);
	}

	static AutoFix insertionAfter(const Token token, string content, string name = null)
	{
		auto tokenText = token.text.length ? token.text : str(token.type);
		return insertionAt(token.index + tokenText.length, content, name);
	}

	static AutoFix insertionAt(size_t index, string content, string name = null)
	{
		assert(content.length > 0, "generated auto fix inserting text without content");
		AutoFix ret;
		ret.name = name.length
			? name
			: content.strip.length
				? "Insert `" ~ content.strip ~ "`"
				: "Insert whitespace";
		ret.replacements = [
			AutoFix.CodeReplacement([index, index], content)
		];
		return ret;
	}

	static AutoFix indentLines(scope const(Token)[] tokens, const AutoFixFormatting formatting, string name = "Indent code")
	{
		CodeReplacement[] inserts;
		size_t line = -1;
		foreach (token; tokens)
		{
			if (line != token.line)
			{
				line = token.line;
				inserts ~= CodeReplacement([token.index, token.index], formatting.indentation);
			}
		}
		AutoFix ret;
		ret.name = name;
		ret.replacements = inserts;
		return ret;
	}

	AutoFix concat(AutoFix other) const
	{
		import std.algorithm : sort;

		static immutable string errorMsg = "Cannot concatenate code replacement with late-resolve";

		AutoFix ret;
		ret.name = name;
		CodeReplacement[] concatenated = expectReplacements(errorMsg).dup
			~ other.expectReplacements(errorMsg);
		concatenated.sort!"a.range[0] < b.range[0]";
		ret.replacements = concatenated;
		return ret;
	}

	CodeReplacement[] expectReplacements(
		string errorMsg = "Expected available code replacements, not something to resolve later"
	) @safe pure nothrow @nogc
	{
		return replacements.match!(
			(replacement)
			{
				if (false) return CodeReplacement[].init;
				static if (is(immutable typeof(replacement) == immutable CodeReplacement[]))
					return replacement;
				else
					assert(false, errorMsg);
			}
		);
	}

	const(CodeReplacement[]) expectReplacements(
		string errorMsg = "Expected available code replacements, not something to resolve later"
	) const @safe pure nothrow @nogc
	{
		return replacements.match!(
			(const replacement)
			{
				if (false) return CodeReplacement[].init;
				static if (is(immutable typeof(replacement) == immutable CodeReplacement[]))
					return replacement;
				else
					assert(false, errorMsg);
			}
		);
	}
}

/// Formatting style for autofix generation (only available for resolve autofix)
struct AutoFixFormatting
{
	enum AutoFixFormatting invalid = AutoFixFormatting(BraceStyle.invalid, null, 0, null);

	enum BraceStyle
	{
		/// invalid, shouldn't appear in usable configs
		invalid,
		/// $(LINK https://en.wikipedia.org/wiki/Indent_style#Allman_style)
		allman,
		/// $(LINK https://en.wikipedia.org/wiki/Indent_style#Variant:_1TBS)
		otbs,
		/// $(LINK https://en.wikipedia.org/wiki/Indent_style#Variant:_Stroustrup)
		stroustrup,
		/// $(LINK https://en.wikipedia.org/wiki/Indentation_style#K&R_style)
		knr,
	}

	/// Brace style config
	BraceStyle braceStyle = BraceStyle.allman;
	/// String to insert on indentations
	string indentation = "\t";
	/// For calculating indentation size
	uint indentationWidth = 4;
	/// String to insert on line endings
	string eol = "\n";

	invariant
	{
		import std.algorithm : all;

		assert(!indentation.length
			|| indentation == "\t"
			|| indentation.all!(c => c == ' '));
	}

	string getWhitespaceBeforeOpeningBrace(string lastLineIndent, bool isFuncDecl) pure nothrow @safe const
	{
		final switch (braceStyle)
		{
		case BraceStyle.invalid:
			assert(false, "invalid formatter config");
		case BraceStyle.knr:
			if (isFuncDecl)
				goto case BraceStyle.allman;
			else
				goto case BraceStyle.otbs;
		case BraceStyle.otbs:
		case BraceStyle.stroustrup:
			return " ";
		case BraceStyle.allman:
			return eol ~ lastLineIndent;
		}
	}
}

/// A diagnostic message. Each message defines one issue in the file, which
/// consists of one or more squiggly line ranges within the code, as well as
/// human readable descriptions and optionally also one or more automatic code
/// fixes that can be applied.
struct Message
{
	/// A squiggly line range within the code. May be the issue itself if it's
	/// the `diagnostic` member or supplemental information that can aid the
	/// user in resolving the issue.
	struct Diagnostic
	{
		/// Name of the file where the warning was triggered.
		string fileName;
		/// Byte index from start of file the warning was triggered.
		size_t startIndex, endIndex;
		/// Line number where the warning was triggered, 1-based.
		size_t startLine, endLine;
		/// Column number where the warning was triggered. (in bytes)
		size_t startColumn, endColumn;
		/// Warning message, may be null for supplemental diagnostics.
		string message;

		deprecated("Use startLine instead") alias line = startLine;
		deprecated("Use startColumn instead") alias column = startColumn;

		static Diagnostic from(string fileName, const BaseNode node, string message)
		{
			return from(fileName, node !is null ? node.tokens : [], message);
		}

		static Diagnostic from(string fileName, const Token token, string message)
		{
			auto text = token.text.length ? token.text : str(token.type);
			return from(fileName,
				[token.index, token.index + text.length],
				token.line,
				[token.column, token.column + text.length],
				message);
		}

		static Diagnostic from(string fileName, const Token[] tokens, string message)
		{
			auto start = tokens.length ? tokens[0] : Token.init;
			auto end = tokens.length ? tokens[$ - 1] : Token.init;
			auto endText = end.text.length ? end.text : str(end.type);
			return from(fileName,
				[start.index, end.index + endText.length],
				[start.line, end.line],
				[start.column, end.column + endText.length],
				message);
		}

		static Diagnostic from(string fileName, size_t[2] index, size_t line, size_t[2] columns, string message)
		{
			return Message.Diagnostic(fileName, index[0], index[1], line, line, columns[0], columns[1], message);
		}

		static Diagnostic from(string fileName, size_t[2] index, size_t[2] lines, size_t[2] columns, string message)
		{
			return Message.Diagnostic(fileName, index[0], index[1], lines[0], lines[1], columns[0], columns[1], message);
		}
	}

	/// Primary warning
	Diagnostic diagnostic;
	/// List of supplemental warnings / hint locations
	Diagnostic[] supplemental;
	/// Name of the warning
	string key;
	/// Check name
	string checkName;

	/// Either immediate code changes that can be applied or context to call
	/// the `BaseAnalyzer.resolveAutoFix` method with.
	AutoFix[] autofixes;

	deprecated this(string fileName, size_t line, size_t column, string key = null, string message = null, string checkName = null)
	{
		diagnostic.fileName = fileName;
		diagnostic.startLine = diagnostic.endLine = line;
		diagnostic.startColumn = diagnostic.endColumn = column;
		diagnostic.message = message;
		this.key = key;
		this.checkName = checkName;
	}

	this(string fileName, size_t line, size_t column, string key = null, string message = null, string checkName = null, AutoFix[] autofixes = null)
	{
		diagnostic.fileName = fileName;
		diagnostic.startLine = diagnostic.endLine = line;
		diagnostic.startColumn = diagnostic.endColumn = column;
		diagnostic.message = message;
		this.key = key;
		this.checkName = checkName;
		this.autofixes = autofixes;
	}

	this(Diagnostic diagnostic, string key = null, string checkName = null, AutoFix[] autofixes = null)
	{
		this.diagnostic = diagnostic;
		this.key = key;
		this.checkName = checkName;
		this.autofixes = autofixes;
	}

	this(Diagnostic diagnostic, Diagnostic[] supplemental, string key = null, string checkName = null, AutoFix[] autofixes = null)
	{
		this.diagnostic = diagnostic;
		this.supplemental = supplemental;
		this.key = key;
		this.checkName = checkName;
		this.autofixes = autofixes;
	}

	alias diagnostic this;
}

enum comparitor = q{ a.startLine < b.startLine || (a.startLine == b.startLine && a.startColumn < b.startColumn) };

alias MessageSet = RedBlackTree!(Message, comparitor, true);

/**
 * Should be present in all visitors to specify the name of the check
 *  done by a patricular visitor
 */
mixin template AnalyzerInfo(string checkName)
{
	enum string name = checkName;

	extern(D) override protected string getName()
	{
		return name;
	}
}

struct BaseAnalyzerArguments
{
	string fileName;
	const(Token)[] tokens;
	const Scope* sc;
	bool skipTests = false;

	BaseAnalyzerArguments setSkipTests(bool v)
	{
		auto ret = this;
		ret.skipTests = v;
		return ret;
	}
}

abstract class BaseAnalyzer : ASTVisitor
{
public:
	deprecated("Don't use this constructor, use the one taking BaseAnalyzerArguments")
	this(string fileName, const Scope* sc, bool skipTests = false)
	{
		BaseAnalyzerArguments args = {
			fileName: fileName,
			sc: sc,
			skipTests: skipTests
		};
		this(args);
	}

	this(BaseAnalyzerArguments args)
	{
		this.sc = args.sc;
		this.tokens = args.tokens;
		this.fileName = args.fileName;
		this.skipTests = args.skipTests;
		_messages = new MessageSet;
	}

	string getName()
	{
		assert(0);
	}

	Message[] messages()
	{
		return _messages[].array;
	}

	alias visit = ASTVisitor.visit;

	/**
	* Visits a unittest.
	*
	* When overriden, the protected bool "skipTests" should be handled
	* so that the content of the test is not analyzed.
	*/
	override void visit(const Unittest unittest_)
	{
		if (!skipTests)
			unittest_.accept(this);
	}

	/**
	* Visits a module declaration.
	*
	* When overriden, make sure to keep this structure
	*/
	override void visit(const(Module) mod)
	{
		if (mod.moduleDeclaration !is null)
		{
			with (noLint.push(NoLintFactory.fromModuleDeclaration(mod.moduleDeclaration)))
				mod.accept(this);
		}
		else
		{
			mod.accept(this);
		}
	}

	/**
	* Visits a declaration.
	*
	* When overriden, make sure to disable and reenable error messages
	*/
	override void visit(const(Declaration) decl)
	{
		with (noLint.push(NoLintFactory.fromDeclaration(decl)))
			decl.accept(this);
	}

	AutoFix.CodeReplacement[] resolveAutoFix(
		const Module mod,
		scope const(Token)[] tokens,
		const AutoFix.ResolveContext context,
		const AutoFixFormatting formatting,
	)
	{
		cast(void) mod;
		cast(void) tokens;
		cast(void) context;
		cast(void) formatting;
		assert(0);
	}

protected:

	bool inAggregate;
	bool skipTests;
	const(Token)[] tokens;
	NoLint noLint;

	template visitTemplate(T)
	{
		override void visit(const T structDec)
		{
			inAggregate = true;
			structDec.accept(this);
			inAggregate = false;
		}
	}

	deprecated("Use the overload taking start and end locations or a Node instead")
	void addErrorMessage(size_t line, size_t column, string key, string message)
	{
		if (noLint.containsCheck(key))
			return;
		_messages.insert(Message(fileName, line, column, key, message, getName()));
	}

	void addErrorMessage(const BaseNode node, string key, string message, AutoFix[] autofixes = null)
	{
		if (noLint.containsCheck(key))
			return;
		addErrorMessage(Message.Diagnostic.from(fileName, node, message), key, autofixes);
	}

	void addErrorMessage(const Token token, string key, string message, AutoFix[] autofixes = null)
	{
		if (noLint.containsCheck(key))
			return;
		addErrorMessage(Message.Diagnostic.from(fileName, token, message), key, autofixes);
	}

	void addErrorMessage(const Token[] tokens, string key, string message, AutoFix[] autofixes = null)
	{
		if (noLint.containsCheck(key))
			return;
		addErrorMessage(Message.Diagnostic.from(fileName, tokens, message), key, autofixes);
	}

	void addErrorMessage(size_t[2] index, size_t line, size_t[2] columns, string key, string message, AutoFix[] autofixes = null)
	{
		if (noLint.containsCheck(key))
			return;
		addErrorMessage(index, [line, line], columns, key, message, autofixes);
	}

	void addErrorMessage(size_t[2] index, size_t[2] lines, size_t[2] columns, string key, string message, AutoFix[] autofixes = null)
	{
		if (noLint.containsCheck(key))
			return;
		auto d = Message.Diagnostic.from(fileName, index, lines, columns, message);
		_messages.insert(Message(d, key, getName(), autofixes));
	}

	void addErrorMessage(Message.Diagnostic diagnostic, string key, AutoFix[] autofixes = null)
	{
		if (noLint.containsCheck(key))
			return;
		_messages.insert(Message(diagnostic, key, getName(), autofixes));
	}

	void addErrorMessage(Message.Diagnostic diagnostic, Message.Diagnostic[] supplemental, string key, AutoFix[] autofixes = null)
	{
		if (noLint.containsCheck(key))
			return;
		_messages.insert(Message(diagnostic, supplemental, key, getName(), autofixes));
	}

	/**
	 * The file name
	 */
	string fileName;

	const(Scope)* sc;

	MessageSet _messages;
}

/// Find the token with the given type, otherwise returns the whole range or a user-specified fallback, if set.
const(Token)[] findTokenForDisplay(const BaseNode node, IdType type, const(Token)[] fallback = null)
{
	return node.tokens.findTokenForDisplay(type, fallback);
}
/// ditto
const(Token)[] findTokenForDisplay(const Token[] tokens, IdType type, const(Token)[] fallback = null)
{
	foreach (i, token; tokens)
		if (token.type == type)
			return tokens[i .. i + 1];
	return fallback is null ? tokens : fallback;
}

abstract class ScopedBaseAnalyzer : BaseAnalyzer
{
public:
	this(BaseAnalyzerArguments args)
	{
		super(args);
	}


	template ScopedVisit(NodeType)
	{
		override void visit(const NodeType n)
		{
			pushScopeImpl();
			scope (exit)
				popScopeImpl();
			n.accept(this);
		}
	}

	alias visit = BaseAnalyzer.visit;

	mixin ScopedVisit!BlockStatement;
	mixin ScopedVisit!ForeachStatement;
	mixin ScopedVisit!ForStatement;
	mixin ScopedVisit!Module;
	mixin ScopedVisit!StructBody;
	mixin ScopedVisit!TemplateDeclaration;
	mixin ScopedVisit!WithStatement;
	mixin ScopedVisit!WhileStatement;
	mixin ScopedVisit!DoStatement;
	// mixin ScopedVisit!SpecifiedFunctionBody; // covered by BlockStatement
	mixin ScopedVisit!ShortenedFunctionBody;

	override void visit(const SwitchStatement switchStatement)
	{
		switchStack.length++;
		scope (exit)
			switchStack.length--;
		switchStatement.accept(this);
	}

	override void visit(const IfStatement ifStatement)
	{
		pushScopeImpl();
		if (ifStatement.condition)
			ifStatement.condition.accept(this);
		if (ifStatement.thenStatement)
			ifStatement.thenStatement.accept(this);
		popScopeImpl();

		if (ifStatement.elseStatement)
		{
			pushScopeImpl();
			ifStatement.elseStatement.accept(this);
			popScopeImpl();
		}
	}

	static foreach (T; AliasSeq!(CaseStatement, DefaultStatement, CaseRangeStatement))
		override void visit(const T stmt)
		{
			// case and default statements always open new scopes and close
			// previous case scopes
			bool close = switchStack.length && switchStack[$ - 1].inCase;
			bool b = switchStack[$ - 1].inCase;
			switchStack[$ - 1].inCase = true;
			scope (exit)
				switchStack[$ - 1].inCase = b;
			if (close)
			{
				popScope();
				pushScope();
				stmt.accept(this);
			}
			else
			{
				pushScope();
				stmt.accept(this);
				popScope();
			}
		}

protected:
	/// Called on new scopes, which includes for example:
	///
	/// - `module m; /* here, entire file */`
	/// - `{ /* here */ }`
	/// - `if () { /* here */ } else { /* here */ }`
	/// - `foreach (...) { /* here */ }`
	/// - `case 1: /* here */ break;`
	/// - `case 1: /* here, up to next case */ goto case; case 2: /* here 2 */ break;`
	/// - `default: /* here */ break;`
	/// - `struct S { /* here */ }`
	///
	/// But doesn't include:
	///
	/// - `static if (x) { /* not a separate scope */ }` (use `mixin ScopedVisit!ConditionalDeclaration;`)
	///
	/// You can `mixin ScopedVisit!NodeType` to automatically call push/popScope
	/// on occurences of that NodeType.
	abstract void pushScope();
	/// ditto
	abstract void popScope();

	void pushScopeImpl()
	{
		if (switchStack.length)
			switchStack[$ - 1].scopeDepth++;
		pushScope();
	}

	void popScopeImpl()
	{
		if (switchStack.length)
			switchStack[$ - 1].scopeDepth--;
		popScope();
	}

	struct SwitchStack
	{
		int scopeDepth;
		bool inCase;
	}

	SwitchStack[] switchStack;
}

unittest
{
	import core.exception : AssertError;
	import dparse.lexer : getTokensForParser, LexerConfig, StringCache;
	import dparse.parser : parseModule;
	import dparse.rollback_allocator : RollbackAllocator;
	import std.conv : to;
	import std.exception : assertThrown;

	// test where we can:
	// call `depth(1);` to check that the scope depth is at 1
	// if calls are syntactically not valid, define `auto depth = 1;`
	//
	// call `isNewScope();` to check that the scope hasn't been checked with isNewScope before
	// if calls are syntactically not valid, define `auto isNewScope = void;`
	//
	// call `isOldScope();` to check that the scope has already been checked with isNewScope
	// if calls are syntactically not valid, define `auto isOldScope = void;`

	class TestScopedAnalyzer : ScopedBaseAnalyzer
	{
		this(size_t codeLine)
		{
			super(BaseAnalyzerArguments("stdin"));

			this.codeLine = codeLine;
		}

		override void visit(const FunctionCallExpression f)
		{
			int depth = cast(int) stack.length;
			if (f.unaryExpression && f.unaryExpression.primaryExpression
				&& f.unaryExpression.primaryExpression.identifierOrTemplateInstance)
			{
				auto fname = f.unaryExpression.primaryExpression.identifierOrTemplateInstance.identifier.text;
				if (fname == "depth")
				{
					assert(f.arguments.tokens.length == 3);
					auto expected = f.arguments.tokens[1].text.to!int;
					assert(expected == depth, "Expected depth="
						~ expected.to!string ~ " in line " ~ (codeLine + f.tokens[0].line).to!string
						~ ", but got depth=" ~ depth.to!string);
				}
				else if (fname == "isNewScope")
				{
					assert(!stack[$ - 1]);
					stack[$ - 1] = true;
				}
				else if (fname == "isOldScope")
				{
					assert(stack[$ - 1]);
				}
			}
		}

		override void visit(const AutoDeclarationPart p)
		{
			int depth = cast(int) stack.length;

			if (p.identifier.text == "depth")
			{
				assert(p.initializer.tokens.length == 1);
				auto expected = p.initializer.tokens[0].text.to!int;
				assert(expected == depth, "Expected depth="
					~ expected.to!string ~ " in line " ~ (codeLine + p.tokens[0].line).to!string
					~ ", but got depth=" ~ depth.to!string);
			}
			else if (p.identifier.text == "isNewScope")
			{
				assert(!stack[$ - 1]);
				stack[$ - 1] = true;
			}
			else if (p.identifier.text == "isOldScope")
			{
				assert(stack[$ - 1]);
			}
		}

		override void pushScope()
		{
			stack.length++;
		}

		override void popScope()
		{
			stack.length--;
		}

		alias visit = ScopedBaseAnalyzer.visit;

		bool[] stack;
		size_t codeLine;
	}

	void testScopes(string code, size_t codeLine = __LINE__ - 1)
	{
		StringCache cache = StringCache(4096);
		LexerConfig config;
		RollbackAllocator rba;
		auto tokens = getTokensForParser(code, config, &cache);
		Module m = parseModule(tokens, "stdin", &rba);

		auto analyzer = new TestScopedAnalyzer(codeLine);
		analyzer.visit(m);
	}

	testScopes(q{
		auto isNewScope = void;
		auto depth = 1;
		auto isOldScope = void;
	});

	assertThrown!AssertError(testScopes(q{
		auto isNewScope = void;
		auto isNewScope = void;
	}));

	assertThrown!AssertError(testScopes(q{
		auto isOldScope = void;
	}));

	assertThrown!AssertError(testScopes(q{
		auto depth = 2;
	}));

	testScopes(q{
		auto isNewScope = void;
		auto depth = 1;

		void foo() {
			isNewScope();
			isOldScope();
			depth(2);
			switch (a)
			{
			case 1:
				isNewScope();
				depth(4);
				break;
				depth(4);
				isOldScope();
			case 2:
				isNewScope();
				depth(4);
				if (a)
				{
					isNewScope();
					depth(6);
				default:
					isNewScope();
					depth(6); // since cases/default opens new scope
					break;
				case 3:
					isNewScope();
					depth(6); // since cases/default opens new scope
					break;
				default:
					isNewScope();
					depth(6); // since cases/default opens new scope
					break;
				}
				break;
				depth(4);
			default:
				isNewScope();
				depth(4);
				break;
				depth(4);
			}

			isOldScope();
			depth(2);

			switch (a)
			{
				isNewScope();
				depth(3);
				isOldScope();
			default:
				isNewScope();
				depth(4);
				break;
				isOldScope();
			case 1:
				isNewScope();
				depth(4);
				break;
				isOldScope();
			}
		}

		auto isOldScope = void;
	});
}

/**
 * Visitor that implements the AST traversal logic.
 * Supports collecting error messages
 */
extern(C++) class BaseAnalyzerDmd : SemanticTimeTransitiveVisitor
{
	alias visit = SemanticTimeTransitiveVisitor.visit;

	extern(D) this(string fileName, bool skipTests = false)
	{
		this.fileName = fileName;
		this.skipTests = skipTests;
		_messages = new MessageSet;
	}

	/**
	 * Ensures that template AnalyzerInfo is instantiated in all classes
	 *  deriving from this class
	 */
	extern(D) protected string getName()
	{
		assert(0);
	}

	extern(D) Message[] messages()
	{
		return _messages[].array;
	}

	override void visit(UnitTestDeclaration ud)
	{
		if (!skipTests)
			super.visit(ud);
	}


protected:

	extern (D) void addErrorMessage(size_t line, size_t column, string key, string message)
	{
		_messages.insert(Message(fileName, line, column, key, message, getName()));
	}

	extern (D) void addErrorMessage(size_t line, size_t column, string key, string message, AutoFix[] autofixes)
	{
		_messages.insert(Message(fileName, line, column, key, message, getName(), autofixes));
	}

	extern (D) bool skipTests;

	/**
	 * The file name
	 */
	extern (D) string fileName;

	extern (D) MessageSet _messages;
}
