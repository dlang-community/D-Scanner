module dscanner.analysis.base;

import dparse.ast;
import dparse.lexer : IdType, str, Token;
import dsymbol.scope_ : Scope;
import std.array;
import std.container;
import std.string;
import std.sumtype;

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
	SumType!(CodeReplacement[], ResolveContext) autofix;

	invariant
	{
		autofix.match!(
			(const CodeReplacement[] replacement)
			{
				import std.algorithm : all, isSorted;

				assert(replacement.all!"a.range[0] <= a.range[1]");
				assert(replacement.isSorted!"a.range[0] < b.range[0]");
			},
			(_) {}
		);
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
		ret.autofix = [
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
		ret.autofix = [
			AutoFix.CodeReplacement([index, index], content)
		];
		return ret;
	}

	AutoFix concat(AutoFix other) const
	{
		import std.algorithm : sort;

		AutoFix ret;
		ret.name = name;
		CodeReplacement[] concatenated;
		autofix.match!(
			(const CodeReplacement[] replacement)
			{
				concatenated = replacement.dup;
			},
			_ => assert(false, "Cannot concatenate code replacement with late-resolve")
		);
		other.autofix.match!(
			(const CodeReplacement[] concat)
			{
				concatenated ~= concat.dup;
			},
			_ => assert(false, "Cannot concatenate code replacement with late-resolve")
		);
		concatenated.sort!"a.range[0] < b.range[0]";
		ret.autofix = concatenated;
		return ret;
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

mixin template AnalyzerInfo(string checkName)
{
	enum string name = checkName;

	override protected string getName()
	{
		return name;
	}
}

abstract class BaseAnalyzer : ASTVisitor
{
public:
	this(string fileName, const Scope* sc, bool skipTests = false)
	{
		this.sc = sc;
		this.fileName = fileName;
		this.skipTests = skipTests;
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

	AutoFix.CodeReplacement[] resolveAutoFix(
		const Module mod,
		const(Token)[] tokens,
		const Message message,
		const AutoFix.ResolveContext context
	)
	{
		cast(void) mod;
		cast(void) tokens;
		cast(void) message;
		cast(void) context;
		assert(0);
	}

protected:

	bool inAggregate;
	bool skipTests;

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
		_messages.insert(Message(fileName, line, column, key, message, getName()));
	}

	void addErrorMessage(const BaseNode node, string key, string message, AutoFix[] autofixes = null)
	{
		addErrorMessage(Message.Diagnostic.from(fileName, node, message), key, autofixes);
	}

	void addErrorMessage(const Token token, string key, string message, AutoFix[] autofixes = null)
	{
		addErrorMessage(Message.Diagnostic.from(fileName, token, message), key, autofixes);
	}

	void addErrorMessage(const Token[] tokens, string key, string message, AutoFix[] autofixes = null)
	{
		addErrorMessage(Message.Diagnostic.from(fileName, tokens, message), key, autofixes);
	}

	void addErrorMessage(size_t[2] index, size_t line, size_t[2] columns, string key, string message, AutoFix[] autofixes = null)
	{
		addErrorMessage(index, [line, line], columns, key, message, autofixes);
	}

	void addErrorMessage(size_t[2] index, size_t[2] lines, size_t[2] columns, string key, string message, AutoFix[] autofixes = null)
	{
		auto d = Message.Diagnostic.from(fileName, index, lines, columns, message);
		_messages.insert(Message(d, key, getName(), autofixes));
	}

	void addErrorMessage(Message.Diagnostic diagnostic, string key, AutoFix[] autofixes = null)
	{
		_messages.insert(Message(diagnostic, key, getName(), autofixes));
	}

	void addErrorMessage(Message.Diagnostic diagnostic, Message.Diagnostic[] supplemental, string key, AutoFix[] autofixes = null)
	{
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
