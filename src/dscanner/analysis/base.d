module dscanner.analysis.base;

import std.container;
import std.string;
import dparse.ast;
import std.array;
import dsymbol.scope_ : Scope;
import dparse.lexer : Token, str, IdType;

struct Message
{
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

		// TODO: add auto-fix suggestion API here

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

	deprecated this(string fileName, size_t line, size_t column, string key = null, string message = null, string checkName = null)
	{
		diagnostic.fileName = fileName;
		diagnostic.startLine = diagnostic.endLine = line;
		diagnostic.startColumn = diagnostic.endColumn = column;
		diagnostic.message = message;
		this.key = key;
		this.checkName = checkName;
	}

	this(Diagnostic diagnostic, string key = null, string checkName = null)
	{
		this.diagnostic = diagnostic;
		this.key = key;
		this.checkName = checkName;
	}

	this(Diagnostic diagnostic, Diagnostic[] supplemental, string key = null, string checkName = null)
	{
		this.diagnostic = diagnostic;
		this.supplemental = supplemental;
		this.key = key;
		this.checkName = checkName;
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

	protected string getName()
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

	void addErrorMessage(const BaseNode node, string key, string message)
	{
		addErrorMessage(Message.Diagnostic.from(fileName, node, message), key);
	}

	void addErrorMessage(const Token token, string key, string message)
	{
		addErrorMessage(Message.Diagnostic.from(fileName, token, message), key);
	}

	void addErrorMessage(const Token[] tokens, string key, string message)
	{
		addErrorMessage(Message.Diagnostic.from(fileName, tokens, message), key);
	}

	void addErrorMessage(size_t[2] index, size_t line, size_t[2] columns, string key, string message)
	{
		addErrorMessage(index, [line, line], columns, key, message);
	}

	void addErrorMessage(size_t[2] index, size_t[2] lines, size_t[2] columns, string key, string message)
	{
		auto d = Message.Diagnostic.from(fileName, index, lines, columns, message);
		_messages.insert(Message(d, key, getName()));
	}

	void addErrorMessage(Message.Diagnostic diagnostic, string key)
	{
		_messages.insert(Message(diagnostic, key, getName()));
	}

	void addErrorMessage(Message.Diagnostic diagnostic, Message.Diagnostic[] supplemental, string key)
	{
		_messages.insert(Message(diagnostic, supplemental, key, getName()));
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
