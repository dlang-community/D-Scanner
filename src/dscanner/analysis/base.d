module dscanner.analysis.base;

import std.container;
import std.string;
import dparse.ast;
import std.array;
import dsymbol.scope_ : Scope;

struct Message
{
	/// Name of the file where the warning was triggered
	string fileName;
	/// Line number where the warning was triggered
	size_t line;
	/// Column number where the warning was triggered (in bytes)
	size_t column;
	/// Name of the warning
	string key;
	/// Warning message
	string message;
	/// Check name
	string checkName;
}

enum comparitor = q{ a.line < b.line || (a.line == b.line && a.column < b.column) };

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
			() @trusted { structDec.accept(this); } ();
			inAggregate = false;
		}
	}

	void addErrorMessage(size_t line, size_t column, string key, string message)
	{
		_messages.insert(Message(fileName, line, column, key, message, getName()));
	}

	/**
	 * The file name
	 */
	string fileName;

	const(Scope)* sc;

	MessageSet _messages;
}
