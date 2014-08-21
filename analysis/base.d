module analysis.base;

import std.container;
import std.string;
import std.d.ast;
import std.array;

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
}

enum comparitor = q{ a.line < b.line || (a.line == b.line && a.column < b.column) };

alias MessageSet = RedBlackTree!(Message, comparitor, true);

abstract class BaseAnalyzer : ASTVisitor
{
public:
	this(string fileName)
	{
		this.fileName = fileName;
		_messages = new MessageSet;
	}

	Message[] messages()
	{
		return _messages[].array;
	}

protected:

	bool inAggregate = false;

	template visitTemplate(T)
	{
		override void visit(const T structDec)
		{
			inAggregate = true;
			structDec.accept(this);
			inAggregate = false;
		}
	}

	import core.vararg;

	void addErrorMessage(size_t line, size_t column, string key, string message)
	{
		_messages.insert(Message(fileName, line, column, key, message));
	}

	/**
	 * The file name
	 */
	string fileName;

	MessageSet _messages;
}

