module analysis.base;

import std.container;
import std.string;
import stdx.d.ast;
import std.array;

struct Message
{
	string fileName;
	size_t line;
	size_t column;
	string message;
}

enum comparitor = q{ a.line < b.line || a.line < b.line };

alias MessageSet = RedBlackTree!(Message, comparitor);

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

	void addErrorMessage(size_t line, size_t column, string message)
	{
		_messages.insert(Message(fileName, line, column, message));
	}

	/**
	 * The file name
	 */
	string fileName;

	MessageSet _messages;
}
