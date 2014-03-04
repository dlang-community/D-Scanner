module analysis.base;

import std.string;
import stdx.d.ast;

abstract class BaseAnalyzer : ASTVisitor
{
public:
	this(string fileName)
	{
		this.fileName = fileName;
	}

	string[] messages()
	{
		return _messages;
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
		_messages ~= format("%s(%d:%d)[warn]: %s", fileName, line, column, message);
	}

	/**
	 * The file name
	 */
	string fileName;

	/**
	 * Map of file names to warning messages for that file
	 */
	string[] _messages;
}
