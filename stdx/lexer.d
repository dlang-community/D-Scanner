// Written in the D programming language

/**
 * This module contains a range-based _lexer generator.
 *
 * Copyright: Brian Schott 2013
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt Boost, License 1.0)
 * Authors: Brian Schott, with ideas shamelessly stolen from Andrei Alexandrescu
 * Source: $(PHOBOSSRC std/_lexer.d)
 */

module stdx.lexer;
import std.typecons;
import std.algorithm;
import std.range;
import std.traits;
import std.conv;
import std.math;

template TokenIdType(alias staticTokens, alias dynamicTokens,
	alias possibleDefaultTokens)
{
  static if ((staticTokens.length + dynamicTokens.length + possibleDefaultTokens.length) <= ubyte.max)
		alias TokenIdType = ubyte;
	else static if ((staticTokens.length + dynamicTokens.length + possibleDefaultTokens.length) <= ushort.max)
		alias TokenIdType = ushort;
	else static if ((staticTokens.length + dynamicTokens.length + possibleDefaultTokens.length) <= uint.max)
		alias TokenIdType = uint;
	else
		static assert (false);
}

string TokenStringRepresentation(IdType, alias staticTokens, alias possibleDefaultTokens)(IdType type) @property
{
	if (type == 0)
		return "!ERROR!";
	else if (type < staticTokens.length)
		return staticTokens[type - 1];
	else if (type < staticTokens.length + possibleDefaultTokens.length)
		return possibleDefaultTokens[type - staticTokens.length];
	else
		return null;
}

template TokenId(IdType, alias staticTokens, alias dynamicTokens,
	alias possibleDefaultTokens, string symbol)
{
	static if (symbol == "")
	{
	  enum id = 0;
		alias id TokenId;
	}
	else static if (symbol == "\0")
	{
		enum id = 1 + staticTokens.length + dynamicTokens.length + possibleDefaultTokens.length;
		alias id TokenId;
	}
	else
	{
		enum i = staticTokens.countUntil(symbol);
		static if (i >= 0)
		{
			enum id = i + 1;
			alias id TokenId;
		}
		else
		{
			enum ii = possibleDefaultTokens.countUntil(symbol);
			static if (ii >= 0)
			{
				enum id = ii + staticTokens.length;
				static assert (id >= 0 && id < IdType.max, "Invalid token: " ~ symbol);
				alias id TokenId;
			}
			else
			{
				enum dynamicId = dynamicTokens.countUntil(symbol);
				enum id = dynamicId >= 0 ? i + staticTokens.length + possibleDefaultTokens.length + dynamicId : -1;
				static assert (id >= 0 && id < IdType.max, "Invalid token: " ~ symbol);
				alias id TokenId;
			}
		}
	}
}

struct TokenStructure(IDType)
{
	bool opEquals(IDType type) const pure nothrow @safe
	{
		return this.type == type;
	}

	IDType type;  
	string text;
	size_t line;
	size_t column;
	size_t index;
}

mixin template Lexer(R, IDType, Token, alias isSeparating, alias defaultTokenFunction,
	alias staticTokens, alias dynamicTokens, alias pseudoTokens,
	alias possibleDefaultTokens) if (isForwardRange!R)
{
	enum size_t lookAhead = chain(staticTokens, pseudoTokens).map!"a.length".reduce!"max(a, b)"();
	alias PeekRange!(R, lookAhead) RangeType;

	static string generateCaseStatements(string[] tokens, size_t offset = 0)
	{
		string code;
		for (size_t i = 0; i < tokens.length; i++)
		{
			auto indent = "";
			foreach (k; 0 .. offset)
				indent ~= "    ";
			size_t j = i + 1;

			if (offset < tokens[i].length)
			{
				while (j < tokens.length && offset < tokens[j].length
					&& tokens[i][offset] == tokens[j][offset]) j++;
				code ~= indent ~ "case " ~ text(cast(ubyte) tokens[i][offset]) ~ ":\n";
				if (i + 1 >= j)
				{
					if (offset + 1 == tokens[i].length)
						code ~= generateLeaf(tokens[i], indent ~ "    ");
					else
					{
						code ~= indent ~ "    if (!range.canPeek(" ~ text(tokens[i].length - 1) ~ "))\n";
						code ~= indent ~ "        goto outer_default;\n";
						code ~= indent ~ "    if (range.startsWith(\"" ~ escape(tokens[i]) ~ "\"))\n";
						code ~= indent ~ "    {\n";
						code ~= generateLeaf(tokens[i], indent ~ "        ");
						code ~= indent ~ "    }\n";
						code ~= indent ~ "    else\n";
						code ~= indent ~ "        goto outer_default;\n";
					}
				}
				else
				{
					code ~= indent ~ "    if (!range.canPeek(" ~ text(offset + 1) ~ "))\n";
					code ~= indent ~ "    {\n";
					code ~= generateLeaf(tokens[i][0 .. offset + 1], indent ~ "        ");
					code ~= indent ~ "    }\n";
					code ~= indent ~ "    switch (range.peek(" ~ text(offset + 1) ~ "))\n";
					code ~= indent ~ "    {\n";
					code ~= generateCaseStatements(tokens[i .. j], offset + 1);
					code ~= indent ~ "    default:\n";
					code ~= generateLeaf(tokens[i][0 .. offset + 1], indent ~ "        ");
					code ~= indent ~ "    }\n";
				}
			}
			i = j - 1;
		}
		return code;
	}
	
	static string generateLeaf(string token, string indent)
	{
		string code;
		if (staticTokens.countUntil(token) >= 0)
		{
			code ~= indent ~ "range.popFrontN(" ~ text(token.length) ~ ");\n";
			code ~= indent ~ "return Token(tok!\"" ~ escape(token) ~"\", null, range.line, range.column, range.index);\n";
		}
		else if (pseudoTokens.countUntil(token) >= 0)
			code ~= indent ~ "return postProcess(pseudoTok!\"" ~ escape(token) ~"\");\n";
		else if (possibleDefaultTokens.countUntil(token) >= 0)
		{
			code ~= indent ~ "if (!range.canPeek(" ~ text(token.length) ~ ") || isSeparating(range.peek(" ~ text(token.length) ~ ")))\n";
			code ~= indent ~ "{\n";
			code ~= indent ~ "    range.popFrontN(" ~ text(token.length) ~ ");\n";
			code ~= indent ~ "    return Token(tok!\"" ~ escape(token) ~"\", null, range.line, range.column, range.index);\n";
			code ~= indent ~ "}\n";
			code ~= indent ~ "else\n";
			code ~= indent ~ "    goto outer_default;\n";
		}
		else
			code ~= indent ~ "goto outer_default;\n";
		return code;
	}

	Token front() @property
	{
		return _front;
	}

	void popFront()
	{
		_front = advance();
	}

	bool empty() const nothrow @property
	{
		return _front.type == tok!"\0";
	}

	void registerPostProcess(alias t)(Token delegate(ref RangeType) fun)
	{
		post[pseudoTok!t] = fun;
	}

	template pseudoTok(string symbol)
	{
		static assert (pseudoTokens.countUntil(symbol) >= 0);
		enum index = cast(IDType) pseudoTokens.countUntil(symbol);
		alias index pseudoTok;
	}

	static string escape(string input)
	{
		string rVal;
		foreach (ubyte c; cast(ubyte[]) input)
		{
			switch (c)
			{
			case '\\': rVal ~= `\\`; break;
			case '"': rVal ~= `\"`; break;
			case '\'': rVal ~= `\'`; break;
			case '\t': rVal ~= `\t`; break;
			case '\n': rVal ~= `\n`; break;
			case '\r': rVal ~= `\r`; break;
			default: rVal ~= c; break;
			}
		}
		return rVal;
	}

	Token advance()
	{
		if (range.empty)
			return Token(tok!"\0");
		auto r = range.save;
		lexerLoop: switch (range.front)
		{
		mixin(generateCaseStatements(stupidToArray(sort(staticTokens ~ pseudoTokens ~ possibleDefaultTokens))));
		//pragma(msg, generateCaseStatements(stupidToArray(sort(staticTokens ~ pseudoTokens ~ possibleDefaultTokens))));
		outer_default:
		default:
			range = r;
			return defaultTokenFunction(range);
		}
	}

    /**
     * This only exists because the real array() can't be called at compile-time
     */
	static T[] stupidToArray(R, T = ElementType!R)(R range)
	{
		T[] rVal;
		foreach (v; range)
			rVal ~= v;
		return rVal;
	}

	Token postProcess(IDType i)
	{
		assert (post[i] !is null, "No post-processing function registered for " ~ pseudoTokens[i]);
		return post[i](range);
	}

	Token delegate(ref RangeType)[pseudoTokens.length] post;
	RangeType range;
	Token _front;
}

struct PeekRange(R, size_t peekSupported = 1) if (isRandomAccessRange!R && isForwardRange!R)
{
public:

	this(R range)
	{
		this.range = range;
	}

	bool empty() pure nothrow const @property
	{
		return _index >= range.length;
	}

	ElementType!R front() pure nothrow const @property
	in
	{
		assert (!empty);
	}
	body
	{
		return range[_index];
	}

	void popFront() pure nothrow
	{
		_index++;
		_column++;
	}
	
	void popFrontN(size_t n) pure nothrow
	{
		foreach (i; 0 .. n)
			popFront();
	}

	ElementType!R peek(int offset = 1) pure nothrow const
	in
	{
		assert (canPeek(offset));
	}
	body
	{
		return range[_index + offset];
	}

	bool canPeek(int offset = 1) pure nothrow const
	{
		return _index + offset >= 0 && _index + offset < range.length;
	}

	typeof(this) save() @property
	{
		typeof(this) copy;
		copy.range = range;
		copy._index = _index;
		copy._column = _column;
		copy._line = _line;
		return copy;
	}

	void mark() nothrow pure
	{
		markBegin = index;
	}

	R getMarked() nothrow pure
	{
		return range[markBegin .. index];
	}
	
	void incrementLine() pure nothrow
	{
		_column = 1;
		_line++;
	}
	
	size_t line() pure nothrow const @property { return _line; }
	size_t column() pure nothrow const @property { return _column; }
	size_t index() pure nothrow const @property { return _index; }

private:
	size_t markBegin;
	size_t _column = 1;
	size_t _line = 1;
	size_t _index = 0;
	R range;
}

struct PeekRange(R, size_t peekSupported = 1)
	if (!isRandomAccessRange!R && isForwardRange!R)
{
public:

	this(R range)
	{
		this.range = range;
		for (size_t i = 0; !this.range.empty && i < peekSupported; i++)
		{
			rangeSizeCount++;
			buffer[i] = this.range.front;
			range.popFront();
		}
	}

	ElementType!R front() const @property
	in
	{
		assert (!empty);
	}
	body
	{
		return buffer[bufferIndex];
	}

	void popFront()
	in
	{
		assert (!empty);
	}
	body
	{
		index++;
		column++;
		count++;
		bufferIndex = bufferIndex + 1 > buffer.length ? 0 : bufferIndex + 1;
		if (marking)
			markBuffer.put(buffer[bufferIndex]);
		if (!range.empty)
		{
			buffer[bufferIndex + peekSupported % buffer.length] = range.front();
			range.popFront();
			rangeSizeCount++;
		}
	}

	bool empty() const nothrow pure @property
	{
		return rangeSizeCount == count;
	}

	ElementType!R peek(int offset = 1) pure nothrow const
	in
	{
		assert (canPeek(offset));
	}
	body
	{
		return buffer[(bufferIndex + offset) % buffer.length];
	}

	bool canPeek(int offset = 1) pure nothrow const
	{
		return offset >= 0
			? offset <= peekSupported && count + offset <= rangeSizeCount
			: abs(offset) <= peekSupported && (count - abs(offset)) >= 0;
	}

	typeof(this) save() @property
	{
		typeof(this) newRange;
		newRange.count = count;
		newRange.rangeSizeCount = count;
		newRange.buffer = buffer.dup;
		newRange.bufferIndex = bufferIndex;
		newRange.range = range.save;
		return newRange;
	}

	void mark()
	{
		marking = true;
		markBuffer.clear();
	}

	ElementEncodingType!R[] getMarked()
	{
		marking = false;
		return markBuffer.data;
	}
	
	void incrementLine() pure nothrow
	{
		_column = 1;
		_line++;
	}
	
	size_t line() pure nothrow const @property { return _line; }
	size_t column() pure nothrow const @property { return _column; }
	size_t index() pure nothrow const @property { return _index; }

private:
	auto markBuffer = appender!(ElementType!R[])();
	bool marking;
	size_t count;
	size_t rangeSizeCount;
	ElementType!(R)[(peekSupported * 2) + 1] buffer;
	size_t bufferIndex;
	size_t _column = 1;
	size_t _line = 1;
	size_t _index = 0;
	R range;
}
