//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module circularbuffer;

import std.math;
import std.array;
import std.range;

class CircularBuffer(T) : InputRange!(T)

{
public:

    this (size_t size, InputRange!(T) range)
    {
        this.range = range;
        this.margin = size;
        data = new T[(margin * 2) + 1];
        if (range.empty())
        {
            _empty = true;
            return;
        }
        for (size_t i = 0; i <= margin && !this.range.empty(); ++i)
        {
            data[i] = this.range.front();
            this.range.popFront();
			end++;
        }
    }

    override T front() const @property
    {
        return data[index];
    }

	T peek(int offset = 1)
	in
	{
		assert(canPeek(offset));
	}
	body
	{
		return data[(index + offset) % data.length];
	}

	bool canPeek(int offset = 1)
	{
		return abs(offset) <= margin && sourceIndex + offset >= 0;
	}

    override void popFront()
	in
	{
		assert (!_empty);
	}
	body
    {
		index = (index + 1) % data.length;
		++sourceIndex;
        if (range.empty())
		{
			if (index == end)
				_empty = true;
		}
        else
        {
			data[end] = range.front();
			end = (end + 1) % data.length;
			range.popFront();
        }
    }

    bool empty() const @property
    {
        return _empty;
    }

	override T moveFront()
	{
		auto r = front();
		popFront();
		return r;
	}

	override int opApply(int delegate(T) dg)
	{
		int result = 0;
		while (!empty)
		{
			result = dg(front);
			if (result)
				break;
		}
		return result;
	}

	override int opApply(int delegate(size_t, T) dg)
	{
		int result = 0;
		int i = 0;
		while (!empty)
		{
			result = dg(i, front);
			if (result)
				break;
		}
		return result;
	}

private:
    InputRange!(T) range;
    immutable size_t margin;
    T[] data;
	size_t sourceIndex;
	size_t end;
    size_t index;
    bool _empty;
}

unittest
{
	int[] items = [1, 2];
	auto buf = CircularBuffer!(int, int[])(5, items);
	auto result = array(buf);
	assert(result == items);
}

unittest
{
    int[] arr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto buf = CircularBuffer!(int, int[])(2, arr);
    assert (buf.data.length == 5);
    auto iterated = array(buf);
	assert (iterated == arr);
}

unittest
{
	int[] arr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
	auto buf = CircularBuffer!(int, int[])(2, arr);
	buf.popFront();
	buf.popFront();
	buf.popFront();
	buf.popFront();
	assert (buf.front == 4);
}

unittest
{
	int[] arr = [0, 1, 2, 3];
	auto buf = CircularBuffer!(int, int[])(2, arr);
	assert (buf.peek(0) == 0);
	assert (buf.peek(1) == 1);
	assert (buf.peek(2) == 2);
	buf.popFront();
	buf.popFront();
	assert (buf.peek(-2) == 0);
	assert (buf.peek(-1) == 1);
	assert (buf.peek(0) == 2);
	assert (buf.peek(1) == 3);
}

