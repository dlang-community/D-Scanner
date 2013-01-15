//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module circularbuffer;

import std.math;
import std.array;
import std.range;

struct CircularBuffer(T, R) if (isInputRange!(R) && is (ElementType!(R) == T))
{
public:

    this (size_t size, R range)
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

    T opIndex(size_t index) const
	in
	{
		assert (index <= sourceIndex + margin);
		assert (index >= sourceIndex - margin);
	}
	body
    {
        return data[index % data.length];
    }

    T front() const @property
    {
        return data[index];
    }

	T peek(int offset)
	in
	{
		assert(abs(offset) <= margin);
		assert(sourceIndex + offset >= 0);
	}
	body
	{
		return data[(index + offset) % data.length];
	}

    T popFront()
	in
	{
		assert (!_empty);
	}
	body
    {
		T v = data[index];
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
        return v;
    }

    bool empty() const @property
    {
        return _empty;
    }

private:
    R range;
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
	assert (buf[2] == 2);
	assert (buf[6] == 6);
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

