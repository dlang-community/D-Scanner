//          Copyright Brian Schott (Hackerpilot) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module althighlighter;

import std.stdio;
import std.array;
import std.conv;
import std.string;
import dparse.lexer;

// http://ethanschoonover.com/solarized
void highlight(R)(ref R tokens, string fileName)
{
	while (!tokens.empty)
	{
		auto t = tokens.front;
		tokens.popFront();
		if (isBasicType(t.type))
		{
			write("type");
			writeln("," ~ to!string(t.index) ~ "," ~ to!string(str(t.type).length));
		}
		else if (isKeyword(t.type))
		{
			write("keyword");
			writeln("," ~ to!string(t.index) ~ "," ~ to!string(str(t.type).length));
		}
		else if (t.type == tok!"comment")
		{
			write("com");
			writeln("," ~ to!string(t.index) ~ "," ~ to!string(t.text.length));
		}
		else if (isStringLiteral(t.type) || t.type == tok!"characterLiteral")
		{
			write("str");
			writeln("," ~ to!string(t.index) ~ "," ~ to!string(t.text.length));
		}
		else if (isNumberLiteral(t.type))
		{
			write("num");
			writeln("," ~ to!string(t.index) ~ "," ~ to!string(t.text.length));
		}
		else if (isOperator(t.type))
		{
			write("op");
			writeln("," ~ to!string(t.index) ~ "," ~ to!string(str(t.type).length));
		}
		else if (t.type == tok!"specialTokenSequence" || t.type == tok!"scriptLine")
		{
			write("special");
			writeln("," ~ to!string(t.index) ~ "," ~ to!string(t.text.length));
		}
		else
		{
			string text = t.text;
			text = text.replace("<", "&lt;");

			version(Windows)
				text = text.replace("\r", "");

			text = strip(text);

			if (text.length == 0)
				continue;

			write("other");
			writeln("," ~ to!string(t.index) ~ "," ~ to!string(t.text.length));
		}
			
		

		

	}
}

void writeSpan(string cssClass, string value)
{
	version (Windows)
		stdout.write(`<span class="`, cssClass, `">`, value.replace("&",
				"&amp;").replace("<", "&lt;").replace("\r", ""), `</span>`);
	else
		stdout.write(`<span class="`, cssClass, `">`, value.replace("&",
				"&amp;").replace("<", "&lt;"), `</span>`);
}
