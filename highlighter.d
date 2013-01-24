
//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module highlighter;

import std.stdio;
import std.array;
import std.d.lexer;

import langutils;

void writeSpan(string cssClass, string value)
{
	stdout.write(`<span class="`, cssClass, `">`, value.replace("&", "&amp;").replace("<", "&lt;"), `</span>`);
}

void highlight(R)(R tokens)
{
	stdout.writeln(q"[<!DOCTYPE html>
<html>
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
<body>
<style type="text/css">
html { background-color: #fff; color: #222; }
.kwrd { font-weight: bold; color: blue; }
.com { color: green; font-style: italic;}
.num { color: orangered; font-weigth: bold; }
.str { color: red; font-style: italic; }
.op { color: 333; font-weight: bold; }
.type { color: magenta; font-weight: bold; }
</style>
<pre>]");

	foreach (Token t; tokens)
	{
		if (t.type > TokenType.TYPES_BEGIN && t.type < TokenType.TYPES_END)
			writeSpan("type", t.value);
		else if (t.type > TokenType.KEYWORDS_BEGIN && t.type < TokenType.KEYWORDS_END)
			writeSpan("kwrd", t.value);
		else if (t.type == TokenType.Comment)
			writeSpan("com", t.value);
		else if (t.type > TokenType.STRINGS_BEGIN && t.type < TokenType.STRINGS_END)
			writeSpan("str", t.value);
		else if (t.type > TokenType.NUMBERS_BEGIN && t.type < TokenType.NUMBERS_END)
			writeSpan("num", t.value);
		else if (t.type > TokenType.OPERATORS_BEGIN && t.type < TokenType.OPERATORS_END)
			writeSpan("op", t.value);
		else
			stdout.write(t.value.replace("<", "&lt;"));
	}
	stdout.writeln("</pre>\n</body></html>");
}
