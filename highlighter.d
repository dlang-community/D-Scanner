
//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module highlighter;

import std.stdio;
import std.array;
import std.d.lexer;

void writeSpan(string cssClass, string value)
{
	stdout.write(`<span class="`, cssClass, `">`, value.replace("&", "&amp;").replace("<", "&lt;"), `</span>`);
}


// http://ethanschoonover.com/solarized
void highlight(R)(R tokens)
{
	stdout.writeln(q"[<!DOCTYPE html>
<html>
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
<body>
<style type="text/css">
html  { background-color: #fdf6e3; color: #002b36; }
.kwrd { color: #b58900; font-weight: bold;  }
.com  { color: #93a1a1; font-style: italic; }
.num  { color: #dc322f; font-weigth: bold;  }
.str  { color: #2aa198; font-style: italic; }
.op   { color: #586e75; font-weight: bold;  }
.type { color: #268bd2; font-weight: bold;  }
.cons { color: #859900; font-weight: bold;  }
</style>
<pre>]");

	foreach (Token t; tokens)
	{
		if (isType(t.type))
			writeSpan("type", t.value);
		else if (isKeyword(t.type))
			writeSpan("kwrd", t.value);
		else if (t.type == TokenType.Comment)
			writeSpan("com", t.value);
		else if (isStringLiteral(t.type))
			writeSpan("str", t.value);
		else if (isNumberLiteral(t.type))
			writeSpan("num", t.value);
		else if (isOperator(t.type))
			writeSpan("op", t.value);
		else
			stdout.write(t.value.replace("<", "&lt;"));
	}
	stdout.writeln("</pre>\n</body></html>");
}
