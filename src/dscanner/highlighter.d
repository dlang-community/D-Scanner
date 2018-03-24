//          Copyright Brian Schott (Hackerpilot) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.highlighter;

import std.stdio;
import std.array;
import dparse.lexer;

// http://ethanschoonover.com/solarized
void highlight(R)(ref R tokens, string fileName)
{
	stdout.writeln(q"[
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8"/>]");
	stdout.writeln("<title>", fileName, "</title>");
	stdout.writeln(q"[</head>
<body>
<style type="text/css">
html  { background-color: #fdf6e3; color: #002b36; }
.kwrd { color: #b58900; font-weight: bold;  }
.com  { color: #93a1a1; font-style: italic; }
.num  { color: #dc322f; font-weight: bold;  }
.str  { color: #2aa198; font-style: italic; }
.op   { color: #586e75; font-weight: bold;  }
.type { color: #268bd2; font-weight: bold;  }
.cons { color: #859900; font-weight: bold;  }
</style>
<pre>]");

	while (!tokens.empty)
	{
		auto t = tokens.front;
		tokens.popFront();
		if (isBasicType(t.type))
			writeSpan("type", str(t.type));
		else if (isKeyword(t.type))
			writeSpan("kwrd", str(t.type));
		else if (t.type == tok!"comment")
			writeSpan("com", t.text);
		else if (isStringLiteral(t.type) || t.type == tok!"characterLiteral")
			writeSpan("str", t.text);
		else if (isNumberLiteral(t.type))
			writeSpan("num", t.text);
		else if (isOperator(t.type))
			writeSpan("op", str(t.type));
		else if (t.type == tok!"specialTokenSequence" || t.type == tok!"scriptLine")
			writeSpan("cons", t.text.replace("<", "&lt;"));
		else
		{
			version (Windows)
			{
				// Stupid Windows automatically does a LF → CRLF, so
				// CRLF → CRCRLF, which is obviously wrong.
				// Strip out the CR characters here to avoid this.
				stdout.write(t.text.replace("<", "&lt;").replace("\r", ""));
			}
			else
				stdout.write(t.text.replace("<", "&lt;"));
		}

	}
	stdout.writeln("</pre>\n</body></html>");
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
