
//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module highlighter;

import std.stdio;
import langutils;
import std.array;

void writeSpan(string cssClass, string value)
{
	stdout.write(`<span class="`, cssClass, `">`, value.replace("<", "&lt;"), `</span>`);
}

void highlight(Token[] tokens)
{
	stdout.writeln(q"[<!DOCTYPE html>
<html>
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
<body>
<style type="text/css">
html { background-color: #111; color: #ccc; }
.keyword { font-weight: bold; color: DeepSkyBlue; }
.comment { color: lightgreen; font-style: italic;}
.number { color: red; font-weigth: bold; }
.string { color: Tomato; font-style: italic; }
.property { color: HotPink; font-weight: bold;}
.operator { color: tan; font-weight: bold; }
.type { color: cyan; font-weight: bold; }
</style>
<pre>]");

	foreach (Token t; tokens)
	{
		switch (t.type)
		{
		case TokenType.KEYWORDS_BEGIN: .. case TokenType.KEYWORDS_END:
			writeSpan("keyword", t.value);
			break;
		case TokenType.TYPES_BEGIN: .. case TokenType.TYPES_END:
			writeSpan("type", t.value);
			break;
		case TokenType.comment:
			writeSpan("comment", t.value);
			break;
		case TokenType.stringLiteral:
			writeSpan("string", t.value);
			break;
		case TokenType.NUMBERS_BEGIN: .. case TokenType.NUMBERS_END:
			writeSpan("number", t.value);
			break;
		case TokenType.OPERATORS_BEGIN: .. case TokenType.OPERATORS_END:
			writeSpan("operator", t.value);
			break;
		case TokenType.PROPERTIES_BEGIN: .. case TokenType.PROPERTIES_END:
			writeSpan("property", t.value);
			break;
		default:
			stdout.write(t.value.replace("<", "&lt;"));
			break;
		}
	}
	stdout.writeln("</pre>\n</body></html>");
}
