/*******************************************************************************
 * The MIT License
 *
 * Copyright (c) 2012 Brian Schott (Sir Alaran)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

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
.type { color: cyan; }
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
		case TokenType.numberLiteral:
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
