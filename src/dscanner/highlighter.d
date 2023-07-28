//          Copyright Brian Schott (Hackerpilot) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.highlighter;

import std.stdio;
import std.array;
import dparse.lexer;

// http://ethanschoonover.com/solarized
void highlight(R)(ref R tokens, string fileName, string themeName)
{
	immutable(Theme)* theme = getTheme(themeName);

	stdout.writeln(q"[
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8"/>]");
	stdout.writeln("<title>", fileName, "</title>");
	stdout.writeln(q"[</head>
<body>
<style type="text/css">]");
	stdout.writefln("
html  { background-color: %s; color: %s; }
.kwrd { color: %s; font-weight: bold;  }
.com  { color: %s; font-style: italic; }
.num  { color: %s; font-weight: bold;  }
.str  { color: %s; font-style: italic; }
.op   { color: %s; font-weight: bold;  }
.type { color: %s; font-weight: bold;  }
.cons { color: %s; font-weight: bold;  }
</style>
<pre>", theme.bg, theme.fg, theme.kwrd, theme.com, theme.num, theme.str,
			theme.op, theme.type, theme.cons);

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

struct Theme
{
	string bg;
	string fg;
	string kwrd;
	string com;
	string num;
	string str;
	string op;
	string type;
	string cons;
}

immutable(Theme)* getTheme(string themeName)
{
	immutable Theme[string] themes = [
		"solarized": Theme("#fdf6e3", "#002b36", "#b58900", "#93a1a1", "#dc322f", "#2aa198", "#586e75",
				"#268bd2", "#859900"),
		"solarized-dark": Theme("#002b36", "#fdf6e3", "#b58900", "#586e75", "#dc322f", "#2aa198",
				"#93a1a1", "#268bd2", "#859900"),
		"gruvbox": Theme("#fbf1c7", "#282828", "#b57614", "#a89984", "#9d0006", "#427b58",
				"#504945", "#076678", "#79740e"),
		"gruvbox-dark": Theme("#282828", "#fbf1c7", "#d79921", "#7c6f64",
				"#cc241d", "#689d6a", "#a89984", "#458588", "#98971a")
	];

	immutable(Theme)* theme = themeName in themes;
	// Default theme
	if (theme is null)
		theme = &themes["solarized"];

	return theme;
}
