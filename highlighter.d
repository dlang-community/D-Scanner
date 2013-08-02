
//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module highlighter;

import std.stdio;
import std.array;
import std.range;
import std.d.lexer;

void writeSpan(Sink)(ref Sink sink, string cssClass, string value)
	if(isOutputRange!(Sink, string))
{
	sink.put(`<span class="`);
	sink.put(cssClass);
	sink.put(`">`);
	sink.put(value.replace("&", "&amp;").replace("<", "&lt;"));
	sink.put(`</span>`);
}


void highlight(R)(TokenRange!R tokens, string fileName)
{
	struct StdoutSink
	{
		void put(string data)
		{
			stdout.write(data);
		}
	}
	
	StdoutSink sink;
	highlight(tokens, sink, fileName);
}

// http://ethanschoonover.com/solarized
void highlight(R, Sink)(TokenRange!R tokens, ref Sink sink, string fileName)
	if(isOutputRange!(Sink, string))
{
	sink.put(q"[
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
]");
	sink.put("<title>");
	sink.put(fileName);
	sink.put("</title>\n");
	sink.put(q"[</head>
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
<pre>
]");

	foreach (Token t; tokens)
	{
		if (isBasicType(t.type))
			writeSpan(sink, "type", t.value);
		else if (isKeyword(t.type))
			writeSpan(sink, "kwrd", t.value);
		else if (t.type == TokenType.comment)
			writeSpan(sink, "com", t.value);
		else if (isStringLiteral(t.type) || t.type == TokenType.characterLiteral)
			writeSpan(sink, "str", t.value);
		else if (isNumberLiteral(t.type))
			writeSpan(sink, "num", t.value);
		else if (isOperator(t.type))
			writeSpan(sink, "op", t.value);
		else
			sink.put(t.value.replace("<", "&lt;"));
	}
	sink.put("</pre>\n</body></html>\n");
}

/+void main(string[] args)
{
	LexerConfig config;
	config.tokenStyle = TokenStyle.source;
	config.iterStyle = IterationStyle.everything;
	config.fileName = args[1];
	auto f = File(args[1]);
	(cast(ubyte[]) f.byLine(KeepTerminator.yes).join()).byToken(config).highlight();
}+/
