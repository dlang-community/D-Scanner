module analysis.output;

import std.stdio;
import std.algorithm;
import stdx.d.lexer;
import analysis.base;
import analysis.linespan;
import highlighter;

void writeWhitespace(File file, string text, ref uint line, MessageSet messages,
	ref const LineSpans spans)
{
	foreach (char c; text)
	{
		if (c == '\r')
			continue;
		else if (c == '\n')
		{
			if (spans.containsLine(line))
				file.write("\n");
			foreach (message; messages[].filter!(a => a.line == line - 1))
				writeMessage(file, message);
			bool prevWasVisible = false;
			if (spans.containsLine(line))
			{
				prevWasVisible = true;
				file.writef("<span class=\"ln\">%d</span>", line);
			}
			line++;
			if (!spans.containsLine(line) && prevWasVisible)
				file.writeln("<div class=\"separator\"/></div>");
		}
		else if (spans.containsLine(line))
		{
			if (c == '\t')
				file.write("    ");
			else
				file.write(c);
		}
	}
}

void writeStrOrCom(File file, string text, string cssClass, ref uint line,
	ref const LineSpans spans)
{
	file.write("<span class=\"", cssClass, "\">");
	foreach (char c; text)
	{
		if (c == '\r')
			continue;
		else if (c == '\n')
		{
			bool prevWasVisible = false;
			if (spans.containsLine(line))
			{
				prevWasVisible = true;
				file.writef("\n</span><span class=\"ln\">%d</span><span class=\"%s\">", line, cssClass);
			}
			line++;
			if (!spans.containsLine(line) && prevWasVisible)
				file.writeln("<div class=\"separator\"/></div>");
		}
		else if (spans.containsLine(line))
		{
			if (c == '<')
				file.write("&lt;");
			else if (c == '&')
				file.write("&amp;");
			else
				file.write(c);
		}
	}
	file.write("</span>");
}

void writeToken(File file, ref const Token t, ref uint line, MessageSet messages,
	ref const LineSpans spans)
{
	if (t == tok!"whitespace")
		writeWhitespace(file, t.text, line, messages, spans);
	else if (t.type == tok!"comment")
		writeStrOrCom(file, t.text, "com", line, spans);
	else if (isStringLiteral(t.type) || t.type == tok!"characterLiteral")
		writeStrOrCom(file, t.text, "str", line, spans);
	else if (spans.containsLine(line))
	{
		if (isBasicType(t.type))
			file.writeSpan("type", str(t.type));
		else if (isKeyword(t.type))
			file.writeSpan("kwrd", str(t.type));
		else if (isNumberLiteral(t.type))
			file.writeSpan("num", t.text);
		else if (isOperator(t.type))
			file.writeSpan("op", str(t.type));
		else
			file.write(t.text);
	}
}

void writeMessage(File file, ref const Message message)
{
	file.write("<div class=\"warning\">");
	file.write(message.message);
	file.writeln("</div>");
}

void writeHeader(File file)
{
	file.writeln(q"[
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8"/>]");
	file.writeln("<title>D-Scanner Report</title>");
	file.writeln("<style type=\"text/css\">\n", SOLARIZED_CSS, "\n",
		STATIC_ANALYSIS_CSS, "\n</style>");
	file.writeln("</head>");
	file.writeln("<body>");
}

void writeFooter(File file)
{
	file.writeln("</body>");
	file.writeln("</html>");
}

void writeHtmlReport(File file, MessageSet[string] messages, shared(StringCache)* cache)
{
	import std.array;
	import std.conv;
	writeHeader(file);
	writeSummary(file, messages);
	foreach (fileName, messageSet; messages)
	{
		writeln("Processing messages for ", fileName);
		if (messageSet.empty)
			continue;
		File f = File(fileName);
		auto bytes = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(bytes);
		LexerConfig config;
		config.commentBehavior = CommentBehavior.include;
		config.whitespaceBehavior = WhitespaceBehavior.include;
		config.stringBehavior = StringBehavior.source;
		const(Token)[] tokens = byToken(bytes, config, cache).array;
		file.writeln("<div class=\"section\" name=\"", fileName, "\">");
		file.writeln("<h1><a name=\"", fileName, "\">", fileName, "</a></h1>");
		file.writeln("<pre>");
		uint currentLine = 2;
		LineSpans ls = generateLineSpans(messageSet);
		if (ls.containsLine(1))
			file.write("<span class=\"ln\">1</span>");
		foreach (token; tokens)
		{
			writeToken(file, token, currentLine, messageSet, ls);
		}
		file.writeln("</pre>");
		file.writeln("</div>");

	}
	writeFooter(file);
}

void writeSummary(File file, MessageSet[string] messages)
{
	size_t[string] warningCounts;
	bool hasWarnings;
	foreach (fileName, messageSet; messages)
	{
		warningCounts[fileName] = messageSet.length;
		if (messageSet.length > 0)
			hasWarnings = true;
	}
	if (!hasWarnings)
	{
		file.writeln("<div id=\"summary\" class=\"section\">No warnings detected</div>");
		return;
	}
	file.writeln("<div id=\"summary\" class=\"section\">");
	file.writeln("<h1>Summary</h1>");
	file.write("<table>");
	file.writeln("<thead><tr><th>File</th></th><th>Warning Count</th></tr></thead>");
	file.write("<tbody>");
	foreach (fileName, warningCount; warningCounts)
	{
		if (warningCount > 0)
			file.writeln("<tr><td><a href=\"#", fileName, "\">", fileName, "</td><td>", warningCount, "</td></tr>");
	}
	file.write("</tbody>");
	file.write("</table>");
	file.write("</div>");
}

LineSpans generateLineSpans(MessageSet messages)
{
	LineSpans l;
	foreach (message; messages[])
	{
		l.addLine(message.line);
	}
	return l;
}

immutable string STATIC_ANALYSIS_CSS = "
.ln {
	width: 5em;
	display: inline-block;
	margin-left: -4em;
	border-right: .1em solid #839496;
	margin-right: 1em;
}
.warning {
	display: block;
	border-radius: 1em;
	border-top: .1em solid ##dc322f;
	color: #002b36;
	background-color: #fdf6e3;
	padding: 1em;
	margin: .5em 0 -.5em 0;
}
pre {
	padding-left: 5em;
	margin: 0;
	border-radius: 0 0 1em 1em;
}

.section {
	border: .1em solid #839496;
	margin: 2em 0;
	padding: 0;
	border-radius: 1em;
}

.section h1 {
    font-weight: normal;
    color: #002b36;
	background-color: #fdf6e3;
    margin: 0;
    padding: .5em;
    border-radius: 1em 1em 0 0;
    font-size: medium;
}

.separator {
	display: block;
	height: 1em;
	margin: 0;
	padding: 0;
	border-bottom: .1em dashed #fdf6e3;
}
";
