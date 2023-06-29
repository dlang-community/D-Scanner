//			Copyright Brian Schott (Hackerpilot) 2012.
// Distributed under the Boost Software License, Version 1.0.
//	  (See accompanying file LICENSE_1_0.txt or copy at
//			http://www.boost.org/LICENSE_1_0.txt)

module dscanner.reports;

import std.json;
import std.algorithm : map;
import std.array : split, array, Appender, appender;

import dscanner.analysis.base : Message, MessageSet;
import dscanner.analysis.stats_collector;

class DScannerJsonReporter
{
	struct Issue
	{
		Message message;
		string type;
	}

	private Appender!(Issue[]) _issues;

	this()
	{
		_issues = appender!(Issue[]);
	}

	void addMessageSet(MessageSet messageSet)
	{
		_issues ~= toIssues(messageSet);
	}

	void addMessage(Message message, bool isError = false)
	{
		_issues ~= toIssue(message, isError);
	}

	string getContent(StatsCollector stats, ulong lineOfCodeCount)
	{
		JSONValue result = [
			"issues" : JSONValue(_issues.data.map!(e => toJson(e)).array),
			"interfaceCount": JSONValue(stats.interfaceCount),
			"classCount": JSONValue(stats.classCount),
			"functionCount": JSONValue(stats.functionCount),
			"templateCount": JSONValue(stats.templateCount),
			"structCount": JSONValue(stats.structCount),
			"statementCount": JSONValue(stats.statementCount),
			"lineOfCodeCount": JSONValue(lineOfCodeCount),
			"undocumentedPublicSymbols": JSONValue(stats.undocumentedPublicSymbols)
		];
		return result.toPrettyString();
	}

	private static JSONValue toJson(Issue issue)
	{
		// dfmt off
		JSONValue js = JSONValue([
			"key": JSONValue(issue.message.key),
			"fileName": JSONValue(issue.message.fileName),
			"line": JSONValue(issue.message.startLine),
			"column": JSONValue(issue.message.startColumn),
			"endLine": JSONValue(issue.message.endLine),
			"endColumn": JSONValue(issue.message.endColumn),
			"message": JSONValue(issue.message.message),
			"type": JSONValue(issue.type),
			"supplemental": JSONValue(
				issue.message.supplemental.map!(a =>
					JSONValue([
						"fileName": JSONValue(a.fileName),
						"line": JSONValue(a.startLine),
						"column": JSONValue(a.startColumn),
						"endLine": JSONValue(a.endLine),
						"endColumn": JSONValue(a.endColumn),
						"message": JSONValue(a.message),
					])
				).array
			)
		]);
		// dfmt on

		if (issue.message.checkName !is null)
		{
			js["name"] = JSONValue(issue.message.checkName);
		}

		return js;
	}

	private static Issue[] toIssues(MessageSet messageSet)
	{
		return messageSet[].map!(e => toIssue(e)).array;
	}

	private static Issue toIssue(Message message, bool isError = false)
	{
		// dfmt off
		Issue issue = {
			message: message,
			type : isError ? "error" : "warn"
		};
		// dfmt on
		return issue;
	}
}

class SonarQubeGenericIssueDataReporter
{
	enum Type
	{
		bug = "BUG",
		vulnerability = "VULNERABILITY",
		codeSmell = "CODE_SMELL"
	}

	enum Severity
	{
		blocker = "BLOCKER",
		critical = "CRITICAL",
		major = "MAJOR",
		minor = "MINOR",
		info = "INFO"
	}

	struct Issue
	{
		string engineId;
		string ruleId;
		Location primaryLocation;
		string type;
		string severity;
		int effortMinutes;
		Location[] secondaryLocations;
	}

	struct Location
	{
		string message;
		string filePath;
		TextRange textRange;
	}

	struct TextRange
	{
		long startLine;
		long endLine;
		long startColumn;
		long endColumn;
	}

	private Appender!(Issue[]) _issues;

	this()
	{
		_issues = appender!(Issue[]);
	}

	void addMessageSet(MessageSet messageSet)
	{
		_issues ~= toIssues(messageSet);
	}
	
	void addMessage(Message message, bool isError = false)
	{
		_issues ~= toIssue(message, isError);
	}

	string getContent()
	{
		JSONValue result = [
			"issues" : JSONValue(_issues.data.map!(e => toJson(e)).array)
		];
		return result.toPrettyString();
	}

	private static JSONValue toJson(Location location)
	{
		return JSONValue([
			"message": JSONValue(location.message),
			"filePath": JSONValue(location.filePath),
			"textRange": JSONValue([
				"startLine": JSONValue(location.textRange.startLine),
				"endLine": JSONValue(location.textRange.endLine),
				"startColumn": JSONValue(location.textRange.startColumn),
				"endColumn": JSONValue(location.textRange.endColumn)
			]),
		]);
	}

	private static JSONValue toJson(Issue issue)
	{
		// dfmt off
		return JSONValue([
			"engineId": JSONValue(issue.engineId),
			"ruleId": JSONValue(issue.ruleId),
			"severity": JSONValue(issue.severity),
			"type": JSONValue(issue.type),
			"primaryLocation": toJson(issue.primaryLocation),
			"secondaryLocations": JSONValue(issue.secondaryLocations.map!toJson.array),
		]);
		// dfmt on
	}

	private static Issue[] toIssues(MessageSet messageSet)
	{
		return messageSet[].map!(e => toIssue(e)).array;
	}

	private static Issue toIssue(Message message, bool isError = false)
	{		
		// dfmt off
		Issue issue = {
			engineId: "dscanner",
			ruleId: message.key,
			severity: (isError) ? Severity.blocker : getSeverity(message.key),
			type: getType(message.key),
			primaryLocation: getLocation(message.diagnostic),
			secondaryLocations: message.supplemental.map!getLocation.array
		};
		// dfmt on
		return issue;
	}

	private static Location getLocation(Message.Diagnostic diag)
	{
		return Location(diag.message, diag.fileName,
			TextRange(diag.startLine, diag.endLine, diag.startColumn, diag.endColumn));
	}

	private static string getSeverity(string key)
	{
		auto a = key.split(".");

		if (a.length <= 1)
		{
			return Severity.major;
		}
		else
		{
			switch (a[1])
			{
				case "style":
					return Severity.minor;
				default:
					return Severity.major;
			}
		}
	}

	private static string getType(string key)
	{
		auto a = key.split(".");

		if (a.length <= 1)
		{
			return Type.bug;
		}
		else
		{
			switch (a[1])
			{
				case "style":
					return Type.codeSmell;
				default:
					return Type.bug;
			}
		}
	}
}