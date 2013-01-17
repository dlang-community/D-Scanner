//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module cache;

import etc.c.sqlite3;
import std.c.stdlib;
import std.datetime;
import std.file;
import std.uuid;
import std.array;
import std.string;
import std.conv;

import location;
import parser;
import types;
import tokenizer;

private sqlite3* database;

version (Posix)
{
	private immutable char* DB_PATH = "~/.dscanner/cache.db";
}
else version (Windows)
{
	pragma(msg, "Caching not supported on Windows yet");
	immutable string DB_PATH = "";
}

private enum Queries : string
{
	getUpdateTime = "select mtime from files where filepath = ?",
	insertContainer = "insert into containers values ()",
	deleteContainer = "delete from containers where fileId = ?",
	deleteSymbol = "delete from symbols where containerId = ?",
	deleteFile = "delete from files where path = ?",
	getPublicImports = "select importedId from publicImports where importerId = ?",
	getModuleId = "select id from files where path = ?",
	getContainersByModule = "select id from containers where fileId = ?"
}

private sqlite3* getDatabase()
{
	if (database !is null)
		return database;
	int status = sqlite3_open(DB_PATH, &database);
	if (status != SQLITE_OK)
	{
		throw new Exception("Could not open %s: %s".format(DB_PATH,
			sqlite3_errmsg(database)));
	}
	return database;
}

void closeDatabase()
{
	if (database !is null)
	{
		sqlite3_close(database);
		database = null;
	}
}

private long getCachedModTime(sqlite3* db, sqlite3_stmt* statement, string filePath)
{
	bindText(statement, 1, filePath);
	if (sqlite3_step(statement) != SQLITE_ROW)
		throw new Exception("%s".format(sqlite3_errmsg(db)));
	return sqlite3_column_int64(statement, 1);
}

/**
 * Updates the sqlite database with current autocomplete information for the
 * given modules.
 */
void updateCache(string dirs[], string moduleNames[])
{
	string[] filePaths;
	foreach (moduleName; moduleNames)
	{
		string path = findAbsPath(dirs, moduleName);
		if (path is null)
			continue;
		filePaths ~= path;
	}

	sqlite3* db = getDatabase();
	sqlite3_stmt* statement;
	scope(exit) { if (statement) sqlite3_finalize(statement); }
	char* pzTail;
	scope(exit) { if (pzTail) free(pzTail); }
	sqlite3_prepare_v2(db, Queries.getUpdateTime.toStringz(),
		cast(int) Queries.getUpdateTime.length + 1, &statement, &pzTail);

	foreach (string filePath; filePaths)
	{
		immutable long mtime = getCachedModTime(db, statement, filePath);
		SysTime timeLastModified = timeLastModified(filePath);
		// if the times match, we don't need to update the cache.
		if (timeLastModified.stdTime == mtime)
			continue;

		// re-parse the module
		Module m = parseModule(tokenize(readText(filePath)));

		updateCache(m);

		sqlite3_reset(statement);
	}
}

private void updateCache(const Module m)
in
{
	assert(m !is null);
}
body
{
}

private string[] getImportedModules(string modulePath, sqlite3_stmt* statement = null)
{
	auto app = appender!(string[])();
	sqlite3* db = getDatabase();
	bool statementAllocated = false;
	scope(exit) { if (statementAllocated && statement !is null) sqlite3_finalize(statement); }
	if (statement is null)
	{
		statementAllocated = true;
		char* pzTail;
		scope(exit) { if (pzTail) free(pzTail); }
		sqlite3_prepare_v2(db, Queries.getPublicImports.toStringz(),
			cast(int) Queries.getPublicImports.length + 1, &statement, &pzTail);
	}

	string moduleId = getModuleIdFromPath(modulePath);
	bindText(statement, 1, moduleId);
	while (sqlite3_step(statement) == SQLITE_ROW)
	{
		app.put(to!string(sqlite3_column_text(statement, 1)));
	}
	sqlite3_reset(statement);
	foreach (string imported; app.data)
	{
		string[] r = getImportedModules(imported, statement);
	}
	return app.data;
}

private string getModuleIdFromPath(string filePath)
{
	sqlite3* db = getDatabase();
	sqlite3_stmt* statement;
	char* pzTail;
	scope(exit) if (pzTail) free(pzTail);
	sqlite3_prepare_v2(db, Queries.getModuleId.toStringz(),
		cast(int) Queries.getModuleId.length + 1, &statement,
		&pzTail);
	bindText(statement, 1, filePath);
	if (sqlite3_step(statement) != SQLITE_ROW)
		return null;
	return to!string(sqlite3_column_text(statement, 1));
}

/**
 * Returns: the container IDs of the containers that have
 * been imported
 */
public string[] getContainersImported(string modulePath)
{
	immutable string moduleId = getModuleIdFromPath(modulePath);
	sqlite3* db = getDatabase();
	sqlite3_stmt* statement;
	char* pzTail;
	scope(exit) if (pzTail) free(pzTail);
	string[] moduleIds = getImportedModules(modulePath);
	string[] containerIds;
	foreach (string id; moduleIds)
	{
		containerIds ~= getContainersByModule(id);
	}
	return containerIds;
}

private string[] getContainersByModule(string moduleId)
{
	sqlite3* db = getDatabase();
	sqlite3_stmt* statement;
	scope(exit) if (statement !is null) sqlite3_finalize(statement);
	char* pzTail;
	prepareStatement(db, statement, Queries.getContainersByModule);
	bindText(statement, 1, moduleId);
	string[] rVal;
	while (sqlite3_step(statement) == SQLITE_ROW)
	{
		rVal ~= to!string(sqlite3_column_text(statement, 1));
	}
	return rVal;
}

private void prepareStatement(sqlite3* db, sqlite3_stmt* statement, string query)
{
	char* pzTail;
	scope(exit) if (pzTail) free(pzTail);
	sqlite3_prepare_v2(db, query.toStringz(), cast(int) query.length + 1,
		&statement, &pzTail);
}

private void bindText(sqlite3_stmt* statement, int argPos, string text)
{
	sqlite3_bind_text(statement, argPos, text.toStringz(),
		cast(int) text.length + 1, SQLITE_TRANSIENT);
}
