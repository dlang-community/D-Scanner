//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module location;

import std.string;
import std.path;
import std.file;
import std.stdio;
import std.array;

/**
 * Returns: the absolute path of the given module, or null if it could not be
 *     found.
 */
string findAbsPath(string[] dirs, string moduleName)
{
	// For file names
	if (endsWith(moduleName, ".d") || endsWith(moduleName, ".di"))
	{
		if (isAbsolute(moduleName))
			return moduleName;
		else
			return buildPath(getcwd(), moduleName);
	}

	// Try to find the file name from a module name like "std.stdio"
	foreach(dir; dirs)
	{
		string fileLocation = buildPath(dir, replace(moduleName, ".", dirSeparator));
		string dfile = fileLocation ~ ".d";
		if (exists(dfile) && isFile(dfile))
		{
			return dfile;
		}
		if (exists(fileLocation  ~ ".di") && isFile(fileLocation  ~ ".di"))
		{
			return fileLocation ~ ".di";
		}
	}
	stderr.writeln("Could not locate import ", moduleName, " in ", dirs);
	return null;
}
