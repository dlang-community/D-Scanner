//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.dscanner_version;

/**
 * Human-readable version number
 */
enum DEFAUULT_DSCANNER_VERSION = "v0.5.5";

version (built_with_dub)
{
	enum DSCANNER_VERSION = import("dubhash.txt");
}
else version (Windows)
{
	enum DSCANNER_VERSION = DEFAUULT_DSCANNER_VERSION;
}
else
{
	/**
	 * Current build's Git commit hash
	 */
	enum DSCANNER_VERSION = import("githash.txt");
}
