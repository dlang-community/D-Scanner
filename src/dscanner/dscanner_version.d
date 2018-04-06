//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.dscanner_version;

/**
 * Human-readable version number
 */
enum DSCANNER_VERSION = "v0.4.0";

version (built_with_dub)
{
	enum GIT_HASH = import("dubhash.txt");
}
else version (Windows)
{
}
else
{
	/**
	 * Current build's Git commit hash
	 */
	enum GIT_HASH = import("githash.txt");
}
