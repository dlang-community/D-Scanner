//          Copyright Brian Schott (Hackerpilot) 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dscanner.dscanner_version;

import std.string : strip;

/**
 * Human-readable version number
 */

version (built_with_dub)
{
	enum DSCANNER_VERSION = import("dubhash.txt").strip;
}
else
{
	/**
	 * Current build's Git commit hash
	 */
	enum DSCANNER_VERSION = import("githash.txt").strip;
}
