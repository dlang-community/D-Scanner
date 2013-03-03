//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module ctags;

void printCtags(Tokens)(File output, ref Tokens tokens)
{
	output.write("!_TAG_FILE_FORMAT 2\n"
		~ "!_TAG_FILE_SORTED 1\n"
		~ "!_TAG_PROGRAM_URL https://github.com/Hackerpilot/Dscanner/\n");

}
