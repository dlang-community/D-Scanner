--------------------------------------------------------------------------------
-- The MIT License
--
-- Copyright (c) 2012 Brian Schott
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
--------------------------------------------------------------------------------


local M = {}

_M.common.cstyle = require "common.cstyle"

if type(_G.snippets) == 'table' then
  _G.snippets.dmd = {}
end

if type(_G.keys) == 'table' then
  _G.keys.dmd = {}
end

-- functions icon
M.FUNCTIONS = [[
/* XPM */
static char * function_xpm[] = {
"16 16 54 1",
" 	c None",
".	c #287E03",
"+	c #42C009",
"@	c #2B8303",
"#	c #2E8B04",
"$	c #4BCA0B",
"%	c #308E04",
"&	c #2C8503",
"*	c #47C50A",
"=	c #2E8A04",
"-	c #2F8B04",
";	c #48C70A",
">	c #2C8703",
",	c #2F8C04",
"'	c #4BCB0B",
")	c #2B8403",
"!	c #43C10A",
"~	c #297F03",
"{	c #2E8A03",
"]	c #2B8503",
"^	c #3EBA09",
"/	c #267803",
"(	c #2A8303",
"_	c #42BF09",
":	c #257603",
"<	c #39B408",
"[	c #227103",
"}	c #3FBB09",
"|	c #267903",
"1	c #227203",
"2	c #37B108",
"3	c #216D03",
"4	c #277C03",
"5	c #3DB909",
"6	c #257703",
"7	c #227003",
"8	c #35AF07",
"9	c #206B03",
"0	c #267A03",
"a	c #3CB708",
"b	c #247503",
"c	c #216E03",
"d	c #34AD07",
"e	c #1F6902",
"f	c #257803",
"g	c #3AB508",
"h	c #237303",
"i	c #206C03",
"j	c #32AB07",
"k	c #1E6702",
"l	c #39B308",
"m	c #1F6A02",
"n	c #31A907",
"o	c #1C6502",
"                ",
"                ",
"                ",
"   .+@  #$%     ",
"    &*=  -;>    ",
"     ,',  )!~   ",
"      {*]  .^/  ",
"       (_.  :<[ ",
"       .}|  123 ",
"      456  789  ",
"     0ab  cde   ",
"    fgh  ijk    ",
"   bl[  mno     ",
"                ",
"                ",
"                "};
]]

-- fields icon
M.FIELDS = [[
/* XPM */
static char * class_xpm[] = {
"16 16 41 1",
" 	c None",
".	c #010100",
"+	c #020100",
"@	c #BA1DBA",
"#	c #C120C1",
"$	c #BA1CBA",
"%	c #C824C8",
"&	c #D229D2",
"*	c #C724C7",
"=	c #B91CB9",
"-	c #010000",
";	c #AE16AE",
">	c #BF1FBF",
",	c #D129D1",
"'	c #E231E2",
")	c #E131E1",
"!	c #D028D0",
"~	c #AD16AD",
"{	c #DF2FDF",
"]	c #DE2FDE",
"^	c #CF28CF",
"/	c #BE1EBE",
"(	c #AC15AC",
"_	c #A813A8",
":	c #B71BB7",
"<	c #C622C5",
"[	c #CE27CF",
"}	c #CE27CE",
"|	c #C522C5",
"1	c #A713A7",
"2	c #000000",
"3	c #9E0E9E",
"4	c #BD1EBD",
"5	c #BC1EBC",
"6	c #B61AB6",
"7	c #AC15AB",
"8	c #A612A6",
"9	c #AB15AB",
"0	c #9E0E9D",
"a	c #960A96",
"b	c #990C99",
"                ",
"                ",
"                ",
"      .++.      ",
"     .@##$.     ",
"    .$%&&*=.    ",
"   -;>,')!>~-   ",
"   -~>!{]^/(-   ",
"   -_:<[}|:1-   ",
"   23(:456732   ",
"    23899802    ",
"     2abba2     ",
"      2222      ",
"                ",
"                ",
"                "};

]]

--package icon
M.PACKAGE = [[
/* XPM */
static char * package_xpm[] = {
"16 16 6 1",
" 	c None",
".	c #000100",
"+	c #050777",
"@	c #242BAE",
"#	c #2E36BF",
"$	c #434FE5",
"                ",
"  ............  ",
" .$$$$$$$$$$$$. ",
" .$##@@+$##@@+. ",
" .$#@@@+$#@@@+. ",
" .$@@@#+$@@@#+. ",
" .$@@##+$@@##+. ",
" .$+++++$+++++. ",
" .$$$$$$$$$$$$. ",
" .$##@@+$##@@+. ",
" .$#@@@+$#@@@+. ",
" .$@@@#+$@@@#+. ",
" .$@@##+$@@##+. ",
" .$+++++$+++++. ",
"  ............  ",
"                "};
]]

-- module icon
M.MODULE = [[
/* XPM */
static char * module_xpm[] = {
"16 16 14 1",
" 	c None",
".	c #000000",
"+	c #000100",
"@	c #FFFF83",
"#	c #FFFF00",
"$	c #FFFF28",
"%	c #FFFF6A",
"&	c #FFFF4C",
"*	c #D5D500",
"=	c #CDCD00",
"-	c #A3A300",
";	c #B2B200",
">	c #C3C300",
",	c #919100",
"                ",
"       .+       ",
"      .@#+      ",
"      .@#+      ",
"     .$@##+     ",
"    ..%@##++    ",
"  ..&%%@####++  ",
" .@@@@@%######+ ",
" +*****=-;;;;;+ ",
"  ++>==*;--,..  ",
"    ++=*;-..    ",
"     +>*;,.     ",
"      +*;.      ",
"      +*;.      ",
"       ++       ",
"                "};
]]

M.KEYWORD = [[
/* XPM */
static char * keyword_xpm[] = {
"16 16 24 1",
" 	c None",
".	c #B91C1C",
"+	c #BA1C1C",
"@	c #BE1D1D",
"#	c #C31E1E",
"$	c #C21E1E",
"%	c #F0F0F0",
"&	c #C71E1E",
"*	c #F5F5F5",
"=	c #CC1F1F",
"-	c #FBFBFB",
";	c #CB1F1F",
">	c #CD1F1F",
",	c #FDFDFD",
"'	c #C91F1F",
")	c #F7F7F7",
"!	c #C41E1E",
"~	c #F2F2F2",
"{	c #C01D1D",
"q	c #ECECEC",
"^	c #BB1D1D",
"/	c #E7E7E7",
"(	c #B71C1C",
"_	c #B21B1B",
"                ",
"                ",
"   ..........   ",
"  @@@@@@@@@@@@  ",
"  #$%%%%%%$$#$  ",
"  &&*******&&&  ",
"  ==--==;---==  ",
"  >>,,>>>>,,>>  ",
"  ''))''''))''  ",
"  !!~~!!!~~~!!  ",
"  {{qqqqqqq{{{  ",
"  ^^//////^^^^  ",
"  ((((((((((((  ",
"   __________   ",
"                ",
"                "};
]]


local keywords = {
	"abstract?5", "alias?5", "align?5", "asm?5", "assert?5", "auto?5", "body?5",
	"bool?5", "break?5", "byte?5", "case?5", "cast?5", "catch?5", "cdouble?5",
	"cent?5", "cfloat?5", "char?5", "class?5", "const?5", "continue?5", "creal?5",
	"dchar?5", "debug?5", "default?5", "delegate?5", "delete?5", "deprecated?5",
	"@disable?5", "do?5", "double?5", "dstring?5", "else?5", "enum?5",
	"export?5", "extern?5", "false?5", "__FILE__?5", "finally?5",
	"final?5", "float?5", "foreach_reverse?5", "foreach?5", "for?5", "function?5",
	"goto?5", "__gshared?5", "idouble?5", "ifloat?5", "if?5", "immutable?5",
	"import?5", "inout?5", "interface?5", "in?5", "int?5", "invariant?5",
	"ireal?5", "is?5", "lazy?5", "__LINE__?5", "long?5", "macro?5",
	"mixin?5", "module?5", "new?5", "nothrow?5", "null?5", "out?5",
	"override?5", "package?5", "pragma?5", "private?5", "@property?5",
	"protected?5", "public?5", "pure?5", "real?5",
	"ref?5", "return?5", "@safe?5", "scope?5", "shared?5", "short?5",
	"static?5", "string?5", "struct?5", "super?5", "switch?5", "synchronized?5",
	"@system?5", "template?5", "this?5", "__thread?5", "throw?5",
	"__traits?5", "true?5", "@trusted?5", "try?5", "typedef?5", "typeid?5",
	"typeof?5", "ubyte?5", "ucent?5", "uint?5", "ulong?5", "union?5",
	"unittest?5", "ushort?5", "version?5", "void?5", "volatile?5",
	"wchar?5", "while?5", "with?5", "wstring?5",
}


-- For this module to work the dscanner program must be installed. Configure the
-- path to the executable here
M.PATH_TO_DSCANNER = "dscanner"

_M.textadept.editing.comment_string.dmd = '//'
_M.textadept.run.compile_command.dmd = 'dmd -c -o- %(filename)'
_M.textadept.run.error_detail.dmd = {
	pattern = '^(.-)%((%d+)%): (.+)$',
	filename = 1, line = 2, message = 3
}

local function registerImages()
	buffer:register_image(1, M.FIELDS)
	buffer:register_image(2, M.FUNCTIONS)
	buffer:register_image(3, M.PACKAGE)
	buffer:register_image(4, M.MODULE)
	buffer:register_image(5, M.KEYWORD)
end


local function showCompletionList(r)
	registerImages()
	local setting = buffer.auto_c_choose_single
	buffer.auto_c_choose_single = false;
	buffer.auto_c_max_width = 0
	local completions = {}
	for symbol, kind in r:gmatch("([@%w_]+) (%a)\n") do
		completion = symbol
		if kind == "k" then
			completion = completion .. "?5"
		elseif kind == "v" then
			completion = completion .. "?1"
		elseif kind == "e" then
			completion = completion .. "?1"
		elseif kind == "m" then
			completion = completion .. "?1"
		elseif kind == "c" then
			completion = completion .. "?1"
		elseif kind == "i" then
			completion = completion .. "?1"
		elseif kind == "f" then
			completion = completion .. "?2"
		elseif kind == "M" then
			completion = completion .. "?4"
		elseif kind == "P" then
			completion = completion .. "?3"
		end
		completions[#completions + 1] = completion
	end
	buffer:auto_c_show(0, table.concat(completions, " "))
	buffer.auto_c_choose_single = setting
end

events.connect(events.CHAR_ADDED, function(ch)
	if buffer:get_lexer() ~= "dmd" then return end
	if ch > 255 then return end
	local character = string.char(ch)
	if character == "." or character == "(" then
		local fileName = os.tmpname()
		local tmpFile = io.open(fileName, "w")
		tmpFile:write(buffer:get_text())
		local command = M.PATH_TO_DSCANNER
			.. (character == "." and " --dotComplete " or " --parenComplete ")
			.. fileName .. " " .. buffer.current_pos .. " -I" .. buffer.filename:match(".+[\\/]")
		local p = io.popen(command)
		local r = p:read("*a")
		if r ~= "\n" then
			if character == "." then
				showCompletionList(r)
			elseif character == "(" then
				if r:find("^completions\n") then
					showCompletionList(r)
				elseif r:find("^calltips\n.*") then
					r = r:gsub("^calltips\n", "")
					buffer:call_tip_show(buffer.current_pos, r:gsub("\\n", "\n"):gsub("\\t", "\t"):match("(.*)%s+$"))
				end
			end
		end
		os.remove(fileName)
	end
end)


local function autocomplete()
	registerImages()
	_M.textadept.editing.autocomplete_word("@%w_", keywords)
end

-- D-specific key commands.
keys.dmd = {
	[keys.LANGUAGE_MODULE_PREFIX] = {
		m = { io.open_file,
		(_USERHOME..'/modules/dmd/init.lua'):iconv('UTF-8', _CHARSET) },
	},
	['a\n'] = {_M.common.cstyle.newline},
	['s\n'] = {_M.common.cstyle.newline_semicolon},
	['c;'] = {_M.common.cstyle.endline_semicolon},
	['}'] = {_M.common.cstyle.match_brace_indent},
	['c{'] = {_M.common.cstyle.openBraceMagic, true},
	['cs\n'] = {_M.common.cstyle.closeTagComStr},
	['cM'] = {_M.common.cstyle.selectScope},
	['\n'] = {_M.common.cstyle.enter_key_pressed},
	['c\n'] = {autocomplete},
}

local snippets = _G.snippets

if type(snippets) == 'table' then
	snippets.dmd = {
		gpl = [[/*******************************************************************************
 * Authors: %1(Your name here)
 * Copyright: %1
 * Date: %[date | cut -c 5-10] %[date | cut -c 25-]
 *
 * License:
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License version
 * 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 ******************************************************************************/

%0]],
			gpl3 = [[/*******************************************************************************
 * Authors: %1(Your name here)
 * Copyright: %1
 * Date: %[date | cut -c 5-10] %[date | cut -c 25-]
 *
 * License:
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

%0]],
		mit = [[/*******************************************************************************
 * The MIT License
 *
 * Copyright (c) %[date | cut -c 25-] %1(Your name here)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

%0]],
		boost = [[/*******************************************************************************
 * Boost Software License - Version 1.0 - August 17th, 2003
 *
 * Permission is hereby granted, free of charge, to any person or organization
 * obtaining a copy of the software and accompanying documentation covered by
 * this license (the "Software") to use, reproduce, display, distribute,
 * execute, and transmit the Software, and to prepare derivative works of the
 * Software, and to permit third-parties to whom the Software is furnished to
 * do so, all subject to the following:
 *
 * The copyright notices in the Software and this entire statement, including
 * the above license grant, this restriction and the following disclaimer,
 * must be included in all copies of the Software, in whole or in part, and
 * all derivative works of the Software, unless such copies or derivative
 * works are solely in the form of machine-executable object code generated by
 * a source language processor.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO EVENT
 * SHALL THE COPYRIGHT HOLDERS OR ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE
 * FOR ANY DAMAGES OR OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 ******************************************************************************/
]],
		banner = [[/%<string.rep("*", buffer.edge_column - buffer.column[buffer.current_pos] - 1)>
 * %0
 %<string.rep("*", buffer.edge_column - buffer.column[buffer.current_pos] - 2)>/]],
		fun = [[%1(return type) %2(name)(%3(parameters))
{
	%0
	return ;
}]],
		vfun = [[void %1(name)(%2(parameters))
{
	%0
}]],
		main = [[void main(string[] args)
{
	%0
}]],
		['for'] = [[for (%1(initilization); %2(condition); %3(increment))
{
	%0
}]],
		fore = [[foreach (%1(var); %2(range))
{
	%0
}]],
		forei = [[foreach (%1(i); 0..%2(n))
{
	%0
}]],
		forr = [[foreach (ref %1(var); %2(range))
{
	%0
}]],
		fori = [[for (size_t i = 0; i != %1(condition); ++i)
{
	%0
}]],
		['while'] = [[while (%1(condition))
{
	%0
}]],
		['if'] = [[if (%1(condition))
{
	%0
}]],
		dw = [[do
{
	%0
} while (%1(condition));]],
		switch = [[switch (%1(value))
{
%0
default:
	break;
}]],
		fswitch = [[final switch (%1(value))
{
%0
default:
	break;
}]],
		case = [[case %1:
	%0
	break;]],
		class = [[class %1(name)
{
public:

private:
	%0
}]],
		struct = [[struct %1(name)
{
	%0
}]],
		mem = 'm_%1 = %1;\n%0',
		wf = 'writef(%0);',
		wl = 'writeln(%0);',
		wfl = 'writefln(%0);',
		imp = 'import',
		sta = 'static',
		st = 'string',
		wch = 'wchar',
		dch = 'dchar',
		ch = 'char',
		dou = 'double',
		fl = 'float',
		by = 'byte',
		ret = 'return',
		im = 'immutable',
		co = 'const',
		ty = 'typeof',
		iit = [[if(is(typeof(%1)))
{
	%0
}]],
		itc = [[if(__traits(compiles, %1))
{
	%0
}]],
		sif = [[static if(%1)
{
	%0
}]],
	}
end


function M.set_buffer_properties()
end

return M
