// Written in the D programming language

/**
 * This module contains a range-based _lexer for the D programming language.
 *
 * For performance reasons the _lexer contained in this module operates only on
 * ASCII and UTF-8 encoded source code. If the use of other encodings is
 * desired, the source code must be converted to UTF-8 before passing it to this
 * _lexer.
 *
 * To use the _lexer, create a LexerConfig struct
 * ---
 * LexerConfig config;
 * config.iterStyle = IterationStyle.everything;
 * config.tokenStyle = IterationStyle.source;
 * config.versionNumber = 2061;
 * config.vendorString = "Lexer Example";
 * ---
 * Once you have configured the _lexer, call byToken$(LPAREN)$(RPAREN) on your
 * source code, passing in the configuration.
 * ---
 * auto source = "import std.stdio;"c;
 * auto tokens = byToken(source, config);
 * ---
 * The result of byToken$(LPAREN)$(RPAREN) is a forward range of tokens that can
 * be used easily with the algorithms from std.algorithm or iterated over with
 * $(D_KEYWORD foreach)
 * ---
 * assert (tokens.front.type == TokenType.import_);
 * assert (tokens.front.value == "import");
 * assert (tokens.front.line == 1);
 * assert (tokens.front.startIndex == 0);
 * ---
 *
 * Examples:
 *
 * Generate HTML markup of D code.
 * ---
 * module highlighter;
 *
 * import std.stdio;
 * import std.array;
 * import std.d.lexer;
 *
 * void writeSpan(string cssClass, string value)
 * {
 *     stdout.write(`<span class="`, cssClass, `">`, value.replace("&", "&amp;").replace("<", "&lt;"), `</span>`);
 * }
 *
 *
 * // http://ethanschoonover.com/solarized
 * void highlight(R)(R tokens)
 * {
 *     stdout.writeln(q"[<!DOCTYPE html>
 * <html>
 * <head>
 * <meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
 * <body>
 * <style type="text/css">
 * html  { background-color: #fdf6e3; color: #002b36; }
 * .kwrd { color: #b58900; font-weight: bold;  }
 * .com  { color: #93a1a1; font-style: italic; }
 * .num  { color: #dc322f; font-weigth: bold;  }
 * .str  { color: #2aa198; font-style: italic; }
 * .op   { color: #586e75; font-weight: bold;  }
 * .type { color: #268bd2; font-weight: bold;  }
 * .cons { color: #859900; font-weight: bold;  }
 * </style>
 * <pre>]");
 *
 *     foreach (Token t; tokens)
 *     {
 *         if (isType(t.type))
 *             writeSpan("type", t.value);
 *         else if (isKeyword(t.type))
 *             writeSpan("kwrd", t.value);
 *         else if (t.type == TokenType.comment)
 *             writeSpan("com", t.value);
 *         else if (isStringLiteral(t.type))
 *             writeSpan("str", t.value);
 *         else if (isNumberLiteral(t.type))
 *             writeSpan("num", t.value);
 *         else if (isOperator(t.type))
 *             writeSpan("op", t.value);
 *         else
 *             stdout.write(t.value.replace("<", "&lt;"));
 *     }
 *     stdout.writeln("</pre>\n</body></html>");
 * }
 *
 * void main(string[] args)
 * {
 *     LexerConfig config;
 *     config.tokenStyle = TokenStyle.source;
 *     config.iterStyle = IterationStyle.everything;
 *     config.fileName = args[1];
 *     auto f = File(args[1]);
 *     (cast(ubyte[]) f.byLine(KeepTerminator.yes).join()).byToken(config).highlight();
 * }
 * ---
 *
 * Copyright: Brian Schott 2013
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt Boost, License 1.0)
 * Authors: Brian Schott
 * Source: $(PHOBOSSRC std/d/_lexer.d)
 */

module std.d.lexer;

import std.algorithm;
import std.ascii;
import std.conv;
import std.d.entities;
import std.datetime;
import std.exception;
import std.range;
import std.string;
import std.traits;
import std.uni;
import std.utf;
import std.regex;

import std.stdio;

public:

/**
 * Represents a D token
 */
struct Token
{
    /**
     * The token type.
     */
    TokenType type;

    /**
     * The representation of the token in the original source code.
     */
    string value;

    /**
     * The number of the line the token is on.
     */
    uint line;

    /**
     * The column number of the start of the token in the original source.
     * $(LPAREN)measured in ASCII characters or UTF-8 code units$(RPAREN)
     */
    uint column;

    /**
     * The index of the start of the token in the original source.
     * $(LPAREN)measured in ASCII characters or UTF-8 code units$(RPAREN)
     */
    uint startIndex;

    /**
     * Check to see if the token is of the same type and has the same string
     * representation as the given token.
     */
    bool opEquals(ref const(Token) other) const
    {
        return other.type == type && other.value == value;
    }

    /**
     * Checks to see if the token's string representation is equal to the given
     * string.
     */
    bool opEquals(string value) const { return this.value == value; }

    /**
     * Checks to see if the token is of the given type.
     */
    bool opEquals(TokenType type) const { return type == type; }

    /**
     * Comparison operator orders tokens by start index.
     */
    int opCmp(size_t i) const
    {
        if (startIndex < i) return -1;
        if (startIndex > i) return 1;
        return 0;
    }
}

/**
 * Configure the behavior of the byToken() function. These flags may be
 * combined using a bitwise or.
 */
enum IterationStyle
{
    /// Only include code, not whitespace or comments
    codeOnly = 0,
    /// Includes comments
    includeComments = 0b0001,
    /// Includes whitespace
    includeWhitespace = 0b0010,
    /// Include $(LINK2 http://dlang.org/lex.html#specialtokens, special tokens)
    includeSpecialTokens = 0b0100,
    /// Do not stop iteration on reaching the ___EOF__ token
    ignoreEOF = 0b1000,
    /// Include everything
    everything = includeComments | includeWhitespace | ignoreEOF
}

/**
 * Configuration of the token lexing style. These flags may be combined with a
 * bitwise or.
 */
enum TokenStyle : uint
{
    /**
     * Escape sequences will be replaced with their equivalent characters,
     * enclosing quote characters will not be included. Special tokens such as
     * __VENDOR__ will be replaced with their equivalent strings. Useful for
     * creating a compiler or interpreter.
     */
    default_ = 0b0000,

    /**
     * Escape sequences will not be processed. An escaped quote character will
     * not terminate string lexing, but it will not be replaced with the quote
     * character in the token.
     */
    notEscaped = 0b0001,

    /**
     * Strings will include their opening and closing quote characters as well
     * as any prefixes or suffixes $(LPAREN)e.g.: $(D_STRING "abcde"w) will
     * include the $(D_STRING 'w') character as well as the opening and closing
     * quotes$(RPAREN)
     */
    includeQuotes = 0b0010,

    /**
     * Do not replace the value field of the special tokens such as ___DATE__
     * with their string equivalents.
     */
    doNotReplaceSpecial = 0b0100,

    /**
     * Strings will be read exactly as they appeared in the source, including
     * their opening and closing quote characters. Useful for syntax
     * highlighting.
     */
    source = notEscaped | includeQuotes | doNotReplaceSpecial
}

/**
 * Lexer configuration
 */
struct LexerConfig
{
    /**
     * Iteration style
     */
    IterationStyle iterStyle = IterationStyle.codeOnly;

    /**
     * Token style
     */
    TokenStyle tokenStyle = tokenStyle.default_;

    /**
     * Replacement for the ___VERSION__ token. Defaults to 1.
     */
    uint versionNumber = 1;

    /**
     * Replacement for the ___VENDOR__ token. Defaults to $(D_STRING "std.d.lexer")
     */
    string vendorString = "std.d.lexer";

    /**
     * Name used when creating error messages that are sent to errorFunc. This
     * is needed because the lexer operates on any forwarad range of ASCII
     * characters or UTF-8 code units and does not know what to call its input
     * source. Defaults to the empty string.
     */
    string fileName = "";

    /**
     * This function is called when an error is encountered during lexing.
     * Parameters are file name, code uint index, line number, column,
     * and error messsage.
     */
    void delegate(string, uint, uint, uint, string) errorFunc;

    /**
     * Initial size of the lexer's internal token buffer in bytes. The lexer
     * will grow this buffer if necessary.
     */
    size_t bufferSize = 1024 * 4;
}

/**
 * Iterate over the given range of characters by D tokens.
 * Params:
 *     range = the range of characters
 *     config = the lexer configuration
 * Returns:
 *     an input range of tokens
 */
TokenRange!(R) byToken(R)(R range, LexerConfig config) if (isForwardRange!(R))
{
    auto r = TokenRange!(R)(range);
    r.config = config;
    r.lineNumber = 1;
    r.popFront();
    return r;
}

/**
 * Range of tokens. Use byToken$(LPAREN)$(RPAREN) to instantiate.
 */
struct TokenRange(R) if (isForwardRange!(R))
{
    /**
     * Returns: true if the range is empty
     */
    bool empty() const @property
    {
        return _empty;
    }

    /**
     * Returns: the current token
     */
    Token front() const @property
    {
        enforce(!_empty, "Cannot call front() on empty token range");
        return current;
    }

    /**
     * Returns the current token and then removes it from the range
     */
    Token moveFront()
    {
        auto r = front();
        popFront();
        return r;
    }

    /**
     * Range operation
     */
    int opApply(int delegate(Token) dg)
    {
        int result = 0;
        while (!empty)
        {
            result = dg(front);
            if (result)
                break;
            popFront();
        }
        return result;
    }

    /**
     * Range operation
     */
    int opApply(int delegate(size_t, Token) dg)
    {
        int result = 0;
        int i = 0;
        while (!empty)
        {
            result = dg(i, front);
            if (result)
                break;
            popFront();
        }
        return result;
    }

    /**
     * Removes the current token from the range
     */
    void popFront()
    {
        // Filter out tokens we don't care about
        loop: do
        {
            advance();
            switch (current.type)
            {
            case TokenType.whitespace:
                if (config.iterStyle & IterationStyle.includeWhitespace)
                    break loop;
                break;
            case TokenType.comment:
                if (config.iterStyle & IterationStyle.includeComments)
                    break loop;
                break;
            case TokenType.specialTokenSequence:
                if (config.iterStyle & IterationStyle.includeSpecialTokens)
                    break loop;
                break;
            default:
                break loop;
            }
        }
        while (!empty());
    }

private:

    this(ref R range)
    {
        this.range = range;
        buffer = new ubyte[config.bufferSize];
    }

    /*
     * Advances the range to the next token
     */
    void advance()
    {
        if (range.empty)
        {
            _empty = true;
            return;
        }

        bufferIndex = 0;
        current.line = lineNumber;
        current.startIndex = index;
        current.column = column;
        current.value = null;

        if (std.ascii.isWhite(range.front))
        {
            lexWhitespace();
            return;
        }
        outer: switch (range.front)
        {
        mixin(generateCaseTrie(
            "=",    "TokenType.assign",
            "@",    "TokenType.at",
            "&",    "TokenType.bitAnd",
            "&=",   "TokenType.bitAndEquals",
            "|",    "TokenType.bitOr",
            "|=",   "TokenType.bitOrEquals",
            "~=",   "TokenType.catEquals",
            ":",    "TokenType.colon",
            ",",    "TokenType.comma",
            "--",   "TokenType.decrement",
            "$",    "TokenType.dollar",
            "==",   "TokenType.equals",
            "=>",   "TokenType.goesTo",
            ">",    "TokenType.greater",
            ">=",   "TokenType.greaterEqual",
            "++",   "TokenType.increment",
            "{",    "TokenType.lBrace",
            "[",    "TokenType.lBracket",
            "<",    "TokenType.less",
            "<=",   "TokenType.lessEqual",
            "<>=",  "TokenType.lessEqualGreater",
            "<>",   "TokenType.lessOrGreater",
            "&&",   "TokenType.logicAnd",
            "||",   "TokenType.logicOr",
            "(",    "TokenType.lParen",
            "-",    "TokenType.minus",
            "-=",   "TokenType.minusEquals",
            "%",    "TokenType.mod",
            "%=",   "TokenType.modEquals",
            "*=",   "TokenType.mulEquals",
            "!",    "TokenType.not",
            "!=",   "TokenType.notEquals",
            "!>",   "TokenType.notGreater",
            "!>=",  "TokenType.notGreaterEqual",
            "!<",   "TokenType.notLess",
            "!<=",  "TokenType.notLessEqual",
            "!<>",  "TokenType.notLessEqualGreater",
            "+",    "TokenType.plus",
            "+=",   "TokenType.plusEquals",
            "^^",   "TokenType.pow",
            "^^=",  "TokenType.powEquals",
            "}",    "TokenType.rBrace",
            "]",    "TokenType.rBracket",
            ")",    "TokenType.rParen",
            ";",    "TokenType.semicolon",
            "<<",   "TokenType.shiftLeft",
            "<<=",  "TokenType.shiftLeftEqual",
            ">>",   "TokenType.shiftRight",
            ">>=",  "TokenType.shiftRightEqual",
            "*",    "TokenType.star",
            "?",    "TokenType.ternary",
            "~",    "TokenType.tilde",
            "!<>=", "TokenType.unordered",
            ">>>",  "TokenType.unsignedShiftRight",
            ">>>=", "TokenType.unsignedShiftRightEqual",
            "^",    "TokenType.xor",
            "^=",   "TokenType.xorEquals",
        ));
        case '/':
            auto r = range.save();
            r.popFront();
            if (r.isEoF())
            {
                current.type = TokenType.div;
                current.value = "/";
                range.popFront();
                ++index;
                break;
            }
            switch (r.front)
            {
            case '/':
            case '*':
            case '+':
                lexComment();
                break outer;
            case '=':
                current.type = TokenType.divEquals;
                current.value = "/=";
                range.popFront();
                range.popFront();
                index += 2;
                break outer;
            default:
                current.type = TokenType.div;
                current.value = "/";
                ++index;
                range.popFront();
                break outer;
            }
        case '.':
            auto r = range.save();
            r.popFront();
            if (r.isEoF())
            {
                current.type = TokenType.dot;
                current.value = getTokenValue(TokenType.dot);
                range.popFront();
                ++index;
                break outer;
            }
            else if (r.front >= '0' && r.front <= '9')
            {
                lexNumber();
                break outer;
            }
            else if (r.front == '.')
            {
                current.type = TokenType.slice;
                r.popFront();
                if (r.front == '.')
                {
                    current.type = TokenType.vararg;
                    range.popFront();
                    range.popFront();
                    range.popFront();
                    index += 3;
                }
                else
                {

                    range.popFront();
                    range.popFront();
                    index += 2;
                }
                current.value = getTokenValue(current.type);
            }
            else
            {
                range.popFront();
                current.type = TokenType.dot;
                current.value = getTokenValue(TokenType.dot);
            }
            break;
        case '0': .. case '9':
            lexNumber();
            break;
        case '\'':
        case '"':
        case '`':
            lexString();
            break;
        case 'q':
            auto r = range.save;
            r.popFront();
            if (!r.isEoF() && r.front == '{')
            {
                lexTokenString();
                break;
            }
            else if (!r.isEoF() && r.front == '"')
            {
                lexDelimitedString();
                break;
            }
            else
                goto default;
        case 'r':
            auto r = range.save();
            r.popFront();
            if (!r.isEoF() && r.front == '"')
            {
                lexString();
                break;
            }
            else
                goto default;
        case 'x':
            auto r = range.save();
            r.popFront();
            if (!r.isEoF() && r.front == '"')
            {
                lexHexString();
                break;
            }
            else
                goto default;
        case '#':
            lexSpecialTokenSequence();
            break;
        default:
            while(!range.isEoF() && !isSeparating(range.front))
            {
                keepChar();
            }

            current.type = lookupTokenType(cast(char[]) buffer[0 .. bufferIndex]);
            current.value = getTokenValue(current.type);
            if (current.value is null)
                current.value = (cast(char[]) buffer[0 .. bufferIndex]).idup;

            if (!(config.iterStyle & IterationStyle.ignoreEOF) && current.type == TokenType.eof)
            {
                _empty = true;
                return;
            }

            if (!(config.iterStyle & TokenStyle.doNotReplaceSpecial))
                break;

            switch (current.type)
            {
            case TokenType.date:
                current.type = TokenType.stringLiteral;
                auto time = Clock.currTime();
                current.value = format("%s %02d %04d", time.month, time.day, time.year);
                break;
            case TokenType.time:
                auto time = Clock.currTime();
                current.type = TokenType.stringLiteral;
                current.value = (cast(TimeOfDay)(time)).toISOExtString();
                break;
            case TokenType.timestamp:
                auto time = Clock.currTime();
                auto dt = cast(DateTime) time;
                current.type = TokenType.stringLiteral;
                current.value = format("%s %s %02d %02d:%02d:%02d %04d",
                    dt.dayOfWeek, dt.month, dt.day, dt.hour, dt.minute,
                    dt.second, dt.year);
                break;
            case TokenType.vendor:
                current.type = TokenType.stringLiteral;
                current.value = config.vendorString;
                break;
            case TokenType.compilerVersion:
                current.type = TokenType.stringLiteral;
                current.value = format("%d", config.versionNumber);
                break;
            case TokenType.line:
                current.type = TokenType.intLiteral;
                current.value = format("%d", current.line);
                break;
            case TokenType.file:
                current.type = TokenType.stringLiteral;
                current.value = config.fileName;
                break;
            default:
                break;
            }
            break;
        }
    }

    void lexWhitespace()
    {
        current.type = TokenType.whitespace;
        while (!isEoF(range) && std.ascii.isWhite(range.front))
        {
            keepChar();
        }
        if (config.iterStyle & IterationStyle.includeWhitespace)
            current.value = (cast(char[]) buffer[0..bufferIndex]).idup;
    }

    void lexComment()
    in
    {
        assert (range.front == '/');
    }
    body
    {
        current.type = TokenType.comment;
        keepChar();
        switch(range.front)
        {
        case '/':
            while (!isEoF(range) && !isNewline(range))
            {
                keepChar();
            }
            break;
        case '*':
            while (!isEoF(range))
            {
                if (range.front == '*')
                {
                    keepChar();
                    if (range.front == '/')
                    {
                        keepChar();
                        break;
                    }
                }
                else
                    keepChar();
            }
            break;
        case '+':
            int depth = 1;
            while (depth > 0 && !isEoF(range))
            {
                if (range.front == '+')
                {
                    keepChar();
                    if (range.front == '/')
                    {
                        keepChar();
                        --depth;
                    }
                }
                else if (range.front == '/')
                {
                    keepChar();
                    if (range.front == '+')
                    {
                        keepChar();
                        ++depth;
                    }
                }
                else
                    keepChar();
            }
            break;
        default:
            assert(false);
        }
        if (config.iterStyle & IterationStyle.includeComments)
            current.value = (cast(char[]) buffer[0 .. bufferIndex]).idup;
    }

    void lexHexString()
    in
    {
        assert (range.front == 'x');
    }
    body
    {
        current.type = TokenType.stringLiteral;
        size_t i;
        if (config.tokenStyle & TokenStyle.includeQuotes)
        {
            buffer[i++] = 'x';
            buffer[i++] = '"';
        }
        range.popFront();
        range.popFront();
        index += 2;
        while (!range.isEoF())
        {
            if (i >= buffer.length)
            {
                errorMessage("Hex string constant exceeded buffer size");
                return;
            }
            else if (isHexDigit(range.front))
            {
                keepChar();
            }
            else if (std.ascii.isWhite(range.front) && (config.tokenStyle & TokenStyle.notEscaped))
            {
                keepChar();
            }
            else if (range.front == '"')
            {
                if (config.tokenStyle & TokenStyle.includeQuotes)
                    buffer[i++] = '"';
                range.popFront();
                ++index;
                break;
            }
            else
            {
                errorMessage(format("Invalid character '%s' in hex string literal",
                    cast(char) range.front));
            }
        }
        if (!range.isEoF())
        {
            switch (range.front)
            {
            case 'w':
                current.type = TokenType.wstringLiteral;
                goto case 'c';
            case 'd':
                current.type = TokenType.dstringLiteral;
                goto case 'c';
            case 'c':
                if (config.tokenStyle & TokenStyle.includeQuotes)
                    buffer[i++] = range.front;
                range.popFront();
                ++index;
                break;
            default:
                break;
            }
        }
        if (config.tokenStyle & TokenStyle.notEscaped)
            current.value = (cast(char[]) buffer[0 .. i]).idup;
        else
        {
            auto a = appender!(ubyte[])();
            foreach (b; std.range.chunks(buffer[0 .. i], 2))
            {
                string s = to!string(cast(char[]) b);
                a.put(cast(ubyte[]) to!string(cast(dchar) parse!uint(s, 16)));
            }
            current.value = to!string(cast(char[]) a.data);
        }
    }

    void lexNumber()
    in
    {
        assert(isDigit(cast(char) range.front) || range.front == '.');
    }
    body
    {
        // hex and binary can start with zero, anything else is decimal
        if (range.front != '0')
            lexDecimal();
        else
        {
            auto r = range.save();
            r.popFront();
            switch (r.front)
            {
            case 'x':
            case 'X':
                keepChar();
                keepChar();
                lexHex();
                break;
            case 'b':
            case 'B':
                keepChar();
                keepChar();
                lexBinary();
                break;
            default:
                lexDecimal();
                return;
            }
        }
    }

    void lexFloatSuffix()
    {
        switch (range.front)
        {
        case 'L':
            keepChar();
            current.type = TokenType.doubleLiteral;
            break;
        case 'f':
        case 'F':
            keepChar();
            current.type = TokenType.floatLiteral;
            break;
        default:
            break;
        }
        if (!range.isEoF() && range.front == 'i')
        {
            keepChar();
            if (current.type == TokenType.floatLiteral)
                current.type = TokenType.ifloatLiteral;
            else
                current.type = TokenType.idoubleLiteral;
        }
    }

    void lexIntSuffix()
    {
        bool foundU;
        bool foundL;
        while (!range.isEoF())
        {
            switch (range.front)
            {
            case 'u':
            case 'U':
                if (foundU)
                    return;
                switch (current.type)
                {
                case TokenType.intLiteral:
                    current.type = TokenType.uintLiteral;
                    keepChar();
                    break;
                case TokenType.longLiteral:
                    current.type = TokenType.ulongLiteral;
                    keepChar();
                    break;
                default:
                    return;
                }
                foundU = true;
                break;
            case 'L':
                if (foundL)
                    return;
                switch (current.type)
                {
                case TokenType.intLiteral:
                    current.type = TokenType.longLiteral;
                    keepChar();
                    break;
                case TokenType.uintLiteral:
                    current.type = TokenType.ulongLiteral;
                    keepChar();
                    break;
                default:
                    return;
                }
                foundL = true;
                break;
            default:
                return;
            }
        }
    }

    void lexExponent()
    in
    {
        assert (range.front == 'e' || range.front == 'E' || range.front == 'p'
            || range.front == 'P');
    }
    body
    {
        keepChar();
        bool foundSign = false;
        while (!range.isEoF())
        {
            switch (range.front)
            {
            case '-':
            case '+':
                if (foundSign)
                    return;
                foundSign = true;
                keepChar();
            case '0': .. case '9':
            case '_':
                keepChar();
                break;
            case 'L':
            case 'f':
            case 'F':
            case 'i':
                lexFloatSuffix();
                return;
            default:
                return;
            }
        }
    }

    void lexDecimal()
    in
    {
        assert ((range.front >= '0' && range.front <= '9') || range.front == '.');
    }
    body
    {
        bool foundDot = false;
        current.type = TokenType.intLiteral;
        scope(exit) current.value = (cast(char[]) buffer[0 .. bufferIndex]).idup;
        decimalLoop: while (!range.isEoF())
        {
            switch (range.front)
            {
            case '0': .. case '9':
            case '_':
                keepChar();
                break;
            case 'i':
            case 'L':
                if (foundDot)
                {
                    lexFloatSuffix();
                    return;
                }
                else
                {
                    lexIntSuffix();
                    return;
                }
            case 'f':
            case 'F':
                lexFloatSuffix();
                return;
            case 'e':
            case 'E':
                lexExponent();
                return;
            case '.':
                auto r = range.save();
                r.popFront();
                if (!r.isEoF() && r.front == '.')
                    break decimalLoop; // possibly slice expression
                if (foundDot)
                    break decimalLoop; // two dots with other characters between them
                keepChar();
                foundDot = true;
                current.type = TokenType.doubleLiteral;
                break;
            default:
                break decimalLoop;
            }
        }

    }

    void lexBinary()
    {
        current.type = TokenType.intLiteral;
        scope(exit) current.value = (cast(char[]) buffer[0 .. bufferIndex]).idup;
        binaryLoop: while (!range.isEoF())
        {
            switch (range.front)
            {
            case '0':
            case '1':
            case '_':
                keepChar();
                break;
            case 'u':
            case 'U':
            case 'L':
                lexIntSuffix();
                return;
            default:
                break binaryLoop;
            }
        }
    }

    void lexHex()
    {
        current.type = TokenType.intLiteral;
        scope(exit) current.value = (cast(char[]) buffer[0 .. bufferIndex]).idup;
        bool foundDot;
        hexLoop: while (!range.isEoF())
        {
            switch (range.front)
            {
            case 'a': .. case 'f':
            case 'A': .. case 'F':
            case '0': .. case '9':
            case '_':
                keepChar();
                break;
            case 'i':
            case 'L':
                if (foundDot)
                {
                    lexFloatSuffix();
                    return;
                }
                else
                {
                    lexIntSuffix();
                    return;
                }
            case 'p':
            case 'P':
                lexExponent();
                return;
            case '.':
                auto r = range.save();
                r.popFront();
                if (!r.isEoF() && r.front == '.')
                    break hexLoop; // slice expression
                if (foundDot)
                    break hexLoop; // two dots with other characters between them
                keepChar();
                foundDot = true;
                current.type = TokenType.doubleLiteral;
                break;
            default:
                break hexLoop;
            }
        }
    }

    void lexStringSuffix()
    {
        current.type = TokenType.stringLiteral;
        if (!range.isEoF())
        {
            switch (range.front)
            {
            case 'w':
                current.type = TokenType.wstringLiteral;
                goto case 'c';
            case 'd':
                current.type = TokenType.dstringLiteral;
                goto case 'c';
            case 'c':
                keepChar();
                break;
            default:
                break;
            }
        }
    }

    void lexString()
    in
    {
        assert (range.front == '\'' || range.front == '"' || range.front == '`' || range.front == 'r');
    }
    body
    {
        current.type = TokenType.stringLiteral;
        bool isWysiwyg = range.front == 'r' || range.front == '`';
        if (range.front == 'r')
            keepChar();

        scope (exit)
        {
            if (config.tokenStyle & TokenStyle.includeQuotes)
                current.value = (cast(char[]) buffer[0..bufferIndex]).idup;
            else
            {
                if (buffer[0] == 'r')
                    current.value = (cast(char[]) buffer[2..bufferIndex - 1]).idup;
                else
                    current.value = (cast(char[]) buffer[1..bufferIndex - 1]).idup;
            }
        }

        auto quote = range.front;
        keepChar();
        while (true)
        {
            if (range.isEoF())
            {
                errorMessage("Unterminated string literal");
                return;
            }
            else if (range.front == '\\' && !isWysiwyg)
            {
                if (config.tokenStyle & TokenStyle.notEscaped)
                {
                    auto r = range.save();
                    r.popFront();
                    if (r.front == quote && !isWysiwyg)
                    {
                        keepChar();
                        keepChar();
                    }
                    else if (r.front == '\\' && !isWysiwyg)
                    {
                        keepChar();
                        keepChar();
                    }
                    else
                        keepChar();
                }
                else
                    interpretEscapeSequence(range, index, buffer, bufferIndex);
            }
            else if (range.front == quote)
            {
                keepChar();
                break;
            }
            else
                keepChar();
        }
        lexStringSuffix();
    }

    void lexDelimitedString()
    in
    {
        assert(range.front == 'q');
    }
    body
    {
        current.type = TokenType.stringLiteral;

        keepChar();
        keepChar();

        bool heredoc;
        ubyte open;
        ubyte close;

        switch (range.front)
        {
        case '[': open = '['; close = ']'; break;
        case '{': open = '{'; close = '}'; break;
        case '(': open = '('; close = ')'; break;
        case '<': open = '<'; close = '>'; break;
        default: heredoc = true; break;
        }
        if (heredoc)
            lexHeredocString();
        else
            lexNormalDelimitedString(open, close);
    }

    void lexNormalDelimitedString(ubyte open, ubyte close)
    in
    {
        assert(buffer[0 .. bufferIndex] == "q\"");
    }
    body
    {
        current.type = TokenType.stringLiteral;
        int depth = 1;
        keepChar();
        scope (exit)
        {
            if (config.tokenStyle & TokenStyle.includeQuotes)
                current.value = (cast(char[]) buffer[0 .. bufferIndex]).idup;
            else
                current.value = (cast(char[]) buffer[3 .. bufferIndex - 2]).idup;
        }
        while (true)
        {
            if (range.isEoF())
                errorMessage("Unterminated string literal");
            if (range.front == open)
            {
                keepChar();
                ++depth;
            }
            else if (range.front == close)
            {
                keepChar();
                --depth;
                if (depth <= 0)
                {
                    auto r = range.save();
                    if (r.front == '"')
                    {
                        keepChar();
                        return;
                    }
                    else
                    {
                        errorMessage("Expected \" after balanced "
                            ~ cast(char) close ~ " but found "
                            ~ cast(char) r.front ~ " instead.");
                        return;
                    }
                }
            }
            else
                keepChar();
        }

    }

    void lexHeredocString()
    in
    {
        assert (buffer[0 .. bufferIndex] == "q\"");
    }
    body
    {
        auto i = bufferIndex;
        while (true)
        {
            if (range.isEoF())
            {
                errorMessage("Unterminated string literal");
                return;
            }
            else if (isNewline(range))
            {
                keepChar();
                break;
            }
            else if (isSeparating(range.front))
            {
                errorMessage("Unterminated string literal - Separating");
                return;
            }
            else
                keepChar();
        }
        auto ident = buffer[i .. bufferIndex - 1];

        scope(exit)
        {
            if (config.tokenStyle & TokenStyle.includeQuotes)
                current.value = (cast(char[]) buffer[0 .. bufferIndex]).idup;
            else
            {
                size_t b = 2 + ident.length;
                if (buffer[b] == '\r') ++b;
                if (buffer[b] == '\n') ++b;
                size_t e = bufferIndex;
                if (buffer[e - 1] == 'c' || buffer[e - 1] == 'd' || buffer[e - 1] == 'w')
                    --e;
                stderr.writeln("b = ", b, " e = ", e);
                current.value = (cast(char[]) buffer[b .. e]).idup;
            }
        }

        while (true)
        {
            if (range.isEoF())
            {
                errorMessage("Unterminated string literal -- a");
                return;
            }
            else if (buffer[bufferIndex - ident.length .. bufferIndex] == ident)
            {
                if (range.front == '"')
                {
                    keepChar();
                    lexStringSuffix();
                    return;
                }
                else
                {
                    errorMessage("Unterminated string literal -- b");
                    return;
                }
            }
            else
                keepChar();
        }
    }

    void lexTokenString()
    in
    {
        assert (range.front == 'q');
    }
    body
    {
        current.type = TokenType.stringLiteral;
        size_t i;

        scope (exit)
        {
            if (config.tokenStyle & TokenStyle.includeQuotes)
                current.value = (cast(char[]) buffer[0 .. bufferIndex]).idup;
            else
                current.value = (cast(char[]) buffer[2 .. bufferIndex - 1]).idup;
        }

        keepChar();
        keepChar();

        LexerConfig c;
        c.iterStyle = IterationStyle.everything;
        c.tokenStyle = TokenStyle.source;

        auto r = byToken(range, c);
        r.index = index;
        int depth = 1;
        while (!r.empty)
        {
            if (r.front.type == TokenType.lBrace)
            {
                ++depth;
            }
            else if (r.front.type == TokenType.rBrace)
            {
                --depth;
                if (depth <= 0)
                {
                    if (config.tokenStyle & TokenStyle.includeQuotes)
                    {
                        if (bufferIndex >= buffer.length)
                            buffer.length += 1024;
                        buffer[bufferIndex++] = '}';
                    }
                    r.popFront();
                    break;
                }
            }
            if (bufferIndex + r.front.value.length > buffer.length)
                buffer.length += 1024;
            buffer[bufferIndex .. bufferIndex + r.front.value.length] = cast(ubyte[]) r.front.value;
            bufferIndex += r.front.value.length;
            r.popFront();
        }
        lexStringSuffix();
    }

    void lexSpecialTokenSequence()
    in
    {
        assert (range.front == '#');
    }
    body
    {
        keepChar();
        auto r = range.save();
        auto app = appender!(ubyte[])();
        app.put('#');
        while (true)
        {
            if (r.isEoF())
            {
                errorMessage("Found EOF when interpreting special token sequence");
                return;
            }
            else if (isNewline(r))
                break;
            else
            {
                app.put(r.front);
                r.popFront();
            }
        }
        auto m = match((cast(char[]) app.data),
            `#line\s+(?P<line>\d+)\s*(?P<filespec>".+")*?`);
        if (m)
        {
            current.type = TokenType.specialTokenSequence;
            current.value = (cast(char[]) app.data).idup;
            column += app.data.length;
            index += app.data.length;
            range.popFrontN(app.data.length);
            auto c = m.captures;
            if (c["filespec"])
                config.fileName = c["filespec"].idup;
            auto l = c["line"];
            lineNumber = parse!uint(l);

        }
        else
        {
            current.type = TokenType.hash;
            current.value = getTokenValue(TokenType.hash);
        }
    }

    void errorMessage(string s)
    {
        import std.stdio;
        if (config.errorFunc !is null)
            config.errorFunc(config.fileName, current.startIndex,
                current.line, current.column, s);
        else
            stderr.writefln("%s(%d:%d): %s", config.fileName, current.line,
                current.column, s);
    }

    void keepChar()
    {
        if (bufferIndex + 2 >= buffer.length)
            buffer.length += 1024;
        bool foundNewline;
        if (range.front == '\r')
        {
            buffer[bufferIndex++] = range.front;
            range.popFront();
            ++index;
            foundNewline = true;
        }
        if (range.front == '\n')
        {
            buffer[bufferIndex++] = range.front;
            range.popFront();
            ++index;
            foundNewline = true;
        }
        else
        {
            buffer[bufferIndex++] = range.front;
            range.popFront();
            ++index;
            ++column;
        }
        if (foundNewline)
        {
            ++lineNumber;
            column = 0;
        }
    }

    Token current;
    uint lineNumber;
    uint index;
    uint column;
    R range;
    bool _empty;
    ubyte[] buffer;
    size_t bufferIndex;
    LexerConfig config;
}

/**
 * Returns: true if the token is an operator
 */
pure nothrow bool isOperator(const TokenType t)
{
    return t >= TokenType.assign && t <= TokenType.xorEquals;
}

/**
 * Returns: true if the token is a keyword
 */
pure nothrow bool isKeyword(const TokenType t)
{
    return t >= TokenType.bool_ && t <= TokenType.with_;
}

/**
 * Returns: true if the token is a built-in type
 */
pure nothrow bool isType(const TokenType t)
{
    return t >= TokenType.bool_ && t <= TokenType.wstring_;
}

/**
 * Returns: true if the token is an attribute
 */
pure nothrow bool isAttribute(const TokenType t)
{
    return t >= TokenType.align_ && t <= TokenType.static_;
}

/**
 * Returns: true if the token is a protection attribute
 */
pure nothrow bool isProtection(const TokenType t)
{
    return t >= TokenType.export_ && t <= TokenType.public_;
}

/**
 * Returns: true if the token is a compile-time constant such as ___DATE__
 */
pure nothrow bool isConstant(const TokenType t)
{
    return t >= TokenType.date && t <= TokenType.traits;
}

/**
 * Returns: true if the token is a string or number literal
 */
pure nothrow bool isLiteral(const TokenType t)
{
    return t >= TokenType.doubleLiteral && t <= TokenType.wstringLiteral;
}

/**
 * Returns: true if the token is a number literal
 */
pure nothrow bool isNumberLiteral(const TokenType t)
{
    return t >= TokenType.doubleLiteral && t <= TokenType.ulongLiteral;
}

/**
 * Returns: true if the token is a string literal
 */
pure nothrow bool isStringLiteral(const TokenType t)
{
    return t >= TokenType.dstringLiteral && t <= TokenType.wstringLiteral;
}

/**
 * Returns: true if the token is whitespace, a commemnt, a special token
 *     sequence, or an identifier
 */
pure nothrow bool isMisc(const TokenType t)
{
    return t >= TokenType.comment && t <= TokenType.specialTokenSequence;
}

/**
 * Listing of all the tokens in the D language.
 */
enum TokenType: ushort
{
    assign, /// =
    at, /// @
    bitAnd, /// &
    bitAndEquals, /// &=
    bitOr, /// |
    bitOrEquals, /// |=
    catEquals, /// ~=
    colon, /// :
    comma, /// ,
    decrement, /// --
    div, /// /
    divEquals, /// /=
    dollar, /// $
    dot, /// .
    equals, /// ==
    goesTo, /// =>
    greater, /// >
    greaterEqual, /// >=
    hash, /// #
    increment, /// ++
    lBrace, /// {
    lBracket, /// [
    less, /// <
    lessEqual, /// <=
    lessEqualGreater, /// <>=
    lessOrGreater, /// <>
    logicAnd, /// &&
    logicOr, /// ||
    lParen, /// $(LPAREN)
    minus, /// -
    minusEquals, /// -=
    mod, /// %
    modEquals, /// %=
    mulEquals, /// *=
    not, /// !
    notEquals, /// !=
    notGreater, /// !>
    notGreaterEqual, /// !>=
    notLess, /// !<
    notLessEqual, /// !<=
    notLessEqualGreater, /// !<>
    plus, /// +
    plusEquals, /// +=
    pow, /// ^^
    powEquals, /// ^^=
    rBrace, /// }
    rBracket, /// ]
    rParen, /// $(RPAREN)
    semicolon, /// ;
    shiftLeft, /// <<
    shiftLeftEqual, /// <<=
    shiftRight, /// >>
    shiftRightEqual, /// >>=
    slice, /// ..
    star, /// *
    ternary, /// ?
    tilde, /// ~
    unordered, /// !<>=
    unsignedShiftRight, /// >>>
    unsignedShiftRightEqual, /// >>>=
    vararg, /// ...
    xor, /// ^
    xorEquals, /// ^=

    bool_, /// $(D_KEYWORD bool)
    byte_, /// $(D_KEYWORD byte)
    cdouble_, /// $(D_KEYWORD cdouble)
    cent_, /// $(D_KEYWORD cent)
    cfloat_, /// $(D_KEYWORD cfloat)
    char_, /// $(D_KEYWORD char)
    creal_, /// $(D_KEYWORD creal)
    dchar_, /// $(D_KEYWORD dchar)
    double_, /// $(D_KEYWORD double)
    dstring_, /// $(D_KEYWORD dstring)
    float_, /// $(D_KEYWORD float)
    function_, /// $(D_KEYWORD function)
    idouble_, /// $(D_KEYWORD idouble)
    ifloat_, /// $(D_KEYWORD ifloat)
    int_, /// $(D_KEYWORD int)
    ireal_, /// $(D_KEYWORD ireal)
    long_, /// $(D_KEYWORD long)
    real_, /// $(D_KEYWORD real)
    short_, /// $(D_KEYWORD short)
    string_, /// $(D_KEYWORD string)
    ubyte_, /// $(D_KEYWORD ubyte)
    ucent_, /// $(D_KEYWORD ucent)
    uint_, /// $(D_KEYWORD uint)
    ulong_, /// $(D_KEYWORD ulong)
    ushort_, /// $(D_KEYWORD ushort)
    void_, /// $(D_KEYWORD void)
    wchar_, /// $(D_KEYWORD wchar)
    wstring_, /// $(D_KEYWORD wstring)

    align_, /// $(D_KEYWORD align)
    deprecated_, /// $(D_KEYWORD deprecated)
    extern_, /// $(D_KEYWORD extern)
    pragma_, /// $(D_KEYWORD pragma)
    export_, /// $(D_KEYWORD export)
    package_, /// $(D_KEYWORD package)
    private_, /// $(D_KEYWORD private)
    protected_, /// $(D_KEYWORD protected)
    public_, /// $(D_KEYWORD public)
    abstract_, /// $(D_KEYWORD abstract)
    auto_, /// $(D_KEYWORD auto)
    const_, /// $(D_KEYWORD const)
    final_, /// $(D_KEYWORD final)
    gshared, /// $(D_KEYWORD __gshared)
    immutable_, // immutable
    inout_, // inout
    scope_, /// $(D_KEYWORD scope)
    shared_, // shared
    static_, /// $(D_KEYWORD static)

    synchronized_, /// $(D_KEYWORD synchronized)
    alias_, /// $(D_KEYWORD alias)
    asm_, /// $(D_KEYWORD asm)
    assert_, /// $(D_KEYWORD assert)
    body_, /// $(D_KEYWORD body)
    break_, /// $(D_KEYWORD break)
    case_, /// $(D_KEYWORD case)
    cast_, /// $(D_KEYWORD cast)
    catch_, /// $(D_KEYWORD catch)
    class_, /// $(D_KEYWORD class)
    continue_, /// $(D_KEYWORD continue)
    debug_, /// $(D_KEYWORD debug)
    default_, /// $(D_KEYWORD default)
    delegate_, /// $(D_KEYWORD delegate)
    delete_, /// $(D_KEYWORD delete)
    do_, /// $(D_KEYWORD do)
    else_, /// $(D_KEYWORD else)
    enum_, /// $(D_KEYWORD enum)
    false_, /// $(D_KEYWORD false)
    finally_, /// $(D_KEYWORD finally)
    foreach_, /// $(D_KEYWORD foreach)
    foreach_reverse_, /// $(D_KEYWORD foreach_reverse)
    for_, /// $(D_KEYWORD for)
    goto_, /// $(D_KEYWORD goto)
    if_, /// $(D_KEYWORD if)
    import_, /// $(D_KEYWORD import)
    in_, /// $(D_KEYWORD in)
    interface_, /// $(D_KEYWORD interface)
    invariant_, /// $(D_KEYWORD invariant)
    is_, /// $(D_KEYWORD is)
    lazy_, /// $(D_KEYWORD lazy)
    macro_, /// $(D_KEYWORD macro)
    mixin_, /// $(D_KEYWORD mixin)
    module_, /// $(D_KEYWORD module)
    new_, /// $(D_KEYWORD new)
    nothrow_, /// $(D_KEYWORD nothrow)
    null_, /// $(D_KEYWORD null)
    out_, /// $(D_KEYWORD out)
    override_, /// $(D_KEYWORD override)
    pure_, /// $(D_KEYWORD pure)
    ref_, /// $(D_KEYWORD ref)
    return_, /// $(D_KEYWORD return)
    struct_, /// $(D_KEYWORD struct)
    super_, /// $(D_KEYWORD super)
    switch_, /// $(D_KEYWORD switch)
    template_, /// $(D_KEYWORD template)
    this_, /// $(D_KEYWORD this)
    throw_, /// $(D_KEYWORD throw)
    true_, /// $(D_KEYWORD true)
    try_, /// $(D_KEYWORD try)
    typedef_, /// $(D_KEYWORD typedef)
    typeid_, /// $(D_KEYWORD typeid)
    typeof_, /// $(D_KEYWORD typeof)
    union_, /// $(D_KEYWORD union)
    unittest_, /// $(D_KEYWORD unittest)
    version_, /// $(D_KEYWORD version)
    volatile_, /// $(D_KEYWORD volatile)
    while_, /// $(D_KEYWORD while)
    with_, /// $(D_KEYWORD with)

    date, /// ___DATE__
    eof, /// ___EOF__
    time, /// ___TIME__
    timestamp, /// ___TIMESTAMP__
    vendor, /// ___VENDOR__
    compilerVersion, /// ___VERSION__
    file, /// $(D_KEYWORD ___FILE__)
    line, /// $(D_KEYWORD ___LINE__)
    comment, /// $(D_COMMENT /** comment */) or $(D_COMMENT // comment) or $(D_COMMENT ///comment)
    identifier, /// anything else
    scriptLine, // Line at the beginning of source file that starts from #!
    traits, /// $(D_KEYWORD ___traits)
    parameters, /// $(D_KEYWORD ___parameters)
    vector, /// $(D_KEYWORD ___vector)
    whitespace, /// whitespace
    specialTokenSequence, /// #line 10 "file.d"
    doubleLiteral, /// 123.456
    floatLiteral, /// 123.456f or 0x123_45p-3
    idoubleLiteral, /// 123.456i
    ifloatLiteral, /// 123.456fi
    intLiteral, /// 123 or 0b1101010101
    longLiteral, /// 123L
    realLiteral, /// 123.456L
    irealLiteral, /// 123.456Li
    uintLiteral, /// 123u
    ulongLiteral, /// 123uL
    dstringLiteral, /// $(D_STRING "32-bit character string"d)
    stringLiteral, /// $(D_STRING "an 8-bit string")
    wstringLiteral, /// $(D_STRING "16-bit character string"w)
}

// Implementation details follow
private:

/*
 * To avoid memory allocations Token.value is set to a slice of this string
 * for operators and keywords.
 */
immutable string opKwdValues =
      "#/=*=+=++-=--^^=~=<<=%==>>>=||=&&=,;:!<=!<>=!=!>=?...()[]{}@$"
    ~ "boolcdoublecentcfloatcrealdchardstringfunctionidoubleifloatirealubyte"
    ~ "ucentuintulongushortvoidwcharwstringaligndeprecatedexternpragmaexport"
    ~ "packageprivateprotectedpublicabstractautoconstfinal__gsharedimmutable"
    ~ "inoutscopesharedstaticsynchronizedaliasasmassertbodybreakcasecastcatch"
    ~ "classcontinuedebugdefaultdelegatedeleteelseenumfalsefinally"
    ~ "foreach_reversegotoimportinterfaceinvariantlazymacromixinmodule"
    ~ "newnothrownulloverridepurerefreturnstructsuperswitchtemplatethistruetry"
    ~ "typedeftypeidtypeofunionunittestversionvolatilewhilewith__traits"
    ~ "__vector__parameters__DATE__EOF__TIME__TIMESTAMP__VENDOR__VERSION__"
    ~ "FILE__LINE__";

/*
 * Slices of the above string to save memory. This array is automatically
 * generated.
 */
immutable(string[]) tokenValues = [
	opKwdValues[2 .. 3], // =
	opKwdValues[59 .. 60], // @
	opKwdValues[31 .. 32], // &
	opKwdValues[32 .. 34], // &=
	opKwdValues[28 .. 29], // |
	opKwdValues[29 .. 31], // |=
	opKwdValues[16 .. 18], // ~=
	opKwdValues[36 .. 37], // :
	opKwdValues[34 .. 35], // ,
	opKwdValues[11 .. 13], // --
	opKwdValues[1 .. 2], // /
	opKwdValues[1 .. 3], // /=
	opKwdValues[60 .. 61], // $
	opKwdValues[50 .. 51], // .
	opKwdValues[22 .. 24], // ==
	opKwdValues[23 .. 25], // =>
	opKwdValues[24 .. 25], // >
	opKwdValues[26 .. 28], // >=
	opKwdValues[0 .. 1], // #
	opKwdValues[7 .. 9], // ++
	opKwdValues[57 .. 58], // {
	opKwdValues[55 .. 56], // [
	opKwdValues[18 .. 19], // <
	opKwdValues[19 .. 21], // <=
	opKwdValues[41 .. 44], // <>=
	opKwdValues[41 .. 43], // <>
	opKwdValues[31 .. 33], // &&
	opKwdValues[28 .. 30], // ||
	opKwdValues[53 .. 54], // (
	opKwdValues[9 .. 10], // -
	opKwdValues[9 .. 11], // -=
	opKwdValues[21 .. 22], // %
	opKwdValues[21 .. 23], // %=
	opKwdValues[3 .. 5], // *=
	opKwdValues[37 .. 38], // !
	opKwdValues[44 .. 46], // !=
	opKwdValues[46 .. 48], // !>
	opKwdValues[46 .. 49], // !>=
	opKwdValues[37 .. 39], // !<
	opKwdValues[37 .. 40], // !<=
	opKwdValues[40 .. 43], // !<>
	opKwdValues[5 .. 6], // +
	opKwdValues[5 .. 7], // +=
	opKwdValues[13 .. 15], // ^^
	opKwdValues[13 .. 16], // ^^=
	opKwdValues[58 .. 59], // }
	opKwdValues[56 .. 57], // ]
	opKwdValues[54 .. 55], // )
	opKwdValues[35 .. 36], // ;
	opKwdValues[18 .. 20], // <<
	opKwdValues[18 .. 21], // <<=
	opKwdValues[24 .. 26], // >>
	opKwdValues[25 .. 28], // >>=
	opKwdValues[50 .. 52], // ..
	opKwdValues[3 .. 4], // *
	opKwdValues[49 .. 50], // ?
	opKwdValues[16 .. 17], // ~
	opKwdValues[40 .. 44], // !<>=
	opKwdValues[24 .. 27], // >>>
	opKwdValues[24 .. 28], // >>>=
	opKwdValues[50 .. 53], // ...
	opKwdValues[13 .. 14], // ^
	opKwdValues[14 .. 16], // ^=
	opKwdValues[61 .. 65], // bool
	opKwdValues[126 .. 130], // byte
	opKwdValues[65 .. 72], // cdouble
	opKwdValues[72 .. 76], // cent
	opKwdValues[76 .. 82], // cfloat
	opKwdValues[88 .. 92], // char
	opKwdValues[82 .. 87], // creal
	opKwdValues[87 .. 92], // dchar
	opKwdValues[66 .. 72], // double
	opKwdValues[92 .. 99], // dstring
	opKwdValues[77 .. 82], // float
	opKwdValues[99 .. 107], // function
	opKwdValues[107 .. 114], // idouble
	opKwdValues[114 .. 120], // ifloat
	opKwdValues[136 .. 139], // int
	opKwdValues[120 .. 125], // ireal
	opKwdValues[140 .. 144], // long
	opKwdValues[83 .. 87], // real
	opKwdValues[145 .. 150], // short
	opKwdValues[93 .. 99], // string
	opKwdValues[125 .. 130], // ubyte
	opKwdValues[130 .. 135], // ucent
	opKwdValues[135 .. 139], // uint
	opKwdValues[139 .. 144], // ulong
	opKwdValues[144 .. 150], // ushort
	opKwdValues[150 .. 154], // void
	opKwdValues[154 .. 159], // wchar
	opKwdValues[159 .. 166], // wstring
	opKwdValues[166 .. 171], // align
	opKwdValues[171 .. 181], // deprecated
	opKwdValues[181 .. 187], // extern
	opKwdValues[187 .. 193], // pragma
	opKwdValues[193 .. 199], // export
	opKwdValues[199 .. 206], // package
	opKwdValues[206 .. 213], // private
	opKwdValues[213 .. 222], // protected
	opKwdValues[222 .. 228], // public
	opKwdValues[228 .. 236], // abstract
	opKwdValues[236 .. 240], // auto
	opKwdValues[240 .. 245], // const
	opKwdValues[245 .. 250], // final
	opKwdValues[250 .. 259], // __gshared
	opKwdValues[259 .. 268], // immutable
	opKwdValues[268 .. 273], // inout
	opKwdValues[273 .. 278], // scope
	opKwdValues[253 .. 259], // shared
	opKwdValues[284 .. 290], // static
	opKwdValues[290 .. 302], // synchronized
	opKwdValues[302 .. 307], // alias
	opKwdValues[307 .. 310], // asm
	opKwdValues[310 .. 316], // assert
	opKwdValues[316 .. 320], // body
	opKwdValues[320 .. 325], // break
	opKwdValues[325 .. 329], // case
	opKwdValues[329 .. 333], // cast
	opKwdValues[333 .. 338], // catch
	opKwdValues[338 .. 343], // class
	opKwdValues[343 .. 351], // continue
	opKwdValues[351 .. 356], // debug
	opKwdValues[356 .. 363], // default
	opKwdValues[363 .. 371], // delegate
	opKwdValues[371 .. 377], // delete
	opKwdValues[66 .. 68], // do
	opKwdValues[377 .. 381], // else
	opKwdValues[381 .. 385], // enum
	opKwdValues[385 .. 390], // false
	opKwdValues[390 .. 397], // finally
	opKwdValues[397 .. 404], // foreach
	opKwdValues[397 .. 412], // foreach_reverse
	opKwdValues[397 .. 400], // for
	opKwdValues[412 .. 416], // goto
	opKwdValues[114 .. 116], // if
	opKwdValues[416 .. 422], // import
	opKwdValues[96 .. 98], // in
	opKwdValues[422 .. 431], // interface
	opKwdValues[431 .. 440], // invariant
	opKwdValues[522 .. 524], // is
	opKwdValues[440 .. 444], // lazy
	opKwdValues[444 .. 449], // macro
	opKwdValues[449 .. 454], // mixin
	opKwdValues[454 .. 460], // module
	opKwdValues[460 .. 463], // new
	opKwdValues[463 .. 470], // nothrow
	opKwdValues[470 .. 474], // null
	opKwdValues[270 .. 273], // out
	opKwdValues[474 .. 482], // override
	opKwdValues[482 .. 486], // pure
	opKwdValues[486 .. 489], // ref
	opKwdValues[489 .. 495], // return
	opKwdValues[495 .. 501], // struct
	opKwdValues[501 .. 506], // super
	opKwdValues[506 .. 512], // switch
	opKwdValues[512 .. 520], // template
	opKwdValues[520 .. 524], // this
	opKwdValues[465 .. 470], // throw
	opKwdValues[524 .. 528], // true
	opKwdValues[528 .. 531], // try
	opKwdValues[531 .. 538], // typedef
	opKwdValues[538 .. 544], // typeid
	opKwdValues[544 .. 550], // typeof
	opKwdValues[550 .. 555], // union
	opKwdValues[555 .. 563], // unittest
	opKwdValues[563 .. 570], // version
	opKwdValues[570 .. 578], // volatile
	opKwdValues[578 .. 583], // while
	opKwdValues[583 .. 587], // with
	opKwdValues[615 .. 623], // __DATE__
	opKwdValues[621 .. 628], // __EOF__
	opKwdValues[626 .. 634], // __TIME__
	opKwdValues[632 .. 645], // __TIMESTAMP__
	opKwdValues[643 .. 653], // __VENDOR__
	opKwdValues[651 .. 662], // __VERSION__
	opKwdValues[660 .. 668], // __FILE__
	opKwdValues[666 .. 674], // __LINE__
	null,
	null,
	null,
	opKwdValues[587 .. 595], // __traits
	opKwdValues[603 .. 615], // __parameters
	opKwdValues[595 .. 603], // __vector
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
	null,
];

pure string getTokenValue(const TokenType type)
{
    return tokenValues[type];
}

private pure bool isNewline(R)(R range)
{
    return range.front == '\n' || range.front == '\r';
}

pure bool isEoF(R)(R range)
{
    return range.empty || range.front == 0 || range.front == 0x1a;
}

ubyte[] popDigitChars(R, alias isInterestingDigit)(ref R input, ref uint index,
    uint upTo) if (isForwardRange!R)
{
    ubyte[] chars;
    chars.reserve(upTo);
    for (uint i = 0; i != upTo; ++i)
    {
        if (isInterestingDigit(input.front))
        {
            chars ~= input.front;
            input.popFront();
        }
        else
            break;
    }
    return chars;
}

ubyte[] popHexChars(R)(ref R input, ref uint index, uint upTo)
{
    return popDigitChars!(R, isHexDigit)(input, index, upTo);
}

ubyte[] popOctalChars(R)(ref R input, ref uint index, uint upTo)
{
    return popDigitChars!(R, isOctalDigit)(input, index, upTo);
}

void interpretEscapeSequence(R)(ref R input, ref uint index, ref ubyte[] buffer,
    ref size_t i) if (isForwardRange!R)
in
{
    assert(input.front == '\\');
}
body
{
    input.popFront();
    short h = 0;
    switch (input.front)
    {
    case '\'':
    case '\"':
    case '?':
    case '\\':
    case 0:
    case 0x1a:
        auto f = input.front;
        input.popFront();
        ++index;
        auto s = to!string(cast(char) f);
        buffer[i .. i + s.length] = cast(ubyte[]) s;
        return;
    case 'a': input.popFront(); ++index; buffer[i++] = '\a'; return;
    case 'b': input.popFront(); ++index; buffer[i++] = '\b'; return;
    case 'f': input.popFront(); ++index; buffer[i++] = '\f'; return;
    case 'n': input.popFront(); ++index; buffer[i++] = '\n'; return;
    case 'r': input.popFront(); ++index; buffer[i++] = '\r'; return;
    case 't': input.popFront(); ++index; buffer[i++] = '\t'; return;
    case 'v': input.popFront(); ++index; buffer[i++] = '\v'; return;
    case 'x': h = 2; goto hex;
    case 'u': h = 4; goto hex;
    case 'U': h = 8; goto hex;
    case '0': .. case '7':
        auto octalChars = cast(char[]) popOctalChars(input, index, 3);
        char[4] b;
        auto n = encode(b, cast(dchar) parse!uint(octalChars, 8));
        buffer[i .. i + n] = cast(ubyte[]) b[0 .. n];
        i += n;
        return;
    case '&':
        input.popFront();
        ++index;
        auto entity = appender!(ubyte[])();
        while (!input.isEoF() && input.front != ';')
        {
            entity.put(input.front);
            input.popFront();
            ++index;
        }
        if (!isEoF(input))
        {
            auto decoded = to!string(cast(char[]) entity.data) in characterEntities;
            input.popFront();
            ++index;
            if (decoded !is null)
            {
                buffer[i .. i + decoded.length] = cast(ubyte[]) *decoded;
                i += decoded.length;
            }
        }
        return;
    default:
        input.popFront();
        ++index;
        // This is an error
        buffer[i++] = '\\';
        return;
    }

hex:
    input.popFront();
    auto hexChars = cast(char[]) popHexChars(input, index, h);
    char[4] b;
    auto n = encode(b, cast(dchar) parse!uint(hexChars, 16));
    buffer[i .. i + n] = cast(ubyte[]) b[0 .. n];
    i += n;
    return;
}

pure nothrow bool isSeparating(ubyte ch)
{
    return (ch >= '!' && ch <= '/')
        || (ch >= ':' && ch <= '@')
        || (ch >= '[' && ch <= '^')
        || (ch >= '{' && ch <= '~')
        || ch == '`'
        || ch == 0x20
        || ch == 0x09
        || ch == 0x0a;
}

pure nothrow TokenType lookupTokenType(const const(char)[] input)
{
    switch(input.length)
    {
    case 2:
        switch (input)
        {
        case "do": return TokenType.do_;
        case "if": return TokenType.if_;
        case "in": return TokenType.in_;
        case "is": return TokenType.is_;
        default: break;
        }
        break;
    case 3:
        switch (input)
        {
        case "asm": return TokenType.asm_;
        case "for": return TokenType.for_;
        case "int": return TokenType.int_;
        case "new": return TokenType.new_;
        case "out": return TokenType.out_;
        case "ref": return TokenType.ref_;
        case "try": return TokenType.try_;
        default: break;
        }
        break;
    case 4:
        switch (input)
        {
        case "auto": return TokenType.auto_;
        case "body": return TokenType.body_;
        case "bool": return TokenType.bool_;
        case "byte": return TokenType.byte_;
        case "case": return TokenType.case_;
        case "cast": return TokenType.cast_;
        case "cent": return TokenType.cent_;
        case "char": return TokenType.char_;
        case "else": return TokenType.else_;
        case "enum": return TokenType.enum_;
        case "goto": return TokenType.goto_;
        case "lazy": return TokenType.lazy_;
        case "long": return TokenType.long_;
        case "null": return TokenType.null_;
        case "pure": return TokenType.pure_;
        case "real": return TokenType.real_;
        case "this": return TokenType.this_;
        case "true": return TokenType.true_;
        case "uint": return TokenType.uint_;
        case "void": return TokenType.void_;
        case "with": return TokenType.with_;
        default: break;
        }
        break;
    case 5:
        switch (input)
        {
        case "alias": return TokenType.alias_;
        case "align": return TokenType.align_;
        case "break": return TokenType.break_;
        case "catch": return TokenType.catch_;
        case "class": return TokenType.class_;
        case "const": return TokenType.const_;
        case "creal": return TokenType.creal_;
        case "dchar": return TokenType.dchar_;
        case "debug": return TokenType.debug_;
        case "false": return TokenType.false_;
        case "final": return TokenType.final_;
        case "float": return TokenType.float_;
        case "inout": return TokenType.inout_;
        case "ireal": return TokenType.ireal_;
        case "macro": return TokenType.macro_;
        case "mixin": return TokenType.mixin_;
        case "scope": return TokenType.scope_;
        case "short": return TokenType.short_;
        case "super": return TokenType.super_;
        case "throw": return TokenType.throw_;
        case "ubyte": return TokenType.ubyte_;
        case "ucent": return TokenType.ucent_;
        case "ulong": return TokenType.ulong_;
        case "union": return TokenType.union_;
        case "wchar": return TokenType.wchar_;
        case "while": return TokenType.while_;
        default: break;
        }
        break;
    case 6:
        switch (input)
        {
        case "assert": return TokenType.assert_;
        case "cfloat": return TokenType.cfloat_;
        case "delete": return TokenType.delete_;
        case "double": return TokenType.double_;
        case "export": return TokenType.export_;
        case "extern": return TokenType.extern_;
        case "ifloat": return TokenType.ifloat_;
        case "import": return TokenType.import_;
        case "module": return TokenType.module_;
        case "pragma": return TokenType.pragma_;
        case "public": return TokenType.public_;
        case "return": return TokenType.return_;
        case "shared": return TokenType.shared_;
        case "static": return TokenType.static_;
        case "string": return TokenType.string_;
        case "struct": return TokenType.struct_;
        case "switch": return TokenType.switch_;
        case "typeid": return TokenType.typeid_;
        case "typeof": return TokenType.typeof_;
        case "ushort": return TokenType.ushort_;
        default: break;
        }
        break;
    case 7:
        switch (input)
        {
        case "__EOF__": return TokenType.eof;
        case "cdouble": return TokenType.cdouble_;
        case "default": return TokenType.default_;
        case "dstring": return TokenType.dstring_;
        case "finally": return TokenType.finally_;
        case "foreach": return TokenType.foreach_;
        case "idouble": return TokenType.idouble_;
        case "nothrow": return TokenType.nothrow_;
        case "package": return TokenType.package_;
        case "private": return TokenType.private_;
        case "typedef": return TokenType.typedef_;
        case "version": return TokenType.version_;
        case "wstring": return TokenType.wstring_;
        default: break;
        }
        break;
    case 8:
        switch (input)
        {
        case "override": return TokenType.override_;
        case "continue": return TokenType.continue_;
        case "__LINE__": return TokenType.line;
        case "template": return TokenType.template_;
        case "abstract": return TokenType.abstract_;
        case "__traits": return TokenType.traits;
        case "volatile": return TokenType.volatile_;
        case "delegate": return TokenType.delegate_;
        case "function": return TokenType.function_;
        case "unittest": return TokenType.unittest_;
        case "__FILE__": return TokenType.file;
        case "__DATE__": return TokenType.date;
        case "__TIME__": return TokenType.time;
        default: break;
        }
        break;
    case 9:
        switch (input)
        {
        case "__gshared": return TokenType.gshared;
        case "immutable": return TokenType.immutable_;
        case "interface": return TokenType.interface_;
        case "invariant": return TokenType.invariant_;
        case "protected": return TokenType.protected_;
        default: break;
        }
        break;
    case 10:
        switch (input)
        {
        case "deprecated": return TokenType.deprecated_;
        case "__VENDOR__": return TokenType.vendor;
        default: break;
        }
        break;
    case 11:
        if (input == "__VERSION__")
            return TokenType.compilerVersion;
        break;
    case 12:
        if (input == "synchronized")
            return TokenType.synchronized_;
        break;
    case 13:
        if (input == "__TIMESTAMP__")
            return TokenType.timestamp;
        break;
    case 15:
        if (input == "foreach_reverse")
            return TokenType.foreach_reverse_;
        break;
    default: break;
    }
    return TokenType.identifier;
}

class Trie(K, V) if (isInputRange!K): TrieNode!(K, V)
{
    /**
    * Adds the given value to the trie with the given key
    */
    void add(K key, V value) pure
    {
        TrieNode!(K,V) current = this;
        foreach(keyPart; key)
        {
            if ((keyPart in current.children) is null)
            {
                auto node = new TrieNode!(K, V);
                current.children[keyPart] = node;
                current = node;
            }
            else
                current = current.children[keyPart];
        }
        current.value = value;
    }
}

class TrieNode(K, V) if (isInputRange!K)
{
    V value;
    TrieNode!(K,V)[ElementType!K] children;
}

string printCaseStatements(K, V)(TrieNode!(K,V) node, string indentString)
{
    string caseStatement = "";
    foreach(dchar k, TrieNode!(K,V) v; node.children)
    {
        caseStatement ~= indentString;
        caseStatement ~= "case '";
        caseStatement ~= k;
        caseStatement ~= "':\n";
        if (indentString == "")
        {
            caseStatement ~= indentString;
            caseStatement ~= "\tsize_t i = 0;\n";
        }
        caseStatement ~= indentString;
        caseStatement ~= "\tbuffer[i++] = '";
        caseStatement ~= k;
        caseStatement ~= "';\n";
        caseStatement ~= indentString;
        caseStatement ~= "\t++index;\n";
        caseStatement ~= indentString;
        caseStatement ~= "\trange.popFront();\n";
        if (v.children.length > 0)
        {
            caseStatement ~= indentString;
            caseStatement ~= "\tif (range.isEoF())\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t{\n";
            caseStatement ~= indentString;
            caseStatement ~= "\tcurrent.value = getTokenValue(current.type);\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t\tcurrent.type = " ~ node.children[k].value;
            caseStatement ~= ";\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t\tbreak;\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t}\n";
            caseStatement ~= indentString;
            caseStatement ~= "\tswitch (range.front)\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t{\n";
            caseStatement ~= printCaseStatements(v, indentString ~ "\t");
            caseStatement ~= indentString;
            caseStatement ~= "\tdefault:\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t\tcurrent.type = ";
            caseStatement ~= v.value;
            caseStatement ~= ";\n";
            caseStatement ~= indentString;
            caseStatement ~= "\tcurrent.value = getTokenValue(current.type);\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t\tbreak;\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t}\n";
            caseStatement ~= indentString;
            caseStatement ~= "\tbreak;\n";
        }
        else
        {
            caseStatement ~= indentString;
            caseStatement ~= "\tcurrent.type = ";
            caseStatement ~= v.value;
            caseStatement ~= ";\n";
            caseStatement ~= indentString;
            caseStatement ~= "\tcurrent.value = getTokenValue(current.type);\n";
            caseStatement ~= indentString;
            caseStatement ~= "\tbreak;\n";
        }
    }
    return caseStatement;
}

string generateCaseTrie(string[] args ...)
{
    auto t = new Trie!(string, string);
    for(int i = 0; i < args.length; i+=2)
    {
        t.add(args[i], args[i+1]);
    }
    return printCaseStatements(t, "");
}

//void main(string[] args) {}
