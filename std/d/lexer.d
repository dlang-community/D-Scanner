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
* </head>
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
import std.datetime;
import std.d.entities;
import std.exception;
import std.range;
import std.regex;
import std.string;
import std.traits;
import std.utf;
version (unittest) import std.stdio;


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
    size_t startIndex;

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
    int opCmp(ref const(Token) other) const
    {
        if (startIndex < other.startIndex) return -1;
        if (startIndex > other.startIndex) return 1;
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
    uint versionNumber = 100;

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
    void delegate(string, size_t, uint, uint, string) errorFunc;

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
     * Returns the file name of the config object.
     */
    string fileName() const @property
    {
        return config.fileName;
    }

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
    ref const(Token) front() const @property
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
        buffer = uninitializedArray!(ubyte[])(bufferSize);
    }

    /*
    * Advances the range to the next token
    */
    void advance()
    {
        if (isEoF())
        {
            _empty = true;
            return;
        }

        bufferIndex = 0;
        current.line = lineNumber;
        current.startIndex = index;
        current.column = column;
        current.value = null;

        if (isWhite())
        {
            if (config.iterStyle & IterationStyle.includeWhitespace)
                lexWhitespace!true();
            else
                lexWhitespace!false();
            return;
        }

        switch (currentElement())
        {
//        pragma(msg, generateCaseTrie(
        mixin(generateCaseTrie(
            "=",               "TokenType.assign",
            "@",               "TokenType.at",
            "&",               "TokenType.bitAnd",
            "&=",              "TokenType.bitAndEquals",
            "|",               "TokenType.bitOr",
            "|=",              "TokenType.bitOrEquals",
            "~=",              "TokenType.catEquals",
            ":",               "TokenType.colon",
            ",",               "TokenType.comma",
            "--",              "TokenType.decrement",
            "$",               "TokenType.dollar",
            "==",              "TokenType.equals",
            "=>",              "TokenType.goesTo",
            ">",               "TokenType.greater",
            ">=",              "TokenType.greaterEqual",
            "++",              "TokenType.increment",
            "{",               "TokenType.lBrace",
            "[",               "TokenType.lBracket",
            "<",               "TokenType.less",
            "<=",              "TokenType.lessEqual",
            "<>=",             "TokenType.lessEqualGreater",
            "<>",              "TokenType.lessOrGreater",
            "&&",              "TokenType.logicAnd",
            "||",              "TokenType.logicOr",
            "(",               "TokenType.lParen",
            "-",               "TokenType.minus",
            "-=",              "TokenType.minusEquals",
            "%",               "TokenType.mod",
            "%=",              "TokenType.modEquals",
            "*=",              "TokenType.mulEquals",
            "!",               "TokenType.not",
            "!=",              "TokenType.notEquals",
            "!>",              "TokenType.notGreater",
            "!>=",             "TokenType.notGreaterEqual",
            "!<",              "TokenType.notLess",
            "!<=",             "TokenType.notLessEqual",
            "!<>",             "TokenType.notLessEqualGreater",
            "+",               "TokenType.plus",
            "+=",              "TokenType.plusEquals",
            "^^",              "TokenType.pow",
            "^^=",             "TokenType.powEquals",
            "}",               "TokenType.rBrace",
            "]",               "TokenType.rBracket",
            ")",               "TokenType.rParen",
            ";",               "TokenType.semicolon",
            "<<",              "TokenType.shiftLeft",
            "<<=",             "TokenType.shiftLeftEqual",
            ">>",              "TokenType.shiftRight",
            ">>=",             "TokenType.shiftRightEqual",
            "*",               "TokenType.star",
            "?",               "TokenType.ternary",
            "~",               "TokenType.tilde",
            "!<>=",            "TokenType.unordered",
            ">>>",             "TokenType.unsignedShiftRight",
            ">>>=",            "TokenType.unsignedShiftRightEqual",
            "^",               "TokenType.xor",
            "^=",              "TokenType.xorEquals",
        ));
        case '/':
            keepNonNewlineChar();
            if (isEoF())
            {
                current.type = TokenType.div;
                current.value = "/";
                return;
            }
            switch (currentElement())
            {
            case '/':
            case '*':
            case '+':
                if (config.iterStyle & IterationStyle.includeComments)
                    lexComment!true();
                else
                    lexComment!false();
                return;
            case '=':
                current.type = TokenType.divEquals;
                current.value = "/=";
                advanceRange();
                return;
            default:
                current.type = TokenType.div;
                current.value = "/";
                return;
            }
        case '.':
            keepNonNewlineChar();
            if (isEoF())
            {
                current.type = TokenType.dot;
                current.value = getTokenValue(TokenType.dot);
                return;
            }
            switch (currentElement())
            {
            case '0': .. case '9':
                lexNumber();
                return;
            case '.':
                current.type = TokenType.slice;
                keepNonNewlineChar();
                if (currentElement() == '.')
                {
                    current.type = TokenType.vararg;
                    keepNonNewlineChar();
                }
                current.value = getTokenValue(current.type);
                return;
            default:
                current.type = TokenType.dot;
                current.value = getTokenValue(TokenType.dot);
                return;
            }
        case '0': .. case '9':
            keepNonNewlineChar();
            lexNumber();
            return;
        case '\'':
            lexCharacterLiteral();
            return;
        case '"':
        case '`':
            lexString();
            return;
        case 'q':
            keepNonNewlineChar();
            if (isEoF())
                goto default;
            switch (currentElement())
            {
            case '{':
                lexTokenString();
                return;
            case '"':
                lexDelimitedString();
                return;
            default:
                break;
            }
            goto default;
        case 'r':
            keepNonNewlineChar();
            if (isEoF())
                goto default;
            else if (currentElement() == '"')
            {
                lexString();
                return;
            }
            else
                goto default;
        case 'x':
            keepNonNewlineChar();
            if (isEoF())
                goto default;
            else if (currentElement() == '"')
            {
                lexHexString();
                return;
            }
            else
                goto default;
        case '#':
            lexSpecialTokenSequence();
            return;
        default:
            while(!isEoF() && !isSeparating())
            {
                keepNonNewlineChar();
            }

            current.type = lookupTokenType(cast(char[]) buffer[0 .. bufferIndex]);
            current.value = getTokenValue(current.type);
            if (current.value is null)
                setTokenValue();

            if (!(config.iterStyle & IterationStyle.ignoreEOF) && current.type == TokenType.eof)
            {
                _empty = true;
                return;
            }

            if (!(config.iterStyle & TokenStyle.doNotReplaceSpecial))
                return;

            switch (current.type)
            {
            case TokenType.date:
                current.type = TokenType.stringLiteral;
                auto time = Clock.currTime();
                current.value = format("%s %02d %04d", time.month, time.day, time.year);
                return;
            case TokenType.time:
                auto time = Clock.currTime();
                current.type = TokenType.stringLiteral;
                current.value = (cast(TimeOfDay)(time)).toISOExtString();
                return;
            case TokenType.timestamp:
                auto time = Clock.currTime();
                auto dt = cast(DateTime) time;
                current.type = TokenType.stringLiteral;
                current.value = format("%s %s %02d %02d:%02d:%02d %04d",
                    dt.dayOfWeek, dt.month, dt.day, dt.hour, dt.minute,
                    dt.second, dt.year);
                return;
            case TokenType.vendor:
                current.type = TokenType.stringLiteral;
                current.value = config.vendorString;
                return;
            case TokenType.compilerVersion:
                current.type = TokenType.stringLiteral;
                current.value = format("%d", config.versionNumber);
                return;
            case TokenType.line:
                current.type = TokenType.intLiteral;
                current.value = format("%d", current.line);
                return;
            case TokenType.file:
                current.type = TokenType.stringLiteral;
                current.value = config.fileName;
                return;
            default:
                return;
            }
        }
    }

    void lexWhitespace(bool keep)()
    {
        current.type = TokenType.whitespace;
        while (!isEoF() && isWhite())
        {
            static if (keep) keepChar();
            else advanceRange();
        }
        static if (keep) setTokenValue();
    }

    void lexComment(bool keep)()
    in
    {
        assert (currentElement() == '/' || currentElement() == '*' || currentElement() == '+');
    }
    body
    {
        current.type = TokenType.comment;
        switch(currentElement())
        {
        case '/':
            while (!isEoF() && !isNewline(currentElement()))
            {
                static if (keep) keepNonNewlineChar();
                else advanceRange();
            }
            break;
        case '*':
            while (!isEoF())
            {
                if (currentElement() == '*')
                {
                    static if (keep) keepNonNewlineChar();
                    else advanceRange();
                    if (currentElement() == '/')
                    {
                        static if (keep) keepNonNewlineChar();
                        else advanceRange();
                        break;
                    }
                }
                else
                    keepChar();
            }
            break;
        case '+':
            int depth = 1;
            while (depth > 0 && !isEoF())
            {
                if (currentElement() == '+')
                {
                    static if (keep) keepNonNewlineChar();
                    else advanceRange();
                    if (currentElement() == '/')
                    {
                        static if (keep) keepNonNewlineChar();
                        else advanceRange();
                        --depth;
                    }
                }
                else if (currentElement() == '/')
                {
                    static if (keep) keepNonNewlineChar();
                    else advanceRange();
                    if (currentElement() == '+')
                    {
                        static if (keep) keepNonNewlineChar();
                        else advanceRange();
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
        setTokenValue();
    }

    void lexHexString()
    in
    {
        assert (currentElement() == '"' && buffer[0] == 'x');
    }
    body
    {
        current.type = TokenType.stringLiteral;
        keepChar();
        while (true)
        {
            if (isEoF())
            {
                errorMessage("Unterminated hex string literal");
                return;
            }
            else if (isHexDigit(currentElement()))
            {
                keepNonNewlineChar();
            }
            else if (isWhite() && (config.tokenStyle & TokenStyle.notEscaped))
            {
                keepChar();
            }
            else if (currentElement() == '"')
            {
                keepNonNewlineChar();
                break;
            }
            else
            {
                errorMessage(format("Invalid character '%s' in hex string literal",
                    cast(char) currentElement()));
                return;
            }
        }
        lexStringSuffix();
        if (config.tokenStyle & TokenStyle.notEscaped)
        {
            if (config.tokenStyle & TokenStyle.includeQuotes)
                setTokenValue();
            else
                setTokenValue(2, bufferIndex - 1);
        }
        else
        {
            auto a = appender!(ubyte[])();
            foreach (b; std.range.chunks(buffer[2 .. bufferIndex - 1], 2))
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
        assert((buffer[0] >= '0' && buffer[0] <= '9') || buffer[0] == '.');
    }
    body
    {
        // hex and binary can start with zero, anything else is decimal
        if (buffer[0] != '0' || isEoF())
            lexDecimal();
        else
        {
            switch (currentElement())
            {
            case 'x':
            case 'X':
                keepNonNewlineChar();
                lexHex();
                break;
            case 'b':
            case 'B':
                keepNonNewlineChar();
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
        switch (currentElement())
        {
        case 'L':
            keepNonNewlineChar();
            current.type = TokenType.doubleLiteral;
            break;
        case 'f':
        case 'F':
            keepNonNewlineChar();
            current.type = TokenType.floatLiteral;
            break;
        default:
            break;
        }
        if (!isEoF() && currentElement() == 'i')
        {
            keepNonNewlineChar();
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
        while (!isEoF())
        {
            switch (currentElement())
            {
            case 'u':
            case 'U':
                if (foundU)
                    return;
                switch (current.type)
                {
                case TokenType.intLiteral:
                    current.type = TokenType.uintLiteral;
                    keepNonNewlineChar();
                    break;
                case TokenType.longLiteral:
                    current.type = TokenType.ulongLiteral;
                    keepNonNewlineChar();
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
                    keepNonNewlineChar();
                    break;
                case TokenType.uintLiteral:
                    current.type = TokenType.ulongLiteral;
                    keepNonNewlineChar();
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
        assert (currentElement() == 'e' || currentElement() == 'E' || currentElement() == 'p'
            || currentElement() == 'P');
    }
    body
    {
        keepNonNewlineChar();
        bool foundSign = false;
        bool foundDigit = false;
        while (!isEoF())
        {
            switch (currentElement())
            {
            case '-':
            case '+':
                if (foundSign || foundDigit)
                {
                    errorMessage("Expected an exponent");
                    return;
                }
                foundSign = true;
                keepNonNewlineChar();
                break;
            case '0': .. case '9':
            case '_':
                foundDigit = true;
                keepNonNewlineChar();
                break;
            case 'L':
            case 'f':
            case 'F':
            case 'i':
                lexFloatSuffix();
                return;
            default:
                if (!foundDigit)
                    errorMessage("Expected an exponent");
                return;
            }
        }
    }

    void lexDecimal()
    in
    {
        assert ((buffer[0] >= '0' && buffer[0] <= '9') || buffer[0] == '.');
    }
    body
    {
        bool foundDot = buffer[0] == '.';
        current.type = TokenType.intLiteral;
        scope(exit) setTokenValue();
        decimalLoop: while (!isEoF())
        {
            switch (currentElement())
            {
            case '0': .. case '9':
            case '_':
                keepNonNewlineChar();
                break;
            case 'u':
            case 'U':
                if (foundDot)
                {
                    errorMessage("Floating-point literal cannot have %s suffix".format(
                        cast(char) currentElement()));
                    return;
                }
                else
                    lexIntSuffix();
                return;
            case 'i':
                lexFloatSuffix();
                return;
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
                static if (isArray!R)
                    auto r = range[index .. $];
                else
                    auto r = range.save();
                r.popFront();
                if (!r.isRangeEoF() && r.front == '.')
                    break decimalLoop; // possibly slice expression
                if (foundDot)
                    break decimalLoop; // two dots with other characters between them
                keepNonNewlineChar();
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
        scope(exit) setTokenValue();
        binaryLoop: while (!isEoF())
        {
            switch (currentElement())
            {
            case '0':
            case '1':
            case '_':
                keepNonNewlineChar();
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
        scope(exit) setTokenValue();
        bool foundDot;
        hexLoop: while (!isEoF())
        {
            switch (currentElement())
            {
            case 'a': .. case 'f':
            case 'A': .. case 'F':
            case '0': .. case '9':
            case '_':
                keepNonNewlineChar();
                break;
            case 'u':
            case 'U':
                lexIntSuffix();
                return;
            case 'i':
                if (foundDot)
                    lexFloatSuffix();
                return;
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
                static if (isArray!R)
                    auto r = range[index .. $];
                else
                    auto r = range.save();
                r.popFront();
                if (!r.isRangeEoF() && r.front == '.')
                    break hexLoop; // slice expression
                if (foundDot)
                    break hexLoop; // two dots with other characters between them
                keepNonNewlineChar();
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
        if (!isEoF())
        {
            switch (currentElement())
            {
            case 'w':
                current.type = TokenType.wstringLiteral;
                goto case 'c';
            case 'd':
                current.type = TokenType.dstringLiteral;
                goto case 'c';
            case 'c':
                keepNonNewlineChar();
                break;
            default:
                break;
            }
        }
    }

    void lexCharacterLiteral()
    in
    {
        assert (currentElement() == '\'');
    }
    body
    {
        current.type = TokenType.characterLiteral;
        scope (exit)
        {
            if (config.tokenStyle & TokenStyle.includeQuotes)
                setTokenValue();
            else
                setTokenValue(1, bufferIndex - 1);
        }
        keepChar();
        if (isEoF())
        {
            errorMessage("Unterminated character literal");
            return;
        }
        switch (currentElement())
        {
            case '\'':
                return;
            case '\\':
                lexEscapeSequence();
                break;
            default:
                if (currentElement() & 0x80)
                {
                    while (currentElement() & 0x80)
                        keepChar();
                    break;
                }
                else
                {
                    keepChar();
                    break;
                }
        }
        if (currentElement() != '\'')
        {
            errorMessage("Expected \"'\" to end character literal");
            return;
        }
        keepChar();
    }

    void lexString()
    in
    {
        assert (currentElement() == '"' || currentElement() == '`');
    }
    body
    {
        current.type = TokenType.stringLiteral;
        bool isWysiwyg = buffer[0] == 'r' || currentElement() == '`';

        scope (exit)
        {
            if (config.tokenStyle & TokenStyle.includeQuotes)
                setTokenValue();
            else
            {
                if (buffer[0] == 'r')
                    setTokenValue(2, bufferIndex - 1);
                else
                    setTokenValue(1, bufferIndex - 1);
            }
        }

        auto quote = currentElement();
        keepChar();
        while (true)
        {
            if (isEoF())
            {
                errorMessage("Unterminated string literal");
                return;
            }
            else if (currentElement() == '\\')
            {
                if (isWysiwyg)
                    keepChar();
                else
                    lexEscapeSequence();
            }
            else if (currentElement() == quote)
            {
                keepNonNewlineChar();
                break;
            }
            else
                keepChar();
        }
        lexStringSuffix();
    }

    void lexEscapeSequence()
    in
    {
        assert (currentElement() == '\\');
    }
    body
    {
        if (config.tokenStyle & TokenStyle.notEscaped)
        {
            keepChar();
            switch (currentElement())
            {
            case '\'':
            case '"':
            case '?':
            case '\\':
            case 'a':
            case 'b':
            case 'f':
            case 'n':
            case 'r':
            case 't':
            case 'v':
            case 0x0a:
            case 0x00:
                keepChar();
                return;
            case '0': .. case '7':
                foreach(i; 0 .. 3)
                {
                    keepChar();
                    if (currentElement() < '0' || currentElement() > '7') return;
                }
                return;
            case 'x':
                keepChar();
                foreach(i; 0 .. 4)
                {
                    if (!isHexDigit(currentElement()))
                    {
                        errorMessage("Expected hex digit");
                        return;
                    }
                    keepChar();
                }
                return;
            case 'u':
            case 'U':
                uint digits = currentElement() == 'u' ? 4 : 8;
                keepChar();
                foreach (i; 0 .. digits)
                {
                    if (!isHexDigit(currentElement()))
                    {
                        errorMessage("Expected hex digit instead of %s".format(
                            cast(char) currentElement()));
                        return;
                    }
                    keepChar();
                }
                return;
            case '&':
                while (!isEoF())
                {
                    keepChar();
                    if (currentElement() == ';')
                        break;
                }
                return;
            default:
                errorMessage("Invalid escape sequence");
                return;
            }
        }
        else
        {
            advanceRange();
            switch (currentElement())
            {
            case '\'': bufferChar('\''); advanceRange(); return;
            case '"':  bufferChar('"');  advanceRange(); return;
            case '?':  bufferChar('\?'); advanceRange(); return;
            case '\\': bufferChar('\\'); advanceRange(); return;
            case 'a':  bufferChar('\a'); advanceRange(); return;
            case 'b':  bufferChar('\b'); advanceRange(); return;
            case 'f':  bufferChar('\f'); advanceRange(); return;
            case 'n':  bufferChar('\n'); advanceRange(); return;
            case 'r':  bufferChar('\r'); advanceRange(); return;
            case 't':  bufferChar('\t'); advanceRange(); return;
            case 'v':  bufferChar('\v'); advanceRange(); return;
            case 0x0a: bufferChar(0x0a); advanceRange(); return;
            case 0x00: bufferChar(0x00); advanceRange(); return;
            case '0': .. case '7':
                ubyte[3] digits;
                size_t i;
                while(i < 3 && !isEoF())
                {
                    digits[i++] = currentElement();
                    advanceRange();
                    if (currentElement() < '0' || currentElement() > '7') break;
                }
                decodeAndStore(digits, i, 8);
                return;
            case 'x':
                ubyte[2] digits;
                advanceRange();
                foreach(i; 0 .. 2)
                {
                    if (!isHexDigit(currentElement()))
                    {
                        errorMessage("Expected hex digit");
                        return;
                    }
                    digits[i] = currentElement();
                    advanceRange();
                }
                decodeAndStore(digits, 2, 16);
                return;
            case 'u':
            case 'U':
                uint digitCount = currentElement() == 'u' ? 4 : 8;
                advanceRange();
                ubyte[8] digits;
                foreach (i; 0 .. digitCount)
                {
                    if (!isHexDigit(currentElement()))
                    {
                        errorMessage("Expected hex digit");
                        return;
                    }
                    digits[i] = currentElement();
                    advanceRange();
                }
                decodeAndStore(digits, digitCount, 16);
                return;
            case '&':
                advanceRange();
                ubyte[] b;
                while (!isEoF())
                {
                    if (isAlpha(currentElement()))
                    {
                        b ~= currentElement();
                        advanceRange();
                    }
                    else if (currentElement() == ';')
                    {
                        advanceRange();
                        break;
                    }
                    else
                    {
                        errorMessage("Invalid character entity");
                        return;
                    }
                }
                auto entity = (cast(string) b) in characterEntities;
                if (entity is null)
                {
                    errorMessage("Invalid character entity \"&%s;\"".format(
                        cast(char[]) b));
                    return;
                }
                else
                {
                    for (size_t i = 0; i < (*entity).length; i++)
                        bufferChar(cast(ubyte) (*entity)[i]);
                }
                return;
            default:
                errorMessage("Invalid escape sequence");
                return;
            }
        }
    }

    void decodeAndStore(ubyte[] digits, size_t maxIndex, uint base)
    {
        scope(failure)
        {
            import std.stdio;
            stderr.writeln("Failed on line ", lineNumber, " of file ",
                config.fileName);
        }
        char[4] codeUnits;
        auto source = cast(char[]) digits[0 .. maxIndex];
        uint codePoint = parse!uint(source, base);
        ulong unitCount = encode(codeUnits, codePoint);
        foreach (i; 0 .. unitCount)
            bufferChar(codeUnits[i]);
    }

    void lexDelimitedString()
    in
    {
        assert(currentElement() == '"');
    }
    body
    {
        current.type = TokenType.stringLiteral;

        keepChar();

        bool heredoc;
        ubyte open;
        ubyte close;

        switch (currentElement())
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
        assert(buffer[0 .. 2] == `q"`);
    }
    body
    {
        current.type = TokenType.stringLiteral;
        int depth = 1;
        keepChar();
        scope (exit)
        {
            if (config.tokenStyle & TokenStyle.includeQuotes)
                setTokenValue();
            else
                setTokenValue(3, bufferIndex - 2);
        }
        while (true)
        {
            if (isEoF())
                errorMessage("Unterminated string literal");
            if (currentElement() == open)
            {
                keepChar();
                ++depth;
            }
            else if (currentElement() == close)
            {
                keepChar();
                --depth;
                if (depth <= 0)
                {
                    static if (isArray!R)
                        auto r = range[index .. $];
                    else
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
            if (isEoF())
            {
                errorMessage("Unterminated string literal");
                return;
            }
            else if (isNewline(currentElement()))
            {
                keepChar();
                break;
            }
            else if (isSeparating())
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
                setTokenValue();
            else
            {
                size_t b = 2 + ident.length;
                if (buffer[b] == '\r') ++b;
                if (buffer[b] == '\n') ++b;
                size_t e = bufferIndex;
                if (buffer[e - 1] == 'c' || buffer[e - 1] == 'd' || buffer[e - 1] == 'w')
                    --e;
                setTokenValue(b, e);
            }
        }

        while (true)
        {
            if (isEoF())
            {
                errorMessage("Unterminated string literal");
                return;
            }
            else if (buffer[bufferIndex - ident.length .. bufferIndex] == ident)
            {
                if (currentElement() == '"')
                {
                    keepChar();
                    lexStringSuffix();
                    return;
                }
                else
                {
                    errorMessage("Unterminated string literal");
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
        assert (currentElement() == '{');
    }
    body
    {
        current.type = TokenType.stringLiteral;
        keepChar();
        LexerConfig c = config;
        config.iterStyle = IterationStyle.everything;
        config.tokenStyle = TokenStyle.source;
        size_t bi;
        ubyte[] b = uninitializedArray!(ubyte[])(1024 * 4);
        int depth = 1;
        while (!isEoF())
        {
            advance();
            while (bi + current.value.length >= b.length)
                b.length += 1024 * 4;
            b[bi .. bi + current.value.length] = cast(ubyte[]) current.value;
            bi += current.value.length;
            if (current.type == TokenType.lBrace)
                ++depth;
            else if (current.type == TokenType.rBrace)
            {
                --depth;
                if (depth <= 0)
                    break;
            }
        }
        config = c;
        buffer[0] = 'q';
        buffer[1] = '{';
        buffer[2 .. bi + 2] = b[0 .. bi];
        bi++;
        buffer[bi++] = '}';
        bufferIndex = bi;
        if (config.tokenStyle & TokenStyle.includeQuotes)
            setTokenValue();
        else
            setTokenValue(2, bufferIndex - 1);
        lexStringSuffix();
    }

    void lexSpecialTokenSequence()
    in
    {
        assert (currentElement() == '#');
    }
    body
    {
        keepChar();
        static if (isArray!R)
            auto r = range[index .. $];
        else
            auto r = range.save();
        auto app = appender!(ubyte[])();
        app.put('#');
        while (true)
        {
            if (r.isRangeEoF())
            {
                errorMessage("Found EOF when interpreting special token sequence");
                return;
            }
            else if (isNewline(r.front))
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
            foreach (i; 0 .. app.data.length)
                advanceRange();
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

    void keepNonNewlineChar()
    {
        if (bufferIndex >= buffer.length)
            buffer.length += 1024;
        static if (isArray!R)
            buffer[bufferIndex++] = range[index++];
        else
        {
            buffer[bufferIndex++] = currentElement();
            advanceRange();
        }
        ++column;
    }

    void bufferChar(ubyte ch)
    {
        if (bufferIndex >= buffer.length)
            buffer.length += 1024;
        buffer[bufferIndex++] = ch;
    }

    void keepChar()
    {
        while (bufferIndex + 2 >= buffer.length)
            buffer.length += 1024;
        bool foundNewline;
        if (currentElement() == '\r')
        {
            static if (isArray!R)
            {
                buffer[bufferIndex++] = range[index++];
            }
            else
            {
                buffer[bufferIndex++] = currentElement();
                advanceRange();
            }
            foundNewline = true;
        }
        if (currentElement() == '\n')
        {
            static if (isArray!R)
            {
                buffer[bufferIndex++] = range[index++];
            }
            else
            {
                buffer[bufferIndex++] = currentElement();
                advanceRange();
            }
            foundNewline = true;
        }
        else
        {
            static if (isArray!R)
            {
                buffer[bufferIndex++] = range[index++];
            }
            else
            {
                buffer[bufferIndex++] = currentElement();
                advanceRange();
            }
            ++column;
        }
        if (foundNewline)
        {
            ++lineNumber;
            column = 0;
        }
    }

    ElementType!R currentElement() const nothrow
    {
        assert (index < range.length);
        static if (isArray!R)
            return range[index];
        else
            return range.front;
    }

    void advanceRange()
    {
        static if (!isArray!R)
            range.popFront();
        ++index;
    }

    void setTokenValue(size_t startIndex = 0, size_t endIndex = 0)
    {
        if (endIndex == 0)
            endIndex = bufferIndex;
        current.value = cache.get(buffer[startIndex .. endIndex]);
    }

    bool isEoF() const
    {
        static if (isArray!R)
        {
            return index >= range.length || range[index] == 0 || range[index] == 0x1a;
        }
        else
            return range.empty || range.front == 0 || range.front == 0x1a;
    }

    bool isSeparating() const nothrow
    {
        auto ch = currentElement();
        if (ch <= 0x2f) return true;
        if (ch >= ':' && ch <= '@') return true;
        if (ch >= '[' && ch <= '^') return true;
        if (ch >= '{' && ch <= '~') return true;
        if (ch == '`') return true;
        if (isWhite()) return true;
        return false;
    }

    bool isWhite() const nothrow
    {
        auto c = currentElement();
        if (c & 0x80) // multi-byte utf-8
        {
            static if (isArray!R)
            {
                if (index + 2 >= range.length) return false;
                if (range[index] != 0xe2) return false;
                if (range[index + 1] != 0x80) return false;
                if (range[index + 2] != 0xa8 && range[index + 2] != 0xa9) return false;
            }
            else
            {
                auto r = range.save();
                if (r.front != 0xe2)
                    return false;
                else
                    r.popFront();
                if (r.empty || r.front != 0x80)
                    return false;
                else
                    r.popFront();
                if (r.empty || (r.front != 0xa8 && range.front != 0xa9))
                    return false;
            }
            return true;
        }
        else
            return c == 0x20 || (c >= 0x09 && c <= 0x0d);
    }

    immutable bufferSize = 1024 * 8;
    Token current;
    uint lineNumber;
    size_t index;
    uint column;
    R range;
    bool _empty;
    ubyte[] buffer;
    size_t bufferIndex;
    LexerConfig config;
    StringCache cache;
}

/**
 * Returns: true if the token is an operator
 */
pure nothrow bool isOperator(const TokenType t)
{
    return t >= TokenType.assign && t <= TokenType.xorEquals;
}

/**
 * ditto
 */
pure nothrow bool isOperator(ref const Token t)
{
    return isOperator(t.type);
}

/**
 * Returns: true if the token is a keyword
 */
pure nothrow bool isKeyword(const TokenType t)
{
    return t >= TokenType.bool_ && t <= TokenType.with_;
}

/**
 * ditto
 */
pure nothrow bool isKeyword(ref const Token t)
{
    return isKeyword(t.type);
}

/**
 * Returns: true if the token is a built-in type
 */
pure nothrow bool isType(const TokenType t)
{
    return t >= TokenType.bool_ && t <= TokenType.wchar_;
}

/**
 * ditto
 */
pure nothrow bool isType(ref const Token t)
{
    return isType(t.type);
}

/**
 * Returns: true if the token is an attribute
 */
pure nothrow bool isAttribute(const TokenType t)
{
    return t >= TokenType.align_ && t <= TokenType.static_;
}

/**
 * ditto
 */
pure nothrow bool isAttribute(ref const Token t)
{
    return isAttribute(t.type);
}

/**
 * Returns: true if the token is a protection attribute
 */
pure nothrow bool isProtection(const TokenType t)
{
    return t >= TokenType.export_ && t <= TokenType.public_;
}

/**
 * ditto
 */
pure nothrow bool isProtection(ref const Token t)
{
    return isProtection(t.type);
}

/**
 * Returns: true if the token is a compile-time constant such as ___DATE__
 */
pure nothrow bool isConstant(const TokenType t)
{
    return t >= TokenType.date && t <= TokenType.traits;
}

/**
 * ditto
 */
pure nothrow bool isConstant(ref const Token t)
{
    return isConstant(t.type);
}

/**
 * Returns: true if the token is a string or number literal
 */
pure nothrow bool isLiteral(const TokenType t)
{
    return t >= TokenType.doubleLiteral && t <= TokenType.wstringLiteral;
}

/**
 * ditto
 */
pure nothrow bool isLiteral(ref const Token t)
{
    return isLiteral(t.type);
}

/**
 * Returns: true if the token is a number literal
 */
pure nothrow bool isNumberLiteral(const TokenType t)
{
    return t >= TokenType.doubleLiteral && t <= TokenType.ulongLiteral;
}

/**
 * ditto
 */
pure nothrow bool isNumberLiteral(ref const Token t)
{
    return isNumberLiteral(t.type);
}

/**
 * Returns: true if the token is a string literal
 */
pure nothrow bool isStringLiteral(const TokenType t)
{
    return t >= TokenType.dstringLiteral && t <= TokenType.wstringLiteral;
}

/**
 * ditto
 */
pure nothrow bool isStringLiteral(ref const Token t)
{
    return isStringLiteral(t.type);
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
 * ditto
 */
pure nothrow bool isMisc(ref const Token t)
{
    return isMisc(t.type);
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
    float_, /// $(D_KEYWORD float)
    function_, /// $(D_KEYWORD function)
    idouble_, /// $(D_KEYWORD idouble)
    ifloat_, /// $(D_KEYWORD ifloat)
    int_, /// $(D_KEYWORD int)
    ireal_, /// $(D_KEYWORD ireal)
    long_, /// $(D_KEYWORD long)
    real_, /// $(D_KEYWORD real)
    short_, /// $(D_KEYWORD short)
    ubyte_, /// $(D_KEYWORD ubyte)
    ucent_, /// $(D_KEYWORD ucent)
    uint_, /// $(D_KEYWORD uint)
    ulong_, /// $(D_KEYWORD ulong)
    ushort_, /// $(D_KEYWORD ushort)
    void_, /// $(D_KEYWORD void)
    wchar_, /// $(D_KEYWORD wchar)

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
    characterLiteral, /// 'a'
    dstringLiteral, /// $(D_STRING "32-bit character string"d)
    stringLiteral, /// $(D_STRING "an 8-bit string")
    wstringLiteral, /// $(D_STRING "16-bit character string"w)
}

// Implementation details follow
private:

pure nothrow bool isRangeEoF(R)(ref R range)
{
    return range.empty || range.front == 0 || range.front == 0x1a;
}

/*
* Slices of the above string to save memory. This array is automatically
* generated.
*/
immutable(string[TokenType.max + 1]) tokenValues = [
    "=",
    "@",
    "&",
    "&=",
    "|",
    "|=",
    "~=",
    ":",
    ",",
    "--",
    "/",
    "/=",
    "$",
    ".",
    "==",
    "=>",
    ">",
    ">=",
    "#",
    "++",
    "{",
    "[",
    "<",
    "<=",
    "<>=",
    "<>",
    "&&",
    "||",
    "(",
    "-",
    "-=",
    "%",
    "%=",
    "*=",
    "!",
    "!=",
    "!>",
    "!>=",
    "!<",
    "!<=",
    "!<>",
    "+",
    "+=",
    "^^",
    "^^=",
    "}",
    "]",
    ")",
    ";",
    "<<",
    "<<=",
    ">>",
    ">>=",
    "..",
    "*",
    "?",
    "~",
    "!<>=",
    ">>>",
    ">>>=",
    "...",
    "^",
    "^=",
    "bool",
    "byte",
    "cdouble",
    "cent",
    "cfloat",
    "char",
    "creal",
    "dchar",
    "double",
    "float",
    "function",
    "idouble",
    "ifloat",
    "int",
    "ireal",
    "long",
    "real",
    "short",
    "ubyte",
    "ucent",
    "uint",
    "ulong",
    "ushort",
    "void",
    "wchar",
    "align",
    "deprecated",
    "extern",
    "pragma",
    "export",
    "package",
    "private",
    "protected",
    "public",
    "abstract",
    "auto",
    "const",
    "final",
    "__gshared",
    "immutable",
    "inout",
    "scope",
    "shared",
    "static",
    "synchronized",
    "alias",
    "asm",
    "assert",
    "body",
    "break",
    "case",
    "cast",
    "catch",
    "class",
    "continue",
    "debug",
    "default",
    "delegate",
    "delete",
    "do",
    "else",
    "enum",
    "false",
    "finally",
    "foreach",
    "foreach_reverse",
    "for",
    "goto",
    "if",
    "import",
    "in",
    "interface",
    "invariant",
    "is",
    "lazy",
    "macro",
    "mixin",
    "module",
    "new",
    "nothrow",
    "null",
    "out",
    "override",
    "pure",
    "ref",
    "return",
    "struct",
    "super",
    "switch",
    "template",
    "this",
    "throw",
    "true",
    "try",
    "typedef",
    "typeid",
    "typeof",
    "union",
    "unittest",
    "version",
    "volatile",
    "while",
    "with",
    "__DATE__",
    "__EOF__",
    "__TIME__",
    "__TIMESTAMP__",
    "__VENDOR__",
    "__VERSION__",
    "__FILE__",
    "__LINE__",
    null,
    null,
    null,
    "__traits",
    "__parameters",
    "__vector",
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
    null,
];

pure string getTokenValue(const TokenType type)
{
    return tokenValues[type];
}

private pure bool isNewline(ubyte ch)
{
    return ch == '\n' || ch == '\r';
}

pure nothrow TokenType lookupTokenType(const const(char)[] input)
{
    switch(input.length)
    {
    case 2:
        switch (input[0])
        {
        case 'd': if (input == "do") return TokenType.do_; else break;
        case 'i':
            if (input == "if") return TokenType.if_;
            else if (input == "in") return TokenType.in_;
            else if (input == "is") return TokenType.is_;
            else break;
        default: break;
        }
        break;
    case 3:
        switch (input[0])
        {
        case 'a': if (input == "asm") return TokenType.asm_; else break;
        case 'f': if (input == "for") return TokenType.for_; else break;
        case 'i': if (input == "int") return TokenType.int_; else break;
        case 'n': if (input == "new") return TokenType.new_; else break;
        case 'o': if (input == "out") return TokenType.out_; else break;
        case 'r': if (input == "ref") return TokenType.ref_; else break;
        case 't': if (input == "try") return TokenType.try_; else break;
        default: break;
        }
        break;
    case 4:
        switch (input[0])
        {
        case 'a': if (input == "auto") return TokenType.auto_; else break;
        case 'b': if (input == "body") return TokenType.body_;
            else if (input == "bool") return TokenType.bool_;
            else if (input == "byte") return TokenType.byte_;
            else break;
        case 'c': if (input == "case") return TokenType.case_;
            else if (input == "cast") return TokenType.cast_;
            else if (input == "cent") return TokenType.cent_;
            else if (input == "char") return TokenType.char_;
            else break;
        case 'e': if (input == "else") return TokenType.else_;
            else if (input == "enum") return TokenType.enum_;
            else break;
        case 'g': if (input == "goto") return TokenType.goto_; else break;
        case 'l': if (input == "lazy") return TokenType.lazy_;
            else if (input == "long") return TokenType.long_;
            else break;
        case 'n': if (input == "null") return TokenType.null_; else break;
        case 'p': if (input == "pure") return TokenType.pure_; else break;
        case 'r': if (input == "real") return TokenType.real_; else break;
        case 't': if (input == "this") return TokenType.this_;
            else if (input == "true") return TokenType.true_;
            else break;
        case 'u': if (input == "uint") return TokenType.uint_; else break;
        case 'v': if (input == "void") return TokenType.void_; else break;
        case 'w': if (input == "with") return TokenType.with_; else break;
        default: break;
        }
        break;
    case 5:
        switch (input[0])
        {
        case 'a': if (input == "alias") return TokenType.alias_;
            else if (input == "align") return TokenType.align_; else break;
        case 'b': if (input == "break") return TokenType.break_; else break;
        case 'c': if (input == "catch") return TokenType.catch_;
            else if (input == "class") return TokenType.class_;
            else if (input == "const") return TokenType.const_;
            else if (input == "creal") return TokenType.creal_;
            else break;
        case 'd': if (input == "dchar") return TokenType.dchar_;
            else if (input == "debug") return TokenType.debug_; else break;
        case 'f': if (input == "false") return TokenType.false_;
            else if (input == "final") return TokenType.final_;
            else if (input == "float") return TokenType.float_;
            else break;
        case 'i': if (input == "inout") return TokenType.inout_;
            else if (input == "ireal") return TokenType.ireal_; else break;
        case 'm': if (input == "macro") return TokenType.macro_;
            else if (input == "mixin") return TokenType.mixin_; else break;
        case 's': if (input == "scope") return TokenType.scope_;
            else if (input == "short") return TokenType.short_;
            else if (input == "super") return TokenType.super_; else break;
        case 't': if (input == "throw") return TokenType.throw_; else break;
        case 'u': if (input == "ubyte") return TokenType.ubyte_;
            else if (input == "ucent") return TokenType.ucent_;
            else if (input == "ulong") return TokenType.ulong_;
            else if (input == "union") return TokenType.union_;
            else break;
        case 'w': if (input == "wchar") return TokenType.wchar_;
            else if (input == "while") return TokenType.while_;
            else break;
        default: break;
        }
        break;
    case 6:
        switch (input[0])
        {
        case 'a': if (input == "assert") return TokenType.assert_; else break;
        case 'c': if (input == "cfloat") return TokenType.cfloat_; else break;
        case 'd': if (input == "delete") return TokenType.delete_;
            else if (input == "double") return TokenType.double_; else break;
        case 'e': if (input == "export") return TokenType.export_;
            else if (input == "extern") return TokenType.extern_; else break;
        case 'i': if (input == "ifloat") return TokenType.ifloat_;
            else if (input == "import") return TokenType.import_; else break;
        case 'm': if (input == "module") return TokenType.module_; else break;
        case 'p': if (input == "pragma") return TokenType.pragma_;
            else if (input == "public") return TokenType.public_; else break;
        case 'r': if (input == "return") return TokenType.return_; else break;
        case 's': if (input == "shared") return TokenType.shared_;
            else if (input == "static") return TokenType.static_;
            else if (input == "struct") return TokenType.struct_;
            else if (input == "switch") return TokenType.switch_; else break;
        case 't': if (input == "typeid") return TokenType.typeid_;
            else if (input == "typeof") return TokenType.typeof_; else break;
        case 'u': if (input == "ushort") return TokenType.ushort_; else break;
        default: break;
        }
        break;
    case 7:
        switch (input[0])
        {
        case '_': if (input == "__EOF__") return TokenType.eof; else break;
        case 'c': if (input == "cdouble") return TokenType.cdouble_; else break;
        case 'd': if (input == "default") return TokenType.default_; else break;
        case 'f': if (input == "finally") return TokenType.finally_;
            else if (input == "foreach") return TokenType.foreach_; else break;
        case 'i': if (input == "idouble") return TokenType.idouble_; else break;
        case 'n': if (input == "nothrow") return TokenType.nothrow_; else break;
        case 'p': if (input == "package") return TokenType.package_;
            else if (input == "private") return TokenType.private_; else break;
        case 't': if (input == "typedef") return TokenType.typedef_; else break;
        case 'v': if (input == "version") return TokenType.version_; else break;
        default: break;
        }
        break;
    case 8:
        switch (input[0])
        {
        case '_': if (input == "__DATE__") return TokenType.date;
            else if (input == "__FILE__") return TokenType.file;
            else if (input == "__LINE__") return TokenType.line;
            else if (input == "__TIME__") return TokenType.time;
            else if (input == "__traits") return TokenType.traits; else break;
        case 'a': if (input == "abstract") return TokenType.abstract_; else break;
        case 'c': if (input == "continue") return TokenType.continue_; else break;
        case 'd': if (input == "delegate") return TokenType.delegate_; else break;
        case 'f': if (input == "function") return TokenType.function_; else break;
        case 'o': if (input == "override") return TokenType.override_; else break;
        case 't': if (input == "template") return TokenType.template_; else break;
        case 'u': if (input == "unittest") return TokenType.unittest_; else break;
        case 'v': if (input == "volatile") return TokenType.volatile_; else break;
        default: break;
        }
        break;
    case 9:
        switch (input[0])
        {
        case '_': if (input == "__gshared") return TokenType.gshared; else break;
        case 'i': if (input == "immutable") return TokenType.immutable_;
            else if (input == "interface") return TokenType.interface_;
            else if (input == "invariant") return TokenType.invariant_; else break;
        case 'p': if (input == "protected") return TokenType.protected_; else break;
        default: break;
        }
        break;
    case 10:
        switch (input[0])
        {
        case 'd': if (input == "deprecated") return TokenType.deprecated_; else break;
        case '_': if (input == "__VENDOR__") return TokenType.vendor; else break;
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
        caseStatement ~= indentString;
        caseStatement ~= "\tkeepNonNewlineChar();\n";
        if (v.children.length > 0)
        {
            caseStatement ~= indentString;
            caseStatement ~= "\tif (isEoF())\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t{\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t\tcurrent.value = getTokenValue(current.type);\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t\tcurrent.type = " ~ node.children[k].value;
            caseStatement ~= ";\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t\treturn;\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t}\n";
            caseStatement ~= indentString;
            caseStatement ~= "\tswitch (currentElement())\n";
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
            caseStatement ~= "\t\tcurrent.value = getTokenValue(current.type);\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t\treturn;\n";
            caseStatement ~= indentString;
            caseStatement ~= "\t}\n";
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
            caseStatement ~= "\treturn;\n";
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


struct StringCache
{
    string get(const ubyte[] bytes)
    {
        size_t bucket;
        hash_t h;
        string* val = find(bytes, bucket, h);
        if (val !is null)
        {
            return *val;
        }
        else
        {
            auto s = putIntoCache(bytes);
            index[bucket] ~= s;
            return s;
        }
    }

private:

    import std.stdio;
    string* find(const ubyte[] data, out size_t bucket, out hash_t h)
    {
        h = hash(data);
        bucket = h % mapSize;
        foreach (i; 0 .. index[bucket].length)
        {
            if (index[bucket][i] == data)
            {
                return &index[bucket][i];
            }
        }
        return null;
    }

    static hash_t hash(const(ubyte)[] data)
    {
        uint hash = 0;
        foreach ( b; data)
        {
            hash ^= sbox[b];
            hash *= 3;
        }
        return hash;
    }

    enum mapSize = 2048;
    string[][mapSize] index;
    // leave some slack for alloctors/GC meta-data
    enum chunkSize = 16*1024 - size_t.sizeof*8;
    ubyte*[] chunkS;
    size_t next = chunkSize;

    string putIntoCache(const ubyte[] data)
    {
        import core.memory;

        if(next + data.length > chunkSize)
        {
            // avoid huge strings
            if(data.length > chunkSize/4)
                return (cast(char[])data).idup;
            chunkS ~= cast(ubyte*)GC.malloc(chunkSize,
                GC.BlkAttr.NO_SCAN | GC.BlkAttr.NO_INTERIOR);
            next = 0;
        }
        auto slice = chunkS[$-1][next..next+data.length];
        slice[] = data[];
        next += data.length;
        return cast(string)slice;
    }

}

immutable uint[] sbox = [
    0xF53E1837, 0x5F14C86B, 0x9EE3964C, 0xFA796D53,
    0x32223FC3, 0x4D82BC98, 0xA0C7FA62, 0x63E2C982,
    0x24994A5B, 0x1ECE7BEE, 0x292B38EF, 0xD5CD4E56,
    0x514F4303, 0x7BE12B83, 0x7192F195, 0x82DC7300,
    0x084380B4, 0x480B55D3, 0x5F430471, 0x13F75991,
    0x3F9CF22C, 0x2FE0907A, 0xFD8E1E69, 0x7B1D5DE8,
    0xD575A85C, 0xAD01C50A, 0x7EE00737, 0x3CE981E8,
    0x0E447EFA, 0x23089DD6, 0xB59F149F, 0x13600EC7,
    0xE802C8E6, 0x670921E4, 0x7207EFF0, 0xE74761B0,
    0x69035234, 0xBFA40F19, 0xF63651A0, 0x29E64C26,
    0x1F98CCA7, 0xD957007E, 0xE71DDC75, 0x3E729595,
    0x7580B7CC, 0xD7FAF60B, 0x92484323, 0xA44113EB,
    0xE4CBDE08, 0x346827C9, 0x3CF32AFA, 0x0B29BCF1,
    0x6E29F7DF, 0xB01E71CB, 0x3BFBC0D1, 0x62EDC5B8,
    0xB7DE789A, 0xA4748EC9, 0xE17A4C4F, 0x67E5BD03,
    0xF3B33D1A, 0x97D8D3E9, 0x09121BC0, 0x347B2D2C,
    0x79A1913C, 0x504172DE, 0x7F1F8483, 0x13AC3CF6,
    0x7A2094DB, 0xC778FA12, 0xADF7469F, 0x21786B7B,
    0x71A445D0, 0xA8896C1B, 0x656F62FB, 0x83A059B3,
    0x972DFE6E, 0x4122000C, 0x97D9DA19, 0x17D5947B,
    0xB1AFFD0C, 0x6EF83B97, 0xAF7F780B, 0x4613138A,
    0x7C3E73A6, 0xCF15E03D, 0x41576322, 0x672DF292,
    0xB658588D, 0x33EBEFA9, 0x938CBF06, 0x06B67381,
    0x07F192C6, 0x2BDA5855, 0x348EE0E8, 0x19DBB6E3,
    0x3222184B, 0xB69D5DBA, 0x7E760B88, 0xAF4D8154,
    0x007A51AD, 0x35112500, 0xC9CD2D7D, 0x4F4FB761,
    0x694772E3, 0x694C8351, 0x4A7E3AF5, 0x67D65CE1,
    0x9287DE92, 0x2518DB3C, 0x8CB4EC06, 0xD154D38F,
    0xE19A26BB, 0x295EE439, 0xC50A1104, 0x2153C6A7,
    0x82366656, 0x0713BC2F, 0x6462215A, 0x21D9BFCE,
    0xBA8EACE6, 0xAE2DF4C1, 0x2A8D5E80, 0x3F7E52D1,
    0x29359399, 0xFEA1D19C, 0x18879313, 0x455AFA81,
    0xFADFE838, 0x62609838, 0xD1028839, 0x0736E92F,
    0x3BCA22A3, 0x1485B08A, 0x2DA7900B, 0x852C156D,
    0xE8F24803, 0x00078472, 0x13F0D332, 0x2ACFD0CF,
    0x5F747F5C, 0x87BB1E2F, 0xA7EFCB63, 0x23F432F0,
    0xE6CE7C5C, 0x1F954EF6, 0xB609C91B, 0x3B4571BF,
    0xEED17DC0, 0xE556CDA0, 0xA7846A8D, 0xFF105F94,
    0x52B7CCDE, 0x0E33E801, 0x664455EA, 0xF2C70414,
    0x73E7B486, 0x8F830661, 0x8B59E826, 0xBB8AEDCA,
    0xF3D70AB9, 0xD739F2B9, 0x4A04C34A, 0x88D0F089,
    0xE02191A2, 0xD89D9C78, 0x192C2749, 0xFC43A78F,
    0x0AAC88CB, 0x9438D42D, 0x9E280F7A, 0x36063802,
    0x38E8D018, 0x1C42A9CB, 0x92AAFF6C, 0xA24820C5,
    0x007F077F, 0xCE5BC543, 0x69668D58, 0x10D6FF74,
    0xBE00F621, 0x21300BBE, 0x2E9E8F46, 0x5ACEA629,
    0xFA1F86C7, 0x52F206B8, 0x3EDF1A75, 0x6DA8D843,
    0xCF719928, 0x73E3891F, 0xB4B95DD6, 0xB2A42D27,
    0xEDA20BBF, 0x1A58DBDF, 0xA449AD03, 0x6DDEF22B,
    0x900531E6, 0x3D3BFF35, 0x5B24ABA2, 0x472B3E4C,
    0x387F2D75, 0x4D8DBA36, 0x71CB5641, 0xE3473F3F,
    0xF6CD4B7F, 0xBF7D1428, 0x344B64D0, 0xC5CDFCB6,
    0xFE2E0182, 0x2C37A673, 0xDE4EB7A3, 0x63FDC933,
    0x01DC4063, 0x611F3571, 0xD167BFAF, 0x4496596F,
    0x3DEE0689, 0xD8704910, 0x7052A114, 0x068C9EC5,
    0x75D0E766, 0x4D54CC20, 0xB44ECDE2, 0x4ABC653E,
    0x2C550A21, 0x1A52C0DB, 0xCFED03D0, 0x119BAFE2,
    0x876A6133, 0xBC232088, 0x435BA1B2, 0xAE99BBFA,
    0xBB4F08E4, 0xA62B5F49, 0x1DA4B695, 0x336B84DE,
    0xDC813D31, 0x00C134FB, 0x397A98E6, 0x151F0E64,
    0xD9EB3E69, 0xD3C7DF60, 0xD2F2C336, 0x2DDD067B,
    0xBD122835, 0xB0B3BD3A, 0xB0D54E46, 0x8641F1E4,
    0xA0B38F96, 0x51D39199, 0x37A6AD75, 0xDF84EE41,
    0x3C034CBA, 0xACDA62FC, 0x11923B8B, 0x45EF170A,
];

unittest
{
    import std.stdio;
    auto source = cast(ubyte[]) (
        " bool byte cdouble cent cfloat char creal dchar double float function"
        ~ " idouble ifloat int ireal long real short ubyte ucent uint ulong"
        ~ " ushort void wchar align deprecated extern pragma export package private"
        ~ " protected public abstract auto const final __gshared immutable inout"
        ~ " scope shared static synchronized alias asm assert body break case"
        ~ " cast catch class continue debug default delegate delete do else"
        ~ " enum false finally foreach foreach_reverse for goto if import in"
        ~ " interface invariant is lazy macro mixin module new nothrow null"
        ~ " out override pure ref return struct super switch template this"
        ~ " throw true try typedef typeid typeof union unittest version volatile"
        ~ " while with __traits __parameters __vector");
    auto expected = ["bool", "byte", "cdouble",
        "cent", "cfloat", "char", "creal",
        "dchar", "double", "float", "function",
        "idouble", "ifloat", "int", "ireal", "long",
        "real", "short", "ubyte", "ucent", "uint",
        "ulong", "ushort", "void", "wchar", "align",
        "deprecated", "extern", "pragma", "export",
        "package", "private", "protected", "public",
        "abstract", "auto", "const", "final", "__gshared",
        "immutable", "inout", "scope", "shared",
        "static", "synchronized", "alias", "asm", "assert",
        "body", "break", "case", "cast", "catch",
        "class", "continue", "debug", "default", "delegate",
        "delete", "do", "else", "enum", "false",
        "finally", "foreach", "foreach_reverse", "for",
        "goto", "if", "import", "in", "interface",
        "invariant", "is", "lazy","macro", "mixin",
        "module", "new", "nothrow", "null", "out",
        "override", "pure", "ref", "return", "struct",
        "super", "switch", "template", "this", "throw",
        "true", "try", "typedef", "typeid", "typeof",
        "union", "unittest", "version", "volatile",
        "while", "with", "__traits", "__parameters", "__vector"];
    LexerConfig config;
    auto tokens = byToken(source, config);
    //writeln(tokens.map!"a.value"().array());
    assert (equal(map!"a.value"(tokens), expected));
}

unittest
{
    auto source = cast(ubyte[]) ("=@& &=| |=~=:,--/ /=$.===>> >=++{[< <=<>=<>&&||(- -=%%=*=!!=!>!>=!<!<=!<>+ +=^^^^=}]);<< <<=>> >>=..*?~!<>=>>>>>>=...^ ^=");
    auto expected = ["=",  "@", "&", "&=", "|", "|=", "~=",
        ":", ",", "--", "/", "/=", "$", ".", "==",
        "=>", ">", ">=", "++", "{", "[", "<",
        "<=", "<>=", "<>", "&&", "||", "(", "-", "-=", "%",
        "%=", "*=", "!", "!=", "!>", "!>=", "!<",
        "!<=", "!<>", "+", "+=", "^^", "^^=",
        "}", "]", ")", ";", "<<", "<<=", ">>",
        ">>=", "..", "*", "?", "~", "!<>=",
        ">>>", ">>>=", "...", "^", "^="];
    LexerConfig config;
    auto tokens = byToken(source, config);
    //writeln(tokens.map!"a.value"().array());
    assert (equal(map!"a.value"(tokens), expected));
}

unittest
{
    auto source = cast(ubyte[]) (`
        1 1.2 //comment
        1.2f 1u 1uL 0b011 0b1uu 0b1 /+abc/+def+/+/0x11001uL
        123e1L 123e+1f 123e-1i 15e++ 4ea 1.2u 4i 1337L 4.2L 1..2 4.3.5.8
    `);
    auto expected = ["1", "1.2", "1.2f", "1u", "1uL", "0b011", "0b1u", "u", "0b1",
        "0x11001uL", "123e1L", "123e+1f", "123e-1i", "15e+", "+", "4e", "a",
        "1.2", "u", "4i", "1337L", "4.2L", "1", "..", "2", "4.3", ".5", ".8"];
    int errCount = 0;
    void errorFunction(string file, size_t index, uint line, uint col, string msg)
    {
        ++errCount;
    }
    LexerConfig config;
    config.errorFunc = &errorFunction;
    auto tokens = byToken(source, config);
    //writeln(tokens.map!"a.value"().array());
    assert (equal(map!"a.value"(tokens), expected));
    assert (errCount == 3);
}

unittest
{
    auto source = cast(ubyte[]) ("int #line 4\n double q{abcde}");
    LexerConfig config;
    auto tokens = byToken(source, config);
    assert (tokens.front.line == 1);
    assert (tokens.moveFront() == TokenType.int_);
    assert (tokens.front.line == 4);
    assert (isType(tokens.front));
    assert (tokens.front.value == "double");
    tokens.popFront();
    assert (tokens.front.value == "abcde");
    assert (isStringLiteral(tokens.front));
}

unittest
{
    auto source = cast(ubyte[]) (`"string`);
    int errCount = 0;
    void errorFunction(string file, size_t index, uint line, uint col, string msg)
    {
        ++errCount;
    }
    LexerConfig config;
    config.errorFunc = &errorFunction;
    auto tokens = byToken(source, config);
    assert (errCount == 1);
}

unittest
{
    auto source = cast(ubyte[]) ("import foo");
    LexerConfig config;
    auto tokens = byToken(source, config);
    Token a = tokens.moveFront();
    Token b = tokens.moveFront();
    assert (a != b);
    assert (a != "foo");
    assert (a < b);
    assert (b > a);
    assert (!(a > a));
    assert (tokens.empty);
}

unittest
{
    auto source = cast(ubyte[]) ("import std.stdio; void main(){writeln(\"hello world\");}");
    LexerConfig config;
    auto tokens = byToken(source, config);
    int tokenCount = 0;
    foreach (t; tokens)
    {
        ++tokenCount;
    }
    assert (tokenCount == 16);
}


void main(string[] args)
{
}
