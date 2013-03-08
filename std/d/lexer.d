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
* Authors: Brian Schott, Dmitry Olshansky
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
    /// Include _everything
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
     * Replacement for the ___VERSION__ token. Defaults to 100.
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
}

/**
* Iterate over the given range of characters by D tokens.
* Params:
*     range = the range of characters
*     config = the lexer configuration
*     bufferSize = initial size of internal circular buffer
* Returns:
*     an input range of tokens
*/
auto byToken(R)(R range, LexerConfig config, size_t bufferSize = 4*1024)
    if (isForwardRange!(R) && !isRandomAccessRange!(R)
        && is(ElementType!R : const(ubyte)))
{
    // 4K of circular buffer by default
    auto r = TokenRange!(typeof(lexerSource(range)))
        (lexerSource(range, bufferSize), config);
    r.config = config;
    r.lineNumber = 1;
    r.popFront();
    return r;
}

///ditto
auto byToken(R)(R range, LexerConfig config)
    if (isRandomAccessRange!(R) && is(ElementType!R : const(ubyte)))
{
    auto r = TokenRange!(typeof(lexerSource(range)))
        (lexerSource(range), config);
    r.config = config;
    r.lineNumber = 1;
    r.popFront();
    return r;
}

/**
* Range of tokens. Use byToken$(LPAREN)$(RPAREN) to instantiate.
*/
struct TokenRange(LexSrc)
    //if ( is(LexSrc : LexSource!(U...), U...)) //check for LexSource
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
    ref const(Token) front() const @property
    {
        assert(!empty, "trying to get front of an empty token range");
        return current;
    }

    /**
    * Returns the current token and then removes it from the range
    */
    Token moveFront()
    {
        auto r = move(current);
        popFront();
        return r;
    }
    
    /**
    * Removes the current token from the range
    */
    void popFront()
    {
        advance();
    }

private:

    /*
    * Advances the range to the next token
    */
    void advance()
    {
L_advance:
        if (src.empty)
        {
            _empty = true;
            return;
        }
        src.mark(); // mark a start of a lexing "frame"
        current.line = lineNumber;
        current.startIndex = src.index;
        current.column = column;
        current.value = null;
        switch (src.front)
        {
        // handle sentenels for end of input
        case 0:
        case 0x1a:
            // TODO: check config flags, it's cheap
            // since this branch at most is taken once per file
            _empty = true;
            return;
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
            nextCharNonLF();
            if (isEoF())
            {
                current.type = TokenType.div;
                current.value = "/";
                return;
            }
            switch (src.front)
            {
            case '/':
            case '*':
            case '+':
                if (config.iterStyle & IterationStyle.includeComments)
                    return lexComment!true();
                lexComment!false();
                goto L_advance; // tail-recursion

            case '=':
                current.type = TokenType.divEquals;
                current.value = "/=";
                src.popFront();
                return;
            default:
                current.type = TokenType.div;
                current.value = "/";
                return;
            }
        case '.':
            if (!src.canPeek())
            {
                current.type = TokenType.dot;
                current.value = getTokenValue(TokenType.dot);
                return;
            }
            switch (src.peek())
            {
            case '0': .. case '9':
                lexNumber();
                return;
            case '.':
                nextCharNonLF();
                nextCharNonLF();
                current.type = TokenType.slice;
                if (src.front == '.')
                {
                    current.type = TokenType.vararg;
                    nextCharNonLF();
                }
                current.value = getTokenValue(current.type);
                return;
            default:
                nextCharNonLF();
                current.type = TokenType.dot;
                current.value = getTokenValue(TokenType.dot);
                return;
            }
        case '0': .. case '9':
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
            nextCharNonLF();
            if (isEoF())
                goto default;
            switch (src.front)
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
            nextCharNonLF();
            if (isEoF())
                goto default;
            else if (src.front == '"')
            {
                lexString();
                return;
            }
            else
                goto default;
        case 'x':
            nextCharNonLF();
            if (isEoF())
                goto default;
            else if (src.front == '"')
            {
                lexHexString();
                return;
            }
            else
                goto default;
        case '#':
            lexSpecialTokenSequence();
            if(config.iterStyle & IterationStyle.includeSpecialTokens)
                return;
            goto L_advance; // tail-recursion
        // "short" ASCII whites
        case 0x20:
        case 0x09: .. case 0x0d:
             if (config.iterStyle & IterationStyle.includeWhitespace)
                return lexWhitespace!true();
             lexWhitespace!false();
             goto L_advance; // tail-recursion
        default:
            if ((src.front & 0x80) && isLongWhite())
            {
                if (config.iterStyle & IterationStyle.includeWhitespace)
                    return lexWhitespace!true();
                lexWhitespace!false();
                goto L_advance; // tail-recursion
            }
            for(;;)
            {
                if(isSeparating())
                    break;
                nextCharNonLF();
                if(isEoF())
                    break;
            }
            
            current.type = lookupTokenType(src.slice);
            current.value = getTokenValue(current.type);
            if (current.value is null)
                setTokenValue();
            if (!(config.iterStyle & IterationStyle.ignoreEOF) && current.type == TokenType.eof)
            {
                _empty = true;
                return;
            }

            if (config.iterStyle & TokenStyle.doNotReplaceSpecial)
                return;
            expandSpecialToken();
        }
    }

    // TODO: LexSource could be improved for forward ranges
    // to avoid buffering at all (by disabling it for a moment)
    // so keep the 'keep' parameter here and elsewhere
    void lexWhitespace(bool keep)()
    {
        current.type = TokenType.whitespace;
        do
        {
            nextChar();
        } while (!isEoF() && isWhite());
        static if (keep) setTokenValue();
    }

    void lexComment(bool keep)()
    in
    {
        assert (src.front == '/' || src.front == '*' || src.front == '+');
    }
    body
    {
        current.type = TokenType.comment;
        switch(src.front)
        {
        case '/':
            while (!isEoF() && !isNewline(src.front))
            {
                nextCharNonLF();
            }
            break;
        case '*':
            while (!isEoF())
            {
                if (src.front == '*')
                {
                    static if (keep) nextCharNonLF();
                    else src.popFront();
                    if (src.front == '/')
                    {
                        nextCharNonLF();
                        break;
                    }
                }
                else
                    nextChar();
            }
            break;
        case '+':
            int depth = 1;
            while (depth > 0 && !isEoF())
            {
                if (src.front == '+')
                {
                    nextCharNonLF();
                    if (src.front == '/')
                    {
                        nextCharNonLF();
                        --depth;
                    }
                }
                else if (src.front == '/')
                {
                    nextCharNonLF();
                    if (src.front == '+')
                    {
                        nextCharNonLF();
                        ++depth;
                    }
                }
                else
                    nextChar();
            }
            break;
        default:
            assert(false);
        }
        static if (keep)
        setTokenValue();
    }

    void lexHexString()
    in
    {
        assert (src.front == '"');
    }
    body
    {
        current.type = TokenType.stringLiteral;
        nextChar();
        while (true)
        {
            if (isEoF())
            {
                errorMessage("Unterminated hex string literal");
                return;
            }
            else if (isHexDigit(src.front))
            {
                nextCharNonLF();
            }
            else if (isWhite() && (config.tokenStyle & TokenStyle.notEscaped))
            {
                nextChar();
            }
            else if (src.front == '"')
            {
                nextCharNonLF();
                break;
            }
            else
            {
                errorMessage(format("Invalid character '%s' in hex string literal",
                    cast(char) src.front));
                return;
            }
        }
        bool hasSuffix = lexStringSuffix();
        if (config.tokenStyle & TokenStyle.notEscaped)
        {
            if (config.tokenStyle & TokenStyle.includeQuotes)
                setTokenValue();
            else
                setTokenValue(2, hasSuffix ? -2 : -1);
        }
        else
        {
            // TODO: appender is an allocation happy fat pig
            // remove it later
            auto a = appender!(char[])();
            foreach (b; std.range.chunks(src.slice[2 .. $ - 1], 2))
            {
                auto s = cast(char[])b;
                ubyte ch = cast(ubyte)parse!uint(s, 16);
                a.put(ch);
            }
            // can safely assume ownership of data
            current.value = cast(string)a.data;
        }
    }

    void lexNumber()
    in
    {
        assert(isDigit(src.front) || src.front == '.');
    }
    body
    {
        if (src.front != '0')
        {
            lexDecimal();
            return;
        }
        else
        {
            switch (src.peek())
            {
            case 'x':
            case 'X':
                nextCharNonLF();
                nextCharNonLF();
                lexHex();
                break;
            case 'b':
            case 'B':
                nextCharNonLF();
                nextCharNonLF();
                lexBinary();
                break;
            default:
                lexDecimal();
                break;
            }
        }
    }

    void lexFloatSuffix()
    {
        switch (src.front)
        {
        case 'L':
            nextCharNonLF();
            current.type = TokenType.doubleLiteral;
            break;
        case 'f':
        case 'F':
            nextCharNonLF();
            current.type = TokenType.floatLiteral;
            break;
        default:
            break;
        }
        if (!isEoF() && src.front == 'i')
        {
            nextCharNonLF();
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
            switch (src.front)
            {
            case 'u':
            case 'U':
                if (foundU)
                    return;
                switch (current.type)
                {
                case TokenType.intLiteral:
                    current.type = TokenType.uintLiteral;
                    nextCharNonLF();
                    break;
                case TokenType.longLiteral:
                    current.type = TokenType.ulongLiteral;
                    nextCharNonLF();
                    break;
                default:
                    assert (false);
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
                    nextCharNonLF();
                    break;
                case TokenType.uintLiteral:
                    current.type = TokenType.ulongLiteral;
                    nextCharNonLF();
                    break;
                default:
                    assert (false);
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
        assert (src.front == 'e' || src.front == 'E' || src.front == 'p'
            || src.front == 'P');
    }
    body
    {
        nextCharNonLF();
        bool foundSign = false;
        bool foundDigit = false;
        while (!isEoF())
        {
            switch (src.front)
            {
            case '-':
            case '+':
                if (foundSign)
                {
                    if (!foundDigit)
                    errorMessage("Expected an exponent");
                    return;
                }
                foundSign = true;
                nextCharNonLF();
                break;
            case '0': .. case '9':
            case '_':
                foundDigit = true;
                nextCharNonLF();
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
        assert (isDigit(src.front) || src.front == '.');
    }
    body
    {
        bool foundDot = src.front == '.';
        if (foundDot)
            nextCharNonLF();
        current.type = TokenType.intLiteral;
        decimalLoop: while (!isEoF())
        {
            switch (src.front)
            {
            case '0': .. case '9':
            case '_':
                nextCharNonLF();
                break;
            case 'u':
            case 'U':
                if (!foundDot)
                    lexIntSuffix();
                break decimalLoop;
            case 'i':
                lexFloatSuffix();
                break decimalLoop;
            case 'L':
                if (foundDot)
                    lexFloatSuffix();
                else
                    lexIntSuffix();
                break decimalLoop;
            case 'f':
            case 'F':
                lexFloatSuffix();
                break decimalLoop;
            case 'e':
            case 'E':
                lexExponent();
                break decimalLoop;
            case '.':
                if (foundDot)
                    break decimalLoop;
                if (src.canPeek() && src.peek() == '.')
                    break decimalLoop;
                nextCharNonLF();
                foundDot = true;
                current.type = TokenType.doubleLiteral;
                break;
            default:
                break decimalLoop;
            }
        }
        setTokenValue();
    }

    void lexBinary()
    {
        current.type = TokenType.intLiteral;
        binaryLoop: while (!isEoF())
        {
            switch (src.front)
            {
            case '0':
            case '1':
            case '_':
                nextCharNonLF();
                break;
            case 'u':
            case 'U':
            case 'L':
                lexIntSuffix();
                break binaryLoop;
            default:
                break binaryLoop;
            }
        }
        setTokenValue();
    }

    void lexHex()
    {
        current.type = TokenType.intLiteral;
        bool foundDot;
        hexLoop: while (!isEoF())
        {
            switch (src.front)
            {
            case 'a': .. case 'f':
            case 'A': .. case 'F':
            case '0': .. case '9':
            case '_':
                nextCharNonLF();
                break;
            case 'u':
            case 'U':
                lexIntSuffix();
                break hexLoop;
            case 'i':
                if (foundDot)
                    lexFloatSuffix();
                break hexLoop;
            case 'L':
                if (foundDot)
                {
                    lexFloatSuffix();
                    break hexLoop;
                }
                else
                {
                    lexIntSuffix();
                    break hexLoop;
                }
            case 'p':
            case 'P':
                lexExponent();
                break hexLoop;
            case '.':
                if (foundDot)
                    break hexLoop;
                if (src.canPeek() && src.peek() == '.')
                    break hexLoop;
                nextCharNonLF();
                foundDot = true;
                current.type = TokenType.doubleLiteral;
                break;
            default:
                break hexLoop;
            }
        }
        setTokenValue();
    }

    bool lexStringSuffix()
    {
        current.type = TokenType.stringLiteral;
        bool foundSuffix = false;
        if (!isEoF())
        {
            switch (src.front)
            {
            case 'w':
                current.type = TokenType.wstringLiteral;
                goto case 'c';
            case 'd':
                current.type = TokenType.dstringLiteral;
                goto case 'c';
            case 'c':
                foundSuffix = true;
                nextCharNonLF();
                break;
            default:
                break;
            }
        }
        return foundSuffix;
    }

    void lexCharacterLiteral()
    in
    {
        assert (src.front == '\'');
    }
    body
    {
        current.type = TokenType.characterLiteral;
        nextChar();
        if (isEoF())
        {
            errorMessage("Unterminated character literal");
            return;
        }
        switch (src.front)
        {
        case '\'':
            break;
        case '\\':
            if (config.tokenStyle & TokenStyle.notEscaped)
                skipEscapeSequence();
            else
            {
                // the only special path
                // 40 bytes is enough for 2 quotes
                // and the longest character entity
                ubyte[40] utf8;
                size_t len;
                if (config.tokenStyle & TokenStyle.includeQuotes)
                {
                    utf8[0] = '\'';
                    len = decodeEscapeSequence(utf8[1..$]);
                    utf8[len++] = '\'';
                }
                else
                    len = decodeEscapeSequence(utf8[]);
                if (src.front != '\'')
                {
                    errorMessage("Expected \"'\" to end character literal");
                }
                // skip over last "'"
                nextChar();
                setTokenValue(utf8[0..len]);
            return;
            }
            break;
        default:
            if (src.front & 0x80)
            {
                while (src.front & 0x80)
                    nextChar();
                break;
            }
            else
            {
                nextChar();
                break;
            }
        }
        if (src.front != '\'')
            errorMessage("Expected \"'\" to end character literal");
        nextChar();
        if (config.tokenStyle & TokenStyle.includeQuotes)
            setTokenValue();
        else
            setTokenValue(1, -1);
    }

    void lexString()
    in
    {
        //assert (src.front == '"');
    }
    body
    {
        current.type = TokenType.stringLiteral;
        bool longWysiwg = src.slice.length > 0 && src.slice[0] == 'r'; // 2 chars : r"
        bool isWysiwyg = src.front == '`';
        // in case we need to unescape string
        Appender!(ubyte[]) unescaped;
        auto quote = src.front;
        nextChar();
        while (true)
        {
            if (isEoF())
            {
                errorMessage("Unterminated string literal");
                return;
            }
            else if (src.front == '\\')
            {
                if (isWysiwyg || longWysiwg)
                    nextChar();
                else if(config.tokenStyle & TokenStyle.notEscaped)
                {
                    skipEscapeSequence();
                }
                else
                {
                    if(unescaped == Appender!(ubyte[]).init)
                        unescaped = appender!(ubyte[])();
                    unescaped.put(src.slice());
                    decodeEscapeSequence(unescaped);
                    src.mark(); //start next slice after escape sequence
                }
            }
            else if (src.front == quote)
            {
                nextCharNonLF();
                        break;
            }
            else
                nextChar();
        }
        lexStringSuffix();
        // helper to handle quotes
        void setData(R)(R range)
        {
            if (config.tokenStyle & TokenStyle.includeQuotes)
                setTokenValue(range);
            else if (longWysiwg)
                setTokenValue(range[2..$-1]);
            else
                setTokenValue(range[1..$-1]);
        }
        import std.stdio;
        if(unescaped != Appender!(ubyte[]).init)
        {
            //stuff in the last slice and use buffered data
            unescaped.put(src.slice);
            setData(unescaped.data);
        }
        else
        {
            setData(src.slice); //slice directly
        }
    }

    void lexDelimitedString()
    in
    {
        assert(src.front == '"');
    }
    body
    {
        current.type = TokenType.stringLiteral;

        nextChar();

        bool heredoc;
        ubyte open;
        ubyte close;

        switch (src.front)
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
        assert(src.slice[0 .. 2] == `q"`);
    }
    body
    {
        current.type = TokenType.stringLiteral;
        int depth = 1;
        nextChar();
        while (true)
        {
            if (isEoF())
            {
                errorMessage("Unterminated string literal");
                break;
            }
            if (src.front == open)
            {
                nextChar();
                ++depth;
            }
            else if (src.front == close)
            {
                nextChar();
                --depth;
                if (depth <= 0)
                {
                    auto r = src.save(); //TODO: allocates for Fwd range
                    if (r.front == '"')
                    {
                        nextChar();
                        break;
                    }
                    else
                    {
                        errorMessage("Expected \" after balanced "
                            ~ cast(char) close ~ " but found "
                            ~ cast(char) r.front ~ " instead.");
                        break;
                    }
                }
            }
            else
                nextChar();
        }
        if (config.tokenStyle & TokenStyle.includeQuotes)
            setTokenValue();
        else
            setTokenValue(3, -2);
    }

    void lexHeredocString()
    in
    {
        assert (src.slice.equal("q\""));
    }
    body
    {
        typeof(src.slice) ident;
        uint newlineBytes;
        while (true)
        {
            if (isEoF())
            {
                errorMessage("Unterminated string literal");
                return;
            }
            else if (isNewline(src.front))
            {
                ident = src.slice[2..$];
                nextChar();
                newlineBytes = cast(uint) (src.slice.length - 2 - ident.length);
                break;
            }
            else if (isSeparating())
            {
                nextChar();
                ident = src.slice[2..$];
                nextChar();
                newlineBytes = 0;
                break;
            }
            else
            {
                nextChar();
            }
        }
        while (true)
        {
            if (isEoF())
            {
                errorMessage("Unterminated string literal");
                break;
            }
            else if (src.slice.length > ident.length
                && src.slice[$-ident.length .. $].equal(ident))
            {
                if (src.front == '"')
                {
                    nextChar();
                    lexStringSuffix();
                    break;
                }
                else
                {
                    errorMessage("Unterminated string literal: " ~ cast(string) src.slice);
                    break;
                }
            }
            else
                nextChar();
        }

        bool hasSuffix = lexStringSuffix();

        if (config.tokenStyle & TokenStyle.includeQuotes)
            setTokenValue();
        else
        {
            setTokenValue(cast(int) (2 + newlineBytes + ident.length),
                cast(int) (-(ident.length + (hasSuffix ? 2 : 1))));
        }
    }

    void lexTokenString()
    in
    {
        assert (src.front == '{');
    }
    body
    {
        current.type = TokenType.stringLiteral;
        nextChar();
        auto app = appender!(ubyte[])();
        if (config.tokenStyle & TokenStyle.includeQuotes)
        {
            app.put('q');
            app.put('{');
        }
        LexerConfig c = config;
        scope (exit) config = c;
        config.iterStyle = IterationStyle.everything;
        config.tokenStyle = TokenStyle.source;
        int depth = 1;

        while (!isEoF())
        {
            advance();
            if (current.type == TokenType.lBrace)
                ++depth;
            else if (current.type == TokenType.rBrace)
            {
                --depth;
                if (depth <= 0)
                    break;
            }
            app.put(representation(current.value));
        }
        config = c;
        if (config.tokenStyle & TokenStyle.includeQuotes)
        {
            app.put('}');
        }
        if (src.empty)
            current.type = TokenType.stringLiteral;
        else
        {
            switch (src.front)
            {
                case 'd':
                    if (config.tokenStyle & TokenStyle.includeQuotes)
                        app.put('d');
                    current.type = TokenType.dstringLiteral;
                    src.popFront();
                    break;
                case 'w':
                    if (config.tokenStyle & TokenStyle.includeQuotes)
                        app.put('w');
                    current.type = TokenType.wstringLiteral;
                    src.popFront();
                    break;
                case 'c':
                    if (config.tokenStyle & TokenStyle.includeQuotes)
                        app.put('c');
                    src.popFront();
                    goto default;
                default:
                    current.type = TokenType.stringLiteral;
                    break;
            }
        }
        current.value = cast(string) app.data;
    }

    void lexSpecialTokenSequence()
    in
    {
        assert (src.front == '#');
    }
    body
    {
        nextChar();
        auto r = src.save();
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
                src.popFront();
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

//=====================================================================
//          Helpers for lexXYZ functions
//=====================================================================
    void skipEscapeSequence()
    {
        // no decoding, just minor sanity checks
        nextChar();
        switch (src.front)
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
            nextChar();
            return;
        case '0': .. case '7':
            foreach(i; 0 .. 3)
            {
                nextChar();
                if (src.front < '0' || src.front > '7') return;
            }
            return;
        case 'x':
            nextChar();
            foreach(i; 0 .. 2)
            {
                if (!isHexDigit(src.front))
                {
                    errorMessage("Expected hex digit");
                    return;
                }
                nextChar();
            }
            return;
        case 'u':
        case 'U':
            uint digits = src.front == 'u' ? 4 : 8;
            nextChar();
            foreach (i; 0 .. digits)
            {
                if (!isHexDigit(src.front))
                {
                    errorMessage("Expected hex digit instead of %s".format(
                        cast(char) src.front));
                    return;
                }
                nextChar();
            }
            return;
        case '&':
            while (!isEoF())
            {
                nextChar();
                if (src.front == ';')
                    break;
            }
            return;
        default:
            errorMessage("Invalid escape sequence");
            return;
        }
    }

    size_t decodeEscapeSequence(OutputRange)(OutputRange dest)
    in
    {
        assert (src.front == '\\');
    }
    body
    {
        size_t reencodeNumeric(ubyte[] src, int radix, OutputRange dest)
        {
            char[] chunk = cast(char[])src;
            char[4] utfBuf;
            uint codepoint = parse!uint(chunk, radix);
            size_t len;
            try
                 len = encode(utfBuf, codepoint);
            catch (UTFException ex)
            {
                errorMessage(ex.msg);
                return 0;
            }
            dest.put(cast(ubyte[]) utfBuf[0..len]);
            return len;
        }

        ubyte[40] buffer;
        src.popFront();
        switch (src.front)
            {
        case '\'':
        case '"':
        case '?':
        case '\\':
            buffer[0] = src.front;
            src.popFront();
            return 1;
        case 'a':  dest.put('\a'); src.popFront(); return 1;
        case 'b':  dest.put('\b'); src.popFront(); return 1;
        case 'f':  dest.put('\f'); src.popFront(); return 1;
        case 'n':  dest.put('\n'); src.popFront(); return 1;
        case 'r':  dest.put('\r'); src.popFront(); return 1;
        case 't':  dest.put('\t'); src.popFront(); return 1;
        case 'v':  dest.put('\v'); src.popFront(); return 1;
        case 0x0a: dest.put(cast(ubyte)0x0a); src.popFront(); return 1;
        case 0x00: dest.put(cast(ubyte)0x00); src.popFront(); return 1;
        case '0': .. case '7':
            size_t idx = 0;
            while(idx < 3 && !isEoF())
            {
                buffer[idx++] = src.front;
                src.popFront();
                if (src.front < '0' || src.front > '7') break;
            }
            return reencodeNumeric(buffer[0..idx], 8, dest);
        case 'x':
            src.popFront();
            foreach(i; 0 .. 2)
            {
                if (!isHexDigit(src.front))
                {
                    errorMessage("Expected hex digit");
                    return 1;
                }
                buffer[i] = src.front;
                src.popFront();
            }
            return reencodeNumeric(buffer[0..2], 16, dest);
        case 'u':
        case 'U':
            uint digitCount = src.front == 'u' ? 4 : 8;
            src.popFront();
            foreach (i; 0 .. digitCount)
            {
                if (!isHexDigit(src.front))
                {
                    errorMessage("Expected hex digit");
                    return 1;
                }
                buffer[i] = src.front;
                src.popFront();
            }
            return reencodeNumeric(buffer[0..digitCount], 16, dest);
        case '&':
            src.popFront();
            size_t idx = 0;
            while (!isEoF())
            {
                if (isAlpha(src.front))
                {
                    buffer[idx++] = src.front;
                    if(idx == buffer.length) // way over maximum length
                        errorMessage("Invalid character entity");
                    src.popFront();
                }
                else if (src.front == ';')
                {
                    src.popFront();
                    break;
                }
                else
                {
                        errorMessage("Invalid character entity");
                        return idx;
                }
            }
            //TODO: avoid looking up as UTF string, use raw bytes
            string chunk = cast(string)buffer[0..idx];
            auto names = assumeSorted(map!"a.name"(characterEntities));
            auto place = names.lowerBound(chunk).length;
            if (place == names.length || names[place] != chunk)
            {
                errorMessage("Invalid character entity \"&%s;\""
                    .format(cast(string) chunk));
                return 1;
            }
            auto entity = characterEntities[place].value;
            dest.put(cast(ubyte[]) entity);
            return entity.length;
        default:
            errorMessage("Invalid escape sequence");
            return 1;
        }
    }

    // advances underlying mark-slice range and counts lines, cols
    void nextChar()
    {
        bool foundNewline;
        if (src.front == '\r')
        {
            src.popFront();
            foundNewline = true;
        }
        if (src.front == '\n')
        {
            src.popFront();
            foundNewline = true;
        }
        else
        {
            src.popFront();
        }
        if (foundNewline)
        {
            ++lineNumber;
            column = 0;
        }
        else
            ++column;

    }

    //same but don't bother for LF sequences
    void nextCharNonLF()
    {
        src.popFront();
        ++column;
    }

    void setTokenValue()()
    {
        current.value = cache.get(src.slice);
    }

    void setTokenValue()(int startOffset, int endOffset)
    in
    {
        assert(startOffset >= 0);
        assert(endOffset <= 0);
    }
    body
    {
        auto piece = src.slice;
        // avoid unsigned arithmetic as endOffset is negative
        int end = cast(int)piece.length + endOffset;
        current.value = cache.get(src.slice[startOffset .. end]);
    }

    void setTokenValue(R)(R range)
        if(isRandomAccessRange!R && is(ElementType!R : const(ubyte)))
    {
        current.value = cache.get(range);
    }

    bool isEoF() const
    {
        return src.empty || src.front == 0 || src.front == 0x1a;
    }

    bool isSeparating()
    {
        auto ch = src.front;
        if (ch <= 0x2f) return true;
        if (ch >= ':' && ch <= '@') return true;
        if (ch >= '[' && ch <= '^') return true;
        if (ch >= '{' && ch <= '~') return true;
        if (ch == '`') return true;
        if ((ch & 0x80) && isLongWhite()) return true;
        return false;
    }

    bool isWhite()
    {
        auto c = src.front;
        if (c & 0x80) // multi-byte utf-8
        {
            return isLongWhite();
        }
        else
            return c == 0x20 || (c >= 0x09 && c <= 0x0d);
    }

    bool isLongWhite()
    {
        assert(src.front & 0x80); // only non-ascii
        //TODO: here and elsewhere we'd better have
        // some kind of lookahead in LexSource instead of .save
        auto r = src.save();
        if (r.front != 0xe2)
            return false;
        else
            r.popFront();
        if (r.empty || r.front != 0x80)
            return false;
        else
            r.popFront();
        if (r.empty || (r.front != 0xa8 && r.front != 0xa9))
            return false;
        return true;
    }

    void expandSpecialToken()
    {
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

    void errorMessage(string s)
    {
        import std.string: format;
        if (config.errorFunc !is null)
            config.errorFunc(config.fileName, current.startIndex,
                current.line, current.column, s);
        else
            throw new Exception(format("%s(%d:%d): %s",
                config.fileName, current.line, current.column, s));
    }

    this(LexSrc lex, LexerConfig cfg)
    {
        src = move(lex); // lex is r-value
        lineNumber = 1;
        column = 0;
        _empty = false;
        config = move(cfg); // ditto with cfg
        cache = StringCache(initialTableSize);
    }
    enum initialTableSize = 2048;
    Token current;
    uint lineNumber;
    uint column;
    LexSrc src;
    bool _empty;
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

// For now a private helper that is tailored to the way lexer works
// hides away forwardness of range by buffering
// RA-version is strightforward thin wrapping
// ATM it is byte-oriented
private struct LexSource(R)
    if(isForwardRange!R && !isRandomAccessRange!R)
    {
    bool empty() const { return _empty; }

    auto ref front() const
    {
        return accum[accumIdx];
    }

    auto ref peek() const
    in
    {
        assert (accumIdx + 1 < accum.length);
    }
    body
    {
        return accum[accumIdx + 1];
    }

    void popFront()
    {
        ++_index;
        range.popFront();
        // if that was last byte
        // just advance so that open-righted slice just works
        accumIdx =  (accumIdx+1) & mask;
        if(range.empty)
        {
            _empty = true;
            return;
        }
        if(accumIdx == savedAccumIdx)
        {
            // and move stuff around
            auto oldLen = accum.length;
            auto toCopy = oldLen - accumIdx;
            accum.length *= 2; // keep pow of 2
            // copy starting with last item
            copy(retro(accum[accumIdx..oldLen]),
                retro(accum[$-toCopy..$]));
            savedAccumIdx = accum.length - toCopy;
        }
        accum[accumIdx] = range.front;
    }

    auto save()
    {
        typeof(this) copy = this;
        copy.range = range.save;
        // sadly need to dup circular buffer, as it overwrites items
        copy.accum = copy.accum.dup;
        return copy;
    }

    // mark a position to slice from later on
    size_t mark()
    {
        savedAccumIdx = accumIdx;
        return accumIdx;
    }

    // slice to current position from previously marked position
    auto slice() @property
    {
        // it's an open right range as usual
        return CircularRange(accum, savedAccumIdx, accumIdx);
    }

    size_t index() const @property
    {
        return _index;
    }

private:
    this(R src, size_t bufferSize)
    {
        range = src;
        assert(bufferSize > 0);
        assert((bufferSize & (bufferSize-1)) == 0); //is power of 2
        accum = new ubyte[bufferSize];
        if(range.empty)
            _empty = true;
        else
            accum[accumIdx] = range.front; // load front
    }

    // a true RA-range of ubyte
    struct CircularRange
    {
        this(ubyte[] buf, size_t s, size_t e)
        {
            assert((buffer.length & (buffer.length-1)) == 0);
            buffer = buf;
            start = s;
            end = e;
        }
        //Forward range primitives
        @property bool empty() const { return start == end; }
        @property auto ref front() const { return buffer[start]; }
        void popFront() { start = (start + 1) & mask; }
        @property auto save() { return this; }

        //Backwards is a bit slower, but should be rarely used (if at all)
        @property ref back(){ return buffer[(end-1) & mask]; }
        void popBack() { end  = (end - 1) & mask; }

        // RA range primitives
        ref opIndex(size_t idx){ return buffer[(start+idx) & mask]; }
        @property size_t length()
        {
            return end < start ? end + buffer.length -start : end - start;
        }
        alias length opDollar;

        auto opSlice(size_t newStart, size_t newEnd)
        {
            size_t maskedStart = (start+newStart) & mask;
            size_t maskedEnd = (start+newEnd) & mask;
            return typeof(this)(buffer, maskedStart, maskedEnd);
        }
        // @@@bug fwd-ref in ldc0.10 (if placed above previous one)
        auto opSlice(){ return opSlice(0, length); }
    private:
        @property auto mask(){ return buffer.length-1; }
        size_t start, end;
        ubyte[] buffer;
    }

    @property auto mask(){ return accum.length-1; }

    R range;
    bool _empty;
    ubyte[] accum; // accumulator buffer for non-RA ranges
    size_t savedAccumIdx;
    size_t accumIdx; // current index in accumulator
    size_t _index; // index of current element in original range
}

// TODO: make sure it's RandomAccess later
/*static assert(isRandomAccessRange!(
    LexSource!(typeof(filter!"true"(cast(ubyte[])null)))
    .CircularRange)
);*/

//trivial pass-through for RA ranges
private struct LexSource(R)
    if(isRandomAccessRange!R)
{
    bool empty() const @property { return cur >= range.length; }
    bool canPeek() const { return cur + 1 < range.length; }
    auto ref front() const @property { return range[cur]; }
    void popFront(){ cur++; }

    auto ref peek() const
    in
    {
        assert (canPeek());
    }
    body
    {
        return range[cur + 1];
    }

    auto save()
    {
        typeof(this) copy = this;
        copy.range = range.save;
        return copy;
    }

    auto mark()
    {
        saved = cur;
    }

    // use the underliying range slicing capability
    auto slice() @property
    {
        return range[saved..cur];
    }

    size_t index() const @property
    {
        return cur;
    }

private:
    this(R src)
    {
        range = src;
    }
    size_t cur, saved;
    R range;
}

auto lexerSource(Range)(Range range, size_t bufSize=8)
    if(isForwardRange!Range && !isRandomAccessRange!Range
    && is(ElementType!Range : const(ubyte)))
{
    return LexSource!(Range)(range, bufSize);
}

auto lexerSource(Range)(Range range)
    if(isRandomAccessRange!Range
    && is(ElementType!Range : const(ubyte)))
{
    return LexSource!(Range)(range);
}

unittest
{
    // test the basic functionality of a "mark-slice" range
    import std.string, std.stdio;

    static void test_hello(T)(T lexs)
    {
        assert(lexs.front == 'H');
        lexs.popFront();
        assert(lexs.front == 'e');
        foreach(i; 0..2)
        {
            auto saved = lexs.save;
            lexs.mark();
            assert(lexs.slice.equal(""));
            lexs.popFront();
            assert(lexs.slice.equal("e"), text(cast(char)lexs.front));
            lexs.popFrontN(4);
            auto bytes = lexs.slice.map!"cast(char)a".array();
            assert(bytes.equal("ello,"), bytes.to!string);
            lexs.mark();
            assert(lexs.slice.equal(""));
            assert(lexs.front == 'w');
            lexs.popFrontN(6);
            assert(lexs.empty);
            auto s = lexs.slice();
            auto msg = s.save.map!"cast(char)a".array;
            assert(s[].equal("world!"), msg);
            assert(s[2..$-1].equal("rld"), msg);
            assert(s[0] == 'w' && s[$-1] == '!');
            s.popFront();
            assert(s.front == 'o' && s.back == '!');
            s.popBack();
            assert(s.front == 'o' && s.back == 'd');
            //restore and repeat again
            lexs = saved;
        }
    }

    static void test_empty(T)(T lexs)
    {
        assert(lexs.empty);
        lexs.mark();
        assert(lexs.slice().equal(""));
    }

    auto fwdLex = lexerSource(
        "Hello, world!"
        .representation
        .filter!"a != ' '", 16 // and the one that is more then enough
    );
    test_hello(fwdLex);
    fwdLex = lexerSource(
        "Hello, world!"
        .representation
        .filter!"a != ' '", 1 // try the smallest initial buffer
    );
    test_hello(fwdLex);
    fwdLex = lexerSource("".representation.filter!"a != ' '");
    auto raLex = lexerSource("".representation);
    test_empty(raLex);
    test_empty(fwdLex);
    raLex = lexerSource("Hello,world!".representation);
    test_hello(raLex);
}

// uses auto-detection for pure, safe nothrow
bool isRangeEoF(R)(ref R range)
{
    return range.empty || range.front == 0 || range.front == 0x1a;
}

// Lookup table for token values
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

pure TokenType lookupTokenType(R)(R input)
{
    switch(input.length)
    {
    case 2:
        switch (input[0])
        {
        case 'd': if (input[1] == 'o') return TokenType.do_; else break;
        case 'i':
            if (input[1] == 'f') return TokenType.if_;
            else if (input[1] == 'n') return TokenType.in_;
            else if (input[1] == 's') return TokenType.is_;
            else break;
        default: break;
        }
        break;
    case 3:
        switch (input[0])
        {
        case 'a': if (input[1..$].equal("sm")) return TokenType.asm_; else break;
        case 'f': if (input[1..$].equal("or")) return TokenType.for_; else break;
        case 'i': if (input[1..$].equal("nt")) return TokenType.int_; else break;
        case 'n': if (input[1..$].equal("ew")) return TokenType.new_; else break;
        case 'o': if (input[1..$].equal("ut")) return TokenType.out_; else break;
        case 'r': if (input[1..$].equal("ef")) return TokenType.ref_; else break;
        case 't': if (input[1..$].equal("ry")) return TokenType.try_; else break;
        default: break;
        }
        break;
    case 4:
        switch (input[0])
        {
        case 'a': if (input[1..$].equal("uto")) return TokenType.auto_; else break;
        case 'b': if (input[1..$].equal("ody")) return TokenType.body_;
            else if (input[1..$].equal("ool")) return TokenType.bool_;
            else if (input[1..$].equal("yte")) return TokenType.byte_;
            else break;
        case 'c': if (input[1..$].equal("ase")) return TokenType.case_;
            else if (input[1..$].equal("ast")) return TokenType.cast_;
            else if (input[1..$].equal("ent")) return TokenType.cent_;
            else if (input[1..$].equal("har")) return TokenType.char_;
            else break;
        case 'e': if (input[1..$].equal("lse")) return TokenType.else_;
            else if (input[1..$].equal("num")) return TokenType.enum_;
            else break;
        case 'g': if (input[1..$].equal("oto")) return TokenType.goto_; else break;
        case 'l': if (input[1..$].equal("azy")) return TokenType.lazy_;
            else if (input[1..$].equal("ong")) return TokenType.long_;
            else break;
        case 'n': if (input[1..$].equal("ull")) return TokenType.null_; else break;
        case 'p': if (input[1..$].equal("ure")) return TokenType.pure_; else break;
        case 'r': if (input[1..$].equal("eal")) return TokenType.real_; else break;
        case 't': if (input[1..$].equal("his")) return TokenType.this_;
            else if (input[1..$].equal("rue")) return TokenType.true_;
            else break;
        case 'u': if (input[1..$].equal("int")) return TokenType.uint_; else break;
        case 'v': if (input[1..$].equal("oid")) return TokenType.void_; else break;
        case 'w': if (input[1..$].equal("ith")) return TokenType.with_; else break;
        default: break;
        }
        break;
    case 5:
        switch (input[0])
        {
        case 'a': if (input[1..$].equal("lias")) return TokenType.alias_;
            else if (input[1..$].equal("lign")) return TokenType.align_; else break;
        case 'b': if (input[1..$].equal("reak")) return TokenType.break_; else break;
        case 'c': if (input[1..$].equal("atch")) return TokenType.catch_;
            else if (input[1..$].equal("lass")) return TokenType.class_;
            else if (input[1..$].equal("onst")) return TokenType.const_;
            else if (input[1..$].equal("real")) return TokenType.creal_;
            else break;
        case 'd': if (input[1..$].equal("char")) return TokenType.dchar_;
            else if (input[1..$].equal("ebug")) return TokenType.debug_; else break;
        case 'f': if (input[1..$].equal("alse")) return TokenType.false_;
            else if (input[1..$].equal("inal")) return TokenType.final_;
            else if (input[1..$].equal("loat")) return TokenType.float_;
            else break;
        case 'i': if (input[1..$].equal("nout")) return TokenType.inout_;
            else if (input[1..$].equal("real")) return TokenType.ireal_; else break;
        case 'm': if (input[1..$].equal("acro")) return TokenType.macro_;
            else if (input[1..$].equal("ixin")) return TokenType.mixin_; else break;
        case 's': if (input[1..$].equal("cope")) return TokenType.scope_;
            else if (input[1..$].equal("hort")) return TokenType.short_;
            else if (input[1..$].equal("uper")) return TokenType.super_; else break;
        case 't': if (input[1..$].equal("hrow")) return TokenType.throw_; else break;
        case 'u': if (input[1..$].equal("byte")) return TokenType.ubyte_;
            else if (input[1..$].equal("cent")) return TokenType.ucent_;
            else if (input[1..$].equal("long")) return TokenType.ulong_;
            else if (input[1..$].equal("nion")) return TokenType.union_;
            else break;
        case 'w': if (input[1..$].equal("char")) return TokenType.wchar_;
            else if (input[1..$].equal("hile")) return TokenType.while_;
            else break;
        default: break;
        }
        break;
    case 6:
        switch (input[0])
        {
        case 'a': if (input[1..$].equal("ssert")) return TokenType.assert_; else break;
        case 'c': if (input[1..$].equal("float")) return TokenType.cfloat_; else break;
        case 'd': if (input[1..$].equal("elete")) return TokenType.delete_;
            else if (input[1..$].equal("ouble")) return TokenType.double_; else break;
        case 'e': if (input[1..$].equal("xport")) return TokenType.export_;
            else if (input[1..$].equal("xtern")) return TokenType.extern_; else break;
        case 'i': if (input[1..$].equal("float")) return TokenType.ifloat_;
            else if (input[1..$].equal("mport")) return TokenType.import_; else break;
        case 'm': if (input[1..$].equal("odule")) return TokenType.module_; else break;
        case 'p': if (input[1..$].equal("ragma")) return TokenType.pragma_;
            else if (input[1..$].equal("ublic")) return TokenType.public_; else break;
        case 'r': if (input[1..$].equal("eturn")) return TokenType.return_; else break;
        case 's': if (input[1..$].equal("hared")) return TokenType.shared_;
            else if (input[1..$].equal("tatic")) return TokenType.static_;
            else if (input[1..$].equal("truct")) return TokenType.struct_;
            else if (input[1..$].equal("witch")) return TokenType.switch_; else break;
        case 't': if (input[1..$].equal("ypeid")) return TokenType.typeid_;
            else if (input[1..$].equal("ypeof")) return TokenType.typeof_; else break;
        case 'u': if (input[1..$].equal("short")) return TokenType.ushort_; else break;
        default: break;
        }
        break;
    case 7:
        switch (input[0])
        {
        case '_': if (input[1..$].equal("_EOF__")) return TokenType.eof; else break;
        case 'c': if (input[1..$].equal("double")) return TokenType.cdouble_; else break;
        case 'd': if (input[1..$].equal("efault")) return TokenType.default_; else break;
        case 'f': if (input[1..$].equal("inally")) return TokenType.finally_;
            else if (input[1..$].equal("oreach")) return TokenType.foreach_; else break;
        case 'i': if (input[1..$].equal("double")) return TokenType.idouble_; else break;
        case 'n': if (input[1..$].equal("othrow")) return TokenType.nothrow_; else break;
        case 'p': if (input[1..$].equal("ackage")) return TokenType.package_;
            else if (input[1..$].equal("rivate")) return TokenType.private_; else break;
        case 't': if (input[1..$].equal("ypedef")) return TokenType.typedef_; else break;
        case 'v': if (input[1..$].equal("ersion")) return TokenType.version_; else break;
        default: break;
        }
        break;
    case 8:
        switch (input[0])
        {
        case '_': if (input[1..$].equal("_DATE__")) return TokenType.date;
            else if (input[1..$].equal("_FILE__")) return TokenType.file;
            else if (input[1..$].equal("_LINE__")) return TokenType.line;
            else if (input[1..$].equal("_TIME__")) return TokenType.time;
            else if (input[1..$].equal("_traits")) return TokenType.traits; else break;
        case 'a': if (input[1..$].equal("bstract")) return TokenType.abstract_; else break;
        case 'c': if (input[1..$].equal("ontinue")) return TokenType.continue_; else break;
        case 'd': if (input[1..$].equal("elegate")) return TokenType.delegate_; else break;
        case 'f': if (input[1..$].equal("unction")) return TokenType.function_; else break;
        case 'o': if (input[1..$].equal("verride")) return TokenType.override_; else break;
        case 't': if (input[1..$].equal("emplate")) return TokenType.template_; else break;
        case 'u': if (input[1..$].equal("nittest")) return TokenType.unittest_; else break;
        case 'v': if (input[1..$].equal("olatile")) return TokenType.volatile_; else break;
        default: break;
        }
        break;
    case 9:
        switch (input[0])
        {
        case '_': if (input[1..$].equal("_gshared")) return TokenType.gshared; else break;
        case 'i': if (input[1..$].equal("mmutable")) return TokenType.immutable_;
            else if (input[1..$].equal("nterface")) return TokenType.interface_;
            else if (input[1..$].equal("nvariant")) return TokenType.invariant_; else break;
        case 'p': if (input[1..$].equal("rotected")) return TokenType.protected_; else break;
        default: break;
        }
        break;
    case 10:
        switch (input[0])
        {
        case 'd': if (input[1..$].equal("eprecated")) return TokenType.deprecated_; else break;
        case '_': if (input[1..$].equal("_VENDOR__")) return TokenType.vendor; else break;
        default: break;
        }
        break;
    case 11:
        if (input[1..$].equal("_VERSION__"))
            return TokenType.compilerVersion;
        break;
    case 12:
        if (input[1..$].equal("ynchronized"))
            return TokenType.synchronized_;
        break;
    case 13:
        if (input[1..$].equal("_TIMESTAMP__"))
            return TokenType.timestamp;
        break;
    case 15:
        if (input[1..$].equal("oreach_reverse"))
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
        caseStatement ~= "\tnextCharNonLF();\n";
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
            caseStatement ~= "\tswitch (src.front)\n";
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
    this(size_t startSize)
    {
        assert((startSize & (startSize-1)) == 0);
        index = new Slot*[startSize];
    }
    
    string get(R)(R range)
        if(isRandomAccessRange!R
            && is(Unqual!(ElementType!R) : const(ubyte)))
    {
        uint h = hash(range);
        uint bucket = h & (index.length-1);
        Slot *s = index[bucket];
        if(s == null) 
        {
            string str = putIntoCache(range);
            index[bucket] = allocateSlot(str, h);
            uniqueSlots++;
            return str;
        }
        for(;;)
        {
            if(s.hash == h && s.value.equal(range))
                return s.value;      
            if(s.next == null) break;
            s = s.next;
        }
        string str = putIntoCache(range);
        s.next = allocateSlot(str, h);     
        uniqueSlots++;
        // had at least 1 item in this bucket
        // and inserted another one - check load factor
        if(uniqueSlots*loadDenom > index.length*loadQuot)
            rehash();
        return str;
    }
   
private:   

    static uint hash(R)(R data)
    {
        uint hash = 0;
        foreach (b; data)
        {
            hash ^= sbox[b];
            hash *= 3;
        }
        return hash;
    }

    struct Slot
    {
        string value;
        Slot* next;
        uint hash;
    };
    
    void printLoadFactor()
    {
        size_t cnt = 0, maxChain = 0;
        foreach(Slot* s; index)
        {
            size_t chain = 0;
            for(Slot* p = s; p; p = p.next)
            {
                chain++;
            }
            maxChain = max(chain, maxChain);
            cnt += chain;
        }
        import std.stdio;
        assert(cnt == uniqueSlots);
        writefln("Load factor: %.3f; max bucket %d", 
            cast(double)cnt/index.length, 
                maxChain);
    }

    void rehash()
    {
        //writefln("BEFORE (size = %d):", index.length);
        //printLoadFactor();
        size_t oldLen = index.length;
        index.length *= 2;
        for (size_t i = 0; i < oldLen; i++)
        {
            Slot* cur = index[i], prev;
            while(cur)
            {                
                //has extra bit set - move it out
                if(cur.hash & oldLen) 
                {
                    if(prev == null)
                    {
                        Slot* r = cur;
                        index[i] = cur.next;
                        cur = cur.next;
                        insertIntoBucket(r, i + oldLen);
                    }
                    else
                    {
                        Slot* r = removeLink(cur, prev);
                        insertIntoBucket(r, i + oldLen);
                    }
                }
                else
                {
                    prev = cur;
                    cur = cur.next;
                }
            }
        }
        //writefln("AFTER (size = %d):", index.length);
        //printLoadFactor();
    }
    
    static Slot* removeLink(ref Slot* cur, Slot* prev)
    {
        prev.next = cur.next;
        Slot* r = cur;
        cur = cur.next;
        return r;
    }
    
    //insert at front of bucket
    void insertIntoBucket(Slot* what, size_t bucket)
    {
        what.next = null;
        Slot* p = index[bucket];
        what.next = p;
        index[bucket] = what;        
    }
    
    Slot* allocateSlot(string val, uint hash)
    {
        auto slice = allocateInCache(Slot.sizeof);
        auto newSlot = cast(Slot*)slice.ptr;
        *newSlot = Slot(val, null, hash);
        return newSlot;
    }

    Slot*[] index;
    size_t uniqueSlots;
    enum loadQuot = 2, loadDenom = 3;

    // leave some slack for alloctors/GC meta-data
    enum chunkSize = 16*1024 - size_t.sizeof*8;
    ubyte*[] chunkS;
    size_t next = chunkSize;
    //TODO: add aligned variant that allocates at word boundary
    ubyte[] allocateInCache(size_t size)
    {
        import core.memory;        
        if(next + size > chunkSize)
        {
            // avoid huge allocations
            if(size> chunkSize/4)
            {
                ubyte* p = cast(ubyte*)GC.malloc(size,
                    GC.BlkAttr.NO_SCAN);
                return p[0..size];
            }
            chunkS ~= cast(ubyte*)GC.malloc(chunkSize,
                GC.BlkAttr.NO_SCAN);
            next = 0;
        }
        auto slice = chunkS[$-1][next..next+size];
        next += size;
        return slice;
    }

    string putIntoCache(R)(R data)
    {
        auto slice = allocateInCache(data.length);
        slice[] = data[];
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
    LexerConfig cfg;
    auto tkr = "void main(){ }".representation.byToken(cfg);
    assert(tkr.map!"a.value".equal(["void", "main", "(", ")", "{", "}"]));
    tkr = "1234 54.23232".representation.byToken(cfg);
    assert(tkr.equal(["1234", "54.23232"]));
    auto str =  r"0 0. .0 1 0x3 0b102 007";
    cfg.iterStyle = IterationStyle.everything;
    tkr = str.representation.byToken(cfg);
    assert(tkr.map!"a.value".equal(["0", " ", "0.", " ",
        ".0", " ", "1", " ", "0x3", " ", "0b10",
        "2", " ", "007"]
    ), text(tkr.map!"a.value"));
}

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
    assert (equal(map!"a.value"(tokens), expected), map!"a.value"(tokens).text());
}

unittest
{
    auto source = cast(ubyte[]) (`
        1 1.2 //comment
        1.2f 1u 1uL 0b011 0b1uu 0b1 /+abc/+def+/+/0x11001uL
        123e1L 123e+1f 123e-1i 15e++ 4ea 1.2u 4i 1337L 4.2L 1..2 4.3.5.8
        0xabc 0xabcp4 0x1P-10 0x40u 0x29L 0x4Lu 0xdeadbeef
    `);
    auto expected = ["1", "1.2", "1.2f", "1u", "1uL", "0b011", "0b1u", "u", "0b1",
        "0x11001uL", "123e1L", "123e+1f", "123e-1i", "15e+", "+", "4e", "a",
        "1.2", "u", "4i", "1337L", "4.2L", "1", "..", "2", "4.3", ".5", ".8",
        "0xabc", "0xabcp4", "0x1P-10", "0x40u", "0x29L", "0x4Lu", "0xdeadbeef"];
    int errCount = 0;
    void errorFunction(string file, size_t index, uint line, uint col, string msg)
    {
        ++errCount;
    }
    LexerConfig config;
    config.errorFunc = &errorFunction;
    auto tokens = byToken(source, config);
    //writeln(tokens.map!"a.value"());
    assert (equal(map!"a.value"(tokens), expected), map!"a.value"(tokens).text());
    assert (errCount == 2);
}

unittest
{
    auto source = cast(ubyte[]) ("int #line 4\n double q{abcde (a + b) == 0} '\\u0020' q\"HEREDOC\r\nabcde\r\nHEREDOC\"");
    LexerConfig config;
    auto tokens = byToken(source, config);
    assert (tokens.front.line == 1);
    assert (tokens.moveFront() == TokenType.int_);
    assert (tokens.front.line == 4);
    assert (isType(tokens.front));
    assert (tokens.front.value == "double");
    tokens.popFront();
    assert (tokens.front.value == "abcde (a + b) == 0", tokens.front.value);
    assert (isStringLiteral(tokens.front), tokens.front.type.text());
    tokens.popFront();
    assert (tokens.front.value == " ");
    assert (tokens.front.type == TokenType.characterLiteral);
    tokens.popFront();
    assert (tokens.front.value == "abcde\r\n", "[%s]".format(tokens.front.value));
}

unittest
{
    auto source = cast(ubyte[]) "q{(a & 1) == 0} q\"/foo]/\" q\"HEREDOC\r\nabcde\r\nHEREDOC\"";
    LexerConfig config;
    config.tokenStyle = TokenStyle.includeQuotes;
    auto tokens = byToken(source, config);
    assert (tokens.front.value == "q{(a & 1) == 0}", tokens.front.value);
    tokens.popFront();
    assert (tokens.front.value == "q\"/foo]/\"", tokens.front.value);
    tokens.popFront();
    assert (tokens.front.value == "q\"HEREDOC\r\nabcde\r\nHEREDOC\"", tokens.front.value);
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
    assert (a.type == TokenType.import_);
    Token b = tokens.moveFront();
    assert (b.type == TokenType.identifier);
    assert (a != b);
    assert (a != "foo");
    assert (a < b);
    assert (b == "foo");
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

//void main(string[] args){}
