// Written in the D programming language

/**
 * $(H2 Summary)
 * This module contains a range-based compile-time _lexer generator.
 *
 * $(H2 Overview)
 * The _lexer generator consists of a template mixin, $(LREF Lexer), along with
 * several helper templates for generating such things as token identifiers.
 *
 * To write a _lexer using this API:
 * $(OL
 *     $(LI Create the string array costants for your language.
 *         $(UL
 *             $(LI $(LINK2 #.staticTokens, staticTokens))
 *             $(LI $(LINK2 #.dynamicTokens, dynamicTokens))
 *             $(LI $(LINK2 #.possibleDefaultTokens, possibleDefaultTokens))
 *             $(LI $(LINK2 #.tokenHandlers, tokenHandlers))
 *         ))
 *     $(LI Create aliases for the various token and token identifier types
 *         specific to your language.
 *         $(UL
 *             $(LI $(LREF TokenIdType))
 *             $(LI $(LREF tokenStringRepresentation))
 *             $(LI $(LREF TokenStructure))
 *             $(LI $(LREF TokenId))
 *         ))
 *     $(LI Create a struct that mixes in the Lexer template mixin and
 *         implements the necessary functions.
 *         $(UL
 *             $(LI $(LREF Lexer))
 *         ))
 * )
 * Examples:
 * $(UL
 * $(LI A _lexer for D is available $(LINK2 https://github.com/Hackerpilot/Dscanner/blob/master/std/d/lexer.d, here).)
 * $(LI A _lexer for Lua is available $(LINK2 https://github.com/Hackerpilot/lexer-demo/blob/master/lualexer.d, here).)
 * $(LI A _lexer for JSON is available $(LINK2 https://github.com/Hackerpilot/lexer-demo/blob/master/jsonlexer.d, here).)
 * )
 * $(DDOC_ANCHOR TemplateParameters) $(H2 Template Parameter Definitions)
 * $(DL
 * $(DT $(DDOC_ANCHOR defaultTokenFunction) $(B defaultTokenFunction)
 * $(DD A function that serves as the default token lexing function. For most
 *     languages this will be the identifier lexing function.))
 * $(DT $(DDOC_ANCHOR tokenSeparatingFunction) $(B tokenSeparatingFunction))
 * $(DD A function that is able to determine if an identifier/keyword has come
 *     to an end. This function must return bool and take a single size_t
 *     argument representing the number of bytes to skip over before looking for
 *     a separating character.)
 * $(DT $(DDOC_ANCHOR staticTokens) $(B staticTokens))
 * $(DD A listing of the tokens whose exact value never changes and which cannot
 *     possibly be a token handled by the default token lexing function. The
 *     most common example of this kind of token is an operator such as
 *     $(D_STRING "*"), or $(D_STRING "-") in a programming language.)
 * $(DT $(DDOC_ANCHOR dynamicTokens) $(B dynamicTokens))
 * $(DD A listing of tokens whose value is variable, such as whitespace,
 *     identifiers, number literals, and string literals.)
 * $(DT $(DDOC_ANCHOR possibleDefaultTokens) $(B possibleDefaultTokens))
 * $(DD A listing of tokens that could posibly be one of the tokens handled by
 *     the default token handling function. An common example of this is
 *     a keyword such as $(D_STRING "for"), which looks like the beginning of
 *     the identifier $(D_STRING "fortunate"). $(B tokenSeparatingFunction) is
 *     called to determine if the character after the $(D_STRING 'r') separates
 *     the identifier, indicating that the token is $(D_STRING "for"), or if
 *     lexing should be turned over to the $(B defaultTokenFunction).)
 * $(DT $(DDOC_ANCHOR tokenHandlers) $(B tokenHandlers))
 * $(DD A mapping of prefixes to custom token handling function names. The
 *     generated _lexer will search for the even-index elements of this array,
 *     and then call the function whose name is the element immedately after the
 *     even-indexed element. This is used for lexing complex tokens whose prefix
 *     is fixed.)
 * )
 *
 * Here are some example constants for a simple calculator _lexer:
 * ---
 * // There are a near infinite number of valid number literals, so numbers are
 * // dynamic tokens.
 * enum string[] dynamicTokens = ["numberLiteral", "whitespace"];
 *
 * // The operators are always the same, and cannot start a numberLiteral, so
 * // they are staticTokens
 * enum string[] staticTokens = ["-", "+", "*", "/"];
 *
 * // In this simple example there are no keywords or other tokens that could
 * // look like dynamic tokens, so this is blank.
 * enum string[] possibleDefaultTokens = [];
 *
 * // If any whitespace character or digit is encountered, pass lexing over to
 * // our custom handler functions. These will be demonstrated in an example
 * // later on.
 * enum string[] tokenHandlers = [
 *     "0", "lexNumber",
 *     "1", "lexNumber",
 *     "2", "lexNumber",
 *     "3", "lexNumber",
 *     "4", "lexNumber",
 *     "5", "lexNumber",
 *     "6", "lexNumber",
 *     "7", "lexNumber",
 *     "8", "lexNumber",
 *     "9", "lexNumber",
 *     " ", "lexWhitespace",
 *     "\n", "lexWhitespace",
 *     "\t", "lexWhitespace",
 *     "\r", "lexWhitespace"
 * ];
 * ---
 *
 * Copyright: Brian Schott 2013
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt Boost, License 1.0)
 * Authors: Brian Schott, with ideas shamelessly stolen from Andrei Alexandrescu
 * Source: $(PHOBOSSRC std/_lexer.d)
 */

module std.lexer;

/**
 * Template for determining the type used for a token type. Selects the smallest
 * unsigned integral type that is able to hold the value
 * staticTokens.length + dynamicTokens.length + possibleDefaultTokens.length.
 * For example if there are 20 static tokens, 30 dynamic tokens,
 * and 10 possible default tokens, this template will alias itself to ubyte,
 * as 20 + 30 + 10 < $(D_KEYWORD ubyte).max.
 * Examples:
 * ---
 * // In our calculator example this means that IdType is an alias for ubyte.
 * alias IdType = TokenIdType!(staticTokens, dynamicTokens, possibleDefaultTokens);
 * ---
 */
template TokenIdType(alias staticTokens, alias dynamicTokens,
    alias possibleDefaultTokens)
{
    immutable tokenCount = staticTokens.length + dynamicTokens.length
        + possibleDefaultTokens.length + 1;
    static if (tokenCount <= ubyte.max)
        alias TokenIdType = ubyte;
    else static if (tokenCount <= ushort.max)
        alias TokenIdType = ushort;
    else static if (tokenCount <= uint.max)
        alias TokenIdType = uint;
    else
        static assert (false, "The number of tokens must be less than uint.max");
}

/**
 * Looks up the string representation of the given token type. This is the
 * opposite of the function of the TokenId template.
 * Params: type = the token type identifier
 * Examples:
 * ---
 * alias str = tokenStringRepresentation(IdType, staticTokens, dynamicTokens, possibleDefaultTokens);
 * assert (str(tok!"*") == "*");
 * ---
 * See_also: $(LREF TokenId)
 */
string tokenStringRepresentation(IdType, alias staticTokens, alias dynamicTokens,
    alias possibleDefaultTokens)(IdType type) @property
{
    enum tokens = staticTokens ~ dynamicTokens ~ possibleDefaultTokens;

    if (type == 0)
        return "!ERROR!";
    else if (type < tokens.length + 1)
        return tokens[type - 1];
    else
        return null;
}

unittest
{
    /// Fix https://github.com/Hackerpilot/Dscanner/issues/96
    alias IdType = TokenIdType!(["foo"], ["bar"], ["doo"]);
    enum tok(string token) = TokenId!(IdType, ["foo"], ["bar"], ["doo"], token);
    alias str = tokenStringRepresentation!(IdType, ["foo"], ["bar"], ["doo"]);

    static assert(str(tok!"foo") == "foo");
    static assert(str(tok!"bar") == "bar");
    static assert(str(tok!"doo") == "doo");
}

/**
 * Generates the token type identifier for the given symbol. There are two
 * special cases:
 * $(UL
 *     $(LI If symbol is $(D_STRING ""), then the token identifier will be 0)
 *     $(LI If symbol is $(D_STRING "\0"), then the token identifier will be the maximum
 *         valid token type identifier)
 * )
 * In all cases this template will alias itself to a constant of type IdType.
 * This template will fail at compile time if $(D_PARAM symbol) is not one of
 * the staticTokens, dynamicTokens, or possibleDefaultTokens.
 * Examples:
 * ---
 * template tok(string symbol)
 * {
 *     alias tok = TokenId!(IdType, staticTokens, dynamicTokens,
 *         possibleDefaultTokens, symbol);
 * }
 * // num and plus are of type ubyte.
 * IdType plus = tok!"+";
 * IdType num = tok!"numberLiteral";
 * ---
 */
template TokenId(IdType, alias staticTokens, alias dynamicTokens,
    alias possibleDefaultTokens, string symbol)
{
    enum tokens = staticTokens ~ dynamicTokens ~ possibleDefaultTokens;

    import std.algorithm;
    static if (symbol == "")
    {
        enum id = 0;
        alias TokenId = id;
    }
    else static if (symbol == "\0")
    {
        enum id = 1 + tokens.length;
        alias TokenId = id;
    }
    else
    {
        enum i = tokens.countUntil(symbol);
        static if (i != -1)
        {
            enum id = i + 1;
            static assert (id >= 0 && id < IdType.max, "Invalid token: " ~ symbol);
            alias TokenId = id;
        }
        else
            static assert (0, "Invalid token: " ~ symbol);
    }
}

/**
 * The token that is returned by the lexer.
 * Params:
 *     IdType = The D type of the "type" token type field.
 *     extraFields = A string containing D code for any extra fields that should
 *         be included in the token structure body. This string is passed
 *         directly to a mixin statement.
 * Examples:
 * ---
 * // No extra struct fields are desired in this example, so leave it blank.
 * alias Token = TokenStructure!(IdType, "");
 * Token minusToken = Token(tok!"-");
 * ---
 */
struct TokenStructure(IdType, string extraFields = "")
{
public:

    bool opEquals(ref const typeof(this) other) const pure nothrow @safe
    {
        return this.type == other.type && this.text == other.text;
    }

    /**
     * Returs: true if the token has the given type, false otherwise.
     */
    bool opEquals(IdType type) const pure nothrow @safe
    {
        return this.type == type;
    }

    /**
     * Constructs a token from a token type.
     * Params: type = the token type
     */
    this(IdType type)
    {
        this.type = type;
    }

    /**
     * Constructs a token.
     * Params:
     *     type = the token type
     *     text = the text of the token, which may be null
     *     line = the line number at which this token occurs
     *     column = the column number at which this token occurs
     *     index = the byte offset from the beginning of the input at which this
     *         token occurs
     */
    this(IdType type, string text, size_t line, size_t column, size_t index)
    {
        this.text = text;
        this.line = line;
        this.column = column;
        this.type = type;
        this.index = index;
    }

    /**
     * The _text of the token.
     */
    string text;

    /**
     * The _line number at which this token occurs.
     */
    size_t line;

    /**
     * The _column number at which this token occurs. This is measured in bytes
     * and may not be correct when tab characters are involved.
     */
    size_t column;

    /**
     * The byte offset from the beginning of the input at which this token
     * occurs.
     */
    size_t index;

    /**
     * The token type.
     */
    IdType type;

    mixin (extraFields);
}

/**
 * The implementation of the _lexer is contained within this mixin template.
 * To use it, this template should be mixed in to a struct that represents the
 * _lexer for your language. This struct should implement the following methods:
 * $(UL
 *     $(LI popFront, which should call this mixin's _popFront() and
 *         additionally perform any token filtering or shuffling you deem
 *         necessary. For example, you can implement popFront to skip comment or
 *          tokens.)
 *     $(LI A function that serves as the default token lexing function. For
 *         most languages this will be the identifier lexing function. This
 *         should then be passed to the $(LREF Lexer) template mixin as the
 *         $(LINK2 #.defaultTokenFunction defaultTokenFunction) template
 *         parameter).
 *     $(LI A function that is able to determine if an identifier/keyword has
 *         come to an end. This function must return $(D_KEYWORD bool) and take
 *         a single $(D_KEYWORD size_t) argument representing the number of
 *         bytes to skip over before looking for a separating character.)
 *     $(LI Any functions referred to in the tokenHandlers template paramater.
 *         These functions must be marked $(D_KEYWORD pure nothrow), take no
 *         arguments, and return a token)
 *     $(LI A constructor that initializes the range field as well as calls
 *         popFront() exactly once (to initialize the _front field).)
 * )
 * Params:
 *     Token = $(LREF TokenStructure)
 *     defaultTokenFunction = $(LINK2 #.defaultTokenFunction, defaultTokenFunction)
 *     tokenSeparatingFunction = $(LINK2 #.tokenSeparatingFunction, tokenSeparatingFunction)
 *     staticTokens = $(LINK2 #.staticTokens, staticTokens)
 *     dynamicTokens = $(LINK2 #.dynamicTokens, dynamicTokens)
 *     possibleDefaultTokens = $(LINK2 #.possibleDefaultTokens, possibleDefaultTokens)
 *     tokenHandlers = $(LINK2 #.tokenHandlers, tokenHandlers)
 * Examples:
 * ---
 * struct CalculatorLexer
 * {
 *     mixin Lexer!(IdType, Token, defaultTokenFunction, isSeparating,
 *         staticTokens, dynamicTokens, possibleDefaultTokens, tokenHandlers);
 *
 *     this (ubyte[] bytes)
 *     {
 *         this.range = LexerRange(bytes);
 *         popFront();
 *     }
 *
 *     void popFront() pure
 *     {
 *         _popFront();
 *     }
 *
 *     Token lexNumber() pure nothrow @safe
 *     {
 *         // implementation goes here
 *     }
 *
 *     Token lexWhitespace() pure nothrow @safe
 *     {
 *         // implementation goes here
 *     }
 *
 *     Token defaultTokenFunction() pure nothrow @safe
 *     {
 *         // There is no default token in the example calculator language, so
 *         // this is always an error.
 *         range.popFront();
 *         return Token(tok!"");
 *     }
 *
 *     bool isSeparating(size_t offset) pure nothrow @safe
 *     {
 *         // For this example language, always return true.
 *         return true;
 *     }
 * }
 * ---
 */
mixin template Lexer(Token, alias defaultTokenFunction,
    alias tokenSeparatingFunction, alias staticTokens, alias dynamicTokens,
    alias possibleDefaultTokens, alias tokenHandlers)
{
    private alias _IDType = typeof(Token.type);
    private enum _tok(string symbol) = TokenId!(_IDType, staticTokens, dynamicTokens, possibleDefaultTokens, symbol);

    static assert (tokenHandlers.length % 2 == 0, "Each pseudo-token must"
        ~ " have a corresponding handler function name.");

    static string generateMask(const ubyte[] arr)
    {
        import std.string;
        ulong u;
        for (size_t i = 0; i < arr.length && i < 8; i++)
        {
            u |= (cast(ulong) arr[i]) << (i * 8);
        }
        return format("0x%016x", u);
    }

    private static string generateByteMask(size_t l)
    {
        import std.string;
        return format("0x%016x", ulong.max >> ((8 - l) * 8));
    }

    private static string generateCaseStatements()
    {
        import std.algorithm;
        import std.conv;
        import std.string;
        import std.range;

        string[] pseudoTokens = stupidToArray(tokenHandlers.stride(2));
        string[] allTokens = stupidToArray(sort(staticTokens ~ possibleDefaultTokens ~ pseudoTokens).uniq);
        string code;
        for (size_t i = 0; i < allTokens.length; i++)
        {
            if (allTokens[i].length == 0)
                continue;
            size_t j = i + 1;
            while (j < allTokens.length && allTokens[i][0] == allTokens[j][0])
                j++;
            code ~= format("case 0x%02x:\n", cast(ubyte) allTokens[i][0]);
            code ~= printCase(allTokens[i .. j], pseudoTokens);
            i = j - 1;
        }
        return code;
    }

    private static string printCase(string[] tokens, string[] pseudoTokens)
    {
        import std.algorithm;
        string[] t = tokens;
        string[] sortedTokens = stupidToArray(sort!"a.length > b.length"(t));
        import std.conv;

        if (tokens.length == 1 && tokens[0].length == 1)
        {
            if (pseudoTokens.countUntil(tokens[0]) >= 0)
            {
                return "    return "
                    ~ tokenHandlers[tokenHandlers.countUntil(tokens[0]) + 1]
                    ~ "();\n";
            }
            else if (staticTokens.countUntil(tokens[0]) >= 0)
            {
                return "    range.popFront();\n"
                    ~ "    return Token(_tok!\"" ~ escape(tokens[0]) ~ "\", null, line, column, index);\n";
            }
            else if (pseudoTokens.countUntil(tokens[0]) >= 0)
            {
                return "    return "
                    ~ tokenHandlers[tokenHandlers.countUntil(tokens[0]) + 1]
                    ~ "();\n";
            }
        }

        string code;

        foreach (i, token; sortedTokens)
        {
            immutable mask = generateMask(cast (const ubyte[]) token);
            if (token.length >= 8)
                code ~= "    if (frontBytes == " ~ mask ~ ")\n";
            else
                code ~= "    if ((frontBytes & " ~ generateByteMask(token.length) ~ ") == " ~ mask ~ ")\n";
            code ~= "    {\n";
            if (pseudoTokens.countUntil(token) >= 0)
            {
                if (token.length <= 8)
                {
                    code ~= "        return "
                        ~ tokenHandlers[tokenHandlers.countUntil(token) + 1]
                        ~ "();\n";
                }
                else
                {
                    code ~= "        if (range.peek(" ~ text(token.length - 1) ~ ") == \"" ~ escape(token) ~"\")\n";
                    code ~= "            return "
                        ~ tokenHandlers[tokenHandlers.countUntil(token) + 1]
                        ~ "();\n";
                }
            }
            else if (staticTokens.countUntil(token) >= 0)
            {
                if (token.length <= 8)
                {
                    code ~= "        range.popFrontN(" ~ text(token.length) ~ ");\n";
                    code ~= "        return Token(_tok!\"" ~ escape(token) ~ "\", null, line, column, index);\n";
                }
                else
                {
                    code ~= "        pragma(msg, \"long static tokens not supported\"); // " ~ escape(token) ~ "\n";
                }
            }
            else
            {
                // possible default
                if (token.length <= 8)
                {
                    code ~= "        if (tokenSeparatingFunction(" ~ text(token.length) ~ "))\n";
                    code ~= "        {\n";
                    code ~= "            range.popFrontN(" ~ text(token.length) ~ ");\n";
                    code ~= "            return Token(_tok!\"" ~ escape(token) ~ "\", null, line, column, index);\n";
                    code ~= "        }\n";
                    code ~= "        else\n";
                    code ~= "            goto default;\n";
                }
                else
                {
                    code ~= "        if (range.peek(" ~ text(token.length - 1) ~ ") == \"" ~ escape(token) ~"\" && isSeparating(" ~ text(token.length) ~ "))\n";
                    code ~= "        {\n";
                    code ~= "            range.popFrontN(" ~ text(token.length) ~ ");\n";
                    code ~= "            return Token(_tok!\"" ~ escape(token) ~ "\", null, line, column, index);\n";
                    code ~= "        }\n";
                    code ~= "        else\n";
                    code ~= "            goto default;\n";
                }
            }
            code ~= "    }\n";
        }
        code ~= "    else\n";
        code ~= "        goto default;\n";
        return code;
    }

    /**
     * Implements the range primitive _front.
     */
    ref const(Token) front() pure nothrow const @property
    {
        return _front;
    }

    /**
     * Advances the lexer to the next token and stores the new current token in
     * the _front variable.
     */
    void _popFront() pure
    {
        _front = advance();
    }

    /**
     * Implements the range primitive _empty.
     */
    bool empty() pure const nothrow @property
    {
        return _front.type == _tok!"\0";
    }

    static string escape(string input)
    {
        string retVal;
        foreach (ubyte c; cast(ubyte[]) input)
        {
            switch (c)
            {
            case '\\': retVal ~= `\\`; break;
            case '"': retVal ~= `\"`; break;
            case '\'': retVal ~= `\'`; break;
            case '\t': retVal ~= `\t`; break;
            case '\n': retVal ~= `\n`; break;
            case '\r': retVal ~= `\r`; break;
            default: retVal ~= c; break;
            }
        }
        return retVal;
    }

    // This only exists because the real array() can't be called at compile-time
    static string[] stupidToArray(R)(R range)
    {
        string[] retVal;
        foreach (v; range)
            retVal ~= v;
        return retVal;
    }

    enum tokenSearch = generateCaseStatements();

    static ulong getFront(const ubyte[] arr) pure nothrow @trusted
    {
        import std.stdio;
        immutable importantBits = *(cast (ulong*) arr.ptr);
        immutable filler = ulong.max >> ((8 - arr.length) * 8);
        return importantBits & filler;
    }

    Token advance() pure
    {
        if (range.empty)
            return Token(_tok!"\0");
        immutable size_t index = range.index;
        immutable size_t column = range.column;
        immutable size_t line = range.line;
        immutable ulong frontBytes = getFront(range.peek(7));
        switch (frontBytes & 0x00000000_000000ff)
        {
        mixin(tokenSearch);
//        pragma(msg, tokenSearch);
        default:
            return defaultTokenFunction();
        }
    }

    /**
     * The lexer input.
     */
    LexerRange range;

    /**
     * The token that is currently at the front of the range.
     */
    Token _front;
}

/**
 * Range structure that wraps the _lexer's input.
 */
struct LexerRange
{
    /**
     * Params:
     *     bytes = the _lexer input
     *     index = the initial offset from the beginning of $(D_PARAM bytes)
     *     column = the initial _column number
     *     line = the initial _line number
     */
    this(const(ubyte)[] bytes, size_t index = 0, size_t column = 1, size_t line = 1) pure nothrow @safe
    {
        this.bytes = bytes;
        this.index = index;
        this.column = column;
        this.line = line;
    }

    /**
     * Returns: a mark at the current position that can then be used with slice.
     */
    size_t mark() const nothrow pure @safe
    {
        return index;
    }

    /**
     * Sets the range to the given position.
     * Params: m = the position to seek to
     */
    void seek(size_t m) nothrow pure @safe
    {
        index = m;
    }

    /**
     * Returs a slice of the input byte array between the given mark and the
     * current position.
     * Params m = the beginning index of the slice to return
     */
    const(ubyte)[] slice(size_t m) const nothrow pure @safe
    {
        return bytes[m .. index];
    }

    /**
     * Implements the range primitive _empty.
     */
    bool empty() const nothrow pure @safe
    {
        return index >= bytes.length;
    }

    /**
     * Implements the range primitive _front.
     */
    ubyte front() const nothrow pure @safe
    {
        return bytes[index];
    }

    /**
     * Returns: the current item as well as the items $(D_PARAM p) items ahead.
     */
    const(ubyte)[] peek(size_t p) const nothrow pure @safe
    {
        return index + p + 1 > bytes.length
            ? bytes[index .. $]
            : bytes[index .. index + p + 1];
    }

    /**
     *
     */
    ubyte peekAt(size_t offset) const nothrow pure @safe
    {
        return bytes[index + offset];
    }

    /**
     * Returns: true if it is possible to peek $(D_PARAM p) bytes ahead.
     */
    bool canPeek(size_t p) const nothrow pure @safe
    {
        return index + p < bytes.length;
    }

    /**
     * Implements the range primitive _popFront.
     */
    void popFront() pure nothrow @safe
    {
        index++;
        column++;
    }

    /**
     * Implements the algorithm _popFrontN more efficiently. This function does
     * not detect or handle newlines.
     */
    void popFrontN(size_t n) pure nothrow @safe
    {
        index += n;
        column += n;
    }

    /**
     * Increments the range's line number and resets the column counter.
     */
    void incrementLine() pure nothrow @safe
    {
        column = 1;
        line++;
    }

    /**
     * The input _bytes.
     */
    const(ubyte)[] bytes;

    /**
     * The range's current position.
     */
    size_t index;

    /**
     * The current _column number.
     */
    size_t column;

    /**
     * The current _line number.
     */
    size_t line;
}
