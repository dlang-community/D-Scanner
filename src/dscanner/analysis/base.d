module dscanner.analysis.base;

import dparse.ast;
import dparse.lexer : IdType, str, Token, tok;
import dsymbol.scope_ : Scope;
import std.array;
import std.container;
import std.meta : AliasSeq;
import std.string;
import std.sumtype;
import dmd.attrib : UserAttributeDeclaration;
import dmd.visitor.transitive;
import dmd.visitor;
import dmd.func;
import core.stdc.string;
import std.conv : to;

///
struct AutoFix
{
	///
	struct CodeReplacement
	{
		/// Byte index `[start, end)` within the file what text to replace.
		/// `start == end` if text is only getting inserted.
		size_t[2] range;
		/// The new text to put inside the range. (empty to delete text)
		string newText;
	}

	/// Context that the analyzer resolve method can use to generate the
	/// resolved `CodeReplacement` with.
	struct ResolveContext
	{
		/// Arbitrary analyzer-defined parameters. May grow in the future with
		/// more items.
		ulong[3] params;
		/// For dynamically sized data, may contain binary data.
		string extraInfo;
	}

	/// Display name for the UI.
	string name;
	/// Either code replacements, sorted by range start, never overlapping, or a
	/// context that can be passed to `BaseAnalyzer.resolveAutoFix` along with
	/// the message key from the parent `Message` object.
	///
	/// `CodeReplacement[]` should be applied to the code in reverse, otherwise
	/// an offset to the following start indices must be calculated and be kept
	/// track of.
	SumType!(CodeReplacement[], ResolveContext) replacements;

	invariant
	{
		replacements.match!(
			(const CodeReplacement[] replacement)
			{
				import std.algorithm : all, isSorted;

				assert(replacement.all!"a.range[0] <= a.range[1]");
				assert(replacement.isSorted!"a.range[0] < b.range[0]");
			},
			(_) {}
		);
	}

	static AutoFix resolveLater(string name, ulong[3] params, string extraInfo = null)
	{
		AutoFix ret;
		ret.name = name;
		ret.replacements = ResolveContext(params, extraInfo);
		return ret;
	}

	static AutoFix replacement(size_t tokenStart, size_t tokenEnd, string newText, string name)
	{
		AutoFix ret;
		ret.name = name;
		ret.replacements = [
			AutoFix.CodeReplacement([tokenStart, tokenEnd], newText)
		];
		return ret;
	}

	static AutoFix replacement(size_t[2] range, string newText, string name)
	{
		AutoFix ret;
		ret.name = name;
		ret.replacements = [
			AutoFix.CodeReplacement(range, newText)
		];
		return ret;
	}

	static AutoFix insertionAt(size_t index, string content, string name = null)
	{
		assert(content.length > 0, "generated auto fix inserting text without content");
		AutoFix ret;
		ret.name = name.length
			? name
			: content.strip.length
				? "Insert `" ~ content.strip ~ "`"
				: "Insert whitespace";
		ret.replacements = [
			AutoFix.CodeReplacement([index, index], content)
		];
		return ret;
	}

	AutoFix concat(AutoFix other) const
	{
		import std.algorithm : sort;

		static immutable string errorMsg = "Cannot concatenate code replacement with late-resolve";

		AutoFix ret;
		ret.name = name;
		CodeReplacement[] concatenated = expectReplacements(errorMsg).dup
			~ other.expectReplacements(errorMsg);
		concatenated.sort!"a.range[0] < b.range[0]";
		ret.replacements = concatenated;
		return ret;
	}

	CodeReplacement[] expectReplacements(
		string errorMsg = "Expected available code replacements, not something to resolve later"
	) @safe pure nothrow @nogc
	{
		return replacements.match!(
			(replacement)
			{
				if (false) return CodeReplacement[].init;
				static if (is(immutable typeof(replacement) == immutable CodeReplacement[]))
					return replacement;
				else
					assert(false, errorMsg);
			}
		);
	}

	const(CodeReplacement[]) expectReplacements(
		string errorMsg = "Expected available code replacements, not something to resolve later"
	) const @safe pure nothrow @nogc
	{
		return replacements.match!(
			(const replacement)
			{
				if (false) return CodeReplacement[].init;
				static if (is(immutable typeof(replacement) == immutable CodeReplacement[]))
					return replacement;
				else
					assert(false, errorMsg);
			}
		);
	}
}

/// Formatting style for autofix generation (only available for resolve autofix)
struct AutoFixFormatting
{
	enum AutoFixFormatting invalid = AutoFixFormatting(BraceStyle.invalid, null, 0, null);

	enum BraceStyle
	{
		/// invalid, shouldn't appear in usable configs
		invalid,
		/// $(LINK https://en.wikipedia.org/wiki/Indent_style#Allman_style)
		allman,
		/// $(LINK https://en.wikipedia.org/wiki/Indent_style#Variant:_1TBS)
		otbs,
		/// $(LINK https://en.wikipedia.org/wiki/Indent_style#Variant:_Stroustrup)
		stroustrup,
		/// $(LINK https://en.wikipedia.org/wiki/Indentation_style#K&R_style)
		knr,
	}

	/// Brace style config
	BraceStyle braceStyle = BraceStyle.allman;
	/// String to insert on indentations
	string indentation = "\t";
	/// For calculating indentation size
	uint indentationWidth = 4;
	/// String to insert on line endings
	string eol = "\n";

	invariant
	{
		import std.algorithm : all;

		assert(!indentation.length
			|| indentation == "\t"
			|| indentation.all!(c => c == ' '));
	}

	string getWhitespaceBeforeOpeningBrace(string lastLineIndent, bool isFuncDecl) pure nothrow @safe const
	{
		final switch (braceStyle)
		{
		case BraceStyle.invalid:
			assert(false, "invalid formatter config");
		case BraceStyle.knr:
			if (isFuncDecl)
				goto case BraceStyle.allman;
			else
				goto case BraceStyle.otbs;
		case BraceStyle.otbs:
		case BraceStyle.stroustrup:
			return " ";
		case BraceStyle.allman:
			return eol ~ lastLineIndent;
		}
	}
}

/// A diagnostic message. Each message defines one issue in the file, which
/// consists of one or more squiggly line ranges within the code, as well as
/// human readable descriptions and optionally also one or more automatic code
/// fixes that can be applied.
struct Message
{
	/// A squiggly line range within the code. May be the issue itself if it's
	/// the `diagnostic` member or supplemental information that can aid the
	/// user in resolving the issue.
	struct Diagnostic
	{
		/// Name of the file where the warning was triggered.
		string fileName;
		/// Byte index from start of file the warning was triggered.
		size_t startIndex, endIndex;
		/// Line number where the warning was triggered, 1-based.
		size_t startLine, endLine;
		/// Column number where the warning was triggered. (in bytes)
		size_t startColumn, endColumn;
		/// Warning message, may be null for supplemental diagnostics.
		string message;

		deprecated("Use startLine instead") alias line = startLine;
		deprecated("Use startColumn instead") alias column = startColumn;

		static Diagnostic from(string fileName, size_t[2] index, size_t line, size_t[2] columns, string message)
		{
			return Message.Diagnostic(fileName, index[0], index[1], line, line, columns[0], columns[1], message);
		}

		static Diagnostic from(string fileName, size_t[2] index, size_t[2] lines, size_t[2] columns, string message)
		{
			return Message.Diagnostic(fileName, index[0], index[1], lines[0], lines[1], columns[0], columns[1], message);
		}
	}

	/// Primary warning
	Diagnostic diagnostic;
	/// List of supplemental warnings / hint locations
	Diagnostic[] supplemental;
	/// Name of the warning
	string key;
	/// Check name
	string checkName;

	/// Either immediate code changes that can be applied or context to call
	/// the `BaseAnalyzer.resolveAutoFix` method with.
	AutoFix[] autofixes;

	this(string fileName, size_t line, size_t column, string key = null, string message = null, string checkName = null)
	{
		diagnostic.fileName = fileName;
		diagnostic.startLine = diagnostic.endLine = line;
		diagnostic.startColumn = diagnostic.endColumn = column;
		diagnostic.message = message;
		this.key = key;
		this.checkName = checkName;
	}

	this(string fileName, size_t line, size_t column, string key = null, string message = null, string checkName = null, AutoFix[] autofixes = null)
	{
		diagnostic.fileName = fileName;
		diagnostic.startLine = diagnostic.endLine = line;
		diagnostic.startColumn = diagnostic.endColumn = column;
		diagnostic.message = message;
		this.key = key;
		this.checkName = checkName;
		this.autofixes = autofixes;
	}

	this(Diagnostic diagnostic, string key = null, string checkName = null, AutoFix[] autofixes = null)
	{
		this.diagnostic = diagnostic;
		this.key = key;
		this.checkName = checkName;
		this.autofixes = autofixes;
	}

	this(Diagnostic diagnostic, Diagnostic[] supplemental, string key = null, string checkName = null, AutoFix[] autofixes = null)
	{
		this.diagnostic = diagnostic;
		this.supplemental = supplemental;
		this.key = key;
		this.checkName = checkName;
		this.autofixes = autofixes;
	}

	alias diagnostic this;
}

enum comparitor = q{ a.startLine < b.startLine || (a.startLine == b.startLine && a.startColumn < b.startColumn) };

alias MessageSet = RedBlackTree!(Message, comparitor, true);

/**
 * Should be present in all visitors to specify the name of the check
 *  done by a patricular visitor
 */
mixin template AnalyzerInfo(string checkName)
{
	enum string name = checkName;

	extern (D) override protected string getName()
	{
		return name;
	}
}

struct BaseAnalyzerArguments
{
	string fileName;
	const(Token)[] tokens;
	const Scope* sc;
	bool skipTests = false;

	BaseAnalyzerArguments setSkipTests(bool v)
	{
		auto ret = this;
		ret.skipTests = v;
		return ret;
	}
}

abstract class BaseAnalyzer : ASTVisitor
{
public:
	deprecated("Don't use this constructor, use the one taking BaseAnalyzerArguments")
	this(string fileName, const Scope* sc, bool skipTests = false)
	{
		BaseAnalyzerArguments args = {
			fileName: fileName,
			sc: sc,
			skipTests: skipTests
		};
		this(args);
	}

	this(BaseAnalyzerArguments args)
	{
		this.sc = args.sc;
		this.tokens = args.tokens;
		this.fileName = args.fileName;
		this.skipTests = args.skipTests;
		_messages = new MessageSet;
	}

	string getName()
	{
		assert(0);
	}

	Message[] messages()
	{
		return _messages[].array;
	}

	alias visit = ASTVisitor.visit;

	/**
	* Visits a unittest.
	*
	* When overriden, the protected bool "skipTests" should be handled
	* so that the content of the test is not analyzed.
	*/
	override void visit(const Unittest unittest_)
	{
		if (!skipTests)
			unittest_.accept(this);
	}

	/**
	* Visits a module declaration.
	*
	* When overriden, make sure to keep this structure
	*/
	override void visit(const(Module) mod)
	{
		mod.accept(this);
	}

	AutoFix.CodeReplacement[] resolveAutoFix(
		const Module mod,
		scope const(Token)[] tokens,
		const AutoFix.ResolveContext context,
		const AutoFixFormatting formatting,
	)
	{
		cast(void) mod;
		cast(void) tokens;
		cast(void) context;
		cast(void) formatting;
		assert(0);
	}

protected:

	bool inAggregate;
	bool skipTests;
	const(Token)[] tokens;

	template visitTemplate(T)
	{
		override void visit(const T structDec)
		{
			inAggregate = true;
			structDec.accept(this);
			inAggregate = false;
		}
	}

	/**
	 * The file name
	 */
	string fileName;

	const(Scope)* sc;

	MessageSet _messages;
}

/**
 * Visitor that implements the AST traversal logic.
 * Supports collecting error messages
 */
extern(C++) class BaseAnalyzerDmd : SemanticTimeTransitiveVisitor
{
	alias visit = SemanticTimeTransitiveVisitor.visit;

	extern(D) this(string fileName, bool skipTests = false)
	{
		this.fileName = fileName;
		this.skipTests = skipTests;
		_messages = new MessageSet;
	}

	/**
	 * Ensures that template AnalyzerInfo is instantiated in all classes
	 *  deriving from this class
	 */
	extern(D) string getName()
	{
		assert(0);
	}

	extern(D) Message[] messages()
	{
		return _messages[].array;
	}

	override void visit(UnitTestDeclaration ud)
	{
		if (!skipTests)
			super.visit(ud);
	}


protected:

	extern (D) void addErrorMessage(size_t line, size_t column, string key, string message)
	{
		_messages.insert(Message(fileName, line, column, key, message, getName()));
	}

	extern (D) void addErrorMessage(size_t line, size_t column, string key, string message, AutoFix[] autofixes)
	{
		_messages.insert(Message(fileName, line, column, key, message, getName(), autofixes));
	}

	extern (D) void addErrorMessage(size_t[2] index, size_t[2] lines, size_t[2] columns, string key, string message)
	{
		auto diag = Message.Diagnostic.from(fileName, index, lines, columns, message);
		_messages.insert(Message(diag, key, getName()));
	}

	extern (D) void addErrorMessage(size_t[2] index, size_t[2] lines, size_t[2] columns,
		string key, string message, AutoFix[] autofixes)
	{
		auto diag = Message.Diagnostic.from(fileName, index, lines, columns, message);
		_messages.insert(Message(diag, key, getName(), autofixes));
	}

	extern (D) bool skipTests;

	/**
	 * The file name
	 */
	extern (D) string fileName;

	extern (D) MessageSet _messages;

	extern (D) bool shouldIgnoreDecl(UserAttributeDeclaration userAtt, string key)
	{
		import std.algorithm : startsWith;
		import std.string : indexOf;

		if (userAtt is null)
			return false;

		auto atts = userAtt.atts;
		if (atts !is null && (*(atts)).length > 0)
		{
			if (auto att = (*(atts))[0].isStringExp())
			{
				string attStr = cast(string) att.toStringz();
				if (attStr.startsWith("nolint") && attStr.indexOf(key) > 0)
					return true;
			}
		}

		return false;
	}
}
