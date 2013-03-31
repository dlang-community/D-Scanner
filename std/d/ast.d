// Written in the D programming language.

/**
 * This module defines an Abstract Syntax Tree for the D language
 */

module std.d.ast;

import std.container;
import std.d.lexer;


interface ASTVisitor
{
    ///
    void visit(ASTNode node);
    ///
    void visit(Module node);
    ///
    void visit(ModuleDeclaration node);
    ///
    void visit(CaseStatement node);
    ///
    void visit(DefaultStatement node);
    ///
    void visit(CaseRangeStatement node);
    ///
    void visit(LabeledStatement node);
}

interface ASTNode
{
    void accept(ASTVisitor visitor;)
}

immutable string DEFAULT_ACCEPT = q{override void accept(ASTVisitor visitor) { visitor.visit(this); }};

interface DeclDef : ASTNode {}
interface AttributeSpecifier : DeclDef {}
interface EnumDeclaration : DeclDef {}
interface ClassDeclaration : DeclDef {}
interface InterfaceDeclaration : DeclDef {}
interface AggregateDeclaration : DeclDef {}
interface Declaration : DeclDef {}
interface Constructor : DeclDef {}
interface Destructor : DeclDef {}
interface UnitTest : DeclDef {}
interface StaticConstructor : DeclDef {}
interface StaticDestructor : DeclDef {}
interface SharedStaticConstructor : DeclDef {}
interface SharedStaticDestructor : DeclDef {}
interface ConditionalDeclaration : DeclDef {}
interface DebugSpecification : DeclDef {}
interface VersionSpecification : DeclDef {}
interface TemplateDeclaration : DeclDef {}
interface TemplateMixinDeclaration : DeclDef {}
interface MixinDeclaration : DeclDef {}


class Module : ASTNode
{
	ModuleDeclaration declaration;
	DList!(DeclDef) declDefs;
    mixin(DEFAULT_ACCEPT);
}

class ModuleDeclaration : ASTNode
{
	string[] packageName;
	string moduleName;
    mixin(DEFAULT_ACCEPT);
}



struct Import
{
	string moduleName;
	string aliasName;
	string[] symbols;
}


interface Statement : ASTNode {}
class EmptyStatement : Statement, NoScopeStatement
{
    mixin(DEFAULT_ACCEPT);
}
interface NoScopeNonEmptyStatement : ASTNode {}
interface NoScopeStatement : ASTNode {}
interface NonEmptyStatement : NoScopeNonEmptyStatement, NoScopeStatement, Statement {}
interface NonEmptyOrScopeBlockStatement : Statement {} //BUG: The standard does not say that NonEmptyOrScopeBlockStatement is a statement
interface ScopeBlockStatement : NonEmptyOrScopeBlockStatement {}

interface NonEmptyStatementNoCaseNoDefault : NonEmptyStatement {}
class CaseStatement : NonEmptyStatement
{
    mixin(DEFAULT_ACCEPT);
}

class DefaultStatement : NonEmptyStatement
{
    mixin(DEFAULT_ACCEPT);
}

class CaseRangeStatement : NonEmptyStatement
{
    mixin(DEFAULT_ACCEPT);
}

class LabeledStatement : NonEmptyStatementNoCaseNoDefault
{
	string label;
	NoScopeStatement statement;
    mixin(DEFAULT_ACCEPT);
}

interface ExpressionStatement : NonEmptyStatementNoCaseNoDefault {}
interface DeclarationStatement : NonEmptyStatementNoCaseNoDefault {}

class BlockStatement : NoScopeNonEmptyStatement, ScopeBlockStatement, NoScopeStatement
{
	Statement[] statements;
    mixin(DEFAULT_ACCEPT);
}


/**
 * $(LINK2 http://dlang.org/statement.html#IfStatement)
 */
class IfStatement : NonEmptyStatementNoCaseNoDefault
{
    mixin(DEFAULT_ACCEPT);
}

class WhileStatement : NonEmptyStatementNoCaseNoDefault
{
    mixin(DEFAULT_ACCEPT);
}

class DoStatement : NonEmptyStatementNoCaseNoDefault
{
    mixin(DEFAULT_ACCEPT);
}

class ForStatement : NonEmptyStatementNoCaseNoDefault
{
    mixin(DEFAULT_ACCEPT);
}

class ForeachStatement : NonEmptyStatementNoCaseNoDefault
{
    mixin(DEFAULT_ACCEPT);
}

class SwitchStatement : NonEmptyStatementNoCaseNoDefault
{
    mixin(DEFAULT_ACCEPT);
}

class FinalSwitchStatement : NonEmptyStatementNoCaseNoDefault
{
    mixin(DEFAULT_ACCEPT);
}

/**
 * $(LINK http://dlang.org/statement.html#ContinueStatement)
 */
class ContinueStatement : NonEmptyStatementNoCaseNoDefault
{
	string identifier;
    mixin(DEFAULT_ACCEPT);
}

/**
 *
 */
class BreakStatement : NonEmptyStatementNoCaseNoDefault
{
	string identifier;
    mixin(DEFAULT_ACCEPT);
}

class ReturnStatement : NonEmptyStatementNoCaseNoDefault
{
    mixin(DEFAULT_ACCEPT);
}

class GotoStatement : NonEmptyStatementNoCaseNoDefault
{
	enum GotoType
	{
		identifier,
		default_,
		case_,
		caseExpression
	}

	union
	{
		//Expression expression;
		string identifier;
	}

	GotoType type;

    mixin(DEFAULT_ACCEPT);
}
class WithStatement : NonEmptyStatementNoCaseNoDefault {}
class SynchronizedStatement : NonEmptyStatementNoCaseNoDefault
{
	mixin(DEFAULT_ACCEPT);
}
class TryStatement : NonEmptyStatementNoCaseNoDefault
{
	mixin(DEFAULT_ACCEPT);
}
class ScopeGuardStatement : NonEmptyStatementNoCaseNoDefault
{
	mixin(DEFAULT_ACCEPT);
}
class ThrowStatement : NonEmptyStatementNoCaseNoDefault
{
	mixin(DEFAULT_ACCEPT);
}
class AsmStatement : NonEmptyStatementNoCaseNoDefault
{
	mixin(DEFAULT_ACCEPT);
}
class PragmaStatement : NonEmptyStatementNoCaseNoDefault
{
	mixin(DEFAULT_ACCEPT);
}
class MixinStatement : NonEmptyStatementNoCaseNoDefault
{
	mixin(DEFAULT_ACCEPT);
}
class ForeachRangeStatement : NonEmptyStatementNoCaseNoDefault
{
	mixin(DEFAULT_ACCEPT);
}
class ConditionalStatement : NonEmptyStatementNoCaseNoDefault
{
	mixin(DEFAULT_ACCEPT);
}
class StaticAssert : NonEmptyStatementNoCaseNoDefault, DeclDef
{
	mixin(DEFAULT_ACCEPT);
}
class TemplateMixin : NonEmptyStatementNoCaseNoDefault, DeclDef
{
	mixin(DEFAULT_ACCEPT);
}
class ImportDeclaration : NonEmptyStatementNoCaseNoDefault, DeclDef
{
	bool isStatic;
	Import[] importList;
}




interface Expression : ASTNode {}

class CommaExpression : Expression
{
	AssignExpression left;
	AssignExpression right;
    mixin(DEFAULT_ACCEPT);
}

class AssignExpression
{
	ConditionalExpression left;
	ConditionalExpression right;
	TokenType operator;

	invariant()
	{
		assert (
			operator == TokenType.assign
			|| operator == TokenType.plusEqual
			|| operator == TokenType.minusEqual
			|| operator == TokenType.mulEqual
			|| operator == TokenType.divEqual
			|| operator == TokenType.modEqual
			|| operator == TokenType.bitAndEqual
			|| operator == TokenType.bitOrEqual
			|| operator == TokenType.xorEqual
			|| operator == TokenType.catEqual
			|| operator == TokenType.shiftLeftEqual
			|| operator == TokenType.shiftRightEqual
			|| operator == TokenType.unsignedShiftRightEqual
			|| operator == TokenType.powEqual
		);
	}

    mixin(DEFAULT_ACCEPT);
}

interface ConditionalExpression : Expression {}

class TernaryExpression : ConditionalExpression
{
	OrOrExpression left;
	/// Null unless this is a ternary
	Expression middle;
	/// Null unless this is a ternary
	ConditionalExpression right;

    mixin(DEFAULT_ACCEPT);
}

interface OrOrExpression : ConditionalExpression {}

interface AndAndExpression : OrOrExpression {}
interface OrExpression : AndAndExpression {}
interface CmpExpression : AndAndExpression {}
interface XorExpression : OrExpression {}
interface AndExpression : XorExpression {}
interface ShiftExpression : AndExpression {}
interface AddExpression : ShiftExpression {}
interface MulExpression : AddExpression {}
interface CatExpression : AddExpression {}
interface UnaryExpression : MulExpression {}
class ComplementaryExpression : UnaryExpression
{
	UnaryExpression unary;
    mixin(DEFAULT_ACCEPT);
}
interface NewExpression : UnaryExpression {}
interface DeleteExpression : UnaryExpression {}
interface CastExpression : UnaryExpression {}
interface PowExpression : UnaryExpression {}


interface PrimaryExpression : Expression {}
class SingleTokenExpression
{
	Token token;
    mixin(DEFAULT_ACCEPT);
}
class ThisExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class SuperExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class NullExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class TrueExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class FalseExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class DollarExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class FileExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class LineExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class IntegerExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class FloatExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class CharacterExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class StringExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class IdentifierExpression : SingleTokenExpression
{
	mixin(DEFAULT_ACCEPT);
}
class ArrayExpression : PrimaryExpression
{
	mixin(DEFAULT_ACCEPT);
}



interface DefaultInitializerExpression : ASTNode {}

class RelExpression : CmpExpression
{
	ShiftExpression left;
	ShiftExpression right;
	TokenType operator;
    mixin(DEFAULT_ACCEPT);
}

class Parameter : ASTNode
{
	TokenType[] inOut;
	string type;
    mixin(DEFAULT_ACCEPT);
}
