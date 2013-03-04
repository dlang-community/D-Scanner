// Written in the D programming language.

/**
 * This module defines an Abstract Syntax Tree for the D language
 */

module std.d.ast;

import std.container;
import std.d.lexer;

interface ASTNode {}
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
}

class ModuleDeclaration : ASTNode
{
	string[] packageName;
	string moduleName;
}



struct Import
{
	string moduleName;
	string aliasName;
	string[] symbols;
}


interface Statement : ASTNode {}
class EmptyStatement : Statement, NoScopeStatement {}
interface NoScopeNonEmptyStatement : ASTNode {}
interface NoScopeStatement : ASTNode {}
interface NonEmptyStatement : NoScopeNonEmptyStatement, NoScopeStatement, Statement {}
interface NoScopeBlockStatement : Statement {}
interface NonEmptyOrScopeBlockStatement : ASTNode {}
interface ScopeBlockStatement : NonEmptyOrScopeBlockStatement {}

interface NonEmptyStatementNoCaseNoDefault : NonEmptyStatement {}

class LabeledStatement : NonEmptyStatementNoCaseNoDefault
{
	string label;
	NoScopeStatement statement;
}

interface ExpressionStatement : NonEmptyStatementNoCaseNoDefault {}
interface DeclarationStatement : NonEmptyStatementNoCaseNoDefault {}

/**
 * $(LINK2 http://dlang.org/statement.html#IfStatement)
 */
class IfStatement : NonEmptyStatementNoCaseNoDefault
{

}
class WhileStatement : NonEmptyStatementNoCaseNoDefault {}
class DoStatement : NonEmptyStatementNoCaseNoDefault {}
class ForStatement : NonEmptyStatementNoCaseNoDefault {}
class ForeachStatement : NonEmptyStatementNoCaseNoDefault {}
class SwitchStatement : NonEmptyStatementNoCaseNoDefault {}
class FinalSwitchStatement : NonEmptyStatementNoCaseNoDefault {}

/**
 * $(LINK http://dlang.org/statement.html#ContinueStatement)
 */
class ContinueStatement : NonEmptyStatementNoCaseNoDefault
{
	string identifier;
}

/**
 *
 */
class BreakStatement : NonEmptyStatementNoCaseNoDefault
{
	string identifier;
}
class ReturnStatement : NonEmptyStatementNoCaseNoDefault {}
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
}
class WithStatement : NonEmptyStatementNoCaseNoDefault {}
class SynchronizedStatement : NonEmptyStatementNoCaseNoDefault {}
class TryStatement : NonEmptyStatementNoCaseNoDefault {}
class ScopeGuardStatement : NonEmptyStatementNoCaseNoDefault {}
class ThrowStatement : NonEmptyStatementNoCaseNoDefault {}
class AsmStatement : NonEmptyStatementNoCaseNoDefault {}
class PragmaStatement : NonEmptyStatementNoCaseNoDefault {}
class MixinStatement : NonEmptyStatementNoCaseNoDefault {}
class ForeachRangeStatement : NonEmptyStatementNoCaseNoDefault {}
class ConditionalStatement : NonEmptyStatementNoCaseNoDefault {}
class StaticAssert : NonEmptyStatementNoCaseNoDefault, DeclDef {}
class TemplateMixin : NonEmptyStatementNoCaseNoDefault, DeclDef {}
class ImportDeclaration : NonEmptyStatementNoCaseNoDefault, DeclDef
{
	bool isStatic;
	Import[] importList;
}


class BlockStatement : NoScopeNonEmptyStatement, ScopeBlockStatement
{
	Statement[] statements;
}

interface Expression : ASTNode {}
class CommaExpression : Expression
{
	AssignExpression left;
	AssignExpression right;
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
}

interface ConditionalExpression : Expression {}

class TernaryExpression : ConditionalExpression
{
	OrOrExpression left;
	/// Null unless this is a ternary
	Expression middle;
	/// Null unless this is a ternary
	ConditionalExpression right;
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
}
interface NewExpression : UnaryExpression {}
interface DeleteExpression : UnaryExpression {}
interface CastExpression : UnaryExpression {}
interface PowExpression : UnaryExpression {}


interface PrimaryExpression : Expression {}
class SingleTokenExpression
{
	Token token;
}
class ThisExpression : SingleTokenExpression {}
class SuperExpression : SingleTokenExpression {}
class NullExpression : SingleTokenExpression {}
class TrueExpression : SingleTokenExpression {}
class FalseExpression : SingleTokenExpression {}
class DollarExpression : SingleTokenExpression {}
class FileExpression : SingleTokenExpression {}
class LineExpression : SingleTokenExpression {}
class IntegerExpression : SingleTokenExpression {}
class FloatExpression : SingleTokenExpression {}
class CharacterExpression : SingleTokenExpression {}
class StringExpression : SingleTokenExpression {}
class IdentifierExpression : SingleTokenExpression {}
class ArrayExpression : PrimaryExpression {}



interface DefaultInitializerExpression : ASTNode {}

class RelExpression : CmpExpression
{
	ShiftExpression left;
	ShiftExpression right;
	TokenType operator;
}

class Parameter : ASTNode
{

	string[] inOut;
	string type;
}

/+
//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module basicast;

import std.d.lexer;

struct Scope
{
	size_t begin;
	size_t end;
	Scope* parent;
}

struct ModuleDeclaration
{
	string[] package_;
	string name;
}

struct Module
{
	ModuleDeclaration moduleDeclaration;
	VariableDeclaration[] variables;
	FunctionDeclaration[] functions;
	Enum[] enums;
	Scope*[] scopes;
}

enum DeclDefType : ubyte
{
	attributeSpecifier,
	importDeclaration,
	enumDeclaration,
	classDeclaration,
	interfaceDeclaration,
	aggregateDeclaration,
	declaration,
	constructor,
	destructor,
	unitTest,
	staticConstructor,
	staticDestructor,
	sharedStaticConstructor,
	sharedStaticDestructor,
	conditionalDeclaration,
	debugSpecification,
	versionSpecification,
	staticAssert,
	templatedeclaration,
	templateMixinDeclaration,
	templateMixin,
	mixinDeclaration,
	semicolon
}

class DeclDef
{
	DeclDefType type;
}

struct Enum
{
	bool singleValue;
	EnumMember[] members;
	string baseType;
}

struct AttributeList
{
public:

	void set(TokenType attribute)
	in
	{
		assert(isAttribute(attribute));
	}
	body
	{
		attributes ~= attribute;
	}

	const(TokenType)[] get()
	{
		return attributes[];
	}

private:

	TokenType[] attributes;
}

struct Parameter
{
	string name;
	string type;
	string def;
}

struct FunctionDeclaration
{
	AttributeList attributes;
	Parameter[] ctParameters;
	Parameter[] rtParameters;
	string returnType;
	string name;
	uint line;
}

struct VariableDeclaration
{
	AttributeList attributes;
	string name;
	string type;
	uint line;
}

struct Import
{
	struct ImportSymbol
	{
		string symbolName;
		string alias_;
	}

	string alias_;
	string moduleName;
	string[] packageParts;
	ImportSymbol[] symbols;
}

class ImportDeclaration : DeclDef
{
	Import[] imports;
}

class Inherits : DeclDef
{
	//FunctionDeclaration[] functions;
}
+/
