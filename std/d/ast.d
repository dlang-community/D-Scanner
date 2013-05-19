// Written in the D programming language.

/**
 * This module defines an Abstract Syntax Tree for the D language
 */

module std.d.ast;

import std.container;
import std.d.lexer;

interface ASTVisitor
{
}

interface ASTNode
{
    void accept(ASTVisitor visitor);
}

immutable string OVERRIDE_DEFAULT_ACCEPT = q{override void accept(ASTVisitor visitor) { visitor.visit(this); }};
immutable string DEFAULT_ACCEPT = q{void accept(ASTVisitor visitor) { visitor.visit(this); }};

class Module : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    ModuleDeclaration moduleDeclaration;
    Declaration[] declarations;
}

class ModuleDeclaration : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    IdentifierChain moduleName;
}

class IdentifierChain : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token[] identifiers;
}

abstract class Declaration : ASTNode {
    void accept(ASTVisitor visitor)
    {
        visitor.visit(this);
    }
}

class AttributedDeclaration : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
}

class ImportDeclaration : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    bool isStatic;
    ImportList importList;
}

class ImportList : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    SingleImport singleImport;
    ImportList next;
    ImportBindings bindings;
}

class SingleImport : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token rename;
    IdentifierChain import_;
}

class ImportBindings : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    SingleImport bind;
    ImportBindList bindList;
}

class ImportBindList : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    ImportBind[] importBinds;
}

class ImportBind : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token left;
    Token right;
}

class FunctionDeclaration : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    Type returnType;
    Token name;
    TemplateParameters templateParameters;
    Parameters parameters;
    Constraint constraint;
    FunctionBody functionBody;
}

class Parameters : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Parameter[] paramaters;
}

class Parameter : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    ParameterAttribute[] paramaterAttributes;
    Type type;
    Token name;
    DefaultInitializationExpression defaultInitialization;
    bool vararg;
}

class ParameterAttribute: ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token attribute;
    TypeConstructor typeConstructor;
}

class TypeConstructor : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token typeConstructor;
}

class DefaultInitializationExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    AssignExpression assignExpression;
    Token specialKeyword;
}

class FunctionBody : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    InStatement inStatement;
    OutStatement outStatement;
    BodyStatement bodyStatement;
    BlockStatement blockStatement;
}

class InStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    BlockStatement blockStatement;
}

class OutStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token parameter;
    BlockStatement blockStatement;
}

class BodyStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    BlockStatement blockStatement;
}

class VariableDeclaration : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    Type type;
    Declarator[] declarators;
}

class Declarator : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token identifier;
    Initializer initializer;
}

class AliasDeclaration : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    Type type;
    Declarator declarator;
    AliasInitializer[] initializations;
}

class AliasInitializer : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token identifier;
    Type type;
}

class AliasThisDeclaration : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    Token identifier;
}

class AlignAttribute : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token integerLiteral;
}

class StructDeclaration : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    Token name;
    TemplateParameters templateParameters;
    Constraint constraint;
    StructBody structBody;
}

class StructBody : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Declaration[] declarations;
}

class ClassDeclaration : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    Token name;
    TemplateParameters templateParameters;
    Constraint constraint;
    IdentifierList superClasses;
    ClassBody classBody;
}

class IdentifierList : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token[] identifiers;
}

class ClassBody : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);

    struct DeclarationOrInvariant
    {
        bool isDeclaration;
        union
        {
            Declaration declaration;
            Invariant classInvariant;
        }
    }

    DeclarationOrInvariant[] declarationsAndInvariants;
}

class Invariant : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    BlockStatement blockStatement;
}

class InterfaceDeclaration : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    Token name;
    TemplateParameters templateParameters;
    Constraint constraint;
    IdentifierList superInterfaces;
    StructBody interfaceBody;
}

class UnionDeclaration : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    Token name;
    TemplateParameters templateParameters;
    Constraint constraint;
    StructBody unionBody;
}

class MixinDeclaration : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    AssignExpression expression;
}

class Unittest : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    BlockStatement testBody;
}

class TemplateDeclaration : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    Token name;
    TemplateParameters templateParameters;
    Constraint constraint;
    Declaration[] declarations;
}

class TemplateParameters : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    TemplateParameterList parameterList;
}

class TemplateParameterList : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    TemplateParameter[] parameters;
}

class TemplateParameter : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    TemplateTypeParameter valueParameter;
    TemplateValueParameter valueParameter;
    TemplateAliasParameter aliasParameter;
    TemplateTupleParameter tupleParameter;
    TemplateThisParameter thisParameter;
}

class TemplateTypeParameter : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Type templateTypeParameterSpecialization;
    Type templateTypeParameterDefault;
}

class TemplateValueParameter : ASTNode {}

class TemplateAliasParameter : ASTNode {}

class TemplateTupleParameter : ASTNode {}

class TemplateThisParameter : ASTNode {}

class Constraint : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Expression expression;
}

class StaticConstructor : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    FunctionBody functionBody;
}

class StaticDestructor : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    FunctionBody functionBody;
}

class SharedStaticConstructor : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    FunctionBody functionBody;
}

class SharedStaticDestructor : Declaration
{
public:
    mixin(OVERRIDE_DEFAULT_ACCEPT);
    FunctionBody functionBody;
}

class Type
{
    TypeConstructors typeConstructors;
    Type2 type2;
}

class Type2
{
    Type3 type3;
    TypeSuffix typeSuffix;
    Type2 type2;
}

class TypeSuffix : ASTNode
{
    Type type;
    AssignExpression assignExpression;
    bool star;
}

class BuiltinType : ASTNode
{
    Token type;
}

class Type3
{
    BuiltinType builtinType;
    Typeof typeof_;
    IdentifierChain identifierChain;
}

class Expression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    AssignExpression[] expressions;
}

class AssignExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    TernaryExpression ternaryExpression;
    AssignOperator assignOperator;
    AssignExpression assignExpression;
}

class TernaryExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    OrOrExpression orOrExpression;
    Expression expression;
    TernaryExpression ternaryExpression;
}

class OrOrExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    AndAndExpression andandExpression;
    OrOrExpression orOrExpression;
}

class AndAndExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    OrExpression orExpression;
    AndAndExpression andandExpression;
    CmpExpression cmpExpression;
}

class orExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    OrExpression orExpression;
    XorExpression xorExpression;
}

class XorExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    XorExpression xorExpression;
    AndExpression andExpression;
}

class AndExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    AndExpression andExpression;
    shiftExpression shiftExpression;
}

class CmpExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    ShiftExpression shiftExpression;
    EqualExpression equalExpression;
    IdentityExpression identityExpression;
    RelExpression relExpression;
    InExpression inExpression;
}

immutable string SHIFT_SHIFT_BODY = q{
    Token operator;
    ShiftExpression left;
    ShiftExpression right;
};

class EqualExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    mixin(SHIFT_SHIFT_BODY);
}

class IdentityExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    mixin(SHIFT_SHIFT_BODY);
}

class RelExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    mixin(SHIFT_SHIFT_BODY);
}

class InExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    mixin(SHIFT_SHIFT_BODY);
}

class ShiftExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    AddExpression addExpression;
    shiftExpression shiftExpression;
}

class AddExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token operator;
    AddExpression addExpression;
    MulExpression mulExpression;
}

class MulExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token operator;
    PowExpression powExpression;
    MulExpression  mulExpression;
}

class PowExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    PowExpression powExpression;
    UnaryExpression  unaryExpression;
}

class UnaryExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token operator;
    UnaryExpression unaryExpression;
    PreIncDecExpression preIncDecExpression;
    NewExpression newExpression;
    DeleteExpression deleteExpression;
    CastExpression castExpression;
    PrimaryExpression primaryExpression;
    ArgumentList argumentList;
    AssignExpression sliceLower;
    AssignExpression sliceUpper;
    IdentifierOrTemplateInstance identifierOrTemplateInstance;
}

class ArgumentList : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    AssignExpression[] arguments;
}

class Arguments : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    ArgumentList argumentList;
}

class ArrayInitializer : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    ArrayMemberInitializations arrayMemberInitializations;
}

class ArrayLiteral : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    ArgumentList argumentList;
}

class ArrayMemberInitialization : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    AssignExpression assignExpression;
    NonVoidInitializer nonVoidInitializer;
}

class ArrayMemberInitializations : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    ArrayMemberInitialization[] arrayMemberInitializations;
}

class AssertExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    AssignExpression assertion;
    AssignExpression message;
}

class AssertStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    AssertExpression assertExpression;
}

class AssignStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    PreIncDecExpression preIncDecExpression;
    PostIncDecExpression postIncDecExpression;
    UnaryExpression[] unaryExpressions;
    AssignExpression[] assignExpressions;
    Token[] assignOperators;
}

class AssocArrayLiteral : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    KeyValuePairs keyValuePairs;
}

class AtAttribute : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    FunctionCallExpression functionCallExpression;
    ArgumentList argumentList;
    Token identifier;
}

class AttributedDeclaration : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Declaration[] declarations
}

class Attribute : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    LinkageAttribute linkageAttribute;
    AlignAttribute alignAttribute;
    PragmaExpression pragmaExpression;
    Deprecated deprecated_;
    ProtectionAttribute protectionAttribute;
    AtAttribute atAttribute;
    Token attribute;
}

class AutoDeclaration : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    StorageClass storageClass;
    Token[] identifiers;
    Initializer[] initializers;
}

class BlockStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    DeclarationsAndStatements declarationsAndStatements;
}

class BodyStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    BlockStatements declarationsAndStatements;
}

class BreakStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token identifier;
    bool hasIdentifier;
}

class BuiltinType : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token token;
}

class CaseRangeStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    AssignExpression low;
    AssignExpression high;
    DeclarationsAndStatements declarationsAndStatements;
}

class CaseStatement: ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    ArgumentList argumentList;
    DeclarationsAndStatements declarationsAndStatements;
}

class CastExpression: ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Type type;
    CastQualifier castQualifier;
}

class CastQualifier: ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token first;
    Token second;
    bool hasSecond;
}

class Catches: ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Catch[] catches;
    LastCatch lastCatch;
}

class Catch: ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Type type;
    Token identifier;
    NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
}

class ClassBody: ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Declaration[] declarations;
    Invariant[] invariants;
}

class ClassDeclaration: ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token name;
    TemplateParameters templateParameters;
    Constraint constraint;
    IdentifierList superClasses;
    ClassBody classBody;
}

class CmpExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    ShiftExpression shiftExpression;
    EqualExpression equalExpression;
    IdentityExpression identityExpression;
    RelExpression relExpression;
    InExpression inExpression;
}

class CompileCondition : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    VersionCondition versionCondition;
    DebugCondition debugCondition;
    StaticIfCondition staticIfCondition;
}

class ConditionalDeclaration : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    CompileCondition compileCondition;
    Declaration[] trueDeclarations;
    Declaration[] falseDeclarations;
}

class ConditionalStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    CompileCondition compileCondition;
    NonEmptyStatementNoCaseNoDefault trueStatement;
    NonEmptyStatementNoCaseNoDefault falseStatement;
}

class Constraint : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Expression expression;
}

class Constructor : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Parameters parameters;
    FunctionBody functionBody;
}

class ContinueStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    bool hasIdentifier;
    Token identifier;
}

class DebugCondition : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token identifierOrInteger;
    bool hasIdentifierOrInteger;
}

class DebugSpecification : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    Token identifierOrInteger;
}

class  : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);

}
