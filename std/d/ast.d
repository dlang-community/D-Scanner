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
    AliasInitialization[] initializations;
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
    andExpression andExpression;
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
    PreIncDecExpression preIncDecExpression
    NewExpression newExpression;
    DeleteExpression deleteExpression;
    CastExpression castExpression;
    PrimaryExpression primaryExpression;
    ArgumentList argumentList;
    AssignExpression sliceLower;
    AssignExpression sliceUpper;
    IdentifierOrTemplateInstance identifierOrTemplateInstance;
}
