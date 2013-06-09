// Written in the D programming language.

/**
 * This module defines an Abstract Syntax Tree for the D language
 *
 * Examples:
 * ---
 * // TODO
 * ---
 *
 * Copyright: Brian Schott 2013
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt Boost, License 1.0)
 * Authors: Brian Schott
 * Source: $(PHOBOSSRC std/d/_ast.d)
 */

module std.d.ast;

import std.d.lexer;

/**
 * Implements the $(LINK2 http://en.wikipedia.org/wiki/Visitor_pattern, Visitor Pattern)
 * for the various AST ///
classes
 */
abstract ///
class ASTVisitor
{
	/** */void visit(AddExpression addExpression) {}
	/** */void visit(AliasDeclaration aliasDeclaration) {}
	/** */void visit(AliasInitializer aliasInitializer) {}
	/** */void visit(AliasThisDeclaration aliasThisDeclaration) {}
	/** */void visit(AlignAttribute alignAttribute) {}
	/** */void visit(AndAndExpression andAndExpression) {}
	/** */void visit(AndExpression andExpression) {}
	/** */void visit(ArgumentList argumentList) {}
	/** */void visit(Arguments arguments) {}
	/** */void visit(ArrayInitializer arrayInitializer) {}
	/** */void visit(ArrayLiteral arrayLiteral) {}
	/** */void visit(ArrayMemberInitialization arrayMemberInitialization) {}
	/** */void visit(AsmAddExp asmAddExp) {}
	/** */void visit(AsmAndExp asmAndExp) {}
	/** */void visit(AsmBrExp asmBrExp) {}
	/** */void visit(AsmEqualExp asmEqualExp) {}
	/** */void visit(AsmExp asmExp) {}
	/** */void visit(AsmInstruction asmInstruction) {}
	/** */void visit(AsmLogAndExp asmLogAndExp) {}
	/** */void visit(AsmLogOrExp asmLogOrExp) {}
	/** */void visit(AsmMulExp asmMulExp) {}
	/** */void visit(AsmOrExp asmOrExp) {}
	/** */void visit(AsmPrimaryExp asmPrimaryExp) {}
	/** */void visit(AsmRelExp asmRelExp) {}
	/** */void visit(AsmShiftExp asmShiftExp) {}
	/** */void visit(AsmStatement asmStatement) {}
	/** */void visit(AsmTypePrefix asmTypePrefix) {}
	/** */void visit(AsmUnaExp asmUnaExp) {}
	/** */void visit(AsmXorExp asmXorExp) {}
	/** */void visit(AssertExpression assertExpression) {}
	/** */void visit(AssertStatement assertStatement) {}
	/** */void visit(AssignExpression assignExpression) {}
	/** */void visit(AssignStatement assignStatement) {}
	/** */void visit(AssocArrayLiteral assocArrayLiteral) {}
	/** */void visit(AtAttribute atAttribute) {}
	/** */void visit(Attribute attribute) {}
	/** */void visit(AttributedDeclaration attributedDeclaration) {}
	/** */void visit(AutoDeclaration autoDeclaration) {}
	/** */void visit(BlockStatement blockStatement) {}
	/** */void visit(BodyStatement bodyStatement) {}
	/** */void visit(BreakStatement breakStatement) {}
	/** */void visit(BaseClass baseClass) {}
	/** */void visit(BaseClassList baseClassList) {}
	/** */void visit(BasicType builtinType) {}
	/** */void visit(CaseRangeStatement caseRangeStatement) {}
	/** */void visit(CaseStatement caseStatement) {}
	/** */void visit(CastExpression castExpression) {}
	/** */void visit(CastQualifier castQualifier) {}
	/** */void visit(Catch catch_) {}
	/** */void visit(Catches catches) {}
	/** */void visit(ClassBody classBody) {}
	/** */void visit(ClassDeclaration classDeclaration) {}
	/** */void visit(CmpExpression cmpExpression) {}
	/** */void visit(CompileCondition compileCondition) {}
	/** */void visit(ConditionalDeclaration conditionalDeclaration) {}
	/** */void visit(ConditionalStatement conditionalStatement) {}
	/** */void visit(Constraint constraint) {}
	/** */void visit(Constructor constructor) {}
	/** */void visit(ContinueStatement continueStatement) {}
	/** */void visit(DebugCondition debugCondition) {}
	/** */void visit(DebugSpecification debugSpecification) {}
	/** */void visit(Declaration declaration) {}
	/** */void visit(DeclarationsAndStatements declarationsAndStatements) {}
	/** */void visit(DeclarationOrInvariant declarationOrInvariant) {}
	/** */void visit(Declarator declarator) {}
	/** */void visit(DeclaratorSuffix declaratorSuffix) {}
	/** */void visit(DefaultStatement defaultStatement) {}
	/** */void visit(DeleteExpression deleteExpression) {}
	/** */void visit(DeleteStatement deleteStatement) {}
	/** */void visit(Deprecated deprecated_) {}
	/** */void visit(Destructor destructor) {}
	/** */void visit(DoStatement doStatement) {}
	/** */void visit(EnumBody enumBody) {}
	/** */void visit(EnumDeclaration enumDeclaration) {}
	/** */void visit(EnumMember enumMember) {}
	/** */void visit(EqualExpression equalExpression) {}
	/** */void visit(Expression expression) {}
	/** */void visit(FinalSwitchStatement finalSwitchStatement) {}
	/** */void visit(Finally finally_) {}
	/** */void visit(ForStatement forStatement) {}
	/** */void visit(ForeachRangeStatement foreachRangeStatement) {}
	/** */void visit(ForeachStatement foreachStatement) {}
	/** */void visit(ForeachType foreachType) {}
	/** */void visit(ForeachTypeList foreachTypeList) {}
	/** */void visit(FunctionAttribute functionAttribute) {}
	/** */void visit(FunctionBody functionBody) {}
	/** */void visit(FunctionCallExpression functionCallExpression) {}
	/** */void visit(FunctionCallStatement functionCallStatement) {}
	/** */void visit(FunctionDeclaration functionDeclaration) {}
	/** */void visit(FunctionLiteralExpression functionLiteralExpression) {}
	/** */void visit(GotoStatement gotoStatement) {}
	/** */void visit(IdentifierChain identifierChain) {}
	/** */void visit(IdentifierList identifierList) {}
	/** */void visit(IdentifierOrTemplateChain identifierOrTemplateChain) {}
	/** */void visit(IdentifierOrTemplateInstance identifierOrTemplateInstance) {}
	/** */void visit(IdentityExpression identityExpression) {}
	/** */void visit(IfStatement ifStatement) {}
	/** */void visit(ImportBind importBind) {}
	/** */void visit(ImportBindings importBindings) {}
	/** */void visit(ImportDeclaration importDeclaration) {}
	/** */void visit(ImportExpression importExpression) {}
	/** */void visit(ImportList importList) {}
	/** */void visit(IndexExpression indexExpression) {}
	/** */void visit(InExpression inExpression) {}
	/** */void visit(InStatement inStatement) {}
	/** */void visit(Initialize initialize) {}
	/** */void visit(Initializer initializer) {}
	/** */void visit(InterfaceDeclaration interfaceDeclaration) {}
	/** */void visit(Invariant invariant_) {}
	/** */void visit(IsExpression isExpression) {}
	/** */void visit(KeyValuePair keyValuePair) {}
	/** */void visit(KeyValuePairs keyValuePairs) {}
	/** */void visit(LabeledStatement labeledStatement) {}
	/** */void visit(LambdaExpression lambdaExpression) {}
	/** */void visit(LastCatch lastCatch) {}
	/** */void visit(LinkageAttribute linkageAttribute) {}
	/** */void visit(MemberFunctionAttribute memberFunctionAttribute) {}
	/** */void visit(MixinDeclaration mixinDeclaration) {}
	/** */void visit(MixinExpression mixinExpression) {}
	/** */void visit(MixinTemplateName mixinTemplateName) {}
	/** */void visit(Module module_) {}
	/** */void visit(ModuleDeclaration moduleDeclaration) {}
	/** */void visit(MulExpression mulExpression) {}
	/** */void visit(NewAnonClassExpression newAnonClassExpression) {}
	/** */void visit(NewExpression newExpression) {}
	/** */void visit(NonEmptyStatement nonEmptyStatement) {}
	/** */void visit(NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault) {}
	/** */void visit(NonVoidInitializer nonVoidInitializer) {}
	/** */void visit(Operand operand) {}
	/** */void visit(Operands operands) {}
	/** */void visit(OrExpression orExpression) {}
	/** */void visit(OrOrExpression orOrExpression) {}
	/** */void visit(OutStatement outStatement) {}
	/** */void visit(Parameter parameter) {}
	/** */void visit(ParameterAttribute parameterAttribute) {}
	/** */void visit(Parameters parameters) {}
	/** */void visit(Postblit postblit) {}
	/** */void visit(PostIncDecExpression postIncDecExpression) {}
	/** */void visit(PowExpression powExpression) {}
	/** */void visit(PragmaDeclaration pragmaDeclaration) {}
	/** */void visit(PragmaExpression pragmaExpression) {}
	/** */void visit(PreIncDecExpression preIncDecExpression) {}
	/** */void visit(PrimaryExpression primaryExpression) {}
	/** */void visit(Register register) {}
	/** */void visit(RelExpression relExpression) {}
	/** */void visit(ReturnStatement returnStatement) {}
	/** */void visit(ScopeGuardStatement scopeGuardStatement) {}
	/** */void visit(SharedStaticConstructor sharedStaticConstructor) {}
	/** */void visit(SharedStaticDestructor sharedStaticDestructor) {}
	/** */void visit(ShiftExpression shiftExpression) {}
	/** */void visit(SingleImport singleImport) {}
	/** */void visit(SliceExpression sliceExpression) {}
	/** */void visit(Statement statement) {}
	/** */void visit(StatementNoCaseNoDefault statementNoCaseNoDefault) {}
	/** */void visit(StaticAssertDeclaration staticAssertDeclaration) {}
	/** */void visit(StaticAssertStatement staticAssertStatement) {}
	/** */void visit(StaticConstructor staticConstructor) {}
	/** */void visit(StaticDestructor staticDestructor) {}
	/** */void visit(StaticIfCondition staticIfCondition) {}
	/** */void visit(StorageClass storageClass) {}
	/** */void visit(StructBody structBody) {}
	/** */void visit(StructBodyItem structBodyItem) {}
	/** */void visit(StructDeclaration structDeclaration) {}
	/** */void visit(StructInitializer structInitializer) {}
	/** */void visit(StructMemberInitializer structMemberInitializer) {}
	/** */void visit(StructMemberInitializers structMemberInitializers) {}
	/** */void visit(SwitchBody switchBody) {}
	/** */void visit(SwitchStatement switchStatement) {}
	/** */void visit(Symbol symbol) {}
	/** */void visit(SynchronizedStatement synchronizedStatement) {}
	/** */void visit(TemplateAliasParameter templateAliasParameter) {}
	/** */void visit(TemplateArgument templateArgument) {}
	/** */void visit(TemplateArgumentList templateArgumentList) {}
	/** */void visit(TemplateArguments templateArguments) {}
	/** */void visit(TemplateDeclaration templateDeclaration) {}
	/** */void visit(TemplateInstance templateInstance) {}
	/** */void visit(TemplateMixinStatement templateMixinStatement) {}
	/** */void visit(TemplateParameter templateParameter) {}
	/** */void visit(TemplateParameterList templateParameterList) {}
	/** */void visit(TemplateParameters templateParameters) {}
	/** */void visit(TemplateSingleArgument templateSingleArgument) {}
	/** */void visit(TemplateThisParameter templateThisParameter) {}
	/** */void visit(TemplateTupleParameter templateTupleParameter) {}
	/** */void visit(TemplateTypeParameter templateTypeParameter) {}
	/** */void visit(TemplateValueParameter templateValueParameter) {}
	/** */void visit(TemplateValueParameterDefault templateValueParameterDefault) {}
	/** */void visit(TernaryExpression ternaryExpression) {}
	/** */void visit(ThrowStatement throwStatement) {}
	/** */void visit(TraitsArgument traitsArgument) {}
	/** */void visit(TraitsExpression traitsExpression) {}
	/** */void visit(TryStatement tryStatement) {}
	/** */void visit(Type type) {}
	/** */void visit(Type2 type2) {}
	/** */void visit(Type3 type3) {}
	/** */void visit(TypeConstructor typeConstructor) {}
	/** */void visit(TypeConstructors typeConstructors) {}
	/** */void visit(TypeSpecialization typeSpecialization) {}
	/** */void visit(TypeSuffix typeSuffix) {}
	/** */void visit(TypeidExpression typeidExpression) {}
	/** */void visit(TypeofExpression typeofExpression) {}
	/** */void visit(UnaryExpression unaryExpression) {}
	/** */void visit(UnionDeclaration unionDeclaration) {}
	/** */void visit(Unittest unittest_) {}
	/** */void visit(VariableDeclaration variableDeclaration) {}
	/** */void visit(VersionCondition versionCondition) {}
	/** */void visit(VersionSpecification versionSpecification) {}
	/** */void visit(WhileStatement whileStatement) {}
	/** */void visit(WithStatement withStatement) {}
	/** */void visit(XorExpression xorExpression) {}
}

interface ASTNode
{
	/** */ void accept(ASTVisitor visitor);
}

immutable string OVERRIDE_DEFAULT_ACCEPT = q{override void accept(ASTVisitor visitor) { visitor.visit(this); }};
immutable string DEFAULT_ACCEPT = q{void accept(ASTVisitor visitor) { visitor.visit(this); }};


immutable string SHIFT_SHIFT_BODY = q{
	/** */ Token operator;
	/** */ ShiftExpression left;
	/** */ ShiftExpression right;
};

///
class AddExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */TokenType operator;
	/** */AddExpression left;
	/** */MulExpression right;
}

///
class AliasDeclaration : Declaration
{
public:
	mixin(OVERRIDE_DEFAULT_ACCEPT);
	/** */Type type;
	/** */Declarator declarator;
	/** */AliasInitializer[] initializations;
}

///
class AliasInitializer : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */Token identifier;
	/** */Type type;
}

///
class AliasThisDeclaration : Declaration
{
public:
	mixin(OVERRIDE_DEFAULT_ACCEPT);
	/** */Token identifier;
}

///
class AlignAttribute : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */Token intLiteral;
}

///
class AndAndExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AndAndExpression left;
	/** */OrExpression right;
}

///
class AndExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AndExpression andExpression;
	/** */ShiftExpression shiftExpression;
}

///
class ArgumentList : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AssignExpression[] arguments;
}

///
class Arguments : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ArgumentList argumentList;
}

///
class ArrayInitializer : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ArrayMemberInitialization[] arrayMemberInitializations;
}

///
class ArrayLiteral : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ArgumentList argumentList;
}

///
class ArrayMemberInitialization : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AssignExpression assignExpression;
	/** */NonVoidInitializer nonVoidInitializer;
}

///
class AsmAddExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */Token operator;
	/** */AsmMulExp left;
	/** */AsmMulExp right;
}

///
class AsmAndExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmEqualExp left;
	/** */AsmEqualExp right;
}

///
class AsmBrExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmBrExp asmBrExp;
	/** */AsmEqualExp asmEqualExp;
	/** */AsmUnaExp asmUnaExp;
}

///
class AsmEqualExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmRelExp left;
	/** */AsmRelExp right;
	/** */Token operator;
}

///
class AsmExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmLogOrExp left;
	/** */AsmExp middle;
	/** */AsmExp right;
}

///
class AsmInstruction : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */Token identifierOrIntegerOrOpcode;
	/** */bool hasAlign;
	/** */AsmExp asmExp;
	/** */Operands operands;
}

///
class AsmLogAndExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmOrExp left;
	/** */AsmOrExp right;
}

///
class AsmLogOrExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmLogAndExp left;
	/** */AsmLogAndExp right;
}

///
class AsmMulExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmBrExp left;
	/** */AsmBrExp right;
	/** */Token operator;
}

///
class AsmOrExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmXorExp left;
	/** */AsmXorExp right;
}

///
class AsmPrimaryExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */IdentifierChain identifierChain;
	/** */Register register;
	/** */Token token;
}

///
class AsmRelExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmShiftExp left;
	/** */AsmShiftExp right;
	/** */Token operator;
}

///
class AsmShiftExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmAddExp left;
	/** */AsmAddExp right;
	/** */Token operator;
}

///
class AsmStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmInstruction[] asmInstructions;
}

///
class AsmTypePrefix : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */Token left;
	/** */Token right;
}

///
class AsmUnaExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmTypePrefix asmTypePrefix;
	/** */AsmExp asmExp;
	/** */Token prefix;
	/** */AsmPrimaryExp asmPrimaryExp;
	/** */AsmUnaExp asmUnaExp;
}

///
class AsmXorExp : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AsmAndExp left;
	/** */AsmAndExp right;
}

///
class AssertExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AssignExpression assertion;
	/** */AssignExpression message;
}

///
class AssertStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AssertExpression assertExpression;
}

///
class AssignExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */TernaryExpression ternaryExpression;
	/** */AssignExpression assignExpression;
	/** */TokenType operator;
}

///
class AssignStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */PreIncDecExpression preIncDecExpression;
	/** */PostIncDecExpression postIncDecExpression;
	/** */UnaryExpression[] unaryExpressions;
	/** */AssignExpression[] assignExpressions;
	/** */TokenType[] assignOperators;
}

///
class AssocArrayLiteral : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */KeyValuePairs keyValuePairs;
}

///
class AtAttribute : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */FunctionCallExpression functionCallExpression;
	/** */ArgumentList argumentList;
	/** */Token identifier;
}

///
class AttributedDeclaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */Attribute attribute;
	/** */Declaration[] declarations;
}

///
class Attribute : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */LinkageAttribute linkageAttribute;
	/** */AlignAttribute alignAttribute;
	/** */PragmaExpression pragmaExpression;
	/** */Deprecated deprecated_;
	/** */AtAttribute atAttribute;
	/** */TokenType attribute;
}

///
class AutoDeclaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */StorageClass storageClass;
	/** */Token[] identifiers;
	/** */Initializer[] initializers;
}

///
class BlockStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */DeclarationsAndStatements declarationsAndStatements;
}

///
class BodyStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */BlockStatement blockStatement;
}

///
class BreakStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ bool hasIdentifier;
}

///
class BaseClass : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ IdentifierOrTemplateChain identifierOrTemplateChain;
	/** */ TypeofExpression typeofExpression;
}

///
class BaseClassList : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ BaseClass[] baseClasses;
}

///
class BasicType : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */TokenType type;
}

///
class CaseRangeStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */AssignExpression low;
	/** */AssignExpression high;
	/** */DeclarationsAndStatements declarationsAndStatements;
}

///
class CaseStatement: ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ArgumentList argumentList;
	/** */DeclarationsAndStatements declarationsAndStatements;
}

///
class CastExpression: ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */Type type;
	/** */CastQualifier castQualifier;
	/** */UnaryExpression unaryExpression;
}

///
class CastQualifier: ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */TokenType first;
	/** */TokenType second;
	/** */bool hasSecond;
}

///
class Catches: ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Catch[] catches;
	/** */ LastCatch lastCatch;
}

///
class Catch: ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Type type;
	/** */ Token identifier;
	/** */ NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
}

///
class ClassBody: ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ DeclarationOrInvariant[] declarationOrInvariants;
}

///
class ClassDeclaration: ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token name;
	/** */ TemplateParameters templateParameters;
	/** */ Constraint constraint;
	/** */ BaseClassList baseClassList;
	/** */ ClassBody classBody;
}

///
class CmpExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ ShiftExpression shiftExpression;
	/** */ EqualExpression equalExpression;
	/** */ IdentityExpression identityExpression;
	/** */ RelExpression relExpression;
	/** */ InExpression inExpression;
}

///
class CompileCondition : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */VersionCondition versionCondition;
	/** */DebugCondition debugCondition;
	/** */StaticIfCondition staticIfCondition;
}

///
class ConditionalDeclaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */CompileCondition compileCondition;
	/** */Declaration[] trueDeclarations;
	/** */Declaration[] falseDeclarations;
}

///
class ConditionalStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */CompileCondition compileCondition;
	/** */NonEmptyStatementNoCaseNoDefault trueStatement;
	/** */NonEmptyStatementNoCaseNoDefault falseStatement;
}

///
class Constraint : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */Expression expression;
}

///
class Constructor : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */Parameters parameters;
	/** */FunctionBody functionBody;
}

///
class ContinueStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ bool hasIdentifier;
	/** */ Token identifier;
}

///
class DebugCondition : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifierOrInteger;
	/** */ bool hasIdentifierOrInteger;
}

///
class DebugSpecification : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifierOrInteger;
}

///
class Declaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AttributedDeclaration attributedDeclaration;
	/** */ ImportDeclaration importDeclaration;
	/** */ FunctionDeclaration functionDeclaration;
	/** */ VariableDeclaration variableDeclaration;
	/** */ AliasThisDeclaration aliasThisDeclaration;
	/** */ StructDeclaration structDeclaration;
	/** */ ClassDeclaration classDeclaration;
	/** */ InterfaceDeclaration interfaceDeclaration;
	/** */ UnionDeclaration unionDeclaration;
	/** */ EnumDeclaration enumDeclaration;
	/** */ AliasDeclaration aliasDeclaration;
	/** */ MixinDeclaration mixinDeclaration;
	/** */ Unittest unittest_;
	/** */ StaticAssertDeclaration staticAssertDeclaration;
	/** */ TemplateDeclaration templateDeclaration;
	/** */ Constructor constructor;
	/** */ Destructor destructor;
	/** */ StaticConstructor staticConstructor;
	/** */ StaticDestructor staticDestructor;
	/** */ SharedStaticDestructor sharedStaticDestructor;
	/** */ SharedStaticConstructor sharedStaticConstructor;
	/** */ ConditionalDeclaration conditionalDeclaration;
	/** */ PragmaDeclaration pragmaDeclaration;
}

///
class DeclarationsAndStatements : ASTNode
{
	mixin(DEFAULT_ACCEPT);
	/** */ ASTNode[] declarationsAndStatements;
}

///
class DeclarationOrInvariant : ASTNode
{
    mixin(DEFAULT_ACCEPT);
    Declaration declaration;
    Invariant invariant_;
}

///
class Declarator : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ DeclaratorSuffix declaratorSuffix;
	/** */ Initializer initializer;
}

///
class DeclaratorSuffix : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Type type;
	/** */ AssignExpression assignExpression;
}

///
class DefaultStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ DeclarationsAndStatements declarationsAndStatements;
}

///
class DeleteExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ UnaryExpression unaryExpression;
}

///
class DeleteStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ DeleteExpression deleteExpression;
}

///
class Deprecated : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AssignExpression assignExpression;
}

///
class Destructor : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ FunctionBody functionBody;
}

///
class DoStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
	/** */ Expression expression;
}

///
class EnumBody : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ EnumMember[] enumMembers;
}

///
class EnumDeclaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ Type type;
	/** */ EnumBody enumBody;
}

///
class EnumMember : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ Type type;
	/** */ AssignExpression assignExpression;
}

///
class EqualExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	mixin(SHIFT_SHIFT_BODY);
}

///
class Expression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AssignExpression[] assignExpressions;
}

///
class FinalSwitchStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ SwitchStatement switchStatement;
}

///
class Finally : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
}

///
class ForStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Declaration initializationDeclaration;
	/** */ Statement initializationStatement;
	/** */ Expression test;
	/** */ Expression increment;
	/** */ NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
}

///
class ForeachRangeStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ ForeachType foreachType;
	/** */ Expression lower;
	/** */ Expression higher;
	/** */ NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
}

///
class ForeachStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token foreachType;
	/** */ ForeachTypeList foreachTypeList;
	/** */ Expression expression;
	/** */ NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
}

///
class ForeachType : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ bool isRef;
	/** */ Type type;
	/** */ Token identifier;
}

///
class ForeachTypeList : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ ForeachType[] foreachTypes;
}

///
class FunctionAttribute : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token pureOrNothrow;
	/** */ AtAttribute atAttribute;
}

///
class FunctionBody : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ BlockStatement blockStatement;
	/** */ BodyStatement bodyStatement;
	/** */ OutStatement outStatement;
	/** */ InStatement inStatement;
}

///
class FunctionCallExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ UnaryExpression unaryExpression;
	/** */ TemplateArguments templateArguments;
	/** */ Arguments arguments;
}

///
class FunctionCallStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ FunctionCallExpression functionCallExpression;
}

///
class FunctionDeclaration : Declaration
{
public:
	mixin(OVERRIDE_DEFAULT_ACCEPT);
	/** */ Type returnType;
	/** */ Token name;
	/** */ TemplateParameters templateParameters;
	/** */ Parameters parameters;
	/** */ Constraint constraint;
	/** */ FunctionBody functionBody;
}

///
class FunctionLiteralExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token functionOrDelegate;
	/** */ Type type;
	/** */ Parameters parameters;
	/** */ FunctionAttribute[] functionAttributes;
	/** */ FunctionBody functionBody;
}

///
class GotoStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Expression expression;
	/** */ Token identifier;
	/** */ bool isDefault;
}

///
class IdentifierChain : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token[] identifiers;
}

///
class IdentifierList : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token[] identifiers;
}

///
class IdentifierOrTemplateChain : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ IdentifierOrTemplateInstance[] identifiers;
}

///
class IdentifierOrTemplateInstance : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ TemplateInstance templateInstance;
}

///
class IdentityExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	mixin(SHIFT_SHIFT_BODY);
}

///
class IfStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Expression expression;
	/** */ NonEmptyStatementNoCaseNoDefault thenStatement;
	/** */ NonEmptyStatementNoCaseNoDefault elseStatement;
}

///
class ImportBind : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token left;
	/** */ Token right;
}

///
class ImportBindings : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ SingleImport bind;
	/** */ ImportBind[] ImportBinds;
}

///
class ImportDeclaration : Declaration
{
public:
	mixin(OVERRIDE_DEFAULT_ACCEPT);
	/** */ bool isStatic;
	/** */ ImportList importList;
}

///
class ImportExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AssignExpression assignExpression;
}

///
class ImportList : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ SingleImport singleImport;
	/** */ ImportList next;
	/** */ ImportBindings bindings;
}

///
class IndexExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ UnaryExpression unaryExpression;
	/** */ ArgumentList argumentList;
}

///
class InExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	mixin(SHIFT_SHIFT_BODY);
}

///
class InStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ BlockStatement blockStatement;
}

///
class Initialize : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
}

///
class Initializer : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ NonVoidInitializer nonVoidInitializer;
}

///
class InterfaceDeclaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ TemplateParameters templateParameters;
	/** */ Constraint constraint;
	/** */ IdentifierList identifierList;
	/** */ StructBody structBody;
}

///
class Invariant : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ BlockStatement blockStatement;
}

///
class IsExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Type type;
	/** */ AssignExpression assignExpression;
	/** */ Token identifier;
	/** */ TypeSpecialization typeSpecialization;
	/** */ TemplateParameterList templateParameterList;
	/** */ Token equalsOrColon;
}

///
class KeyValuePair : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ KeyValuePair[] keyValuePairs;
}

///
class KeyValuePairs : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AssignExpression key;
	/** */ AssignExpression value;
}

///
class LabeledStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
    Token identifier;
	/** */ Statement statement;
}

///
class LambdaExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ Parameters paramaters;
	/** */ FunctionAttribute[] functionAttributes;
	/** */ AssignExpression assignExpression;
}

///
class LastCatch : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
}

///
class LinkageAttribute : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ bool hasPlusPlus;
}

///
class MemberFunctionAttribute : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token token;
	/** */ FunctionAttribute functionAttribute;
}

///
class MixinDeclaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ MixinExpression mixinExpression;
}

///
class MixinExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AssignExpression assignExpression;
}

///
class MixinTemplateName : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ bool hasDot;
	/** */ IdentifierOrTemplateChain identifierOrTemplateChain;
	/** */ TypeofExpression typeofExpression;
}

///
class Module : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ ModuleDeclaration moduleDeclaration;
	/** */ Declaration[] declarations;
}

///
class ModuleDeclaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ IdentifierChain moduleName;
}


///
class MulExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token operator;
	/** */ UnaryExpression left;
	/** */ UnaryExpression right;
}

///
class NewAnonClassExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Arguments allocatorArguments;
	/** */ Arguments constructorArguments;
	/** */ IdentifierList identifierList;
	/** */ ClassBody ///
classBody;

}

///
class NewExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Type type;
	/** */ NewAnonClassExpression newAnonClassExpression;
	/** */ Arguments arguments;
	/** */ AssignExpression assignExpression;
}

///
class NonEmptyStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
	/** */ CaseStatement caseStatement;
	/** */ CaseRangeStatement caseRangeStatement;
	/** */ DefaultStatement defaultStatement;
}

///
class NonEmptyStatementNoCaseNoDefault : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ LabeledStatement labeledStatement;
	/** */ BlockStatement blockStatement;
	/** */ AssignStatement assignStatement;
	/** */ IfStatement ifStatement;
	/** */ WhileStatement whileStatement;
	/** */ DoStatement doStatement;
	/** */ ForStatement forStatement;
	/** */ ForeachStatement foreachStatement;
	/** */ SwitchStatement switchStatement;
	/** */ FinalSwitchStatement finalSwitchStatement;
	/** */ ContinueStatement continueStatement;
	/** */ BreakStatement breakStatement;
	/** */ ReturnStatement returnStatement;
	/** */ GotoStatement gotoStatement;
	/** */ WithStatement withStatement;
	/** */ SynchronizedStatement synchronizedStatement;
	/** */ TryStatement tryStatement;
	/** */ ThrowStatement throwStatement;
	/** */ ScopeGuardStatement scopeGuardStatement;
	/** */ AsmStatement asmStatement;
	/** */ ForeachRangeStatement foreachRangeStatement;
	/** */ ConditionalStatement conditionalStatement;
	/** */ StaticAssertStatement staticAssertStatement;
	/** */ AssertStatement assertStatement;
	/** */ TemplateMixinStatement templateMixinStatement;
	/** */ VersionSpecification versionSpecification;
	/** */ DebugSpecification debugSpecification;
	/** */ FunctionCallStatement functionCallStatement;
	/** */ DeleteStatement deleteStatement;
}

///
class NonVoidInitializer : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AssignExpression assignExpression;
	/** */ ArrayInitializer arrayInitializer;
	/** */ StructInitializer structInitializer;

}

///
class Operand : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AsmExp asmExp;
}

///
class Operands : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Operand[] operands;
}

///
class OrExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ XorExpression xorExpression;
	/** */ OrExpression orExpression;
}

///
class OrOrExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ OrOrExpression orOrExpression;
	/** */ AndAndExpression andAndExpression;
}

///
class OutStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token parameter;
	/** */ BlockStatement blockStatement;
}

///
class Parameter : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ ParameterAttribute[] paramaterAttributes;
	/** */ Type type;
	/** */ Token name;
	/** */ bool vararg;
}

///
class ParameterAttribute: ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token attribute;
	/** */ TypeConstructor typeConstructor;
}

///
class Parameters : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Parameter[] paramaters;
	/** */ bool hasVarargs;
}

///
class Postblit : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ FunctionBody functionBody;
}

///
class PostIncDecExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token operator;
	/** */ UnaryExpression unaryExpression;
}

///
class PowExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ UnaryExpression unaryExpression;
	/** */ PowExpression powExpression;
}

///
class PragmaDeclaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ PragmaExpression pragmaExpression;
}

///
class PragmaExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ ArgumentList argumentList;
}

///
class PreIncDecExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token operator;
	/** */ UnaryExpression unaryExpression;
}

///
class PrimaryExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token primary;
	/** */ bool hasDot;
    IdentifierOrTemplateInstance identifierOrTemplateInstance;
    Type type;
    TypeofExpression typeofExpression;
    TypeidExpression typeidExpression;
    ArrayLiteral arrayLiteral;
    AssocArrayLiteral assocArrayLiteral;
    Expression expression;
    IsExpression isExpression;
    LambdaExpression lambdaExpression;
    FunctionLiteralExpression functionLiteralExpression;
    TraitsExpression traitsExpression;
    MixinExpression mixinExpression;
    ImportExpression importExpression;
}

///
class Register : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ Token integerLiteral;
	/** */ bool hasIntegerLiteral;
}

///
class RelExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	mixin(SHIFT_SHIFT_BODY);
}

///
class ReturnStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Expression expression;
}

///
class ScopeGuardStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
}

///
class SharedStaticConstructor : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ FunctionBody functionBody;
}

///
class SharedStaticDestructor : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ FunctionBody functionBody;
}

///
class ShiftExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AddExpression addExpression;
	/** */ ShiftExpression shiftExpression;
	/** */ Token operator;
}

///
class SingleImport : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token rename;
	/** */ IdentifierChain import_;
}

///
class SliceExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AssignExpression lower;
	/** */ AssignExpression upper;
}

///
class Statement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ NonEmptyStatement nonEmptyStatement;
}

///
class StatementNoCaseNoDefault : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
}

///
class StaticAssertDeclaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ StaticAssertStatement staticAssertStatement;
}

///
class StaticAssertStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AssertStatement assertStatement;
}

///
class StaticConstructor : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ FunctionBody functionBody;
}

///
class StaticDestructor : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ FunctionBody functionBody;
}

///
class StaticIfCondition : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AssignExpression assignExpression;
}

///
class StorageClass : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AtAttribute atAttribute;
	/** */ TypeConstructor typeConstructor;
	/** */ Token storageClass;
}

///
class StructBody : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ StructBodyItem[] structBodyItems;
}

class StructBodyItem : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Declaration declaration;
	/** */ Invariant invariant_;
	/** */ Postblit postblit;
}

///
class StructDeclaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ TemplateParameters templateParameters;
	/** */ Constraint constraint;
	/** */ StructBody structBody;
}

///
class StructInitializer : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ StructMemberInitializers structMemberInitializers;

}

///
class StructMemberInitializer : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ bool hasIdentifier;
	/** */ NonVoidInitializer nonVoidInitializer;
}

///
class StructMemberInitializers : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ StructMemberInitializer[] structMemberInitializers;
}

///
class SwitchBody : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Statement[] statements;
}

///
class SwitchStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Expression expression;
	/** */ SwitchBody switchBody;
}

///
class Symbol : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ IdentifierOrTemplateChain identifierOrTemplateChain;
	/** */ bool hasDot;
}

///
class SynchronizedStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Expression expression;
	/** */ NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
}

///
class TemplateAliasParameter : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Type type;
	/** */ Token identifier;
	/** */ Type colonType;
	/** */ Expression colonExpression;
	/** */ Type assignType;
	/** */ Expression assignExpression;
}

///
class TemplateArgument : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Type type;
	/** */ AssignExpression assignExpression;
	/** */ Symbol symbol;
}

///
class TemplateArgumentList : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ TemplateArgument[] templateArguments;
}

///
class TemplateArguments : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ TemplateArgumentList templateArgumentList;
	/** */ TemplateSingleArgument templateSingleArgument;
}

///
class TemplateDeclaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ TemplateParameters templateParameters;
	/** */ Constraint constraint;
	/** */ Declaration[] declarations;
}

///
class TemplateInstance : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ TemplateArguments templateArguments;
}

///
class TemplateMixinStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ TemplateArguments templateArguments;
	/** */ MixinTemplateName mixinTemplateName;
}

///
class TemplateParameter : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ TemplateTypeParameter templateTypeParameter;
	/** */ TemplateValueParameter templateValueParameter;
	/** */ TemplateAliasParameter templateAliasParameter;
	/** */ TemplateTupleParameter templateTupleParameter;
	/** */ TemplateThisParameter templateThisParameter;
}

///
class TemplateParameterList : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ TemplateParameter[] templateParameters;
}

///
class TemplateParameters : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ TemplateParameterList templateParameterList;
}

///
class TemplateSingleArgument : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token token;
	/** */ BasicType builtinType;
}

///
class TemplateThisParameter : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ TemplateTypeParameter templateTypeParameter;
}

///
class TemplateTupleParameter : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
}

///
class TemplateTypeParameter : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ Type colonType;
	/** */ Type assignType;
}

///
class TemplateValueParameter : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Type type;
	/** */ Token identifier;
	/** */ Expression expression;
	/** */ TemplateValueParameterDefault templateValueParameterDefault;
}

///
class TemplateValueParameterDefault : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AssignExpression assignExpression;
	/** */ Token token;
}

///
class TernaryExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ OrOrExpression orOrExpression;
	/** */ Expression expression;
	/** */ TernaryExpression ternaryExpression;
}

///
class ThrowStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Expression expression;
}

///
class TraitsArgument : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ AssignExpression assignExpression;
	/** */ Type type;
}

///
class TraitsExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ TraitsArgument[] traitsArguments;
}

///
class TryStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ NonEmptyStatementNoCaseNoDefault nonEmptyStatementNoCaseNoDefault;
	/** */ Catches catches;
	/** */ Finally finally_;
}

///
class Type : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ TypeConstructors typeConstructors;
	/** */ Type2 type2;
}

///
class Type2 : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Type2 type2;
	/** */ Type3 type3;
	/** */ TypeSuffix typeSuffix;
}

///
class Type3 : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ BasicType builtinType;
	/** */ Symbol symbol;
	/** */ TypeofExpression typeofExpression;
	/** */ IdentifierOrTemplateChain identifierOrTemplateChain;
	/** */ TypeConstructor typeConstructor;
	/** */ Type type;
}

///
class TypeConstructor : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token typeConstructor;
}

///
class TypeConstructors : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ TypeConstructor[] typeConstructors;
}

///
class TypeSpecialization : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token token;
	/** */ Type type;
}

///
class TypeSuffix : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token delegateOrFunction;
	/** */ bool star;
	/** */ Type type;
	/** */ AssignExpression assignExpression;
	/** */ Parameters parameters;
	/** */ MemberFunctionAttribute[] memberFunctionAttributes;
}

///
class TypeidExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Type type;
	/** */ Expression expression;
}

///
class TypeofExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Expression expression;
	/** */ Token return_;
}

///
class UnaryExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ PrimaryExpression primaryExpression;
	/** */ UnaryExpression unaryExpression;
	/** */ Token prefix;
	/** */ PreIncDecExpression preIncDecExpression;
	/** */ PostIncDecExpression postIncDecExpression;
	/** */ NewExpression newExpression;
	/** */ DeleteExpression deleteExpression;
	/** */ CastExpression castExpression;
	/** */ FunctionCallExpression functionCallExpression;
	/** */ ArgumentList argumentList;
	/** */ AssignExpression low;
	/** */ AssignExpression high;
	/** */ IdentifierOrTemplateInstance identifierOrTemplateInstance;
	/** */ AssertExpression assertExpression;
}

///
class UnionDeclaration : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token identifier;
	/** */ TemplateParameters templateParameters;
	/** */ Constraint constraint;
	/** */ StructBody structBody;
}

///
class Unittest : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ BlockStatement blockStatement;
}

///
class VariableDeclaration : Declaration
{
public:
	mixin(OVERRIDE_DEFAULT_ACCEPT);
	/** */ Type type;
	/** */ Declarator[] declarators;
	/** */ StorageClass storageClass;
	/** */ AutoDeclaration autoDeclaration;
}

///
class VersionCondition : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token token;
}

///
class VersionSpecification : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Token token;
}

///
class WhileStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Expression expression;
	/** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class WithStatement : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ Expression expression;
	/** */ Symbol symbol;
	/** */ TemplateInstance templateInstance;
}

///
class XorExpression : ASTNode
{
public:
	mixin(DEFAULT_ACCEPT);
	/** */ XorExpression left;
	/** */ AndExpression right;
}
