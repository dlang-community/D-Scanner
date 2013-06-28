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

// TODO: Many of these classes can be simplified by using std.variant.Algebraic

/**
 * Implements the $(LINK2 http://en.wikipedia.org/wiki/Visitor_pattern, Visitor Pattern)
 * for the various AST classes
 */
abstract class ASTVisitor
{
    /** */ void visit(AddExpression addExpression) { addExpression.accept(this); }
    /** */ void visit(AliasDeclaration aliasDeclaration) { aliasDeclaration.accept(this); }
    /** */ void visit(AliasInitializer aliasInitializer) { aliasInitializer.accept(this); }
    /** */ void visit(AliasThisDeclaration aliasThisDeclaration) { aliasThisDeclaration.accept(this); }
    /** */ void visit(AlignAttribute alignAttribute) { alignAttribute.accept(this); }
    /** */ void visit(AndAndExpression andAndExpression) { andAndExpression.accept(this); }
    /** */ void visit(AndExpression andExpression) { andExpression.accept(this); }
    /** */ void visit(ArgumentList argumentList) { argumentList.accept(this); }
    /** */ void visit(Arguments arguments) { arguments.accept(this); }
    /** */ void visit(ArrayInitializer arrayInitializer) { arrayInitializer.accept(this); }
    /** */ void visit(ArrayLiteral arrayLiteral) { arrayLiteral.accept(this); }
    /** */ void visit(ArrayMemberInitialization arrayMemberInitialization) { arrayMemberInitialization.accept(this); }
    /** */ void visit(AsmAddExp asmAddExp) { asmAddExp.accept(this); }
    /** */ void visit(AsmAndExp asmAndExp) { asmAndExp.accept(this); }
    /** */ void visit(AsmBrExp asmBrExp) { asmBrExp.accept(this); }
    /** */ void visit(AsmEqualExp asmEqualExp) { asmEqualExp.accept(this); }
    /** */ void visit(AsmExp asmExp) { asmExp.accept(this); }
    /** */ void visit(AsmInstruction asmInstruction) { asmInstruction.accept(this); }
    /** */ void visit(AsmLogAndExp asmLogAndExp) { asmLogAndExp.accept(this); }
    /** */ void visit(AsmLogOrExp asmLogOrExp) { asmLogOrExp.accept(this); }
    /** */ void visit(AsmMulExp asmMulExp) { asmMulExp.accept(this); }
    /** */ void visit(AsmOrExp asmOrExp) { asmOrExp.accept(this); }
    /** */ void visit(AsmPrimaryExp asmPrimaryExp) { asmPrimaryExp.accept(this); }
    /** */ void visit(AsmRelExp asmRelExp) { asmRelExp.accept(this); }
    /** */ void visit(AsmShiftExp asmShiftExp) { asmShiftExp.accept(this); }
    /** */ void visit(AsmStatement asmStatement) { asmStatement.accept(this); }
    /** */ void visit(AsmTypePrefix asmTypePrefix) { asmTypePrefix.accept(this); }
    /** */ void visit(AsmUnaExp asmUnaExp) { asmUnaExp.accept(this); }
    /** */ void visit(AsmXorExp asmXorExp) { asmXorExp.accept(this); }
    /** */ void visit(AssertExpression assertExpression) { assertExpression.accept(this); }
    /** */ void visit(AssertStatement assertStatement) { assertStatement.accept(this); }
    /** */ void visit(AssignExpression assignExpression) { assignExpression.accept(this); }
    /** */ void visit(AssocArrayLiteral assocArrayLiteral) { assocArrayLiteral.accept(this); }
    /** */ void visit(AtAttribute atAttribute) { atAttribute.accept(this); }
    /** */ void visit(Attribute attribute) { attribute.accept(this); }
    /** */ void visit(AttributedDeclaration attributedDeclaration) { attributedDeclaration.accept(this); }
    /** */ void visit(AutoDeclaration autoDeclaration) { autoDeclaration.accept(this); }
    /** */ void visit(BlockStatement blockStatement) { blockStatement.accept(this); }
    /** */ void visit(BodyStatement bodyStatement) { bodyStatement.accept(this); }
    /** */ void visit(BreakStatement breakStatement) { breakStatement.accept(this); }
    /** */ void visit(BaseClass baseClass) { baseClass.accept(this); }
    /** */ void visit(BaseClassList baseClassList) { baseClassList.accept(this); }
    /** */ void visit(CaseRangeStatement caseRangeStatement) { caseRangeStatement.accept(this); }
    /** */ void visit(CaseStatement caseStatement) { caseStatement.accept(this); }
    /** */ void visit(CastExpression castExpression) { castExpression.accept(this); }
    /** */ void visit(CastQualifier castQualifier) { castQualifier.accept(this); }
    /** */ void visit(Catch catch_) { catch_.accept(this); }
    /** */ void visit(Catches catches) { catches.accept(this); }
    /** */ void visit(ClassBody classBody) { classBody.accept(this); }
    /** */ void visit(ClassDeclaration classDeclaration) { classDeclaration.accept(this); }
    /** */ void visit(CmpExpression cmpExpression) { cmpExpression.accept(this); }
    /** */ void visit(CompileCondition compileCondition) { compileCondition.accept(this); }
    /** */ void visit(ConditionalDeclaration conditionalDeclaration) { conditionalDeclaration.accept(this); }
    /** */ void visit(ConditionalStatement conditionalStatement) { conditionalStatement.accept(this); }
    /** */ void visit(Constraint constraint) { constraint.accept(this); }
    /** */ void visit(Constructor constructor) { constructor.accept(this); }
    /** */ void visit(ContinueStatement continueStatement) { continueStatement.accept(this); }
    /** */ void visit(DebugCondition debugCondition) { debugCondition.accept(this); }
    /** */ void visit(DebugSpecification debugSpecification) { debugSpecification.accept(this); }
    /** */ void visit(Declaration declaration) { declaration.accept(this); }
    /** */ void visit(DeclarationsAndStatements declarationsAndStatements) { declarationsAndStatements.accept(this); }
    /** */ void visit(DeclarationOrInvariant declarationOrInvariant) { declarationOrInvariant.accept(this); }
    /** */ void visit(Declarator declarator) { declarator.accept(this); }
    /** */ void visit(DefaultStatement defaultStatement) { defaultStatement.accept(this); }
    /** */ void visit(DeleteExpression deleteExpression) { deleteExpression.accept(this); }
    /** */ void visit(DeleteStatement deleteStatement) { deleteStatement.accept(this); }
    /** */ void visit(Deprecated deprecated_) { deprecated_.accept(this); }
    /** */ void visit(Destructor destructor) { destructor.accept(this); }
    /** */ void visit(DoStatement doStatement) { doStatement.accept(this); }
    /** */ void visit(EnumBody enumBody) { enumBody.accept(this); }
    /** */ void visit(EnumDeclaration enumDeclaration) { enumDeclaration.accept(this); }
    /** */ void visit(EnumMember enumMember) { enumMember.accept(this); }
    /** */ void visit(EqualExpression equalExpression) { equalExpression.accept(this); }
    /** */ void visit(Expression expression) { expression.accept(this); }
    /** */ void visit(ExpressionStatement expressionStatement) { expressionStatement.accept(this); }
    /** */ void visit(FinalSwitchStatement finalSwitchStatement) { finalSwitchStatement.accept(this); }
    /** */ void visit(Finally finally_) { finally_.accept(this); }
    /** */ void visit(ForStatement forStatement) { forStatement.accept(this); }
    /** */ void visit(ForeachStatement foreachStatement) { foreachStatement.accept(this); }
    /** */ void visit(ForeachType foreachType) { foreachType.accept(this); }
    /** */ void visit(ForeachTypeList foreachTypeList) { foreachTypeList.accept(this); }
    /** */ void visit(FunctionAttribute functionAttribute) { functionAttribute.accept(this); }
    /** */ void visit(FunctionBody functionBody) { functionBody.accept(this); }
    /** */ void visit(FunctionCallExpression functionCallExpression) { functionCallExpression.accept(this); }
    /** */ void visit(FunctionCallStatement functionCallStatement) { functionCallStatement.accept(this); }
    /** */ void visit(FunctionDeclaration functionDeclaration) { functionDeclaration.accept(this); }
    /** */ void visit(FunctionLiteralExpression functionLiteralExpression) { functionLiteralExpression.accept(this); }
    /** */ void visit(GotoStatement gotoStatement) { gotoStatement.accept(this); }
    /** */ void visit(IdentifierChain identifierChain) { identifierChain.accept(this); }
    /** */ void visit(IdentifierList identifierList) { identifierList.accept(this); }
    /** */ void visit(IdentifierOrTemplateChain identifierOrTemplateChain) { identifierOrTemplateChain.accept(this); }
    /** */ void visit(IdentifierOrTemplateInstance identifierOrTemplateInstance) { identifierOrTemplateInstance.accept(this); }
    /** */ void visit(IdentityExpression identityExpression) { identityExpression.accept(this); }
    /** */ void visit(IfStatement ifStatement) { ifStatement.accept(this); }
    /** */ void visit(ImportBind importBind) { importBind.accept(this); }
    /** */ void visit(ImportBindings importBindings) { importBindings.accept(this); }
    /** */ void visit(ImportDeclaration importDeclaration) { importDeclaration.accept(this); }
    /** */ void visit(ImportExpression importExpression) { importExpression.accept(this); }
    /** */ void visit(ImportList importList) { importList.accept(this); }
    /** */ void visit(IndexExpression indexExpression) { indexExpression.accept(this); }
    /** */ void visit(InExpression inExpression) { inExpression.accept(this); }
    /** */ void visit(InStatement inStatement) { inStatement.accept(this); }
    /** */ void visit(Initialize initialize) { initialize.accept(this); }
    /** */ void visit(Initializer initializer) { initializer.accept(this); }
    /** */ void visit(InterfaceDeclaration interfaceDeclaration) { interfaceDeclaration.accept(this); }
    /** */ void visit(Invariant invariant_) { invariant_.accept(this); }
    /** */ void visit(IsExpression isExpression) { isExpression.accept(this); }
    /** */ void visit(KeyValuePair keyValuePair) { keyValuePair.accept(this); }
    /** */ void visit(KeyValuePairs keyValuePairs) { keyValuePairs.accept(this); }
    /** */ void visit(LabeledStatement labeledStatement) { labeledStatement.accept(this); }
    /** */ void visit(LambdaExpression lambdaExpression) { lambdaExpression.accept(this); }
    /** */ void visit(LastCatch lastCatch) { lastCatch.accept(this); }
    /** */ void visit(LinkageAttribute linkageAttribute) { linkageAttribute.accept(this); }
    /** */ void visit(MemberFunctionAttribute memberFunctionAttribute) { memberFunctionAttribute.accept(this); }
    /** */ void visit(MixinDeclaration mixinDeclaration) { mixinDeclaration.accept(this); }
    /** */ void visit(MixinExpression mixinExpression) { mixinExpression.accept(this); }
    /** */ void visit(MixinTemplateName mixinTemplateName) { mixinTemplateName.accept(this); }
    /** */ void visit(Module module_) { module_.accept(this); }
    /** */ void visit(ModuleDeclaration moduleDeclaration) { moduleDeclaration.accept(this); }
    /** */ void visit(MulExpression mulExpression) { mulExpression.accept(this); }
    /** */ void visit(NewAnonClassExpression newAnonClassExpression) { newAnonClassExpression.accept(this); }
    /** */ void visit(NewExpression newExpression) { newExpression.accept(this); }
    /** */ void visit(NonEmptyStatement nonEmptyStatement) { nonEmptyStatement.accept(this); }
    /** */ void visit(NonVoidInitializer nonVoidInitializer) { nonVoidInitializer.accept(this); }
    /** */ void visit(Operand operand) { operand.accept(this); }
    /** */ void visit(Operands operands) { operands.accept(this); }
    /** */ void visit(OrExpression orExpression) { orExpression.accept(this); }
    /** */ void visit(OrOrExpression orOrExpression) { orOrExpression.accept(this); }
    /** */ void visit(OutStatement outStatement) { outStatement.accept(this); }
    /** */ void visit(Parameter parameter) { parameter.accept(this); }
    /** */ void visit(Parameters parameters) { parameters.accept(this); }
    /** */ void visit(Postblit postblit) { postblit.accept(this); }
    /** */ void visit(PostIncDecExpression postIncDecExpression) { postIncDecExpression.accept(this); }
    /** */ void visit(PowExpression powExpression) { powExpression.accept(this); }
    /** */ void visit(PragmaDeclaration pragmaDeclaration) { pragmaDeclaration.accept(this); }
    /** */ void visit(PragmaExpression pragmaExpression) { pragmaExpression.accept(this); }
    /** */ void visit(PreIncDecExpression preIncDecExpression) { preIncDecExpression.accept(this); }
    /** */ void visit(PrimaryExpression primaryExpression) { primaryExpression.accept(this); }
    /** */ void visit(Register register) { register.accept(this); }
    /** */ void visit(RelExpression relExpression) { relExpression.accept(this); }
    /** */ void visit(ReturnStatement returnStatement) { returnStatement.accept(this); }
    /** */ void visit(ScopeGuardStatement scopeGuardStatement) { scopeGuardStatement.accept(this); }
    /** */ void visit(SharedStaticConstructor sharedStaticConstructor) { sharedStaticConstructor.accept(this); }
    /** */ void visit(SharedStaticDestructor sharedStaticDestructor) { sharedStaticDestructor.accept(this); }
    /** */ void visit(ShiftExpression shiftExpression) { shiftExpression.accept(this); }
    /** */ void visit(SingleImport singleImport) { singleImport.accept(this); }
    /** */ void visit(SliceExpression sliceExpression) { sliceExpression.accept(this); }
    /** */ void visit(Statement statement) { statement.accept(this); }
    /** */ void visit(StatementNoCaseNoDefault statementNoCaseNoDefault) { statementNoCaseNoDefault.accept(this); }
    /** */ void visit(StaticAssertDeclaration staticAssertDeclaration) { staticAssertDeclaration.accept(this); }
    /** */ void visit(StaticAssertStatement staticAssertStatement) { staticAssertStatement.accept(this); }
    /** */ void visit(StaticConstructor staticConstructor) { staticConstructor.accept(this); }
    /** */ void visit(StaticDestructor staticDestructor) { staticDestructor.accept(this); }
    /** */ void visit(StaticIfCondition staticIfCondition) { staticIfCondition.accept(this); }
    /** */ void visit(StorageClass storageClass) { storageClass.accept(this); }
    /** */ void visit(StructBody structBody) { structBody.accept(this); }
    /** */ void visit(StructBodyItem structBodyItem) { structBodyItem.accept(this); }
    /** */ void visit(StructDeclaration structDeclaration) { structDeclaration.accept(this); }
    /** */ void visit(StructInitializer structInitializer) { structInitializer.accept(this); }
    /** */ void visit(StructMemberInitializer structMemberInitializer) { structMemberInitializer.accept(this); }
    /** */ void visit(StructMemberInitializers structMemberInitializers) { structMemberInitializers.accept(this); }
    /** */ void visit(SwitchBody switchBody) { switchBody.accept(this); }
    /** */ void visit(SwitchStatement switchStatement) { switchStatement.accept(this); }
    /** */ void visit(Symbol symbol) { symbol.accept(this); }
    /** */ void visit(SynchronizedStatement synchronizedStatement) { synchronizedStatement.accept(this); }
    /** */ void visit(TemplateAliasParameter templateAliasParameter) { templateAliasParameter.accept(this); }
    /** */ void visit(TemplateArgument templateArgument) { templateArgument.accept(this); }
    /** */ void visit(TemplateArgumentList templateArgumentList) { templateArgumentList.accept(this); }
    /** */ void visit(TemplateArguments templateArguments) { templateArguments.accept(this); }
    /** */ void visit(TemplateDeclaration templateDeclaration) { templateDeclaration.accept(this); }
    /** */ void visit(TemplateInstance templateInstance) { templateInstance.accept(this); }
    /** */ void visit(TemplateMixinStatement templateMixinStatement) { templateMixinStatement.accept(this); }
    /** */ void visit(TemplateParameter templateParameter) { templateParameter.accept(this); }
    /** */ void visit(TemplateParameterList templateParameterList) { templateParameterList.accept(this); }
    /** */ void visit(TemplateParameters templateParameters) { templateParameters.accept(this); }
    /** */ void visit(TemplateSingleArgument templateSingleArgument) { templateSingleArgument.accept(this); }
    /** */ void visit(TemplateThisParameter templateThisParameter) { templateThisParameter.accept(this); }
    /** */ void visit(TemplateTupleParameter templateTupleParameter) { templateTupleParameter.accept(this); }
    /** */ void visit(TemplateTypeParameter templateTypeParameter) { templateTypeParameter.accept(this); }
    /** */ void visit(TemplateValueParameter templateValueParameter) { templateValueParameter.accept(this); }
    /** */ void visit(TemplateValueParameterDefault templateValueParameterDefault) { templateValueParameterDefault.accept(this); }
    /** */ void visit(TernaryExpression ternaryExpression) { ternaryExpression.accept(this); }
    /** */ void visit(ThrowStatement throwStatement) { throwStatement.accept(this); }
    /** */ void visit(TraitsExpression traitsExpression) { traitsExpression.accept(this); }
    /** */ void visit(TryStatement tryStatement) { tryStatement.accept(this); }
    /** */ void visit(Type type) { type.accept(this); }
    /** */ void visit(Type2 type2) { type2.accept(this); }
    /** */ void visit(TypeSpecialization typeSpecialization) { typeSpecialization.accept(this); }
    /** */ void visit(TypeSuffix typeSuffix) { typeSuffix.accept(this); }
    /** */ void visit(TypeidExpression typeidExpression) { typeidExpression.accept(this); }
    /** */ void visit(TypeofExpression typeofExpression) { typeofExpression.accept(this); }
    /** */ void visit(UnaryExpression unaryExpression) { unaryExpression.accept(this); }
    /** */ void visit(UnionDeclaration unionDeclaration) { unionDeclaration.accept(this); }
    /** */ void visit(Unittest unittest_) { unittest_.accept(this); }
    /** */ void visit(VariableDeclaration variableDeclaration) { variableDeclaration.accept(this); }
    /** */ void visit(Vector vector) { vector.accept(this); }
    /** */ void visit(VersionCondition versionCondition) { versionCondition.accept(this); }
    /** */ void visit(VersionSpecification versionSpecification) { versionSpecification.accept(this); }
    /** */ void visit(WhileStatement whileStatement) { whileStatement.accept(this); }
    /** */ void visit(WithStatement withStatement) { withStatement.accept(this); }
    /** */ void visit(XorExpression xorExpression) { xorExpression.accept(this); }
}

interface ASTNode
{
    /** */ void accept(ASTVisitor visitor);
}

immutable string DEFAULT_ACCEPT = q{void accept(ASTVisitor visitor) {}};

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
    /** */ TokenType operator;
    /** */ AddExpression left;
    /** */ MulExpression right;
}

///
class AliasDeclaration : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Type type;
    /** */ Declarator declarator;
    /** */ AliasInitializer[] initializers;
}

///
class AliasInitializer : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ Type type;
}

///
class AliasThisDeclaration : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Token identifier;
}

///
class AlignAttribute : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Token intLiteral;
}

///
class AndAndExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AndAndExpression left;
    /** */ OrExpression right;
}

///
class AndExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AndExpression left;
    /** */ CmpExpression right;
}

///
class ArgumentList : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AssignExpression[] items;
}

///
class Arguments : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ ArgumentList argumentList;
}

///
class ArrayInitializer : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ ArrayMemberInitialization[] arrayMemberInitializations;
}

///
class ArrayLiteral : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ ArgumentList argumentList;
}

///
class ArrayMemberInitialization : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AssignExpression assignExpression;
    /** */ NonVoidInitializer nonVoidInitializer;
}

///
class AsmAddExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ TokenType operator;
    /** */ AsmAddExp left;
    /** */ AsmMulExp right;
}

///
class AsmAndExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AsmEqualExp left;
    /** */ AsmEqualExp right;
}

///
class AsmBrExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AsmBrExp asmBrExp;
    /** */ AsmEqualExp asmEqualExp;
    /** */ AsmUnaExp asmUnaExp;
}

///
class AsmEqualExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AsmRelExp left;
    /** */ AsmRelExp right;
    /** */ Token operator;
}

///
class AsmExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AsmLogOrExp left;
    /** */ AsmExp middle;
    /** */ AsmExp right;
}

///
class AsmInstruction : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Token identifierOrIntegerOrOpcode;
    /** */ bool hasAlign;
    /** */ AsmExp asmExp;
    /** */ Operands operands;
}

///
class AsmLogAndExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AsmOrExp left;
    /** */ AsmOrExp right;
}

///
class AsmLogOrExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AsmLogAndExp left;
    /** */ AsmLogAndExp right;
}

///
class AsmMulExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ TokenType operator;
    /** */ AsmMulExp left;
    /** */ AsmBrExp right;

}

///
class AsmOrExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AsmXorExp left;
    /** */ AsmXorExp right;
}

///
class AsmPrimaryExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ IdentifierChain identifierChain;
    /** */ Register register;
    /** */ Token token;
}

///
class AsmRelExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AsmShiftExp left;
    /** */ AsmShiftExp right;
    /** */ Token operator;
}

///
class AsmShiftExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AsmAddExp left;
    /** */ AsmAddExp right;
    /** */ Token operator;
}

///
class AsmStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AsmInstruction[] asmInstructions;
}

///
class AsmTypePrefix : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Token left;
    /** */ Token right;
}

///
class AsmUnaExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AsmTypePrefix asmTypePrefix;
    /** */ AsmExp asmExp;
    /** */ Token prefix;
    /** */ AsmPrimaryExp asmPrimaryExp;
    /** */ AsmUnaExp asmUnaExp;
}

///
class AsmXorExp : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AsmAndExp left;
    /** */ AsmAndExp right;
}

///
class AssertExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AssignExpression assertion;
    /** */ AssignExpression message;
}

///
class AssertStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AssertExpression assertExpression;
}

///
class AssignExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ TernaryExpression ternaryExpression;
    /** */ AssignExpression assignExpression;
    /** */ TokenType operator;
}

///
class AssocArrayLiteral : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ KeyValuePairs keyValuePairs;
}

///
class AtAttribute : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ FunctionCallExpression functionCallExpression;
    /** */ ArgumentList argumentList;
    /** */ Token identifier;
}

///
class AttributedDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        if (attribute !is null)
            visitor.visit(attribute);
        foreach (dec; declarations)
        {
            if (dec !is null)
                visitor.visit(dec);
        }
    }
    /** */ Attribute attribute;
    /** */ Declaration[] declarations;
}

///
class Attribute : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ LinkageAttribute linkageAttribute;
    /** */ AlignAttribute alignAttribute;
    /** */ PragmaExpression pragmaExpression;
    /** */ Deprecated deprecated_;
    /** */ AtAttribute atAttribute;
    /** */ TokenType attribute;
}

///
class AutoDeclaration : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ StorageClass storageClass;
    /** */ Token[] identifiers;
    /** */ Initializer[] initializers;
}

///
class BlockStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ DeclarationsAndStatements declarationsAndStatements;
}

///
class BodyStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ BlockStatement blockStatement;
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
    /** */ BaseClass[] items;
}

///
class CaseRangeStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AssignExpression low;
    /** */ AssignExpression high;
    /** */ DeclarationsAndStatements declarationsAndStatements;
}

///
class CaseStatement: ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ ArgumentList argumentList;
    /** */ DeclarationsAndStatements declarationsAndStatements;
}

///
class CastExpression: ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Type type;
    /** */ CastQualifier castQualifier;
    /** */ UnaryExpression unaryExpression;
}

///
class CastQualifier: ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ TokenType first;
    /** */ TokenType second;
    /** */ bool hasSecond;
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
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
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
    /** */ VersionCondition versionCondition;
    /** */ DebugCondition debugCondition;
    /** */ StaticIfCondition staticIfCondition;
}

///
class ConditionalDeclaration : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ CompileCondition compileCondition;
    /** */ Declaration[] trueDeclarations;
    /** */ Declaration[] falseDeclarations;
}

///
class ConditionalStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ CompileCondition compileCondition;
    /** */ StatementNoCaseNoDefault trueStatement;
    /** */ StatementNoCaseNoDefault falseStatement;
}

///
class Constraint : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Expression expression;
}

///
class Constructor : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Parameters parameters;
    /** */ FunctionBody functionBody;
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

    override void accept(ASTVisitor visitor)
    {
        if (attributedDeclaration !is null) visitor.visit(attributedDeclaration);
        if (importDeclaration !is null) visitor.visit(importDeclaration);
        if (functionDeclaration !is null) visitor.visit(functionDeclaration);
        if (variableDeclaration !is null) visitor.visit(variableDeclaration);
        if (aliasThisDeclaration !is null) visitor.visit(aliasThisDeclaration);
        if (structDeclaration !is null) visitor.visit(structDeclaration);
        if (classDeclaration !is null) visitor.visit(classDeclaration);
        if (interfaceDeclaration !is null) visitor.visit(interfaceDeclaration);
        if (unionDeclaration !is null) visitor.visit(unionDeclaration);
        if (enumDeclaration !is null) visitor.visit(enumDeclaration);
        if (aliasDeclaration !is null) visitor.visit(aliasDeclaration);
        if (mixinDeclaration !is null) visitor.visit(mixinDeclaration);
        if (unittest_ !is null) visitor.visit(unittest_);
        if (staticAssertDeclaration !is null) visitor.visit(staticAssertDeclaration);
        if (templateDeclaration !is null) visitor.visit(templateDeclaration);
        if (constructor !is null) visitor.visit(constructor);
        if (destructor !is null) visitor.visit(destructor);
        if (staticConstructor !is null) visitor.visit(staticConstructor);
        if (staticDestructor !is null) visitor.visit(staticDestructor);
        if (sharedStaticDestructor !is null) visitor.visit(sharedStaticDestructor);
        if (sharedStaticConstructor !is null) visitor.visit(sharedStaticConstructor);
        if (conditionalDeclaration !is null) visitor.visit(conditionalDeclaration);
        if (pragmaDeclaration !is null) visitor.visit(pragmaDeclaration);
        if (versionSpecification !is null) visitor.visit(versionSpecification);
    }

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
    /** */ VersionSpecification versionSpecification;
}

///
class DeclarationsAndStatements : ASTNode
{
    mixin(DEFAULT_ACCEPT);
    /** */ DeclarationOrStatement[] declarationsAndStatements;
}

///
class DeclarationOrInvariant : ASTNode
{
    mixin(DEFAULT_ACCEPT);
    /** */ Declaration declaration;
    /** */ Invariant invariant_;
}

///
class DeclarationOrStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Declaration declaration;
    /** */ StatementNoCaseNoDefault statement;
}

///
class Declarator : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ Initializer initializer;
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
    /** */ TokenType operator;
    /** */ ShiftExpression left;
    /** */ ShiftExpression right;
}

///
class Expression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ AssignExpression[] items;
}

///
class ExpressionStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Expression expression;
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
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
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
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class ForeachStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ TokenType foreachType;
    /** */ ForeachTypeList foreachTypeList;
    /** */ Expression low;
    /** */ Expression high;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class ForeachType : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ TokenType[] typeConstructors;
    /** */ Type type;
    /** */ Token identifier;
}

///
class ForeachTypeList : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ ForeachType[] items;
}

///
class FunctionAttribute : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Token token;
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
class FunctionDeclaration : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ bool hasAuto;
    /** */ bool hasRef;
    /** */ Type returnType;
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Parameters parameters;
    /** */ Constraint constraint;
    /** */ FunctionBody functionBody;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
}

///
class FunctionLiteralExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ TokenType functionOrDelegate;
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
    /** */ Token token;
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
    /** */ IdentifierOrTemplateInstance[] identifierOrTemplateInstances;
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
    /** */ bool negated;
    /** */ ShiftExpression left;
    /** */ ShiftExpression right;
}

///
class IfStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Expression expression;
    /** */ StatementNoCaseNoDefault thenStatement;
    /** */ StatementNoCaseNoDefault elseStatement;
}

///
class ImportBind : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Token left;
    /** */ Token right;
    /** */ bool hasRight;
}

///
class ImportBindings : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ SingleImport singleImport;
    /** */ ImportBind[] importBinds;
}

///
class ImportDeclaration : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ SingleImport[] singleImports;
    /** */ ImportBindings importBindings;
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
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
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
    /** */ BaseClassList baseClassList;
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
    /** */ AssignExpression key;
    /** */ AssignExpression value;
}

///
class KeyValuePairs : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ KeyValuePair[] keyValuePairs;
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
    /** */ Parameters parameters;
    /** */ FunctionAttribute[] functionAttributes;
    /** */ AssignExpression assignExpression;
}

///
class LastCatch : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
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
    override void accept(ASTVisitor visitor)
    {
        if (moduleDeclaration !is null)
            visitor.visit(moduleDeclaration);
        foreach(d; declarations)
            visitor.visit(d);
    }
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
    /** */ TokenType operator;
    /** */ MulExpression left;
    /** */ UnaryExpression right;
}

///
class NewAnonClassExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Arguments allocatorArguments;
    /** */ Arguments constructorArguments;
    /** */ BaseClassList baseClassList;
    /** */ ClassBody classBody;
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
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    /** */ CaseStatement caseStatement;
    /** */ CaseRangeStatement caseRangeStatement;
    /** */ DefaultStatement defaultStatement;
}

///
class StatementNoCaseNoDefault : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ LabeledStatement labeledStatement;
    /** */ BlockStatement blockStatement;
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
    /** */ ConditionalStatement conditionalStatement;
    /** */ StaticAssertStatement staticAssertStatement;
    /** */ TemplateMixinStatement templateMixinStatement;
    /** */ VersionSpecification versionSpecification;
    /** */ DebugSpecification debugSpecification;
    /** */ FunctionCallStatement functionCallStatement;
    /** */ ExpressionStatement expressionStatement;
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
    /** */ OrExpression left;
    /** */ XorExpression right;
}

///
class OrOrExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ OrOrExpression left;
    /** */ AndAndExpression right;
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
    /** */ TokenType[] parameterAttributes;
    /** */ Type type;
    /** */ Token name;
    /** */ bool vararg;
    /** */ AssignExpression default_;
}

///
class Parameters : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Parameter[] parameters;
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
    /** */ PowExpression left;
    /** */ UnaryExpression right;
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
    /** */ Token identifier;
    /** */ IdentifierOrTemplateInstance identifierOrTemplateInstance;
    /** */ TokenType basicType;
    /** */ TypeofExpression typeofExpression;
    /** */ TypeidExpression typeidExpression;
    /** */ ArrayLiteral arrayLiteral;
    /** */ AssocArrayLiteral assocArrayLiteral;
    /** */ Expression expression;
    /** */ IsExpression isExpression;
    /** */ LambdaExpression lambdaExpression;
    /** */ FunctionLiteralExpression functionLiteralExpression;
    /** */ TraitsExpression traitsExpression;
    /** */ MixinExpression mixinExpression;
    /** */ ImportExpression importExpression;
    /** */ Vector vector;
}

///
class Register : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ Token intLiteral;
    /** */ bool hasIntegerLiteral;
}

///
class RelExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ TokenType operator;
    /** */ RelExpression left;
    /** */ ShiftExpression right;
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
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
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
    /** */ TokenType operator;
    /** */ ShiftExpression left;
    /** */ AddExpression right;
}

///
class SingleImport : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ IdentifierChain identifierChain;
}

///
class SliceExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ UnaryExpression unaryExpression;
    /** */ AssignExpression lower;
    /** */ AssignExpression upper;
}

///
class Statement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    /** */ CaseStatement caseStatement;
    /** */ CaseRangeStatement caseRangeStatement;
    /** */ DefaultStatement defaultStatement;
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
    /** */ Token token;
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
    /** */ Token name;
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
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
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
    /** */ TemplateArgument[] items;
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
    /** */ TemplateParameter[] items;
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
class TraitsExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ TemplateArgumentList templateArgumentList;
}

///
class TryStatement : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    /** */ Catches catches;
    /** */ Finally finally_;
}

///
class Type : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ TokenType[] typeConstructors;
    /** */ TypeSuffix[] typeSuffixes;
    /** */ Type2 type2;
}

///
class Type2 : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Token basicType;
    /** */ Symbol symbol;
    /** */ TypeofExpression typeofExpression;
    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ TokenType typeConstructor;
    /** */ Type type;
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
    /** */ bool array;
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
    /** */ Token prefix;
    /** */ Token suffix;
    /** */ UnaryExpression unaryExpression;
    /** */ NewExpression newExpression;
    /** */ DeleteExpression deleteExpression;
    /** */ CastExpression castExpression;
    /** */ FunctionCallExpression functionCallExpression;
    /** */ ArgumentList argumentList;
    /** */ IdentifierOrTemplateInstance identifierOrTemplateInstance;
    /** */ AssertExpression assertExpression;
    /** */ SliceExpression sliceExpression;
    /** */ IndexExpression indexExpression;
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
class VariableDeclaration : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Type type;
    /** */ Declarator[] declarators;
    /** */ StorageClass storageClass;
    /** */ AutoDeclaration autoDeclaration;
}

///
class Vector : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ Type type;
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
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class XorExpression : ASTNode
{
public:
    mixin(DEFAULT_ACCEPT);
    /** */ XorExpression left;
    /** */ AndExpression right;
}
