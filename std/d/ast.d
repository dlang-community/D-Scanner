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
import std.traits;

// TODO: Many of these classes can be simplified by using std.variant.Algebraic

/**
 * Implements the $(LINK2 http://en.wikipedia.org/wiki/Visitor_pattern, Visitor Pattern)
 * for the various AST classes
 */
abstract class ASTVisitor
{
public:
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
    /** */ void visit(AssignExpression assignExpression) { assignExpression.accept(this); }
    /** */ void visit(AssocArrayLiteral assocArrayLiteral) { assocArrayLiteral.accept(this); }
    /** */ void visit(AtAttribute atAttribute) { atAttribute.accept(this); }
    /** */ void visit(Attribute attribute) { attribute.accept(this); }
    /** */ void visit(AttributeDeclaration attributeDeclaration) { attributeDeclaration.accept(this); }
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
    /** */ void visit(DeclarationOrStatement declarationsOrStatement) { declarationsOrStatement.accept(this); }
    /** */ void visit(DeclarationsAndStatements declarationsAndStatements) { declarationsAndStatements.accept(this); }
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
    /** */ void visit(ExpressionNode expressionNode) { expressionNode.accept(this); }
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
    /** */ void visit(MixinTemplateDeclaration mixinTemplateDeclaration) { mixinTemplateDeclaration.accept(this); }
    /** */ void visit(MixinTemplateName mixinTemplateName) { mixinTemplateName.accept(this); }
    /** */ void visit(Module module_) { module_.accept(this); }
    /** */ void visit(ModuleDeclaration moduleDeclaration) { moduleDeclaration.accept(this); }
    /** */ void visit(MulExpression mulExpression) { mulExpression.accept(this); }
    /** */ void visit(NewAnonClassExpression newAnonClassExpression) { newAnonClassExpression.accept(this); }
    /** */ void visit(NewExpression newExpression) { newExpression.accept(this); }
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
    /** */ void visit(TemplateMixinExpression templateMixinExpression) { templateMixinExpression.accept(this); }
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
    /** */ void visit(Token token) { }
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

template visitIfNotNull(fields ...)
{
	static if (fields.length > 1)
		immutable visitIfNotNull = visitIfNotNull!(fields[0]) ~ visitIfNotNull!(fields[1..$]);
	else
    {
        static if (typeof(fields[0]).stringof[$ - 2 .. $] == "[]")
        {
            static if (__traits(hasMember, typeof(fields[0]), "classinfo"))
                immutable visitIfNotNull = "foreach (i; " ~ fields[0].stringof ~ ") if (i !is null) visitor.visit(i);\n";
            else
                immutable visitIfNotNull = "foreach (i; " ~ fields[0].stringof ~ ") visitor.visit(i);\n";
        }
        else static if (__traits(hasMember, typeof(fields[0]), "classinfo"))
            immutable visitIfNotNull = "if (" ~ fields[0].stringof ~ " !is null) visitor.visit(" ~ fields[0].stringof ~ ");\n";
        else
            immutable visitIfNotNull = "visitor.visit(" ~ fields[0].stringof ~ ");\n";
    }
}

abstract class ExpressionNode : ASTNode {}

mixin template BinaryExpressionBody()
{
    ExpressionNode left;
    ExpressionNode right;
}

///
class AddExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TokenType operator;
    mixin BinaryExpressionBody;
}

///
class AliasDeclaration : ASTNode
{
public:
    void accept(ASTVisitor visitor)
	{
		//mixin (visitIfNotNull!(type, declarator, initializers));
	}
    /** */ Type type;
    /** */ Declarator declarator;
    /** */ AliasInitializer[] initializers;
}

///
class AliasInitializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(identifier, type));
	}

    /** */ Token identifier;
    /** */ Type type;
}

///
class AliasThisDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(identifier));
	}
    /** */ Token identifier;
}

///
class AlignAttribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(intLiteral));
	}
    /** */ Token intLiteral;
}

///
class AndAndExpression : ExpressionNode
{
public:
    void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(left, right));
	}
    mixin BinaryExpressionBody;
}

///
class AndExpression : ExpressionNode
{
public:
    void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(left, right));
	}
    mixin BinaryExpressionBody;
}

///
class ArgumentList : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(items));
	}
    /** */ AssignExpression[] items;
}

///
class Arguments : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(argumentList));
	}
    /** */ ArgumentList argumentList;
}

///
class ArrayInitializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(arrayMemberInitializations));
	}
    /** */ ArrayMemberInitialization[] arrayMemberInitializations;
}

///
class ArrayLiteral : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(argumentList));
	}
    /** */ ArgumentList argumentList;
}

///
class ArrayMemberInitialization : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(assignExpression, nonVoidInitializer));
	}
    /** */ AssignExpression assignExpression;
    /** */ NonVoidInitializer nonVoidInitializer;
}

///
class AsmAddExp : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TokenType operator;
    mixin BinaryExpressionBody;
}

///
class AsmAndExp : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    mixin BinaryExpressionBody;
}

///
class AsmBrExp : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ AsmBrExp asmBrExp;
    /** */ AsmEqualExp asmEqualExp;
    /** */ AsmUnaExp asmUnaExp;
}

///
class AsmEqualExp : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    mixin BinaryExpressionBody;
    /** */ Token operator;
}

///
class AsmExp : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ AsmLogOrExp left;
    /** */ AsmExp middle;
    /** */ AsmExp right;
}

///
class AsmInstruction : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifierOrIntegerOrOpcode;
    /** */ bool hasAlign;
    /** */ AsmExp asmExp;
    /** */ Operands operands;
}

///
class AsmLogAndExp : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    mixin BinaryExpressionBody;
}

///
class AsmLogOrExp : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    mixin BinaryExpressionBody;
}

///
class AsmMulExp : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TokenType operator;
    mixin BinaryExpressionBody;

}

///
class AsmOrExp : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    mixin BinaryExpressionBody;
}

///
class AsmPrimaryExp : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ IdentifierChain identifierChain;
    /** */ Register register;
    /** */ Token token;
}

///
class AsmRelExp : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    mixin BinaryExpressionBody;
    /** */ Token operator;
}

///
class AsmShiftExp : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    mixin BinaryExpressionBody;
    /** */ Token operator;
}

///
class AsmStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ AsmInstruction[] asmInstructions;
}

///
class AsmTypePrefix : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token left;
    /** */ Token right;
}

///
class AsmUnaExp : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
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
    mixin (DEFAULT_ACCEPT);
    mixin BinaryExpressionBody;
}

///
class AssertExpression : ExpressionNode
{
public:
    /+override+/ void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(assertion, message));
    }
    /** */ AssignExpression assertion;
    /** */ AssignExpression message;
}

///
class AssignExpression : ExpressionNode
{
public:
    /+override+/ void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(ternaryExpression, assignExpression));
    }
    /** */ ExpressionNode ternaryExpression;
    /** */ ExpressionNode assignExpression;
    /** */ TokenType operator;
}

///
class AssocArrayLiteral : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(keyValuePairs));
    }
    /** */ KeyValuePairs keyValuePairs;
}

///
class AtAttribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(functionCallExpression, argumentList));
    }
    /** */ FunctionCallExpression functionCallExpression;
    /** */ ArgumentList argumentList;
    /** */ Token identifier;
}

///
class Attribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(linkageAttribute, alignAttribute,
            pragmaExpression, storageClass));
    }
    /** */ LinkageAttribute linkageAttribute;
    /** */ AlignAttribute alignAttribute;
    /** */ PragmaExpression pragmaExpression;
    /** */ StorageClass storageClass;
    /** */ TokenType attribute;
}

///
class AttributeDeclaration : ASTNode
{
	override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(attribute));
    }
	/** */ Attribute attribute;
}

///
class AutoDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        foreach (i; 0 .. initializers.length)
        {
            visitor.visit(initializers[i]);
        }
    }
    /** */ Token[] identifiers;
    /** */ Initializer[] initializers;
}

///
class BlockStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(declarationsAndStatements));
	}

    /**
     * Byte position of the opening brace
     */
    size_t startLocation;

    /**
     * Byte position of the closing brace
     */
    size_t endLocation;

    /** */ DeclarationsAndStatements declarationsAndStatements;
}

///
class BodyStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(blockStatement));
	}
    /** */ BlockStatement blockStatement;
}

///
class BreakStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token label;
}

///
class BaseClass : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(identifierOrTemplateChain, typeofExpression));
	}
    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ TypeofExpression typeofExpression;
}

///
class BaseClassList : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(items));
	}
    /** */ BaseClass[] items;
}

///
class CaseRangeStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(low, high, declarationsAndStatements));
	}
    /** */ AssignExpression low;
    /** */ AssignExpression high;
    /** */ DeclarationsAndStatements declarationsAndStatements;
}

///
class CaseStatement: ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(argumentList, declarationsAndStatements));
	}
    /** */ ArgumentList argumentList;
    /** */ DeclarationsAndStatements declarationsAndStatements;
}

///
class CastExpression: ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(type, castQualifier, unaryExpression));
	}
    /** */ Type type;
    /** */ CastQualifier castQualifier;
    /** */ UnaryExpression unaryExpression;
}

///
class CastQualifier: ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TokenType first;
    /** */ TokenType second;
    /** */ bool hasSecond;
}

///
class Catches: ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(catches, lastCatch));
	}
    /** */ Catch[] catches;
    /** */ LastCatch lastCatch;
}

///
class Catch: ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(type, identifier, declarationOrStatement));
	}
    /** */ Type type;
    /** */ Token identifier;
    /** */ DeclarationOrStatement declarationOrStatement;
}

///
class ClassDeclaration: ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(templateParameters, constraint, baseClassList,
            structBody));
    }

    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ BaseClassList baseClassList;
    /** */ StructBody structBody;
}

///
class CmpExpression : ExpressionNode
{
public:
    /+override+/ void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(shiftExpression, equalExpression,
            identityExpression, relExpression, inExpression));
	}
    /** */ ExpressionNode shiftExpression;
    /** */ ExpressionNode equalExpression;
    /** */ ExpressionNode identityExpression;
    /** */ ExpressionNode relExpression;
    /** */ ExpressionNode inExpression;
}

///
class CompileCondition : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(versionCondition, debugCondition, staticIfCondition));
	}
    /** */ VersionCondition versionCondition;
    /** */ DebugCondition debugCondition;
    /** */ StaticIfCondition staticIfCondition;
}

///
class ConditionalDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(compileCondition, trueDeclaration, falseDeclaration));
	}
    /** */ CompileCondition compileCondition;
    /** */ Declaration trueDeclaration;
    /** */ Declaration falseDeclaration;
}

///
class ConditionalStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(compileCondition, trueStatement, falseStatement));
	}
    /** */ CompileCondition compileCondition;
    /** */ DeclarationOrStatement trueStatement;
    /** */ DeclarationOrStatement falseStatement;
}

///
class Constraint : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(expression));
	}
    /** */ Expression expression;
}

///
class Constructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(parameters, templateParameters, constraint,
            memberFunctionAttributes, functionBody));
    }
    /** */ Parameters parameters;
    /** */ FunctionBody functionBody;
    /** */ Constraint constraint;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    /** */ TemplateParameters templateParameters;
}

///
class ContinueStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token label;
}

///
class DebugCondition : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifierOrInteger;
}

///
class DebugSpecification : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifierOrInteger;
}

///
class Declaration : ASTNode
{
public:

    override void accept(ASTVisitor visitor)
    {
		mixin (visitIfNotNull!(attributes, attributeDeclaration,
            importDeclaration, functionDeclaration,
			variableDeclaration, aliasThisDeclaration, structDeclaration,
			classDeclaration, interfaceDeclaration, unionDeclaration,
			enumDeclaration, aliasDeclaration, mixinDeclaration,
			mixinTemplateDeclaration, unittest_, staticAssertDeclaration,
			templateDeclaration, constructor,
			destructor, staticConstructor, staticDestructor,
			sharedStaticDestructor, sharedStaticConstructor,
			conditionalDeclaration, pragmaDeclaration, versionSpecification));
    }

    /** */ Attribute[] attributes;
	/** */ AttributeDeclaration attributeDeclaration;
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
    /** */ MixinTemplateDeclaration mixinTemplateDeclaration;
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
	/** */ Invariant invariant_;
	/** */ Postblit postblit;
	/** */ Declaration[] declarations;
}

///
class DeclarationsAndStatements : ASTNode
{
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(declarationsAndStatements));
	}

    /** */ DeclarationOrStatement[] declarationsAndStatements;
}

///
class DeclarationOrStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(declaration, statement));
	}

    /** */ Declaration declaration;
    /** */ Statement statement;
}

///
class Declarator : ASTNode
{
public:
	override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(initializer));
	}
    /** */ Token name;
    /** */ Initializer initializer;
}

///
class DefaultStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(declarationsAndStatements));
	}
    /** */ DeclarationsAndStatements declarationsAndStatements;
}

///
class DeleteExpression : ExpressionNode
{
public:
    /+override+/ void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(unaryExpression));
	}
    /** */ UnaryExpression unaryExpression;
}

///
class DeleteStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(deleteExpression));
	}
    /** */ DeleteExpression deleteExpression;
}

///
class Deprecated : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(assignExpression));
	}
    /** */ AssignExpression assignExpression;
}

///
class Destructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(functionBody));
	}
    /** */ FunctionBody functionBody;
}

///
class DoStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(expression, statementNoCaseNoDefault));
	}
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    /** */ Expression expression;
}

///
class EnumBody : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(enumMembers));
	}
    /** */ EnumMember[] enumMembers;
}

///
class EnumDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(type, enumBody));
	}
    /** */ Token name;
    /** */ Type type;
    /** */ EnumBody enumBody;
}

///
class EnumMember : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(name, type, assignExpression));
	}
    /** */ Token name;
    /** */ Type type;
    /** */ AssignExpression assignExpression;
}

///
class EqualExpression : ExpressionNode
{
public:
    /+override+/ void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(left, right));
	}
    /** */ TokenType operator;
    mixin BinaryExpressionBody;
}

///
class Expression : ExpressionNode
{
public:
    /+override+/ void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(items));
	}
    /** */ AssignExpression[] items;
}

///
class ExpressionStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(expression));
	}
    /** */ Expression expression;
}

///
class FinalSwitchStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(switchStatement));
	}
    /** */ SwitchStatement switchStatement;
}

///
class Finally : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(declarationOrStatement));
	}
    /** */ DeclarationOrStatement declarationOrStatement;
}

///
class ForStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(declarationOrStatement, test, increment,
            statementNoCaseNoDefault));
	}
    /** */ DeclarationOrStatement declarationOrStatement;
    /** */ ExpressionStatement test;
    /** */ Expression increment;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class ForeachStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
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
    mixin (DEFAULT_ACCEPT);
    /** */ TokenType[] typeConstructors;
    /** */ Type type;
    /** */ Token identifier;
}

///
class ForeachTypeList : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ ForeachType[] items;
}

///
class FunctionAttribute : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token token;
    /** */ AtAttribute atAttribute;
}

///
class FunctionBody : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(inStatement, outStatement, bodyStatement,
            blockStatement));
	}

    /** */ BlockStatement blockStatement;
    /** */ BodyStatement bodyStatement;
    /** */ OutStatement outStatement;
    /** */ InStatement inStatement;
}

///
class FunctionCallExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ UnaryExpression unaryExpression;
    /** */ TemplateArguments templateArguments;
    /** */ Arguments arguments;
}

///
class FunctionCallStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ FunctionCallExpression functionCallExpression;
}

///
class FunctionDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(memberFunctionAttributes, returnType, parameters,
            templateParameters, constraint, functionBody));
	}

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
class FunctionLiteralExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
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
    mixin (DEFAULT_ACCEPT);
    /** */ Expression expression;
    /** */ Token token;
}

///
class IdentifierChain : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifiers));
    }
    /** */ Token[] identifiers;
}

///
class IdentifierList : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifiers));
    }
    /** */ Token[] identifiers;
}

///
class IdentifierOrTemplateChain : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);

    override string toString()
    {
        string rVal;
        bool first = true;
        foreach (iot; identifierOrTemplateInstances)
        {
            if (!first)
                rVal ~= ".";
            first = true;
            rVal ~= iot.toString();
        }
        return rVal;
    }
    /** */ IdentifierOrTemplateInstance[] identifierOrTemplateInstances;
}

///
class IdentifierOrTemplateInstance : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);

    override string toString()
    {
        if (identifier.type == TokenType.identifier)
            return identifier.value;
        else
            return templateInstance.toString();
    }
    /** */ Token identifier;
    /** */ TemplateInstance templateInstance;
}

///
class IdentityExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ bool negated;
    mixin BinaryExpressionBody;
}

///
class IfStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ Type type;
    /** */ Expression expression;
    /** */ DeclarationOrStatement thenStatement;
    /** */ DeclarationOrStatement elseStatement;
}

///
class ImportBind : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token left;
    /** */ Token right;
    /** */ bool hasRight;
}

///
class ImportBindings : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ SingleImport singleImport;
    /** */ ImportBind[] importBinds;
}

///
class ImportDeclaration : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ SingleImport[] singleImports;
    /** */ ImportBindings importBindings;
}

///
class ImportExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ AssignExpression assignExpression;
}

///
class ImportList : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ SingleImport singleImport;
    /** */ ImportList next;
    /** */ ImportBindings bindings;
}

///
class IndexExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ UnaryExpression unaryExpression;
    /** */ ArgumentList argumentList;
}

///
class InExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token operator;
    mixin BinaryExpressionBody;
}

///
class InStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ BlockStatement blockStatement;
}

///
class Initialize : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class Initializer : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ NonVoidInitializer nonVoidInitializer;
}

///
class InterfaceDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(templateParameters, constraint, baseClassList,
            structBody));
    }
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ BaseClassList baseClassList;
    /** */ StructBody structBody;
}

///
class Invariant : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ BlockStatement blockStatement;
}

///
class IsExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Type type;
    /** */ AssignExpression assignExpression;
    /** */ Token identifier;
    /** */ TypeSpecialization typeSpecialization;
    /** */ TemplateParameterList templateParameterList;
    /** */ TokenType equalsOrColon;
}

///
class KeyValuePair : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ AssignExpression key;
    /** */ AssignExpression value;
}

///
class KeyValuePairs : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ KeyValuePair[] keyValuePairs;
}

///
class LabeledStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    Token identifier;
    /** */ DeclarationOrStatement declarationOrStatement;
}

///
class LambdaExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TokenType functionType;
    /** */ Token identifier;
    /** */ Parameters parameters;
    /** */ FunctionAttribute[] functionAttributes;
    /** */ AssignExpression assignExpression;
}

///
class LastCatch : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class LinkageAttribute : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ bool hasPlusPlus;
}

///
class MemberFunctionAttribute : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TokenType tokenType;
    /** */ AtAttribute atAttribute;
}

///
class MixinDeclaration : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ MixinExpression mixinExpression;
    /** */ TemplateMixinExpression templateMixinExpression;
}

///
class MixinExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ AssignExpression assignExpression;
}

///
class MixinTemplateDeclaration : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TemplateDeclaration templateDeclaration;
}

///
class MixinTemplateName : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
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
    override void accept(ASTVisitor visitor)
	{
		if (moduleName !is null) visitor.visit(moduleName);
	}
    /** */ IdentifierChain moduleName;
}


///
class MulExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TokenType operator;
    mixin BinaryExpressionBody;
}

///
class NewAnonClassExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Arguments allocatorArguments;
    /** */ Arguments constructorArguments;
    /** */ BaseClassList baseClassList;
    /** */ StructBody structBody;
}

///
class NewExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Type type;
    /** */ NewAnonClassExpression newAnonClassExpression;
    /** */ Arguments arguments;
    /** */ AssignExpression assignExpression;
}


///
class StatementNoCaseNoDefault : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
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
    /** */ VersionSpecification versionSpecification;
    /** */ DebugSpecification debugSpecification;
    /** */ FunctionCallStatement functionCallStatement;
    /** */ ExpressionStatement expressionStatement;
}

///
class NonVoidInitializer : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ AssignExpression assignExpression;
    /** */ ArrayInitializer arrayInitializer;
    /** */ StructInitializer structInitializer;

}

///
class Operand : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ AsmExp asmExp;
}

///
class Operands : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Operand[] operands;
}

///
class OrExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    mixin BinaryExpressionBody;
}

///
class OrOrExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    mixin BinaryExpressionBody;
}

///
class OutStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token parameter;
    /** */ BlockStatement blockStatement;
}

///
class Parameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type, default_));
    }
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(parameters));
    }
    /** */ Parameter[] parameters;
    /** */ bool hasVarargs;
}

///
class Postblit : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ FunctionBody functionBody;
}

///
class PostIncDecExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token operator;
    /** */ UnaryExpression unaryExpression;
}

///
class PowExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    mixin BinaryExpressionBody;
}

///
class PragmaDeclaration : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ PragmaExpression pragmaExpression;
}

///
class PragmaExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ ArgumentList argumentList;
}

///
class PreIncDecExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token operator;
    /** */ UnaryExpression unaryExpression;
}

///
class PrimaryExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ bool hasDot;
    /** */ Token primary;
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
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ Token intLiteral;
    /** */ bool hasIntegerLiteral;
}

///
class RelExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TokenType operator;
    mixin BinaryExpressionBody;
}

///
class ReturnStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Expression expression;
}

///
class ScopeGuardStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class SharedStaticConstructor : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ FunctionBody functionBody;
}

///
class SharedStaticDestructor : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ FunctionBody functionBody;
}

///
class ShiftExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TokenType operator;
    mixin BinaryExpressionBody;
}

///
class SingleImport : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ IdentifierChain identifierChain;
}

///
class SliceExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ UnaryExpression unaryExpression;
    /** */ AssignExpression lower;
    /** */ AssignExpression upper;
}

///
class Statement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    /** */ CaseStatement caseStatement;
    /** */ CaseRangeStatement caseRangeStatement;
    /** */ DefaultStatement defaultStatement;
}

///
class StaticAssertDeclaration : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ StaticAssertStatement staticAssertStatement;
}

///
class StaticAssertStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ AssertExpression assertExpression;
}

///
class StaticConstructor : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ FunctionBody functionBody;
}

///
class StaticDestructor : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ FunctionBody functionBody;
}

///
class StaticIfCondition : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ AssignExpression assignExpression;
}

///
class StorageClass : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ AtAttribute atAttribute;
    /** */ Deprecated deprecated_;
    /** */ Token token;
}

///
class StructBody : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        foreach (dec; declarations)
        {
            if (dec !is null) visitor.visit(dec);
        }
    }

    /**
     * Byte position of the opening brace
     */
    size_t startLocation;

    /**
     * Byte position of the closing brace
     */
    size_t endLocation;
    /** */ Declaration[] declarations;
}

///
class StructDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(templateParameters, constraint, structBody));
    }
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ StructBody structBody;
}

///
class StructInitializer : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ StructMemberInitializers structMemberInitializers;

}

///
class StructMemberInitializer : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ bool hasIdentifier;
    /** */ NonVoidInitializer nonVoidInitializer;
}

///
class StructMemberInitializers : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ StructMemberInitializer[] structMemberInitializers;
}

///
class SwitchBody : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Statement[] statements;
}

///
class SwitchStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Expression expression;
    /** */ SwitchBody switchBody;
}

///
class Symbol : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);

    override string toString()
    {
        if (hasDot)
            return "." ~ identifierOrTemplateChain.toString();
        else
            return identifierOrTemplateChain.toString();
    }
    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ bool hasDot;
}

///
class SynchronizedStatement : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Expression expression;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class TemplateAliasParameter : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Type type;
    /** */ Token identifier;
    /** */ Type colonType;
    /** */ AssignExpression colonExpression;
    /** */ Type assignType;
    /** */ AssignExpression assignExpression;
}

///
class TemplateArgument : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Type type;
    /** */ AssignExpression assignExpression;
    /** */ Symbol symbol;
}

///
class TemplateArgumentList : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TemplateArgument[] items;
}

///
class TemplateArguments : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TemplateArgumentList templateArgumentList;
    /** */ TemplateSingleArgument templateSingleArgument;
}

///
class TemplateDeclaration : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ Declaration[] declarations;
}

///
class TemplateInstance : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ TemplateArguments templateArguments;
}

///
class TemplateMixinExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ TemplateArguments templateArguments;
    /** */ MixinTemplateName mixinTemplateName;
}

///
class TemplateParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(templateTypeParameter, templateValueParameter,
            templateAliasParameter, templateTupleParameter,
            templateThisParameter));
    }
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(items));
    }
    /** */ TemplateParameter[] items;
}

///
class TemplateParameters : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(templateParameterList));
    }
    /** */ TemplateParameterList templateParameterList;
}

///
class TemplateSingleArgument : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token token;
}

///
class TemplateThisParameter : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ TemplateTypeParameter templateTypeParameter;
}

///
class TemplateTupleParameter : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifier;
}

///
class TemplateTypeParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(colonType, assignType));
    }
    /** */ Token identifier;
    /** */ Type colonType;
    /** */ Type assignType;
}

///
class TemplateValueParameter : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Type type;
    /** */ Token identifier;
    /** */ Expression expression;
    /** */ TemplateValueParameterDefault templateValueParameterDefault;
}

///
class TemplateValueParameterDefault : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ AssignExpression assignExpression;
    /** */ Token token;
}

///
class TernaryExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ ExpressionNode orOrExpression;
    /** */ ExpressionNode expression;
    /** */ ExpressionNode ternaryExpression;
}

///
class ThrowStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(expression));
    }
    /** */ Expression expression;
}

///
class TraitsExpression : ExpressionNode
{
public:
//    override void accept(ASTVisitor visitor)
//    {
//        mixin (visitIfNotNull!(templateArgumentList));
//    }
    mixin (DEFAULT_ACCEPT);
    /** */ Token identifier;
    /** */ TemplateArgumentList templateArgumentList;
}

///
class TryStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(declarationOrStatement, catches, finally_));
    }
    /** */ DeclarationOrStatement declarationOrStatement;
    /** */ Catches catches;
    /** */ Finally finally_;
}

///
class Type : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type2, typeSuffixes));
    }

    override string toString()
    {
        string result;
        bool first = true;
        foreach (constructor; typeConstructors)
        {
            if (!first)
                result ~= " ";
            first = false;
            result ~= getTokenValue(constructor);
        }
        if (typeConstructors.length > 0)
            result ~= " ";
        result ~= type2.toString();
        foreach (suffix; typeSuffixes)
        {
            result ~= suffix.toString();
        }
        return result;
    }

    /** */ TokenType[] typeConstructors;
    /** */ TypeSuffix[] typeSuffixes;
    /** */ Type2 type2;
}

///
class Type2 : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);

    override string toString()
    {
        if (symbol !is null)
        {
            return symbol.toString();
        }
        else if (typeofExpression !is null)
        {
            return "";
        }
        else if (typeConstructor != TokenType.invalid)
        {
            return getTokenValue(typeConstructor) ~ "(" ~ type.toString() ~ ")";
        }
        else
            return getTokenValue(builtinType);
    }
    /** */ TokenType builtinType;
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
    mixin (DEFAULT_ACCEPT);
    /** */ Token token;
    /** */ Type type;
}

///
class TypeSuffix : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);

    override string toString()
    {
        if (star)
            return "*";
        else if (array)
        {
            if (type is null)
            {
                if (low is null)
                    return "[]";
                else
                {
                    if (high is null)
                        return "[" ~ low.toString() ~ "]";
                    else
                        return "[" ~ low.toString() ~ ".." ~ high.toString() ~ "]";
                }
            }
            else
                return "[" ~ type.toString() ~ "]";
        }
        else
        {
            // TODO
            return " " ~ delegateOrFunction.value ~ "()";
        }
    }

    /** */ Token delegateOrFunction;
    /** */ bool star;
    /** */ bool array;
    /** */ Type type;
    /** */ AssignExpression low;
    /** */ AssignExpression high;
    /** */ Parameters parameters;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
}

///
class TypeidExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Type type;
    /** */ Expression expression;
}

///
class TypeofExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Expression expression;
    /** */ Token return_;
}

///
class UnaryExpression : ExpressionNode
{
public:

    mixin (DEFAULT_ACCEPT);
//    override void accept(ASTVisitor visitor)
//    {
//        // TODO prefix, postfix, unary
//        mixin (visitIfNotNull!(type, primaryExpression, newExpression,
//            deleteExpression, castExpression, functionCallExpression, argumentList,
//            identifierOrTemplateInstance, assertExpression, indexExpression));
//    }

    /** */ Type type;
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(templateParameters, constraint, structBody));
    }

    /** */ Token identifier;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ StructBody structBody;
}

///
class Unittest : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(blockStatement));
    }
    /** */ BlockStatement blockStatement;
}

///
class VariableDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(storageClass, type, declarators, autoDeclaration));
    }
    /** */ Type type;
    /** */ Declarator[] declarators;
    /** */ StorageClass storageClass;
    /** */ AutoDeclaration autoDeclaration;
}

///
class Vector : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Type type;
}

///
class VersionCondition : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token token;
}

///
class VersionSpecification : ASTNode
{
public:
    mixin (DEFAULT_ACCEPT);
    /** */ Token token;
}

///
class WhileStatement : ASTNode
{
public:
	override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(expression, statementNoCaseNoDefault));
	}

    /** */ Expression expression;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class WithStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
	{
		mixin (visitIfNotNull!(expression, statementNoCaseNoDefault));
	}

    /** */ Expression expression;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class XorExpression : ExpressionNode
{
public:
    mixin (DEFAULT_ACCEPT);
    mixin BinaryExpressionBody;
}
