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

module stdx.d.ast;

import stdx.d.lexer;
import std.traits;
import std.algorithm;
import std.array;
import std.string;

// TODO: Many of these classes can be simplified by using std.variant.Algebraic

/**
 * Implements the $(LINK2 http://en.wikipedia.org/wiki/Visitor_pattern, Visitor Pattern)
 * for the various AST classes
 */
abstract class ASTVisitor
{
public:

	void visit(ExpressionNode n)
	{
		if (cast(AddExpression) n) visit(cast(AddExpression) n);
		else if (cast(AndAndExpression) n) visit(cast(AndAndExpression) n);
		else if (cast(AndExpression) n) visit(cast(AndExpression) n);
		else if (cast(AsmAddExp) n) visit(cast(AsmAddExp) n);
		else if (cast(AsmAndExp) n) visit(cast(AsmAndExp) n);
		else if (cast(AsmEqualExp) n) visit(cast(AsmEqualExp) n);
		else if (cast(AsmLogAndExp) n) visit(cast(AsmLogAndExp) n);
		else if (cast(AsmLogOrExp) n) visit(cast(AsmLogOrExp) n);
		else if (cast(AsmMulExp) n) visit(cast(AsmMulExp) n);
		else if (cast(AsmOrExp) n) visit(cast(AsmOrExp) n);
		else if (cast(AsmRelExp) n) visit(cast(AsmRelExp) n);
		else if (cast(AsmShiftExp) n) visit(cast(AsmShiftExp) n);
		else if (cast(AssertExpression) n) visit(cast(AssertExpression) n);
		else if (cast(AssignExpression) n) visit(cast(AssignExpression) n);
		else if (cast(CmpExpression) n) visit(cast(CmpExpression) n);
		else if (cast(DeleteExpression) n) visit(cast(DeleteExpression) n);
		else if (cast(EqualExpression) n) visit(cast(EqualExpression) n);
		else if (cast(Expression) n) visit(cast(Expression) n);
		else if (cast(FunctionCallExpression) n) visit(cast(FunctionCallExpression) n);
		else if (cast(FunctionLiteralExpression) n) visit(cast(FunctionLiteralExpression) n);
		else if (cast(IdentityExpression) n) visit(cast(IdentityExpression) n);
		else if (cast(ImportExpression) n) visit(cast(ImportExpression) n);
		else if (cast(IndexExpression) n) visit(cast(IndexExpression) n);
		else if (cast(InExpression) n) visit(cast(InExpression) n);
		else if (cast(IsExpression) n) visit(cast(IsExpression) n);
		else if (cast(LambdaExpression) n) visit(cast(LambdaExpression) n);
		else if (cast(MixinExpression) n) visit(cast(MixinExpression) n);
		else if (cast(MulExpression) n) visit(cast(MulExpression) n);
		else if (cast(NewAnonClassExpression) n) visit(cast(NewAnonClassExpression) n);
		else if (cast(NewExpression) n) visit(cast(NewExpression) n);
		else if (cast(OrExpression) n) visit(cast(OrExpression) n);
		else if (cast(OrOrExpression) n) visit(cast(OrOrExpression) n);
		else if (cast(PostIncDecExpression) n) visit(cast(PostIncDecExpression) n);
		else if (cast(PowExpression) n) visit(cast(PowExpression) n);
		else if (cast(PragmaExpression) n) visit(cast(PragmaExpression) n);
		else if (cast(PreIncDecExpression) n) visit(cast(PreIncDecExpression) n);
		else if (cast(PrimaryExpression) n) visit(cast(PrimaryExpression) n);
		else if (cast(RelExpression) n) visit(cast(RelExpression) n);
		else if (cast(ShiftExpression) n) visit(cast(ShiftExpression) n);
		else if (cast(SliceExpression) n) visit(cast(SliceExpression) n);
		else if (cast(TemplateMixinExpression) n) visit(cast(TemplateMixinExpression) n);
		else if (cast(TernaryExpression) n) visit(cast(TernaryExpression) n);
		else if (cast(TraitsExpression) n) visit(cast(TraitsExpression) n);
		else if (cast(TypeidExpression) n) visit(cast(TypeidExpression) n);
		else if (cast(TypeofExpression) n) visit(cast(TypeofExpression) n);
		else if (cast(UnaryExpression) n) visit(cast(UnaryExpression) n);
		else if (cast(XorExpression) n) visit(cast(XorExpression) n);
	}

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
    /** */ void visit(EponymousTemplateDeclaration eponymousTemplateDeclaration) { eponymousTemplateDeclaration.accept(this); }
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
public:
    /** */ void accept(ASTVisitor visitor);
}

immutable string DEFAULT_ACCEPT = q{override void accept(ASTVisitor visitor) {}};

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

abstract class ExpressionNode : ASTNode
{
public:
	override void accept(ASTVisitor visitor)
	{
		assert (false);
	}
}

mixin template BinaryExpressionBody()
{
    ExpressionNode left;
    ExpressionNode right;
	size_t line;
	size_t column;
}

///
class AddExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ IdType operator;
    mixin BinaryExpressionBody;
}

///
class AliasDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type, name, initializers));
    }
    /** */ Type type;
    /** */ Token name;
    /** */ AliasInitializer[] initializers;
    /** */ string comment;
}

///
class AliasInitializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(name, type));
    }

    /** */ Token name;
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
}

///
class AndExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
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
    /** */ IdType operator;
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
    /** */ IdType operator;
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
    override void accept(ASTVisitor visitor)
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(ternaryExpression, assignExpression));
    }
    /** */ ExpressionNode ternaryExpression;
    /** */ ExpressionNode assignExpression;
    /** */ IdType operator;
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
    /** */ IdType attribute;
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(label));
    }
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(first, second));
    }
    /** */ Token first;
    /** */ Token second;
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
    /** */ string comment;
}

///
class CmpExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
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
        mixin (visitIfNotNull!(compileCondition, trueDeclarations, falseDeclaration));
    }
    /** */ CompileCondition compileCondition;
    /** */ Declaration[] trueDeclarations;
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
    /** */ size_t location;
    /** */ string comment;
}

///
class ContinueStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(label));
    }
    /** */ Token label;
}

///
class DebugCondition : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifierOrInteger));
    }
    /** */ Token identifierOrInteger;
}

///
class DebugSpecification : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifierOrInteger));
    }
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
            conditionalDeclaration, pragmaDeclaration, versionSpecification,
			invariant_, postblit, declarations));
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(unaryExpression));
    }
    /** */ UnaryExpression unaryExpression;
	/** */ size_t line;
	/** */ size_t column;
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
    /** */ size_t location;
    /** */ string comment;
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

    /**
     * Byte position of the opening brace
     */
    size_t startLocation;

    /**
     * Byte position of the closing brace
     */
    size_t endLocation;
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
    /** */ string comment;
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
    /** */ string comment;
}

///
class EponymousTemplateDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(name, templateParameters, assignExpression));
    }
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ AssignExpression assignExpression;
}

///
class EqualExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ IdType operator;
    mixin BinaryExpressionBody;
}

///
class Expression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
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
        mixin (visitIfNotNull!(initialization, test, increment,
            declarationOrStatement));
    }
    /** */ DeclarationOrStatement initialization;
    /** */ ExpressionStatement test;
    /** */ Expression increment;
    /** */ DeclarationOrStatement declarationOrStatement;
	/** */ size_t startIndex;
}

///
class ForeachStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(foreachType, foreachTypeList, low, high,
            declarationOrStatement));
    }
    /** */ IdType type;
    /** */ ForeachTypeList foreachTypeList;
	/** */ ForeachType foreachType;
    /** */ Expression low;
    /** */ Expression high;
    /** */ DeclarationOrStatement declarationOrStatement;
	/** */ size_t startIndex;
}

///
class ForeachType : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type, identifier));
    }
    /** */ IdType[] typeConstructors;
    /** */ Type type;
    /** */ Token identifier;
}

///
class ForeachTypeList : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(items));
    }
    /** */ ForeachType[] items;
}

///
class FunctionAttribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(token, atAttribute));
    }
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(unaryExpression, arguments, templateArguments));
    }
    /** */ UnaryExpression unaryExpression;
    /** */ TemplateArguments templateArguments;
    /** */ Arguments arguments;
}

///
class FunctionCallStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(functionCallExpression));
    }
    /** */ FunctionCallExpression functionCallExpression;
}

///
class FunctionDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(returnType, parameters, templateParameters,
            constraint, memberFunctionAttributes, functionBody));
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
    /** */ string comment;
}

///
class FunctionLiteralExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type, parameters, functionAttributes,
            functionBody));
    }
    /** */ IdType functionOrDelegate;
    /** */ Type type;
    /** */ Parameters parameters;
    /** */ FunctionAttribute[] functionAttributes;
    /** */ FunctionBody functionBody;
}

///
class GotoStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(label, expression));
    }
    /** */ Expression expression;
    /** */ Token label;
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifiersOrTemplateInstances));
    }

    /** */ IdentifierOrTemplateInstance[] identifiersOrTemplateInstances;
}

///
class IdentifierOrTemplateInstance : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier, templateInstance));
    }

    /** */ Token identifier;
    /** */ TemplateInstance templateInstance;
}

///
class IdentityExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ bool negated;
    mixin BinaryExpressionBody;
}

///
class IfStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier, type, expression, thenStatement,
            elseStatement));
    }
    /** */ Token identifier;
    /** */ Type type;
    /** */ Expression expression;
    /** */ DeclarationOrStatement thenStatement;
    /** */ DeclarationOrStatement elseStatement;
	/** */ size_t startIndex;
}

///
class ImportBind : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ Token left;
    /** */ Token right;
}

///
class ImportBindings : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(singleImport, importBinds));
    }
    /** */ SingleImport singleImport;
    /** */ ImportBind[] importBinds;
}

///
class ImportDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(singleImports, importBindings));
    }
    /** */ SingleImport[] singleImports;
    /** */ ImportBindings importBindings;
}

///
class ImportExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(assignExpression));
    }
    /** */ AssignExpression assignExpression;
}

///
class IndexExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(unaryExpression, argumentList));
    }
    /** */ UnaryExpression unaryExpression;
    /** */ ArgumentList argumentList;
}

///
class InExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    bool negated;
}

///
class InStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(blockStatement));
    }
    /** */ BlockStatement blockStatement;
}

///
class Initialize : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(statementNoCaseNoDefault));
    }
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class Initializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(nonVoidInitializer));
    }
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
    /** */ string comment;
}

///
class Invariant : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(blockStatement));
    }
    /** */ BlockStatement blockStatement;
    /** */ string comment;
}

///
class IsExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type, identifier, typeSpecialization,
            templateParameterList));
    }
    /** */ Type type;
    /** */ Token identifier;
    /** */ TypeSpecialization typeSpecialization;
    /** */ TemplateParameterList templateParameterList;
    /** */ IdType equalsOrColon;
}

///
class KeyValuePair : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(key, value));
    }
    /** */ AssignExpression key;
    /** */ AssignExpression value;
}

///
class KeyValuePairs : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(keyValuePairs));
    }
    /** */ KeyValuePair[] keyValuePairs;
}

///
class LabeledStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier, declarationOrStatement));
    }
    Token identifier;
    /** */ DeclarationOrStatement declarationOrStatement;
}

///
class LambdaExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier, parameters, functionAttributes,
            assignExpression));
    }
    /** */ IdType functionType;
    /** */ Token identifier;
    /** */ Parameters parameters;
    /** */ FunctionAttribute[] functionAttributes;
    /** */ AssignExpression assignExpression;
}

///
class LastCatch : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(statementNoCaseNoDefault));
    }
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class LinkageAttribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier));
    }
    /** */ Token identifier;
    /** */ bool hasPlusPlus;
}

///
class MemberFunctionAttribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(atAttribute));
    }
    /** */ IdType tokenType;
    /** */ AtAttribute atAttribute;
}

///
class MixinDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(mixinExpression, templateMixinExpression));
    }
    /** */ MixinExpression mixinExpression;
    /** */ TemplateMixinExpression templateMixinExpression;
}

///
class MixinExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(assignExpression));
    }
    /** */ AssignExpression assignExpression;
}

///
class MixinTemplateDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(templateDeclaration));
    }
    /** */ TemplateDeclaration templateDeclaration;
}

///
class MixinTemplateName : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(symbol, typeofExpression, identifierOrTemplateChain));
    }
    /** */ Symbol symbol;
    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ TypeofExpression typeofExpression;
}

///
class Module : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(moduleDeclaration, declarations));
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
        mixin (visitIfNotNull!(moduleName));
    }
    /** */ IdentifierChain moduleName;
}


///
class MulExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ IdType operator;
    mixin BinaryExpressionBody;
}

///
class NewAnonClassExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(allocatorArguments, constructorArguments,
            baseClassList, structBody));
    }
    /** */ Arguments allocatorArguments;
    /** */ Arguments constructorArguments;
    /** */ BaseClassList baseClassList;
    /** */ StructBody structBody;
}

///
class NewExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(newAnonClassExpression, type, arguments,
            assignExpression));
    }
    /** */ Type type;
    /** */ NewAnonClassExpression newAnonClassExpression;
    /** */ Arguments arguments;
    /** */ AssignExpression assignExpression;
}


///
class StatementNoCaseNoDefault : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(labeledStatement, blockStatement, ifStatement,
            whileStatement, doStatement, forStatement, foreachStatement,
            switchStatement, finalSwitchStatement, continueStatement,
            breakStatement, returnStatement, gotoStatement, withStatement,
            synchronizedStatement, tryStatement, throwStatement,
            scopeGuardStatement, asmStatement, conditionalStatement,
            staticAssertStatement, versionSpecification, debugSpecification,
            functionCallStatement, expressionStatement));
    }
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(assignExpression, arrayInitializer, structInitializer));
    }
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
}

///
class OrOrExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
}

///
class OutStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(parameter, blockStatement));
    }
    /** */ Token parameter;
    /** */ BlockStatement blockStatement;
}

///
class Parameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type, name, default_));
    }

    /** */ IdType[] parameterAttributes;
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(functionBody));
    }
    /** */ FunctionBody functionBody;
}

///
class PostIncDecExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(unaryExpression));
    }
    /** */ IdType operator;
    /** */ UnaryExpression unaryExpression;
}

///
class PowExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
}

///
class PragmaDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(pragmaExpression));
    }
    /** */ PragmaExpression pragmaExpression;
}

///
class PragmaExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier, argumentList));
    }
    /** */ Token identifier;
    /** */ ArgumentList argumentList;
}

///
class PreIncDecExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(unaryExpression));
    }
    /** */ IdType operator;
    /** */ UnaryExpression unaryExpression;
}

///
class PrimaryExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(basicType, primary, typeofExpression,
            typeidExpression, arrayLiteral, assocArrayLiteral, expression,
            dot, identifierOrTemplateInstance, isExpression, lambdaExpression,
            functionLiteralExpression, traitsExpression, mixinExpression,
            importExpression, vector));
    }
    /** */ Token dot;
    /** */ Token primary;
    /** */ IdentifierOrTemplateInstance identifierOrTemplateInstance;
    /** */ Token basicType;
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ IdType operator;
    mixin BinaryExpressionBody;
}

///
class ReturnStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(expression));
    }
    /** */ Expression expression;
}

///
class ScopeGuardStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier, statementNoCaseNoDefault));
    }
    /** */ Token identifier;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
}

///
class SharedStaticConstructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(functionBody));
    }
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ string comment;
}

///
class SharedStaticDestructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(functionBody));
    }
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ string comment;
}

///
class ShiftExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ IdType operator;
    mixin BinaryExpressionBody;
}

///
class SingleImport : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(rename, identifierChain));
    }
    /** */ Token rename;
    /** */ IdentifierChain identifierChain;
}

///
class SliceExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(unaryExpression, lower, upper));
    }
    /** */ UnaryExpression unaryExpression;
    /** */ AssignExpression lower;
    /** */ AssignExpression upper;
}

///
class Statement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(statementNoCaseNoDefault, caseStatement,
            caseRangeStatement, defaultStatement));
    }
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    /** */ CaseStatement caseStatement;
    /** */ CaseRangeStatement caseRangeStatement;
    /** */ DefaultStatement defaultStatement;
}

///
class StaticAssertDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(staticAssertStatement));
    }
    /** */ StaticAssertStatement staticAssertStatement;
}

///
class StaticAssertStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(assertExpression));
    }
    /** */ AssertExpression assertExpression;
}

///
class StaticConstructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(functionBody));
    }
    /** */ FunctionBody functionBody;
    /** */ size_t location;
	/** */ string comment;
}

///
class StaticDestructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(functionBody));
    }
    /** */ FunctionBody functionBody;
    /** */ size_t location;
	/** */ string comment;
}

///
class StaticIfCondition : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(assignExpression));
    }
    /** */ AssignExpression assignExpression;
}

///
class StorageClass : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(token, deprecated_, atAttribute));
    }
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
        mixin (visitIfNotNull!(declarations));
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
    /** */ string comment;
}

///
class StructInitializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(structMemberInitializers));
    }
    /** */ StructMemberInitializers structMemberInitializers;

}

///
class StructMemberInitializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier, nonVoidInitializer));
    }
    /** */ Token identifier;
    /** */ NonVoidInitializer nonVoidInitializer;
}

///
class StructMemberInitializers : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(structMemberInitializers));
    }
    /** */ StructMemberInitializer[] structMemberInitializers;
}

///
class SwitchStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(expression, statement));
    }
    /** */ Expression expression;
    /** */ Statement statement;
}

///
class Symbol : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifierOrTemplateChain));
    }

    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ bool dot;
}

///
class SynchronizedStatement : ASTNode
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
class TemplateAliasParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type, identifier, colonType, colonExpression,
            assignType, assignExpression));
    }
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type, assignExpression));
    }
    /** */ Type type;
    /** */ AssignExpression assignExpression;
}

///
class TemplateArgumentList : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(items));
    }
    /** */ TemplateArgument[] items;
}

///
class TemplateArguments : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(templateArgumentList, templateSingleArgument));
    }
    /** */ TemplateArgumentList templateArgumentList;
    /** */ TemplateSingleArgument templateSingleArgument;
}

///
class TemplateDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(name, templateParameters, constraint,
            declarations, eponymousTemplateDeclaration));
    }
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ Declaration[] declarations;
    /** */ EponymousTemplateDeclaration eponymousTemplateDeclaration;
    /** */ string comment;
}

///
class TemplateInstance : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier, templateArguments));
    }
    /** */ Token identifier;
    /** */ TemplateArguments templateArguments;
}

///
class TemplateMixinExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier, templateArguments, mixinTemplateName));
    }
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(token));
    }
    /** */ Token token;
}

///
class TemplateThisParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(templateTypeParameter));
    }
    /** */ TemplateTypeParameter templateTypeParameter;
}

///
class TemplateTupleParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier));
    }
    /** */ Token identifier;
}

///
class TemplateTypeParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier, colonType, assignType));
    }
    /** */ Token identifier;
    /** */ Type colonType;
    /** */ Type assignType;
}

///
class TemplateValueParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type, identifier, expression,
            templateValueParameterDefault));
    }
    /** */ Type type;
    /** */ Token identifier;
    /** */ Expression expression;
    /** */ TemplateValueParameterDefault templateValueParameterDefault;
}

///
class TemplateValueParameterDefault : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(token, assignExpression));
    }
    /** */ AssignExpression assignExpression;
    /** */ Token token;
}

///
class TernaryExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(orOrExpression, expression, ternaryExpression));
    }
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(identifier, templateArgumentList));
    }
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

    /** */ IdType[] typeConstructors;
    /** */ TypeSuffix[] typeSuffixes;
    /** */ Type2 type2;
}

///
class Type2 : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(symbol, typeofExpression,
            identifierOrTemplateChain, type));
    }

    /** */ IdType builtinType;
    /** */ Symbol symbol;
    /** */ TypeofExpression typeofExpression;
    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ IdType typeConstructor;
    /** */ Type type;
}

///
class TypeSpecialization : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(token, type));
    }
    /** */ Token token;
    /** */ Type type;
}

///
class TypeSuffix : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type, low, high, delegateOrFunction, parameters,
            memberFunctionAttributes));
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type, expression));
    }
    /** */ Type type;
    /** */ Expression expression;
}

///
class TypeofExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(expression, return_));
    }
    /** */ Expression expression;
    /** */ Token return_;
}

///
class UnaryExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        // TODO prefix, postfix, unary
        mixin (visitIfNotNull!(primaryExpression, newExpression,
            deleteExpression, castExpression, functionCallExpression, argumentList,
            unaryExpression, type, identifierOrTemplateInstance, assertExpression,
            indexExpression));
    }

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
        mixin (visitIfNotNull!(name, templateParameters, constraint, structBody));
    }

    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ StructBody structBody;
    /** */ string comment;
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
    /** */ string comment;
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
    /** */ string comment;
}

///
class Vector : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(type));
    }
    /** */ Type type;
}

///
class VersionCondition : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(token));
    }
    /** */ Token token;
}

///
class VersionSpecification : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(token));
    }
    /** */ Token token;
}

///
class WhileStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(expression, declarationOrStatement));
    }

    /** */ Expression expression;
    /** */ DeclarationOrStatement declarationOrStatement;
	/** */ size_t startIndex;
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
    override void accept(ASTVisitor visitor)
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
}
