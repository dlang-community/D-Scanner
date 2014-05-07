module formatter;

import std.d.ast;
import std.d.lexer;

/**
 * The only brace styles worth using.
 */
enum IndentStyle
{
	/**
	 * ---
	 * if (something)
	 * {
	 *     foo();
	 *     bar();
	 * }
	 * else
	 * {
	 *     bar();
	 *     baz();
	 * }
	 * ---
	 */
	allman,
	/**
	 * ---
	 * if (something) {
	 *     foo();
	 *     bar();
	 * } else {
	 *     bar();
	 *     baz();
	 * }
	 * ---
	 */
	otbs,
}

/**
 *
 */
void format(Sink)(Sink sink, Module mod, bool useTabs = false,
	IndentStyle style = IndentStyle.allman, uint indentWith = 4)
{
	Formatter!Sink formatter = new Formatter!(Sink)(sink, useTabs, style, indentWith);
	formatter.format(mod);
}

class Formatter(Sink)
{
	/**
	 * Parameters:
	 *     sink = the output range that the formatted source code is placed in
	 *     useTabs = if true, tabs are used for indent levels instead of spaces
	 *     style = the brace style
	 *     indenteWidth = the number of spaces used for indentation if useTabs is false
	 */
	this(Sink sink, bool useTabs = false, IndentStyle style = IndentStyle.allman, uint indentWidth = 4)
	{
		this.sink = sink;
		this.useTabs = useTabs;
		this.indentWidth = indentWidth;
		this.style = style;
	}

	void format(const AddExpression addExpression)
	{
		format(addExpression.left);
		if (addExpression.right is null)
			return;
		sink.put(" ");
		sink.put(str(addExpression.operator));
		sink.put(" ");
		format(addExpression.right);
	}

	void format(const AliasDeclaration aliasDeclaration)
	{}

	void format(const AliasInitializer aliasInitializer)
	{}

	void format(const AliasThisDeclaration aliasThisDeclaration)
	{}

	void format(const AlignAttribute alignAttribute)
	{}

	void format(const AndAndExpression andAndExpression)
	{}

	void format(const AndExpression andExpression)
	{}

	void format(const ArgumentList argumentList)
	{}

	void format(const Arguments arguments)
	{}

	void format(const ArrayInitializer arrayInitializer)
	{}

	void format(const ArrayLiteral arrayLiteral)
	{}

	void format(const ArrayMemberInitialization arrayMemberInitialization)
	{}

	void format(const AsmAddExp asmAddExp)
	{}

	void format(const AsmAndExp asmAndExp)
	{}

	void format(const AsmBrExp asmBrExp)
	{}

	void format(const AsmEqualExp asmEqualExp)
	{}

	void format(const AsmExp asmExp)
	{}

	void format(const AsmInstruction asmInstruction)
	{}

	void format(const AsmLogAndExp asmLogAndExp)
	{}

	void format(const AsmLogOrExp asmLogOrExp)
	{}

	void format(const AsmMulExp asmMulExp)
	{}

	void format(const AsmOrExp asmOrExp)
	{}

	void format(const AsmPrimaryExp asmPrimaryExp)
	{}

	void format(const AsmRelExp asmRelExp)
	{}

	void format(const AsmShiftExp asmShiftExp)
	{}

	void format(const AsmStatement asmStatement)
	{}

	void format(const AsmTypePrefix asmTypePrefix)
	{}

	void format(const AsmUnaExp asmUnaExp)
	{}

	void format(const AsmXorExp asmXorExp)
	{}

	void format(const AssertExpression assertExpression)
	{}

	void format(const AssignExpression assignExpression)
	{}

	void format(const AssocArrayLiteral assocArrayLiteral)
	{}

	void format(const AtAttribute atAttribute)
	{}

	void format(const Attribute attribute)
	{}

	void format(const AttributeDeclaration attributeDeclaration)
	{}

	void format(const AutoDeclaration autoDeclaration)
	{}

	void format(const BlockStatement blockStatement)
	{}

	void format(const BodyStatement bodyStatement)
	{}

	void format(const BreakStatement breakStatement)
	{}

	void format(const BaseClass baseClass)
	{}

	void format(const BaseClassList baseClassList)
	{}

	void format(const CaseRangeStatement caseRangeStatement)
	{}

	void format(const CaseStatement caseStatement)
	{}

	void format(const CastExpression castExpression)
	{}

	void format(const CastQualifier castQualifier)
	{}

	void format(const Catch catch_)
	{}

	void format(const Catches catches)
	{}

	void format(const ClassDeclaration classDeclaration)
	{}

	void format(const CmpExpression cmpExpression)
	{}

	void format(const CompileCondition compileCondition)
	{}

	void format(const ConditionalDeclaration conditionalDeclaration)
	{}

	void format(const ConditionalStatement conditionalStatement)
	{}

	void format(const Constraint constraint)
	{}

	void format(const Constructor constructor)
	{}

	void format(const ContinueStatement continueStatement)
	{}

	void format(const DebugCondition debugCondition)
	{}

	void format(const DebugSpecification debugSpecification)
	{}

	void format(const Declaration declaration)
	{}

	void format(const DeclarationOrStatement declarationsOrStatement)
	{}

	void format(const DeclarationsAndStatements declarationsAndStatements)
	{}

	void format(const Declarator declarator)
	{}

	void format(const DefaultStatement defaultStatement)
	{}

	void format(const DeleteExpression deleteExpression)
	{}

	void format(const DeleteStatement deleteStatement)
	{}

	void format(const Deprecated deprecated_)
	{}

	void format(const Destructor destructor)
	{}

	void format(const DoStatement doStatement)
	{}

	void format(const EnumBody enumBody)
	{}

	void format(const EnumDeclaration enumDeclaration)
	{}

	void format(const EnumMember enumMember)
	{}

	void format(const EqualExpression equalExpression)
	{}

	void format(const Expression expression)
	{}

	void format(const ExpressionNode expressionNode)
	{}

	void format(const ExpressionStatement expressionStatement)
	{}

	void format(const FinalSwitchStatement finalSwitchStatement)
	{}

	void format(const Finally finally_)
	{}

	void format(const ForStatement forStatement)
	{}

	void format(const ForeachStatement foreachStatement)
	{}

	void format(const ForeachType foreachType)
	{}

	void format(const ForeachTypeList foreachTypeList)
	{}

	void format(const FunctionAttribute functionAttribute)
	{}

	void format(const FunctionBody functionBody)
	{}

	void format(const FunctionCallExpression functionCallExpression)
	{}

	void format(const FunctionCallStatement functionCallStatement)
	{}

	void format(const FunctionDeclaration functionDeclaration)
	{}

	void format(const FunctionLiteralExpression functionLiteralExpression)
	{}

	void format(const GotoStatement gotoStatement)
	{
		sink.put("goto ");
		if (gotoStatement.label != tok!"")
			sink.put(gotoStatement.label.text);
		else
			format(gotoStatement.expression);
		sink.put(";");
	}

	void format(const IdentifierChain identifierChain)
	{
		bool first = true;
		foreach(ident; identifierChain.identifiers)
		{
			if (!first)
				sink.put(".");
			first = false;
			sink.put(ident.text);
		}
	}

	void format(const IdentifierList identifierList)
	{}

	void format(const IdentifierOrTemplateChain identifierOrTemplateChain)
	{
		bool first = true;
		foreach(ident; identifierOrTemplateChain.identifiersOrTemplateInstances)
		{
			if (!first)
				sink.put(".");
			first = false;
			format(ident);
		}
	}

	void format(const IdentifierOrTemplateInstance identifierOrTemplateInstance)
	{
		if (identifierOrTemplateInstance.templateInstance !is null)
			format(identifierOrTemplateInstance.templateInstance);
		else
			sink.put(identifierOrTemplateInstance.identifier.text);

	}

	void format(const IdentityExpression identityExpression)
	{}

	void format(const IfStatement ifStatement)
	{}

	void format(const ImportBind importBind)
	{}

	void format(const ImportBindings importBindings)
	{}

	void format(const ImportDeclaration importDeclaration)
	{}

	void format(const ImportExpression importExpression)
	{}

	void format(const IndexExpression indexExpression)
	{}

	void format(const InExpression inExpression)
	{}

	void format(const InStatement inStatement)
	{}

	void format(const Initialize initialize)
	{}

	void format(const Initializer initializer)
	{}

	void format(const InterfaceDeclaration interfaceDeclaration)
	{}

	void format(const Invariant invariant_)
	{}

	void format(const IsExpression isExpression)
	{}

	void format(const KeyValuePair keyValuePair)
	{}

	void format(const KeyValuePairs keyValuePairs)
	{}

	void format(const LabeledStatement labeledStatement)
	{}

	void format(const LambdaExpression lambdaExpression)
	{}

	void format(const LastCatch lastCatch)
	{}

	void format(const LinkageAttribute linkageAttribute)
	{}

	void format(const MemberFunctionAttribute memberFunctionAttribute)
	{}

	void format(const MixinDeclaration mixinDeclaration)
	{}

	void format(const MixinExpression mixinExpression)
	{}

	void format(const MixinTemplateDeclaration mixinTemplateDeclaration)
	{}

	void format(const MixinTemplateName mixinTemplateName)
	{}

	void format(const Module module_)
	{}

	void format(const ModuleDeclaration moduleDeclaration)
	{}

	void format(const MulExpression mulExpression)
	{}

	void format(const NewAnonClassExpression newAnonClassExpression)
	{}

	void format(const NewExpression newExpression)
	{}

	void format(const NonVoidInitializer nonVoidInitializer)
	{}

	void format(const Operand operand)
	{}

	void format(const Operands operands)
	{}

	void format(const OrExpression orExpression)
	{}

	void format(const OrOrExpression orOrExpression)
	{}

	void format(const OutStatement outStatement)
	{}

	void format(const Parameter parameter)
	{
		foreach (attribute; parameter.parameterAttributes)
		{
			sink.put(str(attribute));
			sink.put(" ");
		}
		if (parameter.type !is null)
			format(parameter.type);
		if (parameter.name.type != tok!"")
		{
			sink.put(" ");
			sink.put(parameter.name.text);
		}
		if (parameter.vararg)
			sink.put(" ...");
	}

	void format(const Parameters parameters)
	{
		sink.put("(");
		bool first = true;
		foreach (param; parameters.parameters)
		{
			if (!first)
				sink.put(", ");
			first = false;
			format(param);
		}
		sink.put(")");
	}

	void format(const Postblit postblit)
	{}

	void format(const PostIncDecExpression postIncDecExpression)
	{}

	void format(const PowExpression powExpression)
	{}

	void format(const PragmaDeclaration pragmaDeclaration)
	{}

	void format(const PragmaExpression pragmaExpression)
	{}

	void format(const PreIncDecExpression preIncDecExpression)
	{}

	void format(const PrimaryExpression primaryExpression)
	{}

	void format(const Register register)
	{}

	void format(const RelExpression relExpression)
	{}

	void format(const ReturnStatement returnStatement)
	{}

	void format(const ScopeGuardStatement scopeGuardStatement)
	{}

	void format(const SharedStaticConstructor sharedStaticConstructor)
	{}

	void format(const SharedStaticDestructor sharedStaticDestructor)
	{}

	void format(const ShiftExpression shiftExpression)
	{}

	void format(const SingleImport singleImport)
	{}

	void format(const SliceExpression sliceExpression)
	{}

	void format(const Statement statement)
	{}

	void format(const StatementNoCaseNoDefault statementNoCaseNoDefault)
	{}

	void format(const StaticAssertDeclaration staticAssertDeclaration)
	{}

	void format(const StaticAssertStatement staticAssertStatement)
	{}

	void format(const StaticConstructor staticConstructor)
	{}

	void format(const StaticDestructor staticDestructor)
	{}

	void format(const StaticIfCondition staticIfCondition)
	{}

	void format(const StorageClass storageClass)
	{}

	void format(const StructBody structBody)
	{}

	void format(const StructDeclaration structDeclaration)
	{}

	void format(const StructInitializer structInitializer)
	{}

	void format(const StructMemberInitializer structMemberInitializer)
	{}

	void format(const StructMemberInitializers structMemberInitializers)
	{}

	void format(const SwitchStatement switchStatement)
	{}

	void format(const Symbol symbol)
	{
        if (symbol.dot != tok!"")
            sink.put(".");
		format(symbol.identifierOrTemplateChain);
	}

	void format(const SynchronizedStatement synchronizedStatement)
	{}

	void format(const TemplateAliasParameter templateAliasParameter)
	{}

	void format(const TemplateArgument templateArgument)
	{}

	void format(const TemplateArgumentList templateArgumentList)
	{}

	void format(const TemplateArguments templateArguments)
	{}

	void format(const TemplateDeclaration templateDeclaration)
	{}

	void format(const TemplateInstance templateInstance)
	{}

	void format(const TemplateMixinExpression templateMixinExpression)
	{}

	void format(const TemplateParameter templateParameter)
	{}

	void format(const TemplateParameterList templateParameterList)
	{}

	void format(const TemplateParameters templateParameters)
	{}

	void format(const TemplateSingleArgument templateSingleArgument)
	{}

	void format(const TemplateThisParameter templateThisParameter)
	{}

	void format(const TemplateTupleParameter templateTupleParameter)
	{}

	void format(const TemplateTypeParameter templateTypeParameter)
	{}

	void format(const TemplateValueParameter templateValueParameter)
	{}

	void format(const TemplateValueParameterDefault templateValueParameterDefault)
	{}

	void format(const TernaryExpression ternaryExpression)
	{}

	void format(const ThrowStatement throwStatement)
	{}

	void format(const Token token)
	{
		sink.put(token.text);
	}

	void format(const TraitsExpression traitsExpression)
	{}

	void format(const TryStatement tryStatement)
	{}

	void format(const Type type)
	{
        bool first = true;
        foreach (constructor; type.typeConstructors)
        {
            if (first)
                sink.put(" ");
            first = false;
            sink.put(str(constructor));
        }
        if (type.typeConstructors.length > 0)
            sink.put(" ");
		format(type.type2);
        foreach (suffix; type.typeSuffixes)
        {
			format(suffix);
        }
	}

	void format(const Type2 type2)
	{
        if (type2.symbol !is null)
        {
			format(type2.symbol);
            return;
        }
        else if (type2.typeofExpression !is null)
        {
            format(type2.typeofExpression);
			return;
        }
        else if (type2.typeConstructor != tok!"")
        {
			sink.put(str(type2.typeConstructor));
			sink.put("(");
			format(type2.type);
			sink.put(")");
			return;
        }
        else
            sink.put(str(type2.builtinType));
	}

	void format(const TypeSpecialization typeSpecialization)
	{}

	void format(const TypeSuffix typeSuffix)
	{
        if (typeSuffix.star)
		{
			sink.put("*");
			return;
		}
        else if (typeSuffix.array)
        {
            if (typeSuffix.type is null)
            {
                if (typeSuffix.low is null)
				{
					sink.put("[]");
					return;
				}
                else
                {
                    if (typeSuffix.high is null)
					{
						sink.put("[");
						format(typeSuffix.low);
						sink.put("]");
						return;
					}
                    else
					{
						sink.put("[");
						format(typeSuffix.low);
						sink.put("..");
						format(typeSuffix.high);
						sink.put("]");
						return;
					}
                }
            }
            else
			{
				sink.put("[");
				format(typeSuffix.type);
				sink.put("]");
				return;
			}
        }
        else
        {
            // TODO
			sink.put(" ");
			format(typeSuffix.delegateOrFunction);
			sink.put("()");
			return;
        }
	}

	void format(const TypeidExpression typeidExpression)
	{}

	void format(const TypeofExpression typeofExpression)
	{}

	void format(const UnaryExpression unaryExpression)
	{}

	void format(const UnionDeclaration unionDeclaration)
	{}

	void format(const Unittest unittest_)
	{}

	void format(const VariableDeclaration variableDeclaration)
	{}

	void format(const Vector vector)
	{}

	void format(const VersionCondition versionCondition)
	{}

	void format(const VersionSpecification versionSpecification)
	{}

	void format(const WhileStatement whileStatement)
	{}

	void format(const WithStatement withStatement)
	{}

	void format(const XorExpression xorExpression)
	{}

private:

	void indent()
	{
		indentLevel++;
	}

	void outdent()
	{
		if (indentLevel == 0)
			return;
		indentLevel--;
	}

	bool useTabs;
	uint indentWidth;
	uint indentLevel;
	IndentStyle style;
	Sink sink;
}
