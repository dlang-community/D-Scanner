module formatter;

import stdx.d.ast;
import stdx.d.lexer;

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

	void format(AddExpression addExpression)
	{
		format(addExpression.left);
		if (addExpression.right is null)
			return;
		sink.put(" ");
		sink.put(getTokenValue(addExpression.operator));
		sink.put(" ");
		format(addExpression.right);
	}

	void format(AliasDeclaration aliasDeclaration)
	{}

	void format(AliasInitializer aliasInitializer)
	{}

	void format(AliasThisDeclaration aliasThisDeclaration)
	{}

	void format(AlignAttribute alignAttribute)
	{}

	void format(AndAndExpression andAndExpression)
	{}

	void format(AndExpression andExpression)
	{}

	void format(ArgumentList argumentList)
	{}

	void format(Arguments arguments)
	{}

	void format(ArrayInitializer arrayInitializer)
	{}

	void format(ArrayLiteral arrayLiteral)
	{}

	void format(ArrayMemberInitialization arrayMemberInitialization)
	{}

	void format(AsmAddExp asmAddExp)
	{}

	void format(AsmAndExp asmAndExp)
	{}

	void format(AsmBrExp asmBrExp)
	{}

	void format(AsmEqualExp asmEqualExp)
	{}

	void format(AsmExp asmExp)
	{}

	void format(AsmInstruction asmInstruction)
	{}

	void format(AsmLogAndExp asmLogAndExp)
	{}

	void format(AsmLogOrExp asmLogOrExp)
	{}

	void format(AsmMulExp asmMulExp)
	{}

	void format(AsmOrExp asmOrExp)
	{}

	void format(AsmPrimaryExp asmPrimaryExp)
	{}

	void format(AsmRelExp asmRelExp)
	{}

	void format(AsmShiftExp asmShiftExp)
	{}

	void format(AsmStatement asmStatement)
	{}

	void format(AsmTypePrefix asmTypePrefix)
	{}

	void format(AsmUnaExp asmUnaExp)
	{}

	void format(AsmXorExp asmXorExp)
	{}

	void format(AssertExpression assertExpression)
	{}

	void format(AssignExpression assignExpression)
	{}

	void format(AssocArrayLiteral assocArrayLiteral)
	{}

	void format(AtAttribute atAttribute)
	{}

	void format(Attribute attribute)
	{}

	void format(AttributeDeclaration attributeDeclaration)
	{}

	void format(AutoDeclaration autoDeclaration)
	{}

	void format(BlockStatement blockStatement)
	{}

	void format(BodyStatement bodyStatement)
	{}

	void format(BreakStatement breakStatement)
	{}

	void format(BaseClass baseClass)
	{}

	void format(BaseClassList baseClassList)
	{}

	void format(CaseRangeStatement caseRangeStatement)
	{}

	void format(CaseStatement caseStatement)
	{}

	void format(CastExpression castExpression)
	{}

	void format(CastQualifier castQualifier)
	{}

	void format(Catch catch_)
	{}

	void format(Catches catches)
	{}

	void format(ClassDeclaration classDeclaration)
	{}

	void format(CmpExpression cmpExpression)
	{}

	void format(CompileCondition compileCondition)
	{}

	void format(ConditionalDeclaration conditionalDeclaration)
	{}

	void format(ConditionalStatement conditionalStatement)
	{}

	void format(Constraint constraint)
	{}

	void format(Constructor constructor)
	{}

	void format(ContinueStatement continueStatement)
	{}

	void format(DebugCondition debugCondition)
	{}

	void format(DebugSpecification debugSpecification)
	{}

	void format(Declaration declaration)
	{}

	void format(DeclarationOrStatement declarationsOrStatement)
	{}

	void format(DeclarationsAndStatements declarationsAndStatements)
	{}

	void format(Declarator declarator)
	{}

	void format(DefaultStatement defaultStatement)
	{}

	void format(DeleteExpression deleteExpression)
	{}

	void format(DeleteStatement deleteStatement)
	{}

	void format(Deprecated deprecated_)
	{}

	void format(Destructor destructor)
	{}

	void format(DoStatement doStatement)
	{}

	void format(EnumBody enumBody)
	{}

	void format(EnumDeclaration enumDeclaration)
	{}

	void format(EnumMember enumMember)
	{}

	void format(EqualExpression equalExpression)
	{}

	void format(Expression expression)
	{}

	void format(ExpressionNode expressionNode)
	{}

	void format(ExpressionStatement expressionStatement)
	{}

	void format(FinalSwitchStatement finalSwitchStatement)
	{}

	void format(Finally finally_)
	{}

	void format(ForStatement forStatement)
	{}

	void format(ForeachStatement foreachStatement)
	{}

	void format(ForeachType foreachType)
	{}

	void format(ForeachTypeList foreachTypeList)
	{}

	void format(FunctionAttribute functionAttribute)
	{}

	void format(FunctionBody functionBody)
	{}

	void format(FunctionCallExpression functionCallExpression)
	{}

	void format(FunctionCallStatement functionCallStatement)
	{}

	void format(FunctionDeclaration functionDeclaration)
	{}

	void format(FunctionLiteralExpression functionLiteralExpression)
	{}

	void format(GotoStatement gotoStatement)
	{
		sink.put("goto ");
		if (gotoStatement.label != TokenType.invalid)
			sink.put(gotoStatement.label.value);
		else
			format(gotoStatement.expression);
		sink.put(";");
	}

	void format(IdentifierChain identifierChain)
	{
		bool first = true;
		foreach(ident; identifierChain.identifiers)
		{
			if (!first)
				sink.put(".");
			first = false;
			sink.put(ident.value);
		}
	}

	void format(IdentifierList identifierList)
	{}

	void format(IdentifierOrTemplateChain identifierOrTemplateChain)
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

	void format(IdentifierOrTemplateInstance identifierOrTemplateInstance)
	{
		if (identifierOrTemplateInstance.templateInstance !is null)
			format(identifierOrTemplateInstance.templateInstance);
		else
			sink.put(identifierOrTemplateInstance.identifier.value);

	}

	void format(IdentityExpression identityExpression)
	{}

	void format(IfStatement ifStatement)
	{}

	void format(ImportBind importBind)
	{}

	void format(ImportBindings importBindings)
	{}

	void format(ImportDeclaration importDeclaration)
	{}

	void format(ImportExpression importExpression)
	{}

	void format(IndexExpression indexExpression)
	{}

	void format(InExpression inExpression)
	{}

	void format(InStatement inStatement)
	{}

	void format(Initialize initialize)
	{}

	void format(Initializer initializer)
	{}

	void format(InterfaceDeclaration interfaceDeclaration)
	{}

	void format(Invariant invariant_)
	{}

	void format(IsExpression isExpression)
	{}

	void format(KeyValuePair keyValuePair)
	{}

	void format(KeyValuePairs keyValuePairs)
	{}

	void format(LabeledStatement labeledStatement)
	{}

	void format(LambdaExpression lambdaExpression)
	{}

	void format(LastCatch lastCatch)
	{}

	void format(LinkageAttribute linkageAttribute)
	{}

	void format(MemberFunctionAttribute memberFunctionAttribute)
	{}

	void format(MixinDeclaration mixinDeclaration)
	{}

	void format(MixinExpression mixinExpression)
	{}

	void format(MixinTemplateDeclaration mixinTemplateDeclaration)
	{}

	void format(MixinTemplateName mixinTemplateName)
	{}

	void format(Module module_)
	{}

	void format(ModuleDeclaration moduleDeclaration)
	{}

	void format(MulExpression mulExpression)
	{}

	void format(NewAnonClassExpression newAnonClassExpression)
	{}

	void format(NewExpression newExpression)
	{}

	void format(NonVoidInitializer nonVoidInitializer)
	{}

	void format(Operand operand)
	{}

	void format(Operands operands)
	{}

	void format(OrExpression orExpression)
	{}

	void format(OrOrExpression orOrExpression)
	{}

	void format(OutStatement outStatement)
	{}

	void format(Parameter parameter)
	{
		foreach (attribute; parameter.parameterAttributes)
		{
			sink.put(getTokenValue(attribute));
		}
		if (parameter.type !is null)
			format(parameter.type);
		if (parameter.vararg)
			sink.put("...");
		if (parameter.name.type != TokenType.invalid)
		{
			sink.put(" ");
			sink.put(parameter.name.value);
		}
	}

	void format(Parameters parameters)
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

	void format(Postblit postblit)
	{}

	void format(PostIncDecExpression postIncDecExpression)
	{}

	void format(PowExpression powExpression)
	{}

	void format(PragmaDeclaration pragmaDeclaration)
	{}

	void format(PragmaExpression pragmaExpression)
	{}

	void format(PreIncDecExpression preIncDecExpression)
	{}

	void format(PrimaryExpression primaryExpression)
	{}

	void format(Register register)
	{}

	void format(RelExpression relExpression)
	{}

	void format(ReturnStatement returnStatement)
	{}

	void format(ScopeGuardStatement scopeGuardStatement)
	{}

	void format(SharedStaticConstructor sharedStaticConstructor)
	{}

	void format(SharedStaticDestructor sharedStaticDestructor)
	{}

	void format(ShiftExpression shiftExpression)
	{}

	void format(SingleImport singleImport)
	{}

	void format(SliceExpression sliceExpression)
	{}

	void format(Statement statement)
	{}

	void format(StatementNoCaseNoDefault statementNoCaseNoDefault)
	{}

	void format(StaticAssertDeclaration staticAssertDeclaration)
	{}

	void format(StaticAssertStatement staticAssertStatement)
	{}

	void format(StaticConstructor staticConstructor)
	{}

	void format(StaticDestructor staticDestructor)
	{}

	void format(StaticIfCondition staticIfCondition)
	{}

	void format(StorageClass storageClass)
	{}

	void format(StructBody structBody)
	{}

	void format(StructDeclaration structDeclaration)
	{}

	void format(StructInitializer structInitializer)
	{}

	void format(StructMemberInitializer structMemberInitializer)
	{}

	void format(StructMemberInitializers structMemberInitializers)
	{}

	void format(SwitchStatement switchStatement)
	{}

	void format(Symbol symbol)
	{
        if (symbol.dot != TokenType.invalid)
            sink.put(".");
		format(symbol.identifierOrTemplateChain);
	}

	void format(SynchronizedStatement synchronizedStatement)
	{}

	void format(TemplateAliasParameter templateAliasParameter)
	{}

	void format(TemplateArgument templateArgument)
	{}

	void format(TemplateArgumentList templateArgumentList)
	{}

	void format(TemplateArguments templateArguments)
	{}

	void format(TemplateDeclaration templateDeclaration)
	{}

	void format(TemplateInstance templateInstance)
	{}

	void format(TemplateMixinExpression templateMixinExpression)
	{}

	void format(TemplateParameter templateParameter)
	{}

	void format(TemplateParameterList templateParameterList)
	{}

	void format(TemplateParameters templateParameters)
	{}

	void format(TemplateSingleArgument templateSingleArgument)
	{}

	void format(TemplateThisParameter templateThisParameter)
	{}

	void format(TemplateTupleParameter templateTupleParameter)
	{}

	void format(TemplateTypeParameter templateTypeParameter)
	{}

	void format(TemplateValueParameter templateValueParameter)
	{}

	void format(TemplateValueParameterDefault templateValueParameterDefault)
	{}

	void format(TernaryExpression ternaryExpression)
	{}

	void format(ThrowStatement throwStatement)
	{}

	void format(Token token)
	{
		sink.put(token.value);
	}

	void format(TraitsExpression traitsExpression)
	{}

	void format(TryStatement tryStatement)
	{}

	void format(Type type)
	{
        bool first = true;
        foreach (constructor; type.typeConstructors)
        {
            if (!first)
                sink.put(" ");
            first = false;
            sink.put(getTokenValue(constructor));
        }
        if (type.typeConstructors.length > 0)
            sink.put(" ");
		format(type.type2);
        foreach (suffix; type.typeSuffixes)
        {
			format(suffix);
        }
	}

	void format(Type2 type2)
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
        else if (type2.typeConstructor != TokenType.invalid)
        {
			sink.put(getTokenValue(type2.typeConstructor));
			sink.put("(");
			format(type2.type);
			sink.put(")");
			return;
        }
        else
            sink.put(getTokenValue(type2.builtinType));
	}

	void format(TypeSpecialization typeSpecialization)
	{}

	void format(TypeSuffix typeSuffix)
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

	void format(TypeidExpression typeidExpression)
	{}

	void format(TypeofExpression typeofExpression)
	{}

	void format(UnaryExpression unaryExpression)
	{}

	void format(UnionDeclaration unionDeclaration)
	{}

	void format(Unittest unittest_)
	{}

	void format(VariableDeclaration variableDeclaration)
	{}

	void format(Vector vector)
	{}

	void format(VersionCondition versionCondition)
	{}

	void format(VersionSpecification versionSpecification)
	{}

	void format(WhileStatement whileStatement)
	{}

	void format(WithStatement withStatement)
	{}

	void format(XorExpression xorExpression)
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
