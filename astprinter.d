import std.d.lexer;
import std.d.ast;
import std.stdio;

template tagAndAccept(string tagName)
{
	immutable tagAndAccept = `output.writeln("<` ~ tagName ~ `>");`
		~ tagName ~ `.accept(this);`
		~ `output.writeln("</` ~ tagName ~ `>");`;
}

class XMLPrinter : ASTVisitor
{
	override void visit(AddExpression addExpression)
	{
		output.writeln("<addExpression operator=\"", getTokenValue(addExpression.operator) ,"\">");
		output.writeln("<left>");
		addExpression.left.accept(this);
		output.writeln("</left>");
		if (addExpression.right !is null)
		{
			output.writeln("<right>");
			addExpression.right.accept(this);
			output.writeln("</right>");
		}
		output.writeln("</addExpression>");
	}

	override void visit(AliasDeclaration aliasDeclaration)
	{
		mixin (tagAndAccept!"aliasDeclaration");
	}

	override void visit(AliasInitializer aliasInitializer)
	{
		mixin (tagAndAccept!"aliasInitializer");
	}

	override void visit(AliasThisDeclaration aliasThisDeclaration)
	{
		mixin (tagAndAccept!"aliasThisDeclaration");
	}

	override void visit(AlignAttribute alignAttribute)
	{
		output.writeln("<alignAttribute align=\"", alignAttribute.intLiteral.value, "\">");
	}

	override void visit(AndAndExpression andAndExpression)
	{
		output.writeln("<andAndExpression>");
		output.writeln("<left>");
		andAndExpression.left.accept(this);
		output.writeln("<left>");
		if (andAndExpression.right !is null)
		{
			output.writeln("<right>");
			andAndExpression.right.accept(this);
			output.writeln("<right>");
		}
		output.writeln("</andAndExpression>");
	}

	override void visit(AndExpression andExpression)
	{
		output.writeln("<andExpression>");
		output.writeln("<left>");
		andExpression.left.accept(this);
		output.writeln("<left>");
		if (andExpression.right !is null)
		{
			output.writeln("<right>");
			andExpression.right.accept(this);
			output.writeln("<right>");
		}
		output.writeln("</andExpression>");
	}

	override void visit(ArgumentList argumentList)
	{
		mixin (tagAndAccept!"argumentList");
	}

	override void visit(Arguments arguments)
	{
		mixin (tagAndAccept!"arguments");
	}

	override void visit(ArrayInitializer arrayInitializer)
	{
		mixin (tagAndAccept!"arrayInitializer");
	}

	override void visit(ArrayLiteral arrayLiteral)
	{
		mixin (tagAndAccept!"arrayLiteral");
	}

	override void visit(ArrayMemberInitialization arrayMemberInitialization)
	{
		mixin (tagAndAccept!"arrayMemberInitialization");
	}

	override void visit(AssertExpression assertExpression)
	{
		output.writeln("<assertExpression>");
		output.writeln("<assertion>");
		assertExpression.assertion.accept(this);
		output.writeln("</assertion>");
		if (assertExpression.message !is null)
		{
			output.writeln("<message>");
			assertExpression.message.accept(this);
			output.writeln("</message>");
		}
		output.writeln("</assertExpression>");
	}

	override void visit(AssignExpression assignExpression)
	{
		if (assignExpression.assignExpression is null)
			output.writeln("<assignExpression>");
		else
			output.writeln("<assignExpression operator=\"",
				getTokenValue(assignExpression.operator), "\">");
		assignExpression.accept(this);
		output.writeln("</assignExpression>");
	}

	override void visit(AssocArrayLiteral assocArrayLiteral)
	{
		mixin (tagAndAccept!"assocArrayLiteral");
	}

	override void visit(AtAttribute atAttribute)
	{
		output.writeln("<atAttribute>");
		if (atAttribute.identifier.type == TokenType.invalid)
			atAttribute.accept(this);
		else
			output.writeln("<identifier>", atAttribute.identifier.value, "</identifier>");
		output.writeln("</atAttribute>");
	}

	override void visit(Attribute attribute)
	{
		output.writeln("<attribute>");
		if (attribute.attribute == TokenType.invalid)
			attribute.accept(this);
		else
			output.writeln(getTokenValue(attribute.attribute));
		output.writeln("</attribute>");
	}

	override void visit(AttributeDeclaration attributeDeclaration)
	{
		assert (attributeDeclaration !is null);
		mixin (tagAndAccept!"attributeDeclaration");
	}

	override void visit(AutoDeclaration autoDec)
	{
		output.writeln("<autoDeclaration>");
		for (size_t i = 0; i < autoDec.identifiers.length; i++)
		{
			output.writeln("<item>");
			output.writeln("<name line=\"", autoDec.identifiers[i].line, "\">",
				autoDec.identifiers[i].value, "</name>");
			visit(autoDec.initializers[i]);
			output.writeln("</item>");
		}
		output.writeln("</autoDeclaration>");
	}

	override void visit(BlockStatement blockStatement)
	{
		output.writeln("<blockStatement>");
		blockStatement.accept(this);
		output.writeln("</blockStatement>");
	}

	override void visit(BodyStatement bodyStatement)
	{
		output.writeln("<bodyStatement>");
		bodyStatement.accept(this);
		output.writeln("</bodyStatement>");
	}

	override void visit(BreakStatement breakStatement)
	{
		if (breakStatement.label.type == TokenType.invalid)
			output.writeln("<breakStatement>");
		else
			output.writeln("<breakStatement label=\"", breakStatement.label, "\">");
	}

	override void visit(BaseClass baseClass)
	{
		mixin (tagAndAccept!"baseClass");
	}

	override void visit(BaseClassList baseClassList)
	{
		mixin (tagAndAccept!"baseClassList");
	}

	override void visit(CaseRangeStatement caseRangeStatement)
	{
		output.writeln("<caseRangeStatement>");
		output.writeln("<low>");
		visit(caseRangeStatement.low);
		output.writeln("</low>");
		output.writeln("<high>");
		visit(caseRangeStatement.high);
		output.writeln("</high>");
		if (caseRangeStatement.declarationsAndStatements !is null)
			visit(caseRangeStatement.declarationsAndStatements);
		output.writeln("</caseRangeStatement>");
	}

	override void visit(CaseStatement caseStatement)
	{
		mixin (tagAndAccept!"caseStatement");
	}

	override void visit(CastExpression castExpression)
	{
		mixin (tagAndAccept!"castExpression");
	}

	override void visit(CastQualifier castQualifier)
	{
		mixin (tagAndAccept!"castQualifier");
	}

	override void visit(Catches catches)
	{
		mixin (tagAndAccept!"catches");
	}

	override void visit(Catch catch_)
	{
		output.writeln("<catch>");
		catch_.accept(this);
		output.writeln("</catch>");
	}

	override void visit(ClassDeclaration classDec)
	{
		output.writeln("<classDeclaration line=\"", classDec.name.line, "\">");
		output.writeln("<name>", classDec.name.value, "</name>");
		classDec.accept(this);
		output.writeln("</classDeclaration>");
	}

	override void visit(CmpExpression cmpExpression)
	{
		mixin (tagAndAccept!"cmpExpression");
	}

	override void visit(CompileCondition compileCondition)
	{
		mixin (tagAndAccept!"compileCondition");
	}

	override void visit(ConditionalDeclaration conditionalDeclaration)
	{
		output.writeln("<conditionalDeclaration>");
		visit(conditionalDeclaration.compileCondition);
		output.writeln("<trueDeclaration>");
		visit(conditionalDeclaration.trueDeclaration);
		output.writeln("</trueDeclaration>");
		if (conditionalDeclaration.falseDeclaration !is null)
		{
			output.writeln("<falseDeclaration>");
			visit(conditionalDeclaration.falseDeclaration);
			output.writeln("</falseDeclaration>");
		}
		output.writeln("</conditionalDeclaration>");
	}

	override void visit(ConditionalStatement conditionalStatement)
	{
		output.writeln("<conditionalStatement>");
		visit(conditionalStatement.compileCondition);
		output.writeln("<trueStatement>");
		visit(conditionalStatement.trueStatement);
		output.writeln("</trueStatement>");
		if (conditionalStatement.falseStatement !is null)
		{
			output.writeln("<falseStatement>");
			visit(conditionalStatement.falseStatement);
			output.writeln("</falseStatement>");
		}
		output.writeln("</conditionalStatement>");
	}

	override void visit(Constraint constraint)
	{
		output.writeln("<constraint>");
		constraint.accept(this);
		output.writeln("</constraint>");
	}

	override void visit(Constructor constructor)
	{
		mixin (tagAndAccept!"constructor");
	}

	override void visit(ContinueStatement continueStatement)
	{
		if (continueStatement.label.type == TokenType.invalid)
			output.writeln("<continueStatement/>");
		else
			output.writeln("<continueStatement label=\"",
				continueStatement.label, "\"/>");
	}

	override void visit(DebugCondition debugCondition)
	{
		if (debugCondition.identifierOrInteger.type == TokenType.invalid)
			output.writeln("<debugCondition/>");
		else
			output.writeln("<debugCondition condition=\"",
				debugCondition.identifierOrInteger.value, "\"/>");
	}

	override void visit(DebugSpecification debugSpecification)
	{
		if (debugSpecification.identifierOrInteger.type == TokenType.invalid)
			output.writeln("<debugSpecification/>");
		else
			output.writeln("<debugSpecification condition=\"",
				debugSpecification.identifierOrInteger.value, "\"/>");
	}

	override void visit(Declaration declaration)
	{
		mixin (tagAndAccept!"declaration");
	}

	override void visit(DeclarationsAndStatements declarationsAndStatements)
	{
		mixin (tagAndAccept!"declarationsAndStatements");
	}

	override void visit(DeclarationOrStatement declarationOrStatement)
	{
		mixin (tagAndAccept!"declarationOrStatement");
	}

	override void visit(Declarator declarator)
	{
		output.writeln("<declarator line=\"", declarator.name.line, "\">");
		output.writeln("<name>", declarator.name.value, "</name>");
		declarator.accept(this);
		output.writeln("</declarator>");
	}

	override void visit(DefaultStatement defaultStatement)
	{
		mixin (tagAndAccept!"defaultStatement");
	}

	override void visit(DeleteExpression deleteExpression)
	{
		mixin (tagAndAccept!"deleteExpression");
	}

	override void visit(DeleteStatement deleteStatement)
	{
		mixin (tagAndAccept!"deleteStatement");
	}

	override void visit(Deprecated deprecated_)
	{
		if (deprecated_.assignExpression !is null)
		{
			output.writeln("<deprecated>");
			deprecated_.accept(this);
			output.writeln("</deprecated>");
		}
		else
			output.writeln("<deprecated/>");
	}

	override void visit(Destructor destructor)
	{
		mixin (tagAndAccept!"destructor");
	}

	override void visit(DoStatement doStatement)
	{
		mixin (tagAndAccept!"doStatement");
	}

	override void visit(EnumBody enumBody)
	{
		mixin (tagAndAccept!"enumBody");
	}

	override void visit(EnumDeclaration enumDec)
	{
		output.writeln("<enumDeclaration line=\"", enumDec.name.line, "\">");
		if (enumDec.name.type == TokenType.identifier)
			output.writeln("<name>", enumDec.name.value, "</name>");
		enumDec.accept(this);
		output.writeln("</enumDeclaration>");
	}

	override void visit(EnumMember enumMem)
	{
		output.writeln("<enumMember line=\"", enumMem.name.line, "\">");
		enumMem.accept(this);
		output.writeln("</enumMember>");
	}

	override void visit(EqualExpression equalExpression)
	{
		output.writeln("<enumMember operator=\"", getTokenValue(equalExpression.operator), "\">");
		output.writeln("<left>");
		visit(equalExpression.left);
		output.writeln("</left>");
		output.writeln("<right>");
		visit(equalExpression.right);
		output.writeln("</right>");
		output.writeln("</enumMember>");
	}

	override void visit(Expression expression)
	{
		output.writeln("<expression>");
		expression.accept(this);
		output.writeln("</expression>");
	}

	override void visit(ExpressionStatement expressionStatement)
	{
		output.writeln("<expressionStatement>");
		expressionStatement.accept(this);
		output.writeln("</expressionStatement>");
	}

	override void visit(FinalSwitchStatement finalSwitchStatement)
	{
		output.writeln("<finalSwitchStatement>");
		finalSwitchStatement.accept(this);
		output.writeln("</finalSwitchStatement>");
	}

	override void visit(Finally finally_)
	{
		output.writeln("<finally>");
		finally_.accept(this);
		output.writeln("</finally>");
	}

	override void visit(ForStatement forStatement)
	{
		output.writeln("<forStatement>");
		output.writeln("<initialize>");
		visit(forStatement.declarationOrStatement);
		output.writeln("</initialize>");
		if (forStatement.test !is null)
		{
			output.writeln("<test>");
			visit(forStatement.test);
			output.writeln("</test>");
		}
		if (forStatement.increment !is null)
		{
			output.writeln("<increment>");
			visit(forStatement.increment);
			output.writeln("</increment>");
		}
		visit(forStatement.statementNoCaseNoDefault);
		output.writeln("</forStatement>");
	}

	override void visit(ForeachStatement foreachStatement)
	{
		output.writeln("<foreachStatement type=\"", getTokenValue(
			foreachStatement.foreachType), "\">");
		output.writeln("</foreachStatement>");
	}

	override void visit(ForeachType foreachType)
	{
		output.writeln("<foreachType>");
		foreach (constructor; foreachType.typeConstructors)
		{
			output.writeln("<typeConstructor>", getTokenValue(constructor), "</typeConstructor>");
		}
		if (foreachType.type !is null)
			visit(foreachType.type);
		visit(foreachType.identifier);
		output.writeln("</foreachType>");

	}

	override void visit(ForeachTypeList foreachTypeList)
	{
		mixin (tagAndAccept!"foreachTypeList");
	}

	override void visit(FunctionAttribute functionAttribute)
	{
		mixin (tagAndAccept!"functionAttribute");
	}

	override void visit(FunctionBody functionBody)
	{
		mixin (tagAndAccept!"functionBody");
	}

	override void visit(FunctionCallExpression functionCallExpression)
	{
		mixin (tagAndAccept!"functionCallExpression");
	}

	override void visit(FunctionCallStatement functionCallStatement)
	{
		mixin (tagAndAccept!"functionCallStatement");
	}

	override void visit(FunctionDeclaration functionDec)
	{
		output.writeln("<functionDeclaration line=\"", functionDec.name.line, "\">");
		output.writeln("<name>", functionDec.name.value, "</name>");
		if (functionDec.hasAuto)
			output.writeln("<auto/>");
		if (functionDec.hasRef)
			output.writeln("<ref/>");
		functionDec.accept(this);
		output.writeln("</functionDeclaration>");
	}

	override void visit(FunctionLiteralExpression functionLiteralExpression)
	{
		output.writeln("<functionLiteralExpression type=\"",
			getTokenValue(functionLiteralExpression.functionOrDelegate), "\">");
		functionLiteralExpression.accept(this);
		output.writeln("</functionLiteralExpression>");
	}

	override void visit(GotoStatement gotoStatement)
	{
		if (gotoStatement.token.type == TokenType.default_)
			output.writeln("<gotoStatement default=\"true\"/>");
		else if (gotoStatement.token.type == TokenType.identifier)
			output.writeln("<gotoStatement label=\"", gotoStatement.token.value, "\">");
		else
		{
			output.writeln("<gotoStatement>");
			output.writeln("<case>");
			visit(gotoStatement.expression);
			output.writeln("</case>");
			output.writeln("</gotoStatement>");
		}
	}

	override void visit(IdentifierChain chain)
	{
		output.writeln("<identifierChain>");
		chain.accept(this);
		output.writeln("</identifierChain>");
	}

	override void visit(IdentifierList list)
	{
		output.writeln("<identifierList>");
		list.accept(this);
		output.writeln("</identifierList>");
	}

	override void visit(IdentifierOrTemplateChain identifierOrTemplateChain)
	{
		mixin (tagAndAccept!"identifierOrTemplateChain");
	}

	override void visit(IdentifierOrTemplateInstance identifierOrTemplateInstance)
	{
		mixin (tagAndAccept!"identifierOrTemplateInstance");
	}

	override void visit(IdentityExpression identityExpression)
	{
		if (identityExpression.negated)
			output.writeln("<identityExpression operator=\"!is\">");
		else
			output.writeln("<identityExpression operator=\"is\">");
		output.writeln("<left>");
		visit(identityExpression.left);
		output.writeln("</left>");
		output.writeln("<right>");
		visit(identityExpression.right);
		output.writeln("</right>");
		output.writeln("</identityExpression>");
	}

	override void visit(IfStatement ifStatement)
	{
		output.writeln("<ifStatement>");

		output.writeln("<condition>");
		if (ifStatement.identifier.type != TokenType.invalid)
		{
			if (ifStatement.type is null)
				output.writeln("<auto/>");
			else
				visit(ifStatement.type);
			visit(ifStatement.identifier);
		}
		visit(ifStatement.expression);
		output.writeln("</condition>");

		output.writeln("<then>");
		visit(ifStatement.thenStatement);
		output.writeln("</then>");

		if (ifStatement.elseStatement !is null)
		{
			output.writeln("<else>");
			visit(ifStatement.elseStatement);
			output.writeln("</else>");
		}
		output.writeln("</ifStatement>");
	}

	override void visit(ImportBind importBind)
	{
		if (importBind.right.type == TokenType.invalid)
			output.writeln("<importBind symbol=\"", importBind.left, "\">");
		else
			output.writeln("<importBind symbol=\"", importBind.right,
				"\" rename=\"", importBind.left, "\">");
	}

	override void visit(ImportBindings importBindings)
	{
		mixin (tagAndAccept!"importBindings");
	}

	override void visit(ImportDeclaration importDeclaration)
	{
		mixin (tagAndAccept!"importDeclaration");
	}

	override void visit(ImportExpression importExpression)
	{
		mixin (tagAndAccept!"importExpression");
	}

	override void visit(IndexExpression indexExpression)
	{
		mixin (tagAndAccept!"indexExpression");
	}

	override void visit(InExpression inExpression)
	{
		if (inExpression.negated)
			output.writeln("<inExpression operator=\"!in\">");
		else
			output.writeln("<inExpression operator=\"in\">");
		output.writeln("<left>");
		visit(inExpression.left);
		output.writeln("</left>");
		output.writeln("<right>");
		visit(inExpression.right);
		output.writeln("</right>");
		output.writeln("</inExpression>");
	}

	override void visit(InStatement inStatement)
	{
		mixin (tagAndAccept!"inStatement");
	}

	override void visit(Initialize initialize)
	{
		if (initialize.statementNoCaseNoDefault is null)
			output.writeln("<initialize/>");
		else
		{
			output.writeln("<initialize>");
			visit(initialize.statementNoCaseNoDefault);
			output.writeln("</initialize>");
		}
	}

	override void visit(Initializer initializer)
	{
		if (initializer.nonVoidInitializer is null)
			output.writeln("<initializer void=\"true\"/>");
		else
		{
			output.writeln("<initializer>");
			visit(initializer.nonVoidInitializer);
			output.writeln("</initializer>");
		}
	}

	override void visit(InterfaceDeclaration interfaceDec)
	{
		output.writeln("<interfaceDeclaration line=\"", interfaceDec.name.line, "\">");
		output.writeln("<name>", interfaceDec.name.value, "</name>");
		interfaceDec.accept(this);
		output.writeln("</interfaceDeclaration>");
	}

	override void visit(Invariant invariant_)
	{
		output.writeln("<invariant>");
		invariant_.accept(this);
		output.writeln("</invariant>");
	}

	override void visit(IsExpression isExpression)
	{
		output.writeln("<isExpression>");
		visit(isExpression.type);
		if (isExpression.identifier.type != TokenType.invalid)
			visit(isExpression.identifier);
		if (isExpression.typeSpecialization !is null)
		{
			if (isExpression.equalsOrColon == TokenType.colon)
				output.writeln("<colon/>");
			else
				output.writeln("<equals/>");
			visit(isExpression.typeSpecialization);
			if (isExpression.templateParameterList !is null)
				visit(isExpression.templateParameterList);
		}
		output.writeln("</isExpression>");
	}

	override void visit(KeyValuePair keyValuePair)
	{
		output.writeln("<keyValuePair>");
		output.writeln("<key>");
		visit(keyValuePair.key);
		output.writeln("</key>");
		output.writeln("<value>");
		visit(keyValuePair.value);
		output.writeln("<value>");
		output.writeln("</keyValuePair>");
	}

	override void visit(KeyValuePairs keyValuePairs)
	{
		mixin (tagAndAccept!"keyValuePairs");
	}

	override void visit (LabeledStatement labeledStatement)
	{
		output.writeln("<labeledStatement label=\"",
			labeledStatement.identifier.value ,"\">");
		visit(labeledStatement.declarationOrStatement);
		output.writeln("</labeledStatement>");
	}

	override void visit(LambdaExpression lambdaExpression)
	{
		output.writeln("<lambdaExpression>");
		if (lambdaExpression.functionType == TokenType.function_)
			output.writeln("<function/>");
		if (lambdaExpression.functionType == TokenType.delegate_)
			output.writeln("<delegate/>");
		lambdaExpression.accept(this);
		output.writeln("</lambdaExpression>");
	}

	override void visit(LastCatch lastCatch)
	{
		mixin (tagAndAccept!"lastCatch");
	}

	override void visit(LinkageAttribute linkageAttribute)
	{
		if (linkageAttribute.hasPlusPlus)
			output.writeln("<linkageAttribute linkage=\"c++\"/>");
		else
			output.writeln("<linkageAttribute linkage=\"",
				linkageAttribute.identifier.value, "\"/>");
	}

	override void visit(MemberFunctionAttribute memberFunctionAttribute)
	{
		output.writeln("<memberFunctionAttribute>");
		if (memberFunctionAttribute.atAttribute is null)
			output.writeln(getTokenValue(memberFunctionAttribute.tokenType));
		else
			memberFunctionAttribute.accept(this);
		output.writeln("</memberFunctionAttribute>");
	}

	override void visit(MixinDeclaration mixinDeclaration)
	{
		mixin (tagAndAccept!"mixinDeclaration");
	}

	override void visit(MixinExpression mixinExpression)
	{
		mixin (tagAndAccept!"mixinExpression");
	}

	override void visit(MixinTemplateDeclaration mixinTemplateDeclaration)
	{
		mixin (tagAndAccept!"mixinTemplateDeclaration");
	}

	override void visit(MixinTemplateName mixinTemplateName)
	{
		mixin (tagAndAccept!"mixinTemplateName");
	}

	override void visit(Module module_)
	{
		output.writeln("<module>");
		module_.accept(this);
		output.writeln("</module>");
	}

	override void visit(ModuleDeclaration moduleDeclaration)
	{
		mixin (tagAndAccept!"moduleDeclaration");
	}

	override void visit(MulExpression mulExpression)
	{
		output.writeln("<mulExpression operator=\"", getTokenValue(mulExpression.operator) ,"\">");
		output.writeln("<left>");
		mulExpression.left.accept(this);
		output.writeln("</left>");
		if (mulExpression.right !is null)
		{
			output.writeln("<right>");
			mulExpression.right.accept(this);
			output.writeln("</right>");
		}
		output.writeln("</mulExpression>");
	}

	override void visit(NewAnonClassExpression newAnonClassExpression)
	{
		mixin (tagAndAccept!"newAnonClassExpression");
	}

	override void visit(NewExpression newExpression)
	{
		mixin (tagAndAccept!"newExpression");
	}

	override void visit(StatementNoCaseNoDefault statementNoCaseNoDefault)
	{
		mixin (tagAndAccept!"statementNoCaseNoDefault");
	}

	override void visit(NonVoidInitializer nonVoidInitializer)
	{
		mixin (tagAndAccept!"nonVoidInitializer");
	}

	override void visit(OrExpression orExpression)
	{
		mixin(tagAndAccept!"orExpression");
	}

	override void visit(OrExpression orOrExpression)
	{
		mixin(tagAndAccept!"orOrExpression");
	}

	/***************************************************************************
	 * BOOKMARK
	 **************************************************************************/

	override void visit(Parameters parameters)
	{
		mixin (tagAndAccept!"parameters");
	}

	override void visit(Parameter param)
	{
		output.writeln("<parameter>");
		if (param.name.type == TokenType.identifier)
			output.writeln("<name>", param.name.value, "</name>");
		foreach (attribute; param.parameterAttributes)
		{
			output.writeln("<parameterAttribute>", getTokenValue(attribute), "</parameterAttribute>");
		}
		param.accept(this);
		if (param.vararg)
			output.writeln("<vararg/>");
		output.writeln("</parameter>");
	}

	override void visit(SingleImport singleImport)
	{
		if (singleImport.rename.type == TokenType.invalid)
			output.writeln("<singleImport>");
		else
			output.writeln("<singleImport rename=\"", singleImport.rename.value, "\">");
		visit(singleImport.identifierChain);
		output.writeln("</singleImport>");
	}

	override void visit(StructDeclaration structDec)
	{
		output.writeln("<structDeclaration line=\"", structDec.name.line, "\">");
		output.writeln("<name>", structDec.name.value, "</name>");
		structDec.accept(this);
		output.writeln("</structDeclaration>");
	}

	override void visit(Token token)
	{
		string tagName;
		with (TokenType) switch (token.type)
		{
		case identifier: tagName = "identifier"; break;
		case doubleLiteral: tagName = "doubleLiteral"; break;
		case idoubleLiteral: tagName = "idoubleLiteral"; break;
		case floatLiteral: tagName = "floatLiteral"; break;
		case ifloatLiteral: tagName = "ifloatLiteral"; break;
		case intLiteral: tagName = "intLiteral"; break;
		case uintLiteral: tagName = "uintLiteral"; break;
		case longLiteral: tagName = "longLiteral"; break;
		case ulongLiteral: tagName = "ulongLiteral"; break;
		case realLiteral: tagName = "realLiteral"; break;
		case irealLiteral: tagName = "irealLiteral"; break;
		case characterLiteral: tagName = "characterLiteral"; break;
		case stringLiteral: tagName = "stringLiteral"; break;
		case dstringLiteral: tagName = "dstringLiteral"; break;
		case wstringLiteral: tagName = "wstringLiteral"; break;
		default: tagName = "token";
		}
		output.writeln("<", tagName, "><![CDATA[", token.value, "]]></", tagName, ">");
	}

	override void visit(Type type)
	{
		output.writeln("<type pretty=\"", type.toString(), "\">");
		type.accept(this);
		output.writeln("</type>");
	}

	override void visit(Unittest unittest_)
	{
		output.writeln("<unittest>");
		unittest_.accept(this);
		output.writeln("</unittest>");
	}

	override void visit(VariableDeclaration variableDeclaration)
	{
		mixin (tagAndAccept!"variableDeclaration");
	}

	alias ASTVisitor.visit visit;

	File output;
}
