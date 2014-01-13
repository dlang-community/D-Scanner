//          Copyright Brian Schott (Sir Alaran) 2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

import stdx.d.lexer;
import stdx.d.ast;
import std.stdio;
import std.string;
import std.array;
import formatter;

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
		output.writeln("<addExpression operator=\"", str(addExpression.operator) ,"\">");
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
		output.writeln("<alignAttribute align=\"", alignAttribute.intLiteral.text, "\">");
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
				str(assignExpression.operator), "\">");
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
		if (atAttribute.identifier.type == tok!"")
			atAttribute.accept(this);
		else
			output.writeln("<identifier>", atAttribute.identifier.text, "</identifier>");
		output.writeln("</atAttribute>");
	}

	override void visit(Attribute attribute)
	{
		output.writeln("<attribute>");
		if (attribute.attribute == tok!"")
			attribute.accept(this);
		else
			output.writeln(str(attribute.attribute));
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
				autoDec.identifiers[i].text, "</name>");
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
		if (breakStatement.label.type == tok!"")
			output.writeln("<breakStatement/>");
		else
			output.writeln("<breakStatement label=\"", breakStatement.label.text, "\"/>");
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
		output.writeln("<name>", classDec.name.text, "</name>");
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
		output.writeln("<trueDeclarations>");
		foreach (dec; conditionalDeclaration.trueDeclarations)
			visit(dec);
		output.writeln("</trueDeclarations>");
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
		if (continueStatement.label.type == tok!"")
			output.writeln("<continueStatement/>");
		else
			output.writeln("<continueStatement label=\"",
				continueStatement.label.text, "\"/>");
	}

	override void visit(DebugCondition debugCondition)
	{
		if (debugCondition.identifierOrInteger.type == tok!"")
			output.writeln("<debugCondition/>");
		else
			output.writeln("<debugCondition condition=\"",
				debugCondition.identifierOrInteger.text, "\"/>");
	}

	override void visit(DebugSpecification debugSpecification)
	{
		if (debugSpecification.identifierOrInteger.type == tok!"")
			output.writeln("<debugSpecification/>");
		else
			output.writeln("<debugSpecification condition=\"",
				debugSpecification.identifierOrInteger.text, "\"/>");
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
		output.writeln("<name>", declarator.name.text, "</name>");
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
        writeDdoc(enumDec.comment);
		if (enumDec.name.type == tok!"identifier")
			output.writeln("<name>", enumDec.name.text, "</name>");
		enumDec.accept(this);
		output.writeln("</enumDeclaration>");
	}

	override void visit(EnumMember enumMem)
	{
		output.writeln("<enumMember line=\"", enumMem.name.line, "\">");
        writeDdoc(enumMem.comment);
		enumMem.accept(this);
		output.writeln("</enumMember>");
	}

	override void visit(EqualExpression equalExpression)
	{
		output.writeln("<enumMember operator=\"", str(equalExpression.operator), "\">");
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
		if (forStatement.declarationOrStatement !is null)
		{
			output.writeln("<initialization>");
			visit(forStatement.initialization);
			output.writeln("</initialization>");
		}
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
		visit(forStatement.declarationOrStatement);
		output.writeln("</forStatement>");
	}

	override void visit(ForeachStatement foreachStatement)
	{
		output.writeln("<foreachStatement type=\"", str(
			foreachStatement.type), "\">");
		if (foreachStatement.foreachType !is null)
			visit(foreachStatement.foreachType);
		if (foreachStatement.foreachTypeList !is null)
			visit(foreachStatement.foreachTypeList);
		output.writeln("<low>");
		visit(foreachStatement.low);
		output.writeln("</low>");
		if (foreachStatement.high !is null)
		{
			output.writeln("<high>");
			visit(foreachStatement.high);
			output.writeln("</high>");
		}
		visit(foreachStatement.declarationOrStatement);
		output.writeln("</foreachStatement>");
	}

	override void visit(ForeachType foreachType)
	{
		output.writeln("<foreachType>");
		foreach (constructor; foreachType.typeConstructors)
		{
			output.writeln("<typeConstructor>", str(constructor), "</typeConstructor>");
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
		output.writeln("<name>", functionDec.name.text, "</name>");
        writeDdoc(functionDec.comment);
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
			str(functionLiteralExpression.functionOrDelegate), "\">");
		functionLiteralExpression.accept(this);
		output.writeln("</functionLiteralExpression>");
	}

	override void visit(GotoStatement gotoStatement)
	{
		if (gotoStatement.label.type == tok!"default")
			output.writeln("<gotoStatement default=\"true\"/>");
		else if (gotoStatement.label.type == tok!"identifier")
			output.writeln("<gotoStatement label=\"", gotoStatement.label.text, "\"/>");
		else
		{
			output.writeln("<gotoStatement>");
			output.writeln("<case>");
			visit(gotoStatement.expression);
			output.writeln("</case>");
			output.writeln("</gotoStatement>");
		}
	}

	override void visit(IdentifierChain identifierChain)
	{
		mixin (tagAndAccept!"identifierChain");
	}

	override void visit(IdentifierList identifierList)
	{
		mixin (tagAndAccept!"identifierList");
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
		if (ifStatement.identifier.type != tok!"")
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
		if (importBind.right.type == tok!"")
			output.writeln("<importBind symbol=\"", importBind.left.text, "\">");
		else
			output.writeln("<importBind symbol=\"", importBind.right.text,
				"\" rename=\"", importBind.left.text, "\">");
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
		output.writeln("<name>", interfaceDec.name.text, "</name>");
        writeDdoc(interfaceDec.comment);
		interfaceDec.accept(this);
		output.writeln("</interfaceDeclaration>");
	}

	override void visit(Invariant invariant_)
	{
		output.writeln("<invariant>");
        writeDdoc(invariant_.comment);
		invariant_.accept(this);
		output.writeln("</invariant>");
	}

	override void visit(IsExpression isExpression)
	{
		output.writeln("<isExpression>");
		visit(isExpression.type);
		if (isExpression.identifier.type != tok!"")
			visit(isExpression.identifier);
		if (isExpression.typeSpecialization !is null)
		{
			if (isExpression.equalsOrColon == tok!":")
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
			labeledStatement.identifier.text ,"\">");
		visit(labeledStatement.declarationOrStatement);
		output.writeln("</labeledStatement>");
	}

	override void visit(LambdaExpression lambdaExpression)
	{
		output.writeln("<lambdaExpression>");
		if (lambdaExpression.functionType == tok!"function")
			output.writeln("<function/>");
		if (lambdaExpression.functionType == tok!"delegate")
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
				linkageAttribute.identifier.text, "\"/>");
	}

	override void visit(MemberFunctionAttribute memberFunctionAttribute)
	{
		output.writeln("<memberFunctionAttribute>");
		if (memberFunctionAttribute.atAttribute is null)
			output.writeln(str(memberFunctionAttribute.tokenType));
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
        output.writeln("<?xml version=\"1.0\"?>");
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
		output.writeln("<mulExpression operator=\"", str(mulExpression.operator) ,"\">");
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
		mixin (tagAndAccept!"orExpression");
	}

	override void visit(OrOrExpression orOrExpression)
	{
		mixin (tagAndAccept!"orOrExpression");
	}

	override void visit(OutStatement outStatement)
	{
		mixin (tagAndAccept!"outStatement");
	}

	override void visit(Parameter param)
	{
		output.writeln("<parameter>");
		if (param.name.type == tok!"identifier")
			output.writeln("<name>", param.name.text, "</name>");
		foreach (attribute; param.parameterAttributes)
		{
			output.writeln("<parameterAttribute>", str(attribute), "</parameterAttribute>");
		}
		param.accept(this);
		if (param.vararg)
			output.writeln("<vararg/>");
		output.writeln("</parameter>");
	}

	override void visit(Parameters parameters)
	{
		mixin (tagAndAccept!"parameters");
	}

	override void visit(Postblit postblit)
	{
		mixin (tagAndAccept!"postblit");
	}

	override void visit(PostIncDecExpression postIncDecExpression)
	{
		output.writeln("<postIncDecExpression operator=\"",
			str(postIncDecExpression.operator), "\">");
		postIncDecExpression.accept(this);
		output.writeln("</postIncDecExpression>");
	}

	override void visit(PowExpression powExpression)
	{
		output.writeln("<powExpression>");
		output.writeln("<left>");
		powExpression.left.accept(this);
		output.writeln("</left>");
		if (powExpression.right !is null)
		{
			output.writeln("<right>");
			powExpression.right.accept(this);
			output.writeln("</right>");
		}
		output.writeln("</powExpression>");
	}

	override void visit(PragmaDeclaration pragmaDeclaration)
	{
		mixin (tagAndAccept!"pragmaDeclaration");
	}

	override void visit(PragmaExpression pragmaExpression)
	{
		mixin (tagAndAccept!"pragmaExpression");
	}

	override void visit(PreIncDecExpression preIncDecExpression)
	{
		output.writeln("<preIncDecExpression operator=\"",
			str(preIncDecExpression.operator), "\">");
		preIncDecExpression.accept(this);
		output.writeln("</preIncDecExpression>");
	}

	override void visit(PrimaryExpression primaryExpression)
	{
		mixin (tagAndAccept!"primaryExpression");
	}

	// TODO: Register

	override void visit(RelExpression relExpression)
	{
		output.writeln("<relExpression operator=\"",
			xmlEscape(str(relExpression.operator)), "\">");
		output.writeln("<left>");
		visit(relExpression.left);
		output.writeln("</left>");
		output.writeln("<right>");
		visit(relExpression.right);
		output.writeln("</right>");
		output.writeln("</relExpression>");
	}

	override void visit(ReturnStatement returnStatement)
	{
		if (returnStatement.expression is null)
			output.writeln("<returnStatement/>");
		else
		{
			output.writeln("<returnStatement>");
			returnStatement.accept(this);
			output.writeln("</returnStatement>");
		}
	}

	override void visit(ScopeGuardStatement scopeGuardStatement)
	{
		mixin (tagAndAccept!"scopeGuardStatement");
	}

	override void visit(SharedStaticConstructor sharedStaticConstructor)
	{
		mixin (tagAndAccept!"sharedStaticConstructor");
	}

	override void visit(SharedStaticDestructor sharedStaticDestructor)
	{
		mixin (tagAndAccept!"sharedStaticDestructor");
	}

	override void visit(ShiftExpression shiftExpression)
	{
		output.writeln("<shiftExpression operator=\"",
			xmlEscape(str(shiftExpression.operator)), "\">");
		output.writeln("<left>");
		visit(shiftExpression.left);
		output.writeln("</left>");
		output.writeln("<right>");
		visit(shiftExpression.right);
		output.writeln("</right>");
		output.writeln("</shiftExpression>");
	}

	override void visit(SingleImport singleImport)
	{
		if (singleImport.rename.type == tok!"")
			output.writeln("<singleImport>");
		else
			output.writeln("<singleImport rename=\"", singleImport.rename.text, "\">");
		visit(singleImport.identifierChain);
		output.writeln("</singleImport>");
	}

	override void visit(SliceExpression sliceExpression)
	{
		output.writeln("<sliceExpression>");
		if (sliceExpression.unaryExpression is null)
		{
			output.writeln("<low");
			visit(sliceExpression.lower);
			output.writeln("</low");
			output.writeln("<high");
			visit(sliceExpression.upper);
			output.writeln("</high");
		}
		else
			visit(sliceExpression.unaryExpression);
		output.writeln("<sliceExpression>");
	}

	override void visit(Statement statement)
	{
		mixin (tagAndAccept!"statement");
	}

	override void visit(StaticAssertDeclaration staticAssertDeclaration)
	{
		mixin (tagAndAccept!"staticAssertDeclaration");
	}

	override void visit(StaticAssertStatement staticAssertStatement)
	{
		mixin (tagAndAccept!"staticAssertStatement");
	}

	override void visit(StaticConstructor staticConstructor)
	{
		mixin (tagAndAccept!"staticConstructor");
	}

	override void visit(StaticDestructor staticDestructor)
	{
		mixin (tagAndAccept!"staticDestructor");
	}

	override void visit(StaticIfCondition staticIfCondition)
	{
		mixin (tagAndAccept!"staticIfCondition");
	}

	override void visit(StorageClass storageClass)
	{
		mixin (tagAndAccept!"storageClass");
	}

	override void visit(StructBody structBody)
	{
		mixin (tagAndAccept!"structBody");
	}

	override void visit(StructDeclaration structDec)
	{
		output.writeln("<structDeclaration line=\"", structDec.name.line, "\">");
		output.writeln("<name>", structDec.name.text, "</name>");
		structDec.accept(this);
		output.writeln("</structDeclaration>");
	}

	override void visit(StructInitializer structInitializer)
	{
		mixin (tagAndAccept!"structInitializer");
	}

	override void visit(StructMemberInitializer structMemberInitializer)
	{
		mixin (tagAndAccept!"structMemberInitializer");
	}

	override void visit(StructMemberInitializers structMemberInitializers)
	{
		mixin (tagAndAccept!"structMemberInitializers");
	}

	override void visit(SwitchStatement switchStatement)
	{
		mixin (tagAndAccept!"switchStatement");
	}

	override void visit(Symbol symbol)
	{
		mixin (tagAndAccept!"symbol");
	}

	override void visit(SynchronizedStatement synchronizedStatement)
	{
		mixin (tagAndAccept!"synchronizedStatement");
	}

	override void visit(TemplateAliasParameter templateAliasParameter)
	{
		output.writeln("<templateAliasParameter>");
		if (templateAliasParameter.type !is null)
			visit(templateAliasParameter.type);
		visit(templateAliasParameter.identifier);
		if (templateAliasParameter.colonExpression !is null)
		{
			output.writeln("<specialization>");
			visit(templateAliasParameter.colonExpression);
			output.writeln("</specialization>");
		}
		else if (templateAliasParameter.colonType !is null)
		{
			output.writeln("<specialization>");
			visit(templateAliasParameter.colonType);
			output.writeln("</specialization>");
		}

		if (templateAliasParameter.assignExpression !is null)
		{
			output.writeln("<default>");
			visit(templateAliasParameter.assignExpression);
			output.writeln("</default>");
		}
		else if (templateAliasParameter.assignType !is null)
		{
			output.writeln("<default>");
			visit(templateAliasParameter.assignType);
			output.writeln("</default>");
		}

		output.writeln("</templateAliasParameter>");
	}

	override void visit(TemplateArgument templateArgument)
	{
		mixin (tagAndAccept!"templateArgument");
	}

	override void visit(TemplateArgumentList templateArgumentList)
	{
		mixin (tagAndAccept!"templateArgumentList");
	}

	override void visit(TemplateArguments templateArguments)
	{
		mixin (tagAndAccept!"templateArguments");
	}

	override void visit (EponymousTemplateDeclaration eponymousTemplateDeclaration)
	{
		mixin (tagAndAccept!"eponymousTemplateDeclaration");
	}

	override void visit(TemplateDeclaration templateDeclaration)
	{
		if (templateDeclaration.eponymousTemplateDeclaration !is null)
		{
			output.writeln("<templateDeclaration>");
			visit(templateDeclaration.eponymousTemplateDeclaration);
			output.writeln("</templateDeclaration>");
			return;
		}
        writeDdoc(templateDeclaration.comment);
		output.writeln("<templateDeclaration line=\"",
			templateDeclaration.name.line, "\">");
		output.writeln("<name>", templateDeclaration.name.text, "</name>");
		visit(templateDeclaration.templateParameters);
		if (templateDeclaration.constraint !is null)
			visit(templateDeclaration.constraint);
		foreach (dec; templateDeclaration.declarations)
		{
			if (dec !is null) visit(dec);
		}
		output.writeln("</templateDeclaration>");
	}

	override void visit(TemplateInstance templateInstance)
	{
		mixin (tagAndAccept!"templateInstance");
	}

	override void visit(TemplateMixinExpression templateMixinExpression)
	{
		mixin (tagAndAccept!"templateMixinExpression");
	}

	override void visit(TemplateParameter templateParameter)
	{
		mixin (tagAndAccept!"templateParameter");
	}

	override void visit(TemplateParameterList templateParameterList)
	{
		mixin (tagAndAccept!"templateParameterList");
	}

	override void visit(TemplateParameters templateParameters)
	{
		mixin (tagAndAccept!"templateParameters");
	}

	override void visit(TemplateSingleArgument templateSingleArgument)
	{
		mixin (tagAndAccept!"templateSingleArgument");
	}

	override void visit(TemplateThisParameter templateThisParameter)
	{
		mixin (tagAndAccept!"templateThisParameter");
	}

	override void visit(TemplateTupleParameter templateTupleParameter)
	{
		mixin (tagAndAccept!"templateTupleParameter");
	}

	override void visit(TemplateTypeParameter templateTypeParameter)
	{
		mixin (tagAndAccept!"templateTypeParameter");
	}

	override void visit(TemplateValueParameter templateValueParameter)
	{
		mixin (tagAndAccept!"templateValueParameter");
	}

	override void visit(TemplateValueParameterDefault templateValueParameterDefault)
	{
		mixin (tagAndAccept!"templateValueParameterDefault");
	}

	override void visit(TernaryExpression ternaryExpression)
	{
		mixin (tagAndAccept!"ternaryExpression");
	}

	override void visit(ThrowStatement throwStatement)
	{
		mixin (tagAndAccept!"throwStatement");
	}

	override void visit(Token token)
	{
		string tagName;
		switch (token.type)
		{
		case tok!"": return;
		case tok!"identifier": tagName = "identifier"; break;
		case tok!"doubleLiteral": tagName = "doubleLiteral"; break;
		case tok!"idoubleLiteral": tagName = "idoubleLiteral"; break;
		case tok!"floatLiteral": tagName = "floatLiteral"; break;
		case tok!"ifloatLiteral": tagName = "ifloatLiteral"; break;
		case tok!"intLiteral": tagName = "intLiteral"; break;
		case tok!"uintLiteral": tagName = "uintLiteral"; break;
		case tok!"longLiteral": tagName = "longLiteral"; break;
		case tok!"ulongLiteral": tagName = "ulongLiteral"; break;
		case tok!"realLiteral": tagName = "realLiteral"; break;
		case tok!"irealLiteral": tagName = "irealLiteral"; break;
		case tok!"characterLiteral": tagName = "characterLiteral"; break;
		case tok!"stringLiteral": tagName = "stringLiteral"; break;
		case tok!"dstringLiteral": tagName = "dstringLiteral"; break;
		case tok!"wstringLiteral": tagName = "wstringLiteral"; break;
		case tok!"$": output.writeln("<dollar/>"); return;
		default: output.writeln("<", str(token.type), "/>"); return;
		}
		output.writeln("<", tagName, ">", xmlEscape(token.text), "</", tagName, ">");
	}

	override void visit(TraitsExpression traitsExpression)
	{
		mixin (tagAndAccept!"traitsExpression");
	}

	override void visit(TryStatement tryStatement)
	{
		mixin (tagAndAccept!"tryStatement");
	}

	override void visit(Type type)
	{
		auto app = appender!string();
		auto formatter = new Formatter!(typeof(app))(app);
		formatter.format(type);
		output.writeln("<type pretty=\"", app.data, "\">");
		type.accept(this);
		output.writeln("</type>");
	}

	override void visit(Type2 type2)
	{
		output.writeln("<type2>");
		if (type2.builtinType != tok!"")
			output.writeln(str(type2.builtinType));
		else
			type2.accept(this);
		output.writeln("</type2>");
	}

	override void visit(TypeSpecialization typeSpecialization)
	{
		mixin (tagAndAccept!"typeSpecialization");
	}

	override void visit(TypeSuffix typeSuffix)
	{
		if (typeSuffix.star)
			output.writeln("<typeSuffix type=\"*\"/>");
		else if (typeSuffix.array)
		{
			if (typeSuffix.low is null && typeSuffix.type is null)
				output.writeln("<typeSuffix type=\"[]\"/>");
			else
			{
				if (typeSuffix.low is null)
				{
					output.writeln("<typeSuffix type=\"[]\">");
					visit(typeSuffix.type);
					output.writeln("</typeSuffix>");
				}
				else
				{
					output.writeln("<typeSuffix type=\"[]\">");
					if (typeSuffix.high !is null)
					{
						output.writeln("<low>");
						visit(typeSuffix.low);
						output.writeln("</low>");
						output.writeln("<high>");
						visit(typeSuffix.high);
						output.writeln("</high>");
					}
					else
						visit(typeSuffix.low);
					output.writeln("</typeSuffix>");
				}
			}
		}
		else
		{
			visit(typeSuffix.delegateOrFunction);
			visit(typeSuffix.parameters);
			foreach (attr; typeSuffix.memberFunctionAttributes)
			{
				if (attr !is null) visit(attr);
			}
		}
	}

	override void visit(TypeidExpression typeidExpression)
	{
		mixin (tagAndAccept!"typeidExpression");
	}

	override void visit(TypeofExpression typeofExpression)
	{
		mixin (tagAndAccept!"typeofExpression");
	}

	override void visit(UnaryExpression unaryExpression)
	{
		output.writeln("<unaryExpression>");
		if (unaryExpression.prefix != tok!"")
		{
			output.writeln("<prefix>", xmlEscape(unaryExpression.prefix.text),
				"</prefix>");
			visit(unaryExpression.unaryExpression);
		}
		if (unaryExpression.suffix != tok!"")
		{
			visit(unaryExpression.unaryExpression);
			output.writeln("<suffix>", unaryExpression.suffix.text,
				"</suffix>");
		}
		else
			unaryExpression.accept(this);
		output.writeln("</unaryExpression>");
	}

	override void visit(UnionDeclaration unionDeclaration)
	{
		output.writeln("<unionDeclaration line=\"", unionDeclaration.name.line, "\">");
		if (unionDeclaration.name != tok!"")
			output.writeln("<name>", unionDeclaration.name.text, "</name>");
		if (unionDeclaration.templateParameters !is null)
			visit(unionDeclaration.templateParameters);
		if (unionDeclaration.constraint !is null)
			visit(unionDeclaration.constraint);
		if (unionDeclaration.structBody !is null)
			visit(unionDeclaration.structBody);
		output.writeln("</unionDeclaration>");
	}

	override void visit(Unittest unittest_)
	{
		output.writeln("<unittest>");
		unittest_.accept(this);
		output.writeln("</unittest>");
	}

	override void visit(VariableDeclaration variableDeclaration)
	{
        output.writeln("<variableDeclaration>");
        writeDdoc(variableDeclaration.comment);
        variableDeclaration.accept(this);
        output.writeln("</variableDeclaration>");
	}

	override void visit(Vector vector)
	{
		mixin (tagAndAccept!"vector");
	}

	override void visit(VersionCondition versionCondition)
	{
		mixin (tagAndAccept!"versionCondition");
	}

	override void visit(VersionSpecification versionSpecification)
	{
		mixin (tagAndAccept!"versionSpecification");
	}

	override void visit(WhileStatement whileStatement)
	{
		mixin (tagAndAccept!"whileStatement");
	}

	override void visit(WithStatement withStatement)
	{
		mixin (tagAndAccept!"withStatement");
	}

	override void visit(XorExpression xorExpression)
	{
		output.writeln("<xorExpression>");
		output.writeln("<left>");
		xorExpression.left.accept(this);
		output.writeln("</left>");
		if (xorExpression.right !is null)
		{
			output.writeln("<right>");
			xorExpression.right.accept(this);
			output.writeln("</right>");
		}
		output.writeln("</xorExpression>");
	}

	alias ASTVisitor.visit visit;

	private static string xmlEscape(string s)
	{
		return s.translate(['<' : "&lt;", '>' : "&gt;", '&' : "&amp;"]);
	}

    private void writeDdoc(string comment)
    {
        if (comment is null) return;
        output.writeln("<ddoc>", xmlEscape(comment), "</ddoc>");
    }

	File output;
}
