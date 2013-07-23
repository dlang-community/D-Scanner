import std.d.lexer;
import std.d.ast;
import std.stdio;

class XMLPrinter : ASTVisitor
{
	override void visit(Module mod)
	{
		output.writeln("<module>");
		mod.accept(this);
		output.writeln("</module>");
	}

	override void visit(ModuleDeclaration modDec)
	{
		output.writeln("<moduleDeclaration>");
		modDec.accept(this);
		output.writeln("</moduleDeclaration>");
	}

	override void visit(IdentifierChain chain)
	{
		output.writeln("<identifierChain>");
		foreach (ident; chain.identifiers)
		{
			output.writeln("<identifier>", ident.value, "</identifier>");
		}
		output.writeln("</identifierChain>");
	}

	override void visit(ClassDeclaration classDec)
	{
		output.writeln("<classDeclaration line=\"", classDec.name.line, "\">");
		output.writeln("<name>", classDec.name.value, "</name>");
		output.writeln("</classDeclaration>");
	}

	override void visit(StructDeclaration structDec)
	{
		output.writeln("<structDeclaration line=\"", structDec.name.line, "\">");
		output.writeln("<name>", structDec.name.value, "</name>");
		output.writeln("</structDeclaration>");
	}

	override void visit(FunctionDeclaration functionDec)
	{
		output.writeln("<functionDeclaration line=\"", functionDec.name.line, "\">");
		output.writeln("<name>", functionDec.name.value, "</name>");
		output.writeln("</functionDeclaration>");
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
		output.writeln("<name>", enumMem.name.value, "</name>");
		enumMem.accept(this);
		output.writeln("</enumMember>");
	}

	alias ASTVisitor.visit visit;

	File output;
}
