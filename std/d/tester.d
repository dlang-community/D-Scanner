import std.d.lexer;
import std.d.ast;
import std.d.parser;
import std.stdio;
import std.file;
import std.array;

class TestVisitor : ASTVisitor
{
	override void visit(ClassDeclaration classDeclaration)
	{
		writeln("class ", classDeclaration.name.value, " on line ", classDeclaration.name.line);
	}

	override void visit(ModuleDeclaration md)
	{
		writeln("module declaration found");
	}

	override void visit(FunctionDeclaration funDec)
	{
		writeln("function ", funDec.name.value, " on line ", funDec.name.line);
	}

	override void visit(VariableDeclaration varDec)
	{
		writeln("variable ", varDec.declarators[0].identifier.value,
			" on line ", varDec.declarators[0].identifier.line);
	}

	override void visit(ImportDeclaration impDec)
	{
		writeln("import declaration found");
	}

	alias ASTVisitor.visit visit;
}

void main(string[] args)
{
	auto de = dirEntry(args[1]);
	ubyte[] sourceBuffer = new ubyte[de.size];
	auto f = File(args[1]);
	ubyte[] rawSource = f.rawRead(sourceBuffer);
	LexerConfig config;
	auto tokens = byToken(rawSource, config).array();
	Module m = parseModule(tokens);
	ASTVisitor visitor = new TestVisitor;
	visitor.visit(m);
}
