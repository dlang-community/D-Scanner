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

    override void visit(StructDeclaration structDeclaration)
    {
        writeln("struct ", structDeclaration.name.value, " on line ", structDeclaration.name.line);
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
        foreach (decl; varDec.declarators)
        {
            writeln("variable ", decl.identifier.value,
                " on line ", decl.identifier.line);
        }
    }

    override void visit(ImportDeclaration impDec)
    {
        writeln("import declaration found");
    }

    override void visit(InterfaceDeclaration intDec)
    {
        writeln("Interface ", intDec.identifier.value,
            " on line ", intDec.identifier.line);
    }

    override void visit(VersionSpecification verSpec)
    {
        writeln("Version specification");
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
    Module m = parseModule(tokens, args[1]);
    ASTVisitor visitor = new TestVisitor;
    visitor.visit(m);
}
