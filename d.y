
%token assign "="
%token at "@"
%token bitAnd "&"
%token bitAndEqual "&="
%token bitOr "|"
%token bitOrEqual "|="
%token catEqual "~="
%token colon ":"
%token comma ","
%token decrement "--"
%token div_ "/"
%token divEqual "/="
%token dollar "$"
%token dot "."
%token equal "=="
%token goesTo "=>"
%token greater ">"
%token greaterEqual ">="
%token hash "#"
%token increment "++"
%token lBrace "{"
%token lBracket "["
%token less "<"
%token lessEqual "<="
%token lessEqualGreater "<>="
%token lessOrGreater "<>"
%token logicAnd "&&"
%token logicOr "||"
%token lParen "("
%token minus "-"
%token minusEqual "-="
%token mod "%"
%token modEqual "%="
%token mulEqual "*="
%token not "!"
%token notEqual "!="
%token notGreater "!>"
%token notGreaterEqual "!>="
%token notLess "!<"
%token notLessEqual "!<="
%token notLessEqualGreater "!<>"
%token plus "+"
%token plusEqual "+="
%token pow "^^"
%token powEqual "^^="
%token rBrace "}"
%token rBracket "]"
%token rParen ")"
%token semicolon ";"
%token shiftLeft "<<"
%token shiftLeftEqual "<<="
%token shiftRight ">>"
%token shiftRightEqual ">>="
%token slice ".."
%token star "*"
%token ternary "?"
%token tilde "~"
%token unordered "!<>="
%token unsignedShiftRight ">>>"
%token unsignedShiftRightEqual ">>>="
%token vararg "..."
%token xor "^"
%token xorEqual "^="
%token bool_ "bool"
%token byte_ "byte"
%token cdouble_ "cdouble"
%token cent_ "cent"
%token cfloat_ "cfloat"
%token char_ "char"
%token creal_ "creal"
%token dchar_ "dchar"
%token double_ "double"
%token float_ "float"
%token function_ "function"
%token idouble_ "idouble"
%token ifloat_ "ifloat"
%token int_ "int"
%token ireal_ "ireal"
%token long_ "long"
%token real_ "real"
%token short_ "short"
%token ubyte_ "ubyte"
%token ucent_ "ucent"
%token uint_ "uint"
%token ulong_ "ulong"
%token ushort_ "ushort"
%token void_ "void"
%token wchar_ "wchar"
%token align_ "align"
%token deprecated_ "deprecated"
%token extern_ "extern"
%token pragma_ "pragma"
%token export_ "export"
%token package_ "package"
%token private_ "private"
%token protected_ "protected"
%token public_ "public"
%token abstract_ "abstract"
%token auto_ "auto"
%token const_ "const"
%token final_ "final"
%token gshared
%token immutable_ "immutable"
%token inout_ "inout"
%token scope_ "scope"
%token shared_ "shared"
%token static_ "static"
%token synchronized_ "synchronized"
%token alias_ "alias"
%token asm_ "asm"
%token assert_ "assert"
%token body_ "body"
%token break_ "break"
%token case_ "case"
%token cast_ "cast"
%token catch_ "catch"
%token class_ "class"
%token continue_ "continue"
%token debug_ "debug"
%token default_ "default"
%token delegate_ "delegate"
%token delete_ "delete"
%token do_ "do"
%token else_ "else"
%token enum_ "enum"
%token false_ "false"
%token finally_ "finally"
%token foreach_ "foreach"
%token foreach_reverse_ "foreach_reverse"
%token for_ "for"
%token goto_ "goto"
%token if_ "if"
%token import_ "import"
%token in_ "in"
%token interface_ "interface"
%token invariant_ "invariant"
%token is_ "is"
%token lazy_ "lazy"
%token macro_ "macro"
%token mixin_ "mixin"
%token module_ "module"
%token new_ "new"
%token nothrow_ "nothrow"
%token null_ "null"
%token out_ "out"
%token override_ "override"
%token pure_ "pure"
%token ref_ "ref"
%token return_ "return"
%token struct_ "struct"
%token super_ "super"
%token switch_ "switch"
%token template_ "template"
%token this_ "this"
%token throw_ "throw"
%token true_ "true"
%token try_ "try"
%token typedef_ "typedef"
%token typeid_ "typeid"
%token typeof_ "typeof"
%token union_ "union"
%token unittest_ "unittest"
%token version_ "version"
%token volatile_ "volatile"
%token while_ "while"
%token with_ "with"
%token date "__DATE__"
%token eof "__EOF__"
%token time "__TIME__"
%token timestamp "__TIMESTAMP__"
%token vendor "__VENDOR__"
%token compilerVersion "__VERSION__"
%token file "__FILE__"
%token line "__LINE__"
%token identifier
%token scriptLine
%token traits
%token parameters
%token vector
%token whitespace
%token specialTokenSequence
%token doubleLiteral
%token floatLiteral
%token idoubleLiteral
%token ifloatLiteral
%token intLiteral
%token longLiteral
%token realLiteral
%token irealLiteral
%token uintLiteral
%token ulongLiteral
%token characterLiteral
%token dstringLiteral
%token stringLiteral
%token wstringLiteral


%{
#include <stdio.h>
%}

%%
Module: ModuleDeclaration Declarations
	| Declarations
    ;

ModuleDeclaration: "module" ModuleFullyQualifiedName
    ;

ModuleFullyQualifiedName: ModuleName
	| Packages "." ModuleName
    ;

ModuleName: identifier
    ;

Packages: PackageName
	| Packages "." PackageName
    ;

PackageName: identifier
    ;

ImportDeclaration: "import" ImportList ";"
	| "static" "import" ImportList ";"
    ;

ImportList: Import
	| ImportBindings
	| Import "," ImportList
    ;

Import: ModuleFullyQualifiedName
	| identifier "=" ModuleFullyQualifiedName
    ;

ImportBindings: Import ":" ImportBindList
    ;

ImportBindList: ImportBind
	| ImportBind "," ImportBindList
    ;

ImportBind: identifier
	| identifier "=" identifier
    ;

Declarations: Declaration
	| Declaration Declarations
    ;

Declaration: ImportDeclaration
    | FunctionDeclaration
    | VariableDeclaration
	| ";"
    ;

Statements: Statement
    | Statement Statements
    ;

Statement: IfStatement
    | WhileStatement
    | DoStatement
    | BlockStatement
    | ScopeStatement
    ;

IfStatement: "if" "(" IfCondition ")" ScopeStatement
    | "if" "(" IfCondition ")" ScopeStatement "else" ScopeStatement
    ;

IfCondition: Expression
    | "auto" identifier "=" Expression
    | BasicType Declarator "=" Expression
    ;

WhileStatement: "while" "(" Expression ")" ScopeStatement
    ;

DoStatement: "do" ScopeStatement "while" "(" Expression ")" ";"
    ;

ScopeStatement: BlockStatement
    ;

BlockStatement: "{" "}"
    ;

FunctionDeclaration: Type Name ParameterList FunctionBody
    | Type Name ParameterList ";"
    ;

Name: identifier
    ;

Type: identifier
    | identifier TypeSuffix
    | BasicTypeX
    | BasicTypeX TypeSuffix
    ;

TypeSuffix: "*"
    | "[" "]"
    | "[" Type "]"
    ;

ParameterList: "(" ")"
    | "(" Parameters ")"
    ;

Parameters: Type identifier
    | Type identifier "," Parameters
    ;

FunctionBody: "{" "}"
    | "{" Declarations "}"
    ;

BasicTypeX: "bool"
    | "byte"
    | "ubyte"
    | "short"
    | "ushort"
    | "int"
    | "uint"
    | "long"
    | "ulong"
    | "char"
    | "wchar"
    | "dchar"
    | "float"
    | "double"
    | "real"
    | "ifloat"
    | "idouble"
    | "ireal"
    | "cfloat"
    | "cdouble"
    | "creal"
    | "void"
    ;

LinkageAttribute: "extern" "(" identifier ")"
    ;

AliasDeclaration: "alias" AliasInitializerList
    /* | "alias" BasicType Declarator */
    ;

AliasInitializerList: AliasInitializer
    | AliasInitializer "," AliasInitializerList
    ;

AliasInitializer: identifier "=" Type
    ;

VariableDeclaration: Type Names ";"
    ;

Names: identifier
    | identifier "," Names
    ;

BasicType: BasicTypeX
    | "." IdentifierList
    | IdentifierList
    | Typeof
    | Typeof "." IdentifierList
    | "const" "(" Type ")"
    | "immutable" "(" Type ")"
    | "shared" "(" Type ")"
    | "inout" "(" Type ")"
    ;

BasicType2: "*"
    | "[" "]"
    | "[" AssignExpression "]"
    | "[" AssignExpression ".." AssignExpression "]"
    | "[" Type "]"
    | "delegate" Parameters
    | "delegate" Parameters MemberFunctionAttributes
    | "function" Parameters
    | "function" Parameters FunctionAttributes
    ;

/*MixinDeclaration: "mixin" "(" AssignExpression ")" ";"
    ;*/

CastQual: "const"
    | "const" "shared"
    | "shared" "const"
    | "inout"
    | "inout" "shared"
    | "shared" "inout"
    | "immutable"
    | "shared"
    ;

/*

Declaration: AliasDeclaration
    | AliasThisDeclaration
    | Decl
    ;

AliasThisDeclaration: "alias" identifier "this"
    ;

Decl: StorageClasses Decl
    | BasicType Declarators ";"
    | BasicType Declarator FunctionBody
    | AutoDeclaration
    ;

Declarators: DeclaratorInitializer
    | DeclaratorInitializer "," DeclaratorIdentifierList
    ;

DeclaratorInitializer: Declarator
    | Declarator "=" Initializer
    ;

DeclaratorIdentifierList: DeclaratorIdentifier
    | DeclaratorIdentifier "," DeclaratorIdentifierList
    ;

DeclaratorIdentifier: identifier
    | identifier "=" Initializer
    ;

Declarator: "(" Declarator ")"
    | BasicType2 "(" Declarator ")" DeclaratorSuffixes
    | "(" Declarator ")" DeclaratorSuffixes
    | BasicType2 "(" Declarator ")" DeclaratorSuffixes
    | identifier
    | identifier DeclaratorSuffixes
    | BasicType2 identifier DeclaratorSuffixes
    ;

DeclaratorSuffixes: DeclaratorSuffix
    | DeclaratorSuffix DeclaratorSuffixes
    ;

DeclaratorSuffix: "[" "]"
    | "[" AssignExpression "]"
    | "[" Type "]"
    | Parameters
    | TemplateParameters Parameters
    | TemplateParameters Parameters MemberFunctionAttributes
    | TemplateParameters Parameters MemberFunctionAttributes Constraint
    | TemplateParameters Parameters Constraint
    | Parameters Constraint
    | Parameters MemberFunctionAttributes
    | Parameters MemberFunctionAttributes Constraint
    ;

IdentifierList: identifier
    | identifier "." IdentifierList
    | TemplateInstance
    | TemplateInstance "." IdentifierList
    ;

StorageClasses: StorageClass
    | StorageClass StorageClasses
    ;

StorageClass: "abstract"
    | "auto"
    | TypeCtor
    | "deprecated"
    | "enum"
    | "extern"
    | "final"
    | "nothrow"
    | "override"
    | "pure"
    | "__gshared"
    | Property
    | "scope"
    | "static"
    | "synchronized"
    ;

PropertyIdentifier: "property"
    | "safe"
    | "trusted"
    | "system"
    | "disable"
    ;

TypeCtors: TypeCtor
    | TypeCtor TypeCtors
    ;

TypeCtor: "const"
    | "immutable"
    | "inout"
    | "shared"
    ;

Type: TypeCtors BasicType
    | BasicType
    | BasicType Declarator2
    | TypeCtors BasicType Declarator2
    ;

Declarator2:
    | BasicType2
    | BasicType2 DeclaratorSuffixes
    | "(" Declarator2 ")"
    | BasicType2 "(" Declarator2 ")"
    | "(" Declarator2 ")" DeclaratorSuffixes
    | BasicType2 "(" Declarator2 ")" DeclaratorSuffixes
    ;

Parameters: "(" ParameterList ")"
    | "(" ")"
    ;

ParameterList: Parameter
    | Parameter "," ParameterList
    | "..."
    ;

Parameter: BasicType Declarator
    | InOut BasicType Declarator
    | BasicType Declarator "..."
    | InOut BasicType Declarator "..."
    | BasicType Declarator "=" DefaultInitializerExpression
    | InOut BasicType Declarator "=" DefaultInitializerExpression
    | Type
    | InOut Type
    | Type "..."
    | InOut Type "..."
    ;

InOut: InOutX
    | InOut InOutX
    ;

InOutX: "auto"
    | TypeCtor
    | "final"
    | "in"
    | "lazy"
    | "out"
    | "ref"
    | "scope"
    ;

FunctionAttributes: FunctionAttribute
    | FunctionAttribute FunctionAttributes
    ;

FunctionAttribute: "nothrow"
    | "pure"
    | Property
    ;

MemberFunctionAttributes: MemberFunctionAttribute
    | MemberFunctionAttribute MemberFunctionAttributes
    ;

MemberFunctionAttribute: "const"
    | "immutable"
    | "inout"
    | "shared"
    | FunctionAttribute
    ;

DefaultInitializerExpression: AssignExpression
    | "__FILE__"
    | "__MODULE__"
    | "__LINE__"
    | "__FUNCTION__"
    | "__PRETTY_FUNCTION__"
    ;

Initializer: VoidInitializer
    | NonVoidInitializer
    ;

NonVoidInitializer: AssignExpression
    | ArrayInitializer
    | StructInitializer
    ;

ArrayInitializer: "[" "]"
    | "[" ArrayMemberInitializations "]"
    ;

ArrayMemberInitializations: ArrayMemberInitialization
    | ArrayMemberInitialization ","
    | ArrayMemberInitialization "," ArrayMemberInitializations
    ;

ArrayMemberInitialization: NonVoidInitializer
    | AssignExpression ":" NonVoidInitializer
    ;

StructInitializer: "{"  "}"
    | "{" StructMemberInitializers "}"
    ;

StructMemberInitializers: StructMemberInitializer
    | StructMemberInitializer ","
    | StructMemberInitializer "," StructMemberInitializers
    ;

StructMemberInitializer: NonVoidInitializer
    | identifier ":" NonVoidInitializer

AutoDeclaration: StorageClasses AutoDeclarationX ";"
    ;

AutoDeclarationX: identifier "=" Initializer
    | AutoDeclarationX "," identifier "=" Initializer
    ;

Typeof: "typeof" "(" Expression ")"
    | "typeof" "(" "return" ")"
    ;

VoidInitializer: "void"

AttributeSpecifier: Attribute ":"
    | Attribute DeclarationBlock
    ;

Attribute: LinkageAttribute
    | AlignAttribute
    | Pragma
    | "deprecated"
    | ProtectionAttribute
    | "static"
    | "extern"
    | "final"
    | "synchronized"
    | "override"
    | "abstract"
    | "const"
    | "auto"
    | "scope"
    | "__gshared"
    | "shared"
    | "immutable"
    | "inout"
    | "@" "disable"
    ;


DeclarationBlock: DeclDef
    | "{" "}"
    | "{" DeclDefs "}"
    ;

LinkageType: "C"
    | "C++"
    | "D"
    | "Windows"
    | "Pascal"
    | "System"
    ;

AlignAttribute: "align"
    | "align" "(" intLiteral ")"
    ;

ProtectionAttribute: "private"
    | "package"
    | "protected"
    | "public"
    | "export"
    ;

StorageClass: UserDefinedAttribute
    ;

UserDefinedAttribute: "@" "(" "ArgumentList" ")"
    | "@" identifier
    ;

Pragma: "pragma" "(" identifier ")"
    | "pragma" "(" identifier "," ArgumentList ")"
    ;

Expression: CommaExpression
    ;

CommaExpression:
    | AssignExpression
    | AssignExpression "," CommaExpression
    ;

AssignExpression: ConditionalExpression
    | ConditionalExpression "=" AssignExpression
    | ConditionalExpression "+=" AssignExpression
    | ConditionalExpression "-=" AssignExpression
    | ConditionalExpression "*=" AssignExpression
    | ConditionalExpression "/=" AssignExpression
    | ConditionalExpression "%=" AssignExpression
    | ConditionalExpression "&=" AssignExpression
    | ConditionalExpression "|=" AssignExpression
    | ConditionalExpression "^=" AssignExpression
    | ConditionalExpression "~=" AssignExpression
    | ConditionalExpression "<<=" AssignExpression
    | ConditionalExpression ">>=" AssignExpression
    | ConditionalExpression ">>>=" AssignExpression
    | ConditionalExpression "^^=" AssignExpression
    ;

ConditionalExpression: OrOrExpression
    | OrOrExpression "?" Expression ":" ConditionalExpression
    ;

OrOrExpression: AndAndExpression
    | OrOrExpression "||" AndAndExpression
    ;

AndAndExpression: OrExpression
    | AndAndExpression "&&" OrExpression
    | CmpExpression
    | AndAndExpression "&&" CmpExpression
    ;

OrExpression: XorExpression
    | OrExpression "|" XorExpression
    ;

XorExpression: AndExpression
    | XorExpression "^" AndExpression
    ;

AndExpression: ShiftExpression
    | AndExpression "&" ShiftExpression
    ;

CmpExpression: ShiftExpression
    | EqualExpression
    | IdentityExpression
    | RelExpression
    | InExpression
    ;

EqualExpression: ShiftExpression "==" ShiftExpression
    | ShiftExpression "!=" ShiftExpression
    ;

IdentityExpression: ShiftExpression "is" ShiftExpression
    | ShiftExpression "!is" ShiftExpression
    ;

RelExpression: ShiftExpression "<" ShiftExpression
    | ShiftExpression "<=" ShiftExpression
    | ShiftExpression ">" ShiftExpression
    | ShiftExpression ">=" ShiftExpression
    | ShiftExpression "!<>=" ShiftExpression
    | ShiftExpression "!<>" ShiftExpression
    | ShiftExpression "<>" ShiftExpression
    | ShiftExpression "<>=" ShiftExpression
    | ShiftExpression "!>" ShiftExpression
    | ShiftExpression "!>=" ShiftExpression
    | ShiftExpression "!<" ShiftExpression
    | ShiftExpression "!<=" ShiftExpression
    ;

InExpression: ShiftExpression "in" ShiftExpression
    | ShiftExpression "!in" ShiftExpression
    ;

ShiftExpression: AddExpression
    | ShiftExpression "<<" AddExpression
    | ShiftExpression ">>" AddExpression
    | ShiftExpression ">>>" AddExpression
    ;

AddExpression: MulExpression
    | AddExpression "+" MulExpression
    | AddExpression "-" MulExpression
    | CatExpression
    ;

CatExpression: AddExpression "~" MulExpression
    ;

MulExpression: UnaryExpression
    | MulExpression "*" UnaryExpression
    | MulExpression "/" UnaryExpression
    | MulExpression "%" UnaryExpression
    ;

UnaryExpression: "&" UnaryExpression
    | "++" UnaryExpression
    | "--" UnaryExpression
    | "*" UnaryExpression
    | "-" UnaryExpression
    | "+" UnaryExpression
    | "!" UnaryExpression
    | ComplementExpression
    | "(" Type ")" "." identifier
    | "(" Type ")" "." TemplateInstance
    | NewExpression
    | DeleteExpression
    | CastExpression
    | PowExpression
    ;

ComplementExpression: "~" UnaryExpression
    ;

NewExpression: "new" Type "[" AssignExpression "]"
    | "new" AllocatorArguments Type "[" AssignExpression "]"
    | "new" Type "(" ArgumentList ")"
    | "new" AllocatorArguments Type "(" ArgumentList ")"
    | "new" Type
    | "new" AllocatorArguments Type
    | NewAnonClassExpression
    ;

AllocatorArguments: "(" ")"
    | "(" ArgumentList ")"
    ;

ArgumentList: AssignExpression
    | AssignExpression ","
    | AssignExpression "," ArgumentList
    ;

DeleteExpression: "delete" UnaryExpression
    ;

CastExpression: "cast" "(" Type ")" UnaryExpression
    | "cast" "(" CastQual ")" UnaryExpression
    | "cast" "(" ")" UnaryExpression
    ;

PowExpression: PostfixExpression
    | PostfixExpression "^^" UnaryExpression
    ;

PostfixExpression: PrimaryExpression
    | PostfixExpression "." identifier
    | PostfixExpression "." TemplateInstance
    | PostfixExpression "." NewExpression
    | PostfixExpression "++"
    | PostfixExpression "--"
    | PostfixExpression "(" ")"
    | PostfixExpression "(" ArgumentList ")"
    | BasicType "(" ")"
    | TypeCtors BasicType "(" ")"
    | BasicType "(" ArgumentList ")"
    | TypeCtors BasicType "(" ArgumentList ")"
    | IndexExpression
    | SliceExpression
    ;

IndexExpression: PostfixExpression "[" ArgumentList "]"
    ;

SliceExpression: PostfixExpression "[" "]"
    | PostfixExpression "[" AssignExpression ".." AssignExpression "]"
    ;

PrimaryExpression: identifier
    | "." identifier
    | TemplateInstance
    | "." TemplateInstance
    | "this"
    | "super"
    | "null"
    | "true"
    | "false"
    | "$"
    | "__FILE__"
    | "__MODULE__"
    | "__LINE__"
    | "__FUNCTION__"
    | "__PRETTY_FUNCTION__"
    | intLiteral
    | floatLiteral
    | characterLiteral
    | StringLiterals
    | ArrayLiteral
    | AssocArrayLiteral
    | Lambda
    | FunctionLiteral
    | AssertExpression
    | MixinExpression
    | ImportExpression
    | BasicType "." identifier
    | Typeof
    | TypeidExpression
    | IsExpression
    | "(" Expression ")"
    | TraitsExpression
    ;

StringLiterals: stringLiteral
    | stringLiteral StringLiterals
    ;

ArrayLiteral: "[" ArgumentList "]"
    ;


AssocArrayLiteral: "[" KeyValuePairs "]"

KeyValuePairs: KeyValuePair
    | KeyValuePair "," KeyValuePairs
    ;

KeyValuePair: KeyExpression ":" ValueExpression
    ;

KeyExpression: AssignExpression
    ;

ValueExpression: AssignExpression
    ;

Lambda: identifier "=>" AssignExpression
    | ParameterAttributes "=>" AssignExpression
    ;

FunctionLiteral: "function" FunctionBody
    | "function" ParameterAttributes FunctionBody
    | "function" Type FunctionBody
    | "function" Type ParameterAttributes FunctionBody
    | "delegate" FunctionBody
    | "delegate" ParameterAttributes FunctionBody
    | "delegate" Type FunctionBody
    | "delegate" Type ParameterAttributes FunctionBody
    | ParameterAttributes FunctionBody
    | FunctionBody
    ;

ParameterAttributes: Parameters
    | Parameters FunctionAttributes
    ;

AssertExpression: "assert" "(" AssignExpression ")"
    | "assert" "(" AssignExpression "," AssignExpression ")"
    ;

MixinExpression: "mixin" "(" AssignExpression ")"
    ;

ImportExpression: "import" "(" AssignExpression ")"
    ;

TypeidExpression: "typeid" "(" Type ")"
    | "typeid" "(" Expression ")"
    ;

IsExpression: "is" "(" Type ")"
    | "is" "(" Type ":" TypeSpecialization ")"
    | "is" "(" Type "==" TypeSpecialization ")"
    | "is" "(" Type ":" TypeSpecialization "," TemplateParameterList ")"
    | "is" "(" Type "==" TypeSpecialization "," TemplateParameterList ")"
    | "is" "(" Type identifier ")"
    | "is" "(" Type identifier ":" TypeSpecialization ")"
    | "is" "(" Type identifier "==" TypeSpecialization ")"
    | "is" "(" Type identifier ":" TypeSpecialization "," TemplateParameterList ")"
    | "is" "(" Type identifier "==" TypeSpecialization "," TemplateParameterList ")"
    ;

TypeSpecialization: Type
    | "struct"
    | "union"
    | "class"
    | "interface"
    | "enum"
    | "function"
    | "delegate"
    | "super"
    | "const"
    | "immutable"
    | "inout"
    | "shared"
    | "return"
    | "__parameters"
    ;

Statement: ";"
    | NonEmptyStatement
    | ScopeBlockStatement
    ;

NoScopeNonEmptyStatement: NonEmptyStatement
    | BlockStatement
    ;

NoScopeStatement: ";"
    | NonEmptyStatement
    | BlockStatement
    ;

NonEmptyOrScopeBlockStatement: NonEmptyStatement
    | ScopeBlockStatement
    ;

NonEmptyStatement: NonEmptyStatementNoCaseNoDefault
    | CaseStatement
    | CaseRangeStatement
    | DefaultStatement
    ;

NonEmptyStatementNoCaseNoDefault: LabeledStatement
    | ExpressionStatement
    | DeclarationStatement
    | IfStatement
    | WhileStatement
    | DoStatement
    | ForStatement
    | ForeachStatement
    | SwitchStatement
    | FinalSwitchStatement
    | ContinueStatement
    | BreakStatement
    | ReturnStatement
    | GotoStatement
    | WithStatement
    | SynchronizedStatement
    | TryStatement
    | ScopeGuardStatement
    | ThrowStatement
    | AsmStatement
    | PragmaStatement
    | MixinStatement
    | ForeachRangeStatement
    | ConditionalStatement
    | StaticAssert
    | TemplateMixin
    | ImportDeclaration
    ;

ScopeStatement: NonEmptyStatement
    | BlockStatement
    ;

ScopeBlockStatement: BlockStatement
    ;

LabeledStatement: identifier ":" NoScopeStatement
    ;

BlockStatement: "{" "}"
    | "{" StatementList "}"
    ;

StatementList: Statement
    | Statement StatementList
    ;

ExpressionStatement: Expression ";"
    ;

DeclarationStatement: Declaration
    ;

IfStatement: "if" "(" IfCondition ")" ThenStatement
    | "if" "(" IfCondition ")" ThenStatement "else" ElseStatement
    ;

IfCondition: Expression
    | "auto" identifier "=" Expression
    | BasicType Declarator "=" Expression
    ;

ThenStatement: ScopeStatement
    ;

ElseStatement: ScopeStatement
    ;

WhileStatement: "while" "(" Expression ")" ScopeStatement
    ;

DoStatement: "do" ScopeStatement "while" "(" Expression ")" ";"
    ;

ForStatement: "for" "(" Initialize ";" ")" ScopeStatement
    | "for" "(" Initialize ";" Increment ")" ScopeStatement
    | "for" "(" Initialize Test ";" ")" ScopeStatement
    | "for" "(" Initialize Test ";" Increment ")" ScopeStatement
    ;

Initialize: ";"
    | NoScopeNonEmptyStatement
    ;

Test: Expression
    ;

Increment: Expression
    ;

ForeachStatement: Foreach "(" ForeachTypeList ";" Aggregate ")" NoScopeNonEmptyStatement
    ;

Foreach: "foreach"
    | "foreach_reverse"
    ;

ForeachTypeList: ForeachType
    | ForeachType "," ForeachTypeList
    ;

ForeachType: BasicType Declarator
    | "ref" BasicType Declarator
    | identifier
    | "ref" identifier
    ;

Aggregate: Expression
    ;

ForeachRangeStatement: Foreach "(" ForeachType ";" LwrExpression ".." UprExpression ")" ScopeStatement
    ;

LwrExpression: Expression
    ;

UprExpression: Expression
    ;

SwitchStatement: "switch" "(" Expression ")" ScopeStatement
    ;

CaseStatement: "case" ArgumentList ":" ScopeStatementList
    ;

CaseRangeStatement: "case" FirstExp ":" ".." "case" LastExp ":" ScopeStatementList
    ;

FirstExp: AssignExpression
    ;

LastExp: AssignExpression
    ;

DefaultStatement: "default" ":" ScopeStatementList
    ;

ScopeStatementList: StatementListNoCaseNoDefault
    ;

StatementListNoCaseNoDefault: StatementNoCaseNoDefault
    | StatementNoCaseNoDefault StatementListNoCaseNoDefault
    ;

StatementNoCaseNoDefault: ";"
    | NonEmptyStatementNoCaseNoDefault
    | ScopeBlockStatement
    ;

FinalSwitchStatement: "final" "switch" "(" Expression ")" ScopeStatement
    ;

ContinueStatement: "continue" ";"
    | "continue" identifier ";"
    ;

BreakStatement: "break" ";"
    | "break" identifier ";"
    ;

ReturnStatement: "return" ";"
    | Expression ";"
    ;

GotoStatement: "goto" identifier ";"
    | "goto" "default" ";"
    | "goto" "case" ";"
    | "goto" "case" Expression ";"
    ;

WithStatement: "with" "(" Expression ")" ScopeStatement
    | "with" "(" Symbol ")" ScopeStatement
    | "with" "(" TemplateInstance ")" ScopeStatement
    ;

SynchronizedStatement: "synchronized" ScopeStatement
    | "synchronized" "(" Expression ")" ScopeStatement
    ;

TryStatement: "try" ScopeStatement Catches
    | "try" ScopeStatement Catches FinallyStatement
    | "try" ScopeStatement FinallyStatement
    ;

Catches: LastCatch
    | Catch
    | Catch Catches
    ;

LastCatch: "catch" NoScopeNonEmptyStatement
    ;

Catch: "catch" "(" CatchParameter ")" NoScopeNonEmptyStatement
    ;

CatchParameter: BasicType identifier
    ;

FinallyStatement: "finally" NoScopeNonEmptyStatement
    ;
ThrowStatement: "throw" Expression ";"
    ;

ScopeGuardStatement: "scope" "(" "exit" ")" NonEmptyOrScopeBlockStatement
    | "scope" "(" "success" ")" NonEmptyOrScopeBlockStatement
    | "scope" "(" "failure" ")" NonEmptyOrScopeBlockStatement
    ;

AsmStatement: "asm" "{" "}"
asm { AsmInstructionList }
AsmInstructionList: AsmInstruction ";"
    AsmInstruction ";" AsmInstructionList

PragmaStatement: Pragma NoScopeStatement
    ;

MixinStatement: "mixin" "(" AssignExpression ")" ";"
    ;

AggregateDeclaration: "struct" identifier StructBody
    | "union" identifier StructBody
    | "struct" identifier ";"
    | "union" identifier ";"
    | StructTemplateDeclaration
    | UnionTemplateDeclaration
    ;

StructBody: "{" "}"
    | "{" StructBodyDeclarations "}"
    ;

StructBodyDeclarations: StructBodyDeclaration
    | StructBodyDeclaration StructBodyDeclarations
    ;

StructBodyDeclaration: DeclDef
    | StructAllocator
    | StructDeallocator
    | StructPostblit
    | AliasThis
    ;

StructAllocator: ClassAllocator
    ;

StructDeallocator: ClassDeallocator
    ;

StructPostblit: "this" "(" "this" ")" FunctionBody
    ;

ClassDeclaration: "class" identifier ClassBody
    | "class" identifier BaseClassList ClassBody
    | ClassTemplateDeclaration
    ;

BaseClassList: ":" SuperClass
    | ":" SuperClass "," Interfaces
    | ":" Interfaces
    ;

SuperClass: identifier
    ;

Interfaces: Interface
    | Interface "," Interfaces
    ;

Interface: identifier
    ;

ClassBody: "{" "}"
    | "{" ClassBodyDeclarations "}"
    ;

ClassBodyDeclarations: ClassBodyDeclaration
    | ClassBodyDeclaration ClassBodyDeclarations
    ;

ClassBodyDeclaration: DeclDef
    | Invariant
    | ClassAllocator
    | ClassDeallocator
    ;

Constructor: "this" Parameters FunctionBody
    | TemplatedConstructor
    ;

Destructor: "~" "this" "(" ")" FunctionBody
    ;

StaticConstructor: "static" "this" "(" ")" FunctionBody
    ;

StaticDestructor: "static" "~" "this" "(" ")" FunctionBody
    ;

SharedStaticConstructor: "shared" "static" "this" "(" ")" FunctionBody
    ;

SharedStaticDestructor: "shared" "static" "~" "this" "(" ")" FunctionBody
    ;

Invariant: "invariant" "(" ")" BlockStatement
    ;

ClassAllocator: "new" Parameters FunctionBody
    ;

ClassDeallocator: "delete" Parameters FunctionBody
    ;

AliasThis: "alias" identifier "this" ";"
    ;

NewAnonClassExpression: "new" "class"
    | "new" "class" Interfaces
    | "new" "class" SuperClass
    | "new" "class" SuperClass Interfaces
    | "new" "class" ClassArguments
    | "new" "class" ClassArguments Interfaces
    | "new" "class" ClassArguments SuperClass
    | "new" "class" ClassArguments SuperClass Interfaces
    | "new" AllocatorArguments "class"
    | "new" AllocatorArguments "class" Interfaces
    | "new" AllocatorArguments "class" SuperClass Interfaces
    | "new" AllocatorArguments "class" SuperClass
    | "new" AllocatorArguments "class" ClassArguments
    | "new" AllocatorArguments "class" ClassArguments SuperClass
    | "new" AllocatorArguments "class" ClassArguments Interfaces
    | "new" AllocatorArguments "class" ClassArguments SuperClass Interfaces
    | ClassBody
    ;

ClassArguments: "(" ")"
    | "(" ArgumentList ")"
    ;

InterfaceDeclaration: "interface" identifier InterfaceBody
	| "interface" identifier BaseInterfaceList InterfaceBody
	| InterfaceTemplateDeclaration
    ;

BaseInterfaceList: ":" Interfaces
    ;

InterfaceBody: "{" "}"
    | "{" DeclDefs "}"
    ;


EnumDeclaration: "enum" EnumTag EnumBody
    | "enum" EnumBody
    | "enum" EnumTag ":" EnumBaseType EnumBody
    | "enum" ":" EnumBaseType EnumBody
    ;

EnumTag: identifier
    ;

EnumBaseType: Type
    ;

EnumBody: EmptyEnumBody
    | EnumMembersBody
    ;

EmptyEnumBody: ";"
    ;

EnumMembersBody: "{" EnumMembers "}"
    ;

EnumMembers: EnumMember
    | EnumMember ","
    | EnumMember "," EnumMembers
    ;

EnumMember: identifier
    | identifier "=" AssignExpression
    | Type "=" AssignExpression
    ;

FunctionBody: BlockStatement
    | BodyStatement
    | InStatement BodyStatement
    | OutStatement BodyStatement
    | InStatement OutStatement BodyStatement
    | OutStatement InStatement BodyStatement
    ;

InStatement: "in" BlockStatement
    ;

OutStatement: "out" BlockStatement
    | "out" "(" identifier ")" BlockStatement
    ;

BodyStatement: "body" BlockStatement
    ;

TemplateDeclaration: "template" TemplateIdentifier TemplateParameters Constraint
    | "template" TemplateIdentifier TemplateParameters
    | "{" DeclDefs "}"
    ;

TemplateIdentifier: identifier
    ;

TemplateParameters: "(" ")"
    | "(" TemplateParameterList ")"
    ;

TemplateParameterList: TemplateParameter
    | TemplateParameter ","
    | TemplateParameter "," TemplateParameterList
    ;

TemplateParameter: TemplateTypeParameter
    | TemplateValueParameter
    | TemplateAliasParameter
    | TemplateTupleParameter
    | TemplateThisParameter
    ;

TemplateInstance: TemplateIdentifier TemplateArguments
    ;

TemplateArguments: "!" "(" ")"
    | "!" "(" TemplateArgumentList ")"
    | "!" TemplateSingleArgument
    ;

TemplateArgumentList: TemplateArgument
    | TemplateArgument ","
    | TemplateArgument "," TemplateArgumentList
    ;

TemplateArgument: Type
    | AssignExpression
    | Symbol
    ;

Symbol: SymbolTail
    | "." SymbolTail
    ;

SymbolTail: identifier
    | identifier "." SymbolTail
    | TemplateInstance
    | TemplateInstance "." SymbolTail
    ;

TemplateSingleArgument: identifier
    | BasicTypeX
    | characterLiteral
    | stringLiteral
    | intLiteral
    | floatLiteral
    | "true"
    | "false"
    | "null"
    | "this"
    | "__FILE__"
    | "__MODULE__"
    | "__LINE__"
    | "__FUNCTION__"
    | "__PRETTY_FUNCTION__"
    ;

TemplateTypeParameter: identifier
    | identifier TemplateTypeParameterSpecialization
    | identifier TemplateTypeParameterDefault
    | identifier TemplateTypeParameterSpecialization TemplateTypeParameterDefault
    ;

TemplateTypeParameterSpecialization: ":" Type
    ;

TemplateTypeParameterDefault: "=" Type
    ;

TemplateThisParameter: "this" TemplateTypeParameter
    ;

TemplateValueParameter: BasicType Declarator
    | BasicType Declarator TemplateValueParameterSpecialization
    | BasicType Declarator TemplateValueParameterDefault
    | BasicType Declarator TemplateValueParameterSpecialization TemplateValueParameterDefault
    ;

TemplateValueParameterSpecialization: ":" ConditionalExpression
    ;

TemplateValueParameterDefault: "=" "__FILE__"
    | "=" "__MODULE__"
    | "=" "__LINE__"
    | "=" "__FUNCTION__"
    | "=" "__PRETTY_FUNCTION__"
    | "=" AssignExpression
    ;

TemplateAliasParameter: "alias" identifier
    | "alias" identifier TemplateAliasParameterDefault
    | "alias" identifier TemplateAliasParameterSpecialization
    | "alias" identifier TemplateAliasParameterSpecialization TemplateAliasParameterDefault
    | "alias" BasicType Declarator
    | "alias" BasicType Declarator TemplateAliasParameterDefault
    | "alias" BasicType Declarator TemplateAliasParameterSpecialization
    | "alias" BasicType Declarator TemplateAliasParameterSpecialization TemplateAliasParameterDefault
    ;

TemplateAliasParameterSpecialization: ":" Type
    | ":" ConditionalExpression
    ;

TemplateAliasParameterDefault: "=" Type
    | "=" ConditionalExpression
    ;

TemplateTupleParameter: identifier "..."
    ;

TemplatedConstructor: "this" TemplateParameters Parameters FunctionBody
    | "this" TemplateParameters Parameters Constraint FunctionBody
    ;

ClassTemplateDeclaration: "class" identifier "(" TemplateParameterList ")" ClassBody
    | "class" identifier "(" TemplateParameterList ")" Constraint ClassBody
    | "class" identifier "(" TemplateParameterList ")" BaseClassList ClassBody
    | "class" identifier "(" TemplateParameterList ")" Constraint BaseClassList ClassBody
    ;

StructTemplateDeclaration: "struct" identifier "(" TemplateParameterList ")" StructBody
    | "struct" identifier "(" TemplateParameterList ")" Constraint StructBody
    ;

UnionTemplateDeclaration: "union" identifier "(" TemplateParameterList ")" StructBody
    | "union" identifier "(" TemplateParameterList ")" Constraint StructBody
    ;

InterfaceTemplateDeclaration: "interface" identifier "(" TemplateParameterList ")" InterfaceBody
    | "interface" identifier "(" TemplateParameterList ")" BaseInterfaceList InterfaceBody
    | "interface" identifier "(" TemplateParameterList ")" Constraint  InterfaceBody
    | "interface" identifier "(" TemplateParameterList ")" Constraint BaseInterfaceList InterfaceBody
    ;

Constraint: "if" "(" ConstraintExpression ")"
    ;

ConstraintExpression: Expression
    ;

TemplateMixinDeclaration: "mixin" "template" TemplateIdentifier TemplateParameters
    | "mixin" "template" TemplateIdentifier TemplateParameters Constraint
    | "{" DeclDefs "}"
    ;

TemplateMixin: "mixin" MixinTemplateName";"
    | "mixin" MixinTemplateName MixinIdentifier ";"
    | "mixin" MixinTemplateName TemplateArguments  ";"
    | "mixin" MixinTemplateName TemplateArguments MixinIdentifier ";"
    ;

MixinTemplateName: "." QualifiedIdentifierList
    | QualifiedIdentifierList
    | Typeof "." QualifiedIdentifierList
    ;

QualifiedIdentifierList: identifier
    | identifier "." QualifiedIdentifierList
    | TemplateInstance "." QualifiedIdentifierList
    ;

MixinIdentifier: identifier
    ;

ConditionalDeclaration: Condition CCDeclarationBlock
    | Condition CCDeclarationBlock "else" CCDeclarationBlock
    | Condition ":" Declarations

CCDeclarationBlock: Declaration
    | "{" Declarations "}"
    | "{" "}"
    ;

Declarations: Declaration
    | Declaration Declarations
    ;

ConditionalStatement: Condition NoScopeNonEmptyStatement
    | Condition NoScopeNonEmptyStatement "else" NoScopeNonEmptyStatement
    ;

Condition: VersionCondition
    | DebugCondition
    | StaticIfCondition
    ;

VersionCondition: "version" "(" intLiteral ")"
    | "version" "(" identifier ")"
    | "version" "(" "unittest" ")"
    | "version" "(" "assert" ")"
    ;

VersionSpecification: "version" "=" identifier ";"
    "version" "=" intLiteral ";"
    ;

DebugCondition: "debug"
    | "debug" "(" intLiteral ")"
    | "debug" "(" identifier ")"
    ;

DebugSpecification: "debug" "=" identifier ";"
    | "debug" "=" intLiteral ";"
    ;

StaticIfCondition: "static" "if" "(" AssignExpression ")"
    ;

StaticAssert: "static" "assert" "(" AssignExpression ")" ";"
    "static" "assert" "(" AssignExpression "," AssignExpression ")" ";"
    ;

TraitsExpression: "__traits" "(" TraitsKeyword "," TraitsArguments ")"

TraitsArguments: TraitsArgument
    TraitsArgument "," TraitsArguments
    ;

TraitsArgument: AssignExpression
    | Type
    ;

UnitTest: "unittest" FunctionBody
    ;

*/
%%

