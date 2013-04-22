grammar D;

Assign: '=';
At: '@';
BitAnd: '&';
BitAndEqual: '&=';
BitOr: '|';
BitOrEqual: '|=';
CatEqual: '~=';
Colon: ':';
Comma: ',';
Decrement: '--';
Div: '/';
DivEqual: '/=';
Dollar: '$';
Dot: '.';
Equal: '==';
GoesTo: '=>';
Greater: '>';
GreaterEqual: '>=';
Hash: '#';
Increment: '++';
LBrace: '{';
LBracket: '[';
Less: '<';
LessEqual: '<=';
LessEqualGreater: '<>=';
LessOrGreater: '<>';
LogicAnd: '&&';
LogicOr: '||';
LParen: '(';
Minus: '-';
MinusEqual: '-=';
Mod: '%';
ModEqual: '%=';
MulEqual: '*=';
Not: '!';
NotEqual: '!=';
NotGreater: '!>';
NotGreaterEqual: '!>=';
NotLess: '!<';
NotLessEqual: '!<=';
NotLessEqualGreater: '!<>';
Plus: '+';
PlusEqual: '+=';
Pow: '^^';
PowEqual: '^^=';
RBrace: '}';
RBracket: ']';
RParen: ')';
Semicolon: ';';
ShiftLeft: '<<';
ShiftLeftEqual: '<<=';
ShiftRight: '>>';
ShiftRightEqual: '>>=';
Slice: '..';
Star: '*';
Ternary: '?';
Tilde: '~';
Unordered: '!<>=';
UnsignedShiftRight: '>>>';
UnsignedShiftRightEqual: '>>>=';
Vararg: '...';
Xor: '^';
XorEqual: '^=';
Bool: 'bool';
Byte: 'byte';
Cdouble: 'cdouble';
Cent: 'cent';
Cfloat: 'cfloat';
Char: 'char';
Creal: 'creal';
Dchar: 'dchar';
Double: 'double';
Float: 'float';
Function: 'function';
Idouble: 'idouble';
Ifloat: 'ifloat';
Int: 'int';
Ireal: 'ireal';
Long: 'long';
Real: 'real';
Short: 'short';
Ubyte: 'ubyte';
Ucent: 'ucent';
Uint: 'uint';
Ulong: 'ulong';
Ushort: 'ushort';
Void: 'void';
Wchar: 'wchar';
Align: 'align';
Deprecated: 'deprecated';
Extern: 'extern';
Pragma: 'pragma';
Export: 'export';
Package: 'package';
Private: 'private';
Protected: 'protected';
Public: 'public';
Abstract: 'abstract';
Auto: 'auto';
Const: 'const';
Final: 'final';
Gshared: '__gshared';
Immutable: 'immutable';
Inout: 'inout';
Scope: 'scope';
Shared: 'shared';
Static: 'static';
Synchronized: 'synchronized';
Alias: 'alias';
Asm: 'asm';
Assert: 'assert';
Body: 'body';
Break: 'break';
Case: 'case';
Cast: 'cast';
Catch: 'catch';
Class: 'class';
Continue: 'continue';
Debug: 'debug';
Default: 'default';
Delegate: 'delegate';
Delete: 'delete';
Do: 'do';
Else: 'else';
Enum: 'enum';
False: 'false';
Finally: 'finally';
Foreach: 'foreach';
Foreach_reverse: 'foreach_reverse';
For: 'for';
Goto: 'goto';
If: 'if';
Import: 'import';
In: 'in';
Interface: 'interface';
Invariant: 'invariant';
Is: 'is';
Lazy: 'lazy';
Macro: 'macro';
Mixin: 'mixin';
Module: 'module';
New: 'new';
Nothrow: 'nothrow';
Null: 'null';
Out: 'out';
Override: 'override';
Pure: 'pure';
Ref: 'ref';
Return: 'return';
Struct: 'struct';
Super: 'super';
Switch: 'switch';
Template: 'template';
This: 'this';
Throw: 'throw';
True: 'true';
Try: 'try';
Typedef: 'typedef';
Typeid: 'typeid';
Typeof: 'typeof';
Union: 'union';
Unittest: 'unittest';
Version: 'version';
Volatile: 'volatile';
While: 'while';
With: 'with';

SpecialDate: '__DATE__';
SpecialEof: '__EOF__';
SpecialTime: '__TIME__';
Specialimestamp: '__TIMESTAMP__';
SpecialVendor: '__VENDOR__';
SpecialVersion: '__VERSION__';
SpecialFile: '__FILE__';
SpecialLine: '__LINE__';
SpecialModule: '__MODULE__';
SpecialFunction: '__FUNCTION__';
SpecialPrettyFunction: '__PRETTY_FUNCTION__';
Traits: '__traits';
Parameters: '__parameters';
Vector: '__vector';

Whitespace: [\u0020\u0009\u000b\u000c\u000a\u000d]+ -> skip;
fragment EndOfLine : '\u000d' | '\u000a' | '\u000d' '\u000a' | '\u2028' | '\u2029';

fragment Character: [\u0001-\uffff];
fragment WysiwygCharacter: Character | Whitespace;
fragment HexDigit: [a-fA-F0-9];
fragment OctalDigit: [0-7];
fragment BinDigit: [01];
fragment DecimalDigit: [0-9];

fragment BlockComment: '/*' (Character | Whitespace)* '*/';
fragment LineComment: '//' (~[\r\n])* EndOfLine;
fragment NestingBlockComment: '/+' (NestingBlockComment | Character*) '+/';
Comment : (BlockComment | LineComment | NestingBlockComment) -> skip;

Identifier : ([a-zA-Z_])([a-zA-Z0-9_])*;

fragment WysiwygString : 'r"' '"' StringPostfix?;
fragment AlternativeWysiwygString : '`' WysiwygCharacter* '`' StringPostfix?;
fragment EscapeSequence : '\\\''
    | '\\"'
    | '\\\\'
    | '\\0'
    | '\\a'
    | '\\b'
    | '\\f'
    | '\\n'
    | '\\r'
    | '\\t'
    | '\\v'
    | '\\x' HexDigit HexDigit
    | '\\' OctalDigit OctalDigit
    | '\\' OctalDigit OctalDigit OctalDigit
    | '\\u' HexDigit HexDigit HexDigit HexDigit
    | '\\U' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit
    | '\\&' Character+ ';';
fragment HexStringChar : [0-9a-fA-F] | Whitespace | EndOfLine;
fragment StringPostfix : [dwc];
fragment DoubleQuotedCharacter : EscapeSequence | ~('"' | '\'');
fragment DoubleQuotedString : '"' DoubleQuotedCharacter* '"'  StringPostfix?;
fragment HexString: 'x"' HexStringChar* '"' StringPostfix?;
// fragment DelimitedString: 'q"' Delimiter WysiwygCharacter+ MatchingDelimiter '"';
// fragment TokenString: 'q{' Token* '}';
StringLiteral : WysiwygString | AlternativeWysiwygString | DoubleQuotedString | HexString /*| DelimitedString | TokenString*/;

CharacterLiteral: '\'' (Character | EscapeSequence) '\'';

IntegerLiteral: Integer IntegerSuffix?;
fragment Integer: BinaryInteger | DecimalInteger | HexadecimalInteger;
fragment IntegerSuffix: ('L' | 'u' | 'U' | 'Lu' | 'LU' | 'uL' | 'UL');
fragment DecimalInteger: DecimalDigit (DecimalDigit | '_')*;
fragment BinaryInteger: ('0b' | '0B') BinDigit (BinDigit | '_')*;
fragment HexadecimalInteger: ('0x' | '0X') HexDigit (HexDigit | '_')*;

FloatLiteral: (FloatOption FloatSuffix?) | (Integer (FloatSuffix | RealSuffix)? ImaginarySuffix);
fragment FloatOption: DecimalFloat | HexFloat;
fragment DecimalFloat: (DecimalInteger '.' DecimalDigit*);
fragment DecimalExponent: ('e' | 'E' | 'e+' | 'E+' | 'e-' | 'E-') DecimalInteger;
fragment FloatSuffix: 'F' | 'f';
fragment RealSuffix: 'L';
fragment ImaginarySuffix: 'i';
fragment HexFloat: ('0x' | '0X') ((HexDigit (HexDigit | '_')* '.' HexDigit (HexDigit | '_')*) | ('.' HexDigit (HexDigit | '_')*) | (HexDigit (HexDigit | '_')*)) HexExponent;
fragment HexExponent: ('p' | 'P' | 'p+' | 'P+' | 'p-' | 'P-') DecimalDigit (DecimalDigit | '_')*;

SpecialTokenSequence: ('#line' IntegerLiteral ('"' Character+ '"')? EndOfLine) -> skip;

module: moduleDeclaration? declaration*
    ;

moduleDeclaration: 'module' identifierChain ';'
    ;

declaration: attributedDeclaration
    | importDeclaration
    | functionDeclaration
    | variableDeclaration
    | aliasThisDeclaration
    | structDeclaration
    | classDeclaration
    | interfaceDeclaration
    | unionDeclaration
    | aliasDeclaration
    | mixinDeclaration
    | unittest
    | templateDeclaration
    | staticConstructor
    | staticDestructor
    | sharedStaticDestructor
    | sharedStaticConstructor
    ;

importDeclaration: 'static'? 'import' importList ';'
    ;

importList: singleImport (',' importList)?
    | importBindings
    ;

singleImport: (Identifier '=')? identifierChain
    ;

importBindings: singleImport ':' importBindList
    ;

importBindList: importBind (',' importBind)?
    ;

importBind: Identifier ('=' Identifier)?
    ;

aliasThisDeclaration: 'alias' Identifier 'this' ';'
    ;

structDeclaration: 'struct' Identifier (templateParameters constraint? structBody | (structBody | ';'))
    ;

templateParameters: '(' templateParameterList? ')'
    ;

constraint: 'if' '(' expression ')'
    ;

structBody: '{' declaration* '}'
    ;

classDeclaration: 'class' Identifier (templateParameters constraint?)? (':' identifierList )? classBody
    ;

classBody: '{' (declaration | invariant)* '}'
    ;

statement: ';'
    | nonemptyStatement
    ;

interfaceDeclaration: 'interface' Identifier (templateParameters constraint?)? (':' identifierList)? structBody
    ;

unionDeclaration: 'union' Identifier ((templateParameters constraint? structBody)? | (structBody | ';'))
    ;

nonemptyStatement: nonEmptyStatementNoCaseNoDefault
    | caseStatement
    | caseRangeStatement
    | defaultStatement
    ;

nonEmptyStatementNoCaseNoDefault: labeledStatement
    | assignStatement
    | ifStatement
    | whileStatement
    | doStatement
    | forStatement
    | foreachStatement
    | switchStatement
    | finalSwitchStatement
    | continueStatement
    | breakStatement
    | returnStatement
    | gotoStatement
    | withStatement
    | synchronizedStatement
    | tryStatement
    | throwStatement
    | scopeGuardStatement
    | asmStatement
    | pragmaStatement
    | foreachRangeStatement
    | conditionalStatement
    | staticAssert
    | templateMixinStatement
    | versionSpecification
    | debugSpecification
    | functionCallStatement
    | deleteStatement
    ;

labeledStatement: Identifier ':' statement
    ;

returnStatement: 'return' expression? ';'
    ;

switchStatement: 'switch' '(' expression ')' blockStatement
    ;

finalSwitchStatement: 'final' switchStatement
    ;

caseStatement: 'case' argumentList ':' statementListNoCaseNoDefault
    ;

caseRangeStatement: 'case' assignExpression ':' '...' 'case' assignExpression ':' statementListNoCaseNoDefault
    ;

defaultStatement: 'default' ':' statementListNoCaseNoDefault
    ;

statementListNoCaseNoDefault: statementNoCaseNoDefault statementListNoCaseNoDefault?
    ;

statementNoCaseNoDefault: ';'
    | nonEmptyStatementNoCaseNoDefault
    ;

continueStatement: 'continue' Identifier? ';'
    ;

breakStatement: 'break' Identifier? ';'
    ;

gotoStatement: 'goto' Identifier ';'
    ;

withStatement: 'with' '(' (expression | symbol | templateInstance) ')' nonEmptyStatementNoCaseNoDefault
    ;

synchronizedStatement: 'synchronized' nonEmptyStatementNoCaseNoDefault
    | 'synchronized' '(' expression ')' nonEmptyStatementNoCaseNoDefault
    ;

tryStatement: 'try' nonEmptyStatementNoCaseNoDefault (catches | catches finally_ | finally_)
    ;

catches: lastcatch
    | catch_ catches?
    ;

lastcatch: 'catch' nonEmptyStatementNoCaseNoDefault
    ;

catch_: 'catch' '(' type Identifier ')' nonEmptyStatementNoCaseNoDefault
    ;

finally_: 'finally' nonEmptyStatementNoCaseNoDefault
    ;

throwStatement: 'throw' expression ';'
    ;

scopeGuardStatement: 'scope' '(' Identifier ')' nonEmptyStatementNoCaseNoDefault
    ;

asmStatement: 'asm' '{' asminstructions '}'
    ;

asminstructions: asminstruction asminstructions?
    ;

asminstruction: Identifier
    | 'align' IntegerLiteral
    | 'align' Identifier
    | Identifier ':' asminstruction
    | Identifier operands
    | opcode operands
    ;

operands: operand operands?
    ;

register: Identifier
    | Identifier '(' IntegerLiteral ')'
    ;

opcode: Identifier
    ;

operand: asmexp
    ;

asmexp: asmlogorexp
    | asmlogorexp '?' asmexp ':' asmexp
    ;

asmlogorexp: asmlogandexp
    | asmlogandexp '||' asmlogandexp
    ;

asmlogandexp: asmorexp
    | asmorexp '&&' asmorexp
    ;

asmorexp: asmxorexp
    | asmxorexp '|' asmxorexp
    ;

asmxorexp: asmandexp
    | asmandexp '^' asmandexp
    ;

asmandexp: asmequalexp
    | asmequalexp '&' asmequalexp
    ;

asmequalexp: asmrelexp
    | asmrelexp '==' asmrelexp
    | asmrelexp '!=' asmrelexp
    ;

asmrelexp: asmshiftexp
    | asmshiftexp '<' asmshiftexp
    | asmshiftexp '<=' asmshiftexp
    | asmshiftexp '>' asmshiftexp
    | asmshiftexp '>=' asmshiftexp
    ;

asmshiftexp: asmaddexp
    | asmaddexp '<<' asmaddexp
    | asmaddexp '>>' asmaddexp
    | asmaddexp '>>>' asmaddexp
    ;

asmaddexp: asmmulexp
    | asmmulexp '+' asmmulexp
    | asmmulexp '-' asmmulexp
    ;

asmmulexp: asmbrexp
    | asmbrexp '*' asmbrexp
    | asmbrexp '/' asmbrexp
    | asmbrexp '%' asmbrexp
    ;

asmbrexp: asmunaexp
    | asmbrexp '[' asmexp ']'
    ;

asmunaexp: asmtypeprefix asmexp
    | Identifier asmexp
    | '+' asmunaexp
    | '-' asmunaexp
    | '!' asmunaexp
    | '~' asmunaexp
    | asmprimaryexp
    ;

asmprimaryexp: IntegerLiteral
    | FloatLiteral
    | '$'
    | register
    | identifierChain
    ;

asmtypeprefix: Identifier Identifier
    | 'byte' Identifier
    | 'short' Identifier
    | 'int' Identifier
    | 'float' Identifier
    | 'double' Identifier
    | 'real' Identifier
    ;

pragmaStatement: pragma ';'
    ;

pragma: 'pragma' '(' Identifier (',' argumentList)? ')'
    ;

foreachRangeStatement: 'foreach' '(' foreachType ';' expression '..' expression ')' nonEmptyStatementNoCaseNoDefault
    ;

conditionalStatement: compileCondition nonEmptyStatementNoCaseNoDefault
    | compileCondition nonEmptyStatementNoCaseNoDefault 'else' nonEmptyStatementNoCaseNoDefault
    ;

compileCondition: versionCondition
    | debugCondition
    | staticIfCondition
    ;

versionCondition: 'version' '(' IntegerLiteral ')'
    | 'version' '(' Identifier ')'
    | 'version' '(' 'unittest' ')'
    | 'version' '(' 'assert' ')'
    ;

versionSpecification: 'version' '=' Identifier ';'
    | 'version' '=' IntegerLiteral ';'
    ;

castExpression: 'cast' '(' type ')' unaryExpression
    | 'cast' '(' castQualifier ')' unaryExpression
    | 'cast' '(' ')' unaryExpression
    ;

castQualifier: 'const'
    | 'const' 'shared'
    | 'shared' 'const'
    | 'inout'
    | 'inout' 'shared'
    | 'shared' 'inout'
    | 'immutable'
    | 'shared'
    ;

debugCondition: 'debug'
    | 'debug' '(' IntegerLiteral ')'
    | 'debug' '(' Identifier ')'
    ;

debugSpecification: 'debug' '=' Identifier ';'
    | 'debug' '=' IntegerLiteral ';'
    ;

staticIfCondition: 'static' 'if' '(' assignExpression ')'
    ;

staticAssert: 'static' 'assert' '(' assignExpression (',' assignExpression)? ')' ';'
    ;

templateMixinStatement: 'mixin' mixinTemplateName templateArguments? Identifier? ';'
    ;

mixinTemplateName: '.' qualifiedIdentifierChain
    | qualifiedIdentifierChain
    | typeof '.' qualifiedIdentifierChain
    ;

qualifiedIdentifierChain: Identifier
    | Identifier '.' qualifiedIdentifierChain
    | templateInstance '.' qualifiedIdentifierChain
    ;

functionCallStatement: functionCallExpression ';'
    ;

deleteStatement: deleteExpression ';'
    ;

assignStatement: unaryExpression assignOperator assignExpression ';'
    | preIncDecExpression ';'
    | postIncDecExpression ';'
    ;

assignOperator: '='
    | '>>>='
    | '>>='
    | '<<='
    | '+='
    | '-='
    | '*='
    | '%='
    | '&='
    | '/='
    | '|='
    | '^^='
    | '^='
    | '~='
    ;

ifStatement: 'if' '(' expression ')' statement ('else' statement)?
    ;

forStatement: 'for' '(' initialize expression ';' expression ')' statement
    | 'for' '(' initialize ';' expression ')' statement
    ;

initialize: ';'
    | nonemptyStatement
    ;

foreachStatement: ('foreach' | 'foreach_reverse') '(' foreachTypeList ';' expression ')' nonEmptyStatementNoCaseNoDefault
    ;

foreachTypeList: foreachType (',' foreachType)*
    ;

foreachType: 'ref'? type? Identifier
    ;

expression: assignExpression (',' assignExpression)*
    ;

identifierOrTemplateInstance: Identifier
    | templateInstance
    ;

templateInstance: Identifier templateArguments
    ;

typeofExpression: 'typeof' '(' (expression | 'return') ')'
    ;

typeidExpression: 'typeid' '(' type ')'
    | 'typeid' '(' expression ')'
    ;

isExpression: 'is' '(' type Identifier? (((':' | '==') typeSpecialization (',' templateParameterList)? ))? ')'
    ;

templateParameterList: templateParameter (','? templateParameter)*
    ;

templateParameter: templateTypeParameter
    | templateValueParameter
    | templateAliasParameter
    | templateTupleParameter
    | templateThisParameter
    ;

templateTypeParameter: Identifier (':' type)? ('=' type)?
    ;

templateValueParameter: type Identifier (':' expression)? templateValueParameterDefault?
    ;

templateValueParameterDefault:  '=' ('__FILE__' | '__MODULE__' | '__LINE__' | '__FUNCTION__' | '__PRETTY_FUNCTION__' | assignExpression)
    ;

templateAliasParameter: 'alias' type? Identifier templatealiasparameterspecialization? templatealiasparameterdefault?
    ;

templatealiasparameterspecialization: ':' (type | expression)
    ;

templatealiasparameterdefault: '=' (type | expression)
    ;

templateTupleParameter: Identifier '...'
    ;

templateThisParameter: 'this' templateTypeParameter
    ;

typeSpecialization: type
    | 'struct'
    | 'union'
    | 'class'
    | 'interface'
    | 'enum'
    | 'function'
    | 'delegate'
    | 'super'
    | 'const'
    | 'immutable'
    | 'inout'
    | 'shared'
    | 'return'
    | '__parameters'
    ;

templateArguments: '!' ('(' templateArgumentList? ')' | templateSingleArgument)
    ;

templateArgumentList: templateArgument (',' templateArgument?)*
    ;

templateArgument: type
    | assignExpression
    | symbol
    ;

symbol: '.'? symbolTail
    ;

symbolTail: identifierOrTemplateInstance ('.' symbolTail)?
    ;

templateSingleArgument: Identifier
    | builtinType
    | CharacterLiteral
    | StringLiteral
    | IntegerLiteral
    | FloatLiteral
    | 'true'
    | 'false'
    | 'null'
    | 'this'
    | '__FILE__'
    | '__MODULE__'
    | '__LINE__'
    | '__FUNCTION__'
    | '__PRETTY_FUNCTION__'
    ;

functionCallExpression: unaryExpression arguments
    ;

arguments: '(' argumentList? ')'
    ;

argumentList: assignExpression (',' argumentList?)?
    ;

newExpression: 'new' type ('[' assignExpression ']' | arguments)?
    | newAnonClassExpression
    ;

newAnonClassExpression: 'new' arguments? 'class' arguments? Identifier identifierList? classBody
    ;

deleteExpression: 'delete' unaryExpression
    ;

assignExpression: ternaryExpression
    | ternaryExpression assignOperator assignExpression
    ;

ternaryExpression: orOrExpression
    | orOrExpression '?' expression ':' ternaryExpression
    ;

orOrExpression: andAndExpression
    | orOrExpression '||' andAndExpression
    ;

andAndExpression: orExpression
    | cmpExpression
    | andAndExpression '&&' (orExpression | cmpExpression)
    ;

orExpression: xorExpression
    | orExpression '|' xorExpression
    ;

xorExpression: andExpression
    | xorExpression '^' andExpression
    ;

andExpression: shiftExpression
    | andExpression '&' shiftExpression
    ;

cmpExpression: shiftExpression
    | equalExpression
    | identityExpression
    | relExpression
    | inExpression
    ;

equalExpression: shiftExpression ('==' | '!=') shiftExpression;

identityExpression: shiftExpression ('is' | '!' 'is') shiftExpression;

relExpression: shiftExpression ('<' | '<=' | '>' | '>=' | '!<>=' | '!<>' | '<>' | '<>=' | '!>' | '!>=' | '!<' | '!<=') shiftExpression;

inExpression: shiftExpression ('in' | '!' 'in') shiftExpression;

shiftExpression: addExpression
    | shiftExpression ('<<' | '>>' | '>>>') addExpression;

addExpression: mulExpression
    | addExpression ('+' | '-' | '~') mulExpression
    ;

mulExpression: powExpression
    | mulExpression ('*' | '/' | '%') powExpression
    ;

powExpression: unaryExpression
    | unaryExpression '^^' powExpression
    ;

unaryExpression: primaryExpression
    | '&' unaryExpression
    | '!' unaryExpression
    | '*' unaryExpression
    | '+' unaryExpression
    | '-' unaryExpression
    | '~' unaryExpression
    | preIncDecExpression
    | newExpression
    | deleteExpression
    | castExpression
    | unaryExpression ('++'| '--') /* postIncDecExpression */ /* This causes an error in ANTLR */
    | unaryExpression '[' ']'
    | unaryExpression '[' argumentList ']'
    | unaryExpression '[' assignExpression '..' assignExpression ']'
    | unaryExpression '.' identifierOrTemplateInstance
    ;

postIncDecExpression: unaryExpression ('++' | '--')
    ;

preIncDecExpression: ('++' | '--') unaryExpression
    ;

primaryExpression: identifierOrTemplateInstance
    | '.' identifierOrTemplateInstance
    | type '.' Identifier
    | typeofExpression
    | typeidExpression
    | 'this'
    | 'super'
    | 'null'
    | 'true'
    | 'false'
    | '__FILE__'
    | '__MODULE__'
    | '__LINE__'
    | '__FUNCTION__'
    | '__PRETTY_FUNCTION__'
    | IntegerLiteral
    | FloatLiteral
    | StringLiteral
    | CharacterLiteral
    | arrayLiteral
    | assocArrayLiteral
    | '(' expression ')'
    | isExpression
    | lambdaExpression
    | traitsExpression
    ;

whileStatement: 'while' '(' expression ')' blockStatement
    ;

doStatement: 'do' blockStatement 'while' '(' expression ')' ';'
    ;

blockStatement: '{' declarationsAndStatements? '}'
    ;

declarationsAndStatements: (declaration | statement)+
    ;

functionDeclaration: type Identifier parameters (functionBody | ';')
    ;

functionTemplateDeclaration: type Identifier templateParameters parameters constraint? functionBody
    ;

type: typeConstructors? type2
    ;

type2: type3 typeSuffix?
    | type2 typeSuffix
    ;

type3: builtinType
    | '.' identifierChain
    | identifierChain
    | typeof
    | typeof '.' identifierChain
    | 'const' '(' type ')'
    | 'immutable' '(' type ')'
    | 'shared' '(' type ')'
    | 'inout' '(' type ')'
    | 'delegate' parameters memberFunctionAttributes?
    | 'function' parameters memberFunctionAttributes?
    ;

typeSuffix: '*'
    | '[' ']'
    | '[' type ']'
    | '[' assignExpression ']'
    ;

builtinType: 'bool'
    | 'byte'
    | 'ubyte'
    | 'short'
    | 'ushort'
    | 'int'
    | 'uint'
    | 'long'
    | 'ulong'
    | 'char'
    | 'wchar'
    | 'dchar'
    | 'float'
    | 'double'
    | 'real'
    | 'ifloat'
    | 'idouble'
    | 'ireal'
    | 'cfloat'
    | 'cdouble'
    | 'creal'
    | 'void'
    ;

typeConstructors: typeConstructor+
    ;

typeConstructor: 'const'
    | 'immutable'
    | 'inout'
    | 'shared'
    ;

typeof: 'typeof' '(' (expression | 'return') ')'
    ;

parameters: '(' (parameter (',' parameter)*)? ')'
    ;

parameter: parameterAttribute* type ('...' | (Identifier ('=' defaultInitializerExpression)?))?
    ;

parameterAttribute: 'auto'
    | 'final'
    | 'in'
    | 'lazy'
    | 'out'
    | 'ref'
    | 'scope'
    | typeConstructor
    ;

defaultInitializerExpression: assignExpression
    | '__FILE__'
    | '__MODULE__'
    | '__LINE__'
    | '__FUNCTION__'
    | '__PRETTY_FUNCTION__'
    ;

functionAttribute: 'nothrow'
    | 'pure'
    | atAttribute
    ;

memberFunctionAttribute: 'const'
    | 'immutable'
    | 'inout'
    | 'shared'
    | functionAttribute
    ;

memberFunctionAttributes: memberFunctionAttribute+
    ;

functionBody: blockStatement
    | (inStatement | outStatement | outStatement inStatement | inStatement outStatement)? bodyStatement
    ;

inStatement: 'in' blockStatement
    ;

outStatement: 'out' ('(' Identifier ')')? blockStatement
    ;

bodyStatement: 'body' blockStatement
    ;

aliasDeclaration: 'alias' (aliasInitializer (',' aliasInitializer)* | type declarator) ';'
    ;

aliasInitializer: Identifier '=' type
    ;

variableDeclaration: type declarator (',' declarator)* ';'
    ;

declarator: Identifier ('=' initializer)?
    ;

mixinDeclaration: 'mixin' '(' assignExpression ')' ';'
    ;

identifierList: Identifier (',' Identifier)*
    ;

identifierChain: Identifier ('.' Identifier)*
    ;

attributedDeclaration: attribute (':' | declaration | '{' declaration* '}')
    ;

attribute: linkageattribute
    | alignattribute
    | pragma
    | protectionAttribute
    | 'deprecated'
    | 'extern'
    | 'final'
    | 'synchronized'
    | 'override'
    | 'abstract'
    | 'const'
    | 'auto'
    | 'scope'
    | '__gshared'
    | 'shared'
    | 'immutable'
    | 'inout'
    | atAttribute
    ;

linkageattribute: 'extern' '(' Identifier '++'? ')'
    ;

atAttribute: '@' (Identifier | '(' argumentList ')' | functionCallExpression)
    ;

alignattribute: 'align' ('(' IntegerLiteral ')')?
    ;

protectionAttribute: 'private'
    | 'package'
    | 'protected'
    | 'public'
    | 'export'
    ;

traitsExpression: '__traits' '(' Identifier ',' traitsArgument (',' traitsArgument)* ')'
    ;

traitsArgument: assignExpression
    | type
    ;

unittest: 'unittest' blockStatement
    ;

templateDeclaration: 'template' Identifier templateParameters constraint? '{' declaration+ '}'
    ;

staticConstructor: 'static' 'this' '(' ')' functionBody
    ;

staticDestructor: 'static' '~' 'this' '(' ')' functionBody
    ;

sharedStaticDestructor: 'shared' 'static' 'this' '(' ')' functionBody
    ;

sharedStaticConstructor: 'shared' 'static' '~' 'this' '(' ')' functionBody
    ;

invariant: 'invariant' '(' ')' blockStatement
    ;

arrayinitializer: '[' arraymemberinitializations? ']'
    ;

arraymemberinitializations: arraymemberinitialization
    | arraymemberinitialization ','
    | arraymemberinitialization ',' arraymemberinitializations
    ;

arraymemberinitialization: (assignExpression ':')? nonVoidInitializer
    ;

initializer: voidinitializer
    | nonVoidInitializer
    ;

voidinitializer: 'void'
    ;

nonVoidInitializer: assignExpression
    | arrayinitializer
    | structinitializer
    ;

structinitializer: '{' structMemberInitializers? '}'
    ;

structMemberInitializers: structMemberInitializer (','? structMemberInitializers)?
    ;

structMemberInitializer: (Identifier ':')? nonVoidInitializer
    ;

lambdaExpression: (Identifier | parameters functionAttribute? ) '=>' assignExpression
    ;

arrayLiteral: '[' argumentList ']'
    ;

assocArrayLiteral: '[' keyValuePairs ']'
    ;

keyValuePairs: keyValuePair
    | keyValuePair ',' keyValuePairs
    ;

keyValuePair: assignExpression ':' assignExpression
    ;
