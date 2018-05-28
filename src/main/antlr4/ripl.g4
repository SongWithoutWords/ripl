grammar ripl;

@header { package ripl.parser.antlr; }


// fragments
fragment Underscore
    : '_';
fragment Upper
    : 'A' .. 'Z';
fragment Lower
    : 'a' .. 'z';
fragment Letter
    : Upper | Lower;
fragment LetterOrUnderscore
    : Letter | Underscore;
fragment Digit
    : '0' .. '9';
fragment LetterOrUnderscoreOrDigit
    : LetterOrUnderscore | Digit;

fragment OpChar
    : '!'
    | '#'
    | '$'
    | '%'
    | '&'
    | '*'
    | '+'
    | '-'
    // | '.' // Not sure it can be used without ambiguity
    | '/'
    | ':'
    | '<'
    | '='
    | '>'
    | '?'
    // | '@' // Not sure it can be used without ambiguity
    | '^'
    // | '~' // Not sure it can be used without ambiguity
    ;


// Tokens

// keywords
If
    : 'if';
Then
    : 'then';
Else
    : 'else';

// punctuation
LParen
    : '(';
RParen
    : ')';
Comma
    : ',';
Period
    : '.';
Quote
    : '"';

// operators
Tilda
    : '~';
Plus
    : '+';
Minus
    : '-';
Star
    : '*';
Slash
    : '/';
Percent
    : '%';
Chevron
    : '^';
Ampersand
    : '&';
LessThan
    : '<';
Equal
    : '=';
GreaterThan
    : '>';
At
    : '@';
EqualEqual
    : '==';
NotEqual
    : '!=';
ThinArrow
    : '->';
FatArrow
    : '=>';

And
    : 'and';
Or
    : 'or';
Not
    : 'not';


// tokens with data
VInt
    : '0' | [1-9][0-9]*;

VFlt
    : VInt '.' [0-9]*;

VBln
    : 'true'
    | 'false';

VStr
    : '"' .*? '"'
    ;

Name
    : LetterOrUnderscore LetterOrUnderscoreOrDigit*
    | OpChar+
    ;

// grammar
exp0
    : Name              #name
    | VBln              #bln
    | VInt              #int
    | VFlt              #flt
    | VStr              #str

    | '(' e=exp2 ')'    #bracketExp

    | '@'               #impure

    // Recover names of operator tokens to enable reference outside of binops
    | Plus              #plus
    | Minus             #minus
    | Star              #star
    | Slash             #slash
    | Percent           #percent
    ;

exp1

    // Left associativity is intuitive for addition, subtraction, multiplication, division
    // Right associativity is intuitive for list construction and exponentiation
    // http://kevincantu.org/code/operators.html

    // Another thought: should selection be treated as another operator?

    : op=('~'|'-'|'not') e=exp1
        #unaryOp

    | e1=exp1 op=('*'|'/'|'%') e2=exp1
        #binOpMulDivMod

    | e1=exp1 op=('+'|'-') e2=exp1
        #binOpAddSub

    | e1=exp1 op=('<'|'<='|'=='|'!='|'>='|'>') e2=exp1
        #binOpCompare

    | e1=exp1 'and' e2=exp1
        #binOpAnd

    | e1=exp1 'or' e2=exp1
        #binOpOr

    | e1=exp1 op=exp0 e2=exp1
        #binOp

    | paramTypes=funTypeParams '->' returnType=exp1
        #funType

    | '(' ( params+=param (',' params+=param)* )? ')' ('->' returnType=exp1)? '=>' exp=exp2
        #fun

    | f=exp0 '(' args=exps? ')'
        #apply

    | e1=exp0 '.' e2=exp0
        #select

    | e=exp0
        #exp10
    ;

exp2
    : If e1=exp1 Then e2=exp1 Else e3=exp2
        #if

    | a=exp1 '=' b=exp1
        #assign

    | exp1
        #exp21
    ;

funTypeParams
    : exp0
        #funTypeParamExp
    | '(' exps ')'
        #funTypeParamExps
    ;

exps
    : exp2 (',' exp2)*
    ;

param
    // : exp0
        // #paramSingle // will be used for purity, e.g. getInput(~@) -> String
    : exp0 exp0
        #paramDouble
    ;

