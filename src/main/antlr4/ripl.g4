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


// tokens with data
VInt
    : '0' | [1-9][0-9]*;

VFlt
    : VInt '.' [0-9]*;

VBln
    : 'true'
    | 'false';

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
    : '~' e=exp1
        #mutable

    | '-' e=exp1
        #negate

    | e1=exp1 '*' e2=exp1
        #multiply

    | e1=exp1 '+' e2=exp1
        #add

    | e1=exp1 op=exp0 e2=exp1
        #binOp

    | paramTypes=funTypeParams '->' returnType=exp1
        #funType

    | '(' params? ')' ('->' exp1)? '=>' exp1
        #fun

    | f=exp0 '(' args=exps? ')'
        #apply

    | e=exp0 '.' name=Name
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

params
    : param (',' param)*
    ;

param
    // : exp0
        // #paramSingle // will be used for purity, e.g. getInput(~@) -> String
    : exp0 exp0
        #paramDouble
    ;

