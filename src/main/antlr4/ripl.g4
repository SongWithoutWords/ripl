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

    | '(' exp2 ')'      #bracketExp

    | '@'               #impure

    // Recover names of operator tokens to enable reference outside of binops
    | Plus              #plus
    | Minus             #minus
    | Star              #star
    | Slash             #slash
    | Percent           #percent
    ;

exp1
    : '~' exp1
        #mutable

    | '-' exp1
        #negate

    | exp1 '*' exp1
        #multiply

    | exp1 '+' exp1
        #add

    | exp1 exp0 exp1
        #binOp

    | funTypeParams '->' exp1
        #funType

    | '(' params? ')' ('->' exp1)? '=>' exp1
        #fun

    | exp0 '(' exps? ')'
        #apply

    | exp0 '.' Name
        #select

    | exp0
        #exp10
    ;

exp2
    : If exp1 Then exp1 Else exp2
        #if

    | exp1 '=' exp1
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

