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
    // | '.' // I don't think a dot can be used unambiguously
    | '/'
    | ':'
    | '<'
    | '='
    | '>'
    | '?'
    | '@'
    | '^'
    // | '~' // I don't think a tilda can be used unambiguously
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
GreaterThan
    : '>';
Equal
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

    // Recover names of operator tokens to enable reference outside of binops
    | Plus              #plus
    | Minus             #minus
    | Star              #star
    | Slash             #slash
    | Percent           #percent
    ;

exp1
    : '-' exp1
        #negate

    | exp1 '*' exp1
        #multiplication

    | exp1 '+' exp1
        #addition

    | exp1 exp0 exp1
        #binOp

    | If exp1 Then exp1 Else exp1
        #ifExp

    | '('exp1? (',' exp1)* ')' '->' exp0
        #funType

    | exp0 '('exp1? (',' exp1)* ')'
        #application

    | exp0 '.' Name
        #selection

    | exp0
        #exp10
    ;

exp2
    : exp1 '=' exp1
        #assignment

    | exp1
        #exp21
    ;

