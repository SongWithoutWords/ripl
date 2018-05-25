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
exp
    : exp2
    | exp1
    | exp0
    ;

exp2
    : exp1 '=' exp1
    ;

exp1
    : exp0 exp0 exp0
        #binaryOp
    | If exp0 Then exp0 Else exp0
        #ifExp
    | '('exp1? (',' exp1)* ')' '->' exp0
        #functionType
    | exp0 '('exp1? (',' exp1)* ')'
        #application
    | exp0 '.' Name
        #selection
    ;

exp0
    // : LParen exp1 RParen
    : Name
        #name
    | VInt
        #int
    | VFlt
        #flt
    | '(' exp ')'
        #bracketExp
    ;


