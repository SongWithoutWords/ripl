lexer grammar RiplLexer;

@header { package ripl.parser.antlr; }

// tokens { INDENT, DEDENT }

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
Indent
    : '{';
Dedent
    : '}';
Semicolon
    : ';';
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
Equal
    : '=';
LT
    : '<';
LTEQ
    : '<=';
EQ
    : '==';
NEQ
    : '!=';
GTEQ
    : '>=';
GT
    : '>';
At
    : '@';

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

