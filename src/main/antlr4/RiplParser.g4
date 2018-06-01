parser grammar RiplParser;

options { tokenVocab = RiplLexer; }

@header { package ripl.parser.antlr; }


exp0 // exp0s are atomic, tightly bound expressions
    : Name                  #name
    | VBln                  #bln
    | VInt                  #int
    | VFlt                  #flt
    | VStr                  #str

    | LParen e=exp2 RParen  #bracketExp

    | At                    #impure

    | e1=exp0 Period e2=exp0
        #select

    | f=exp0 LParen args=exps? RParen
        #apply

    | op=(Tilda | Chevron | Minus | Not) e=exp0
        #unaryOp

    // Recover names of operator tokens to enable reference outside of operations
    | Plus                  #plus
    | Minus                 #minus
    | Star                  #star
    | Slash                 #slash
    | Percent               #percent
    ;

exp1

    // Left associativity is intuitive for addition, subtraction, multiplication, division
    // Right associativity is intuitive for list construction and exponentiation
    // http://kevincantu.org/code/operators.html

    // Another thought: should selection be treated as another operator?

    : e=exp0
        #exp10

    | e1=exp1 op=(Star | Slash | Percent) e2=exp1
        #binOpMulDivMod

    | e1=exp1 op=(Plus | Minus) e2=exp1
        #binOpAddSub

    | e1=exp1 op=(LT | LTEQ | EQ | NEQ | GTEQ | GT) e2=exp1
        #binOpCompare

    | e1=exp1 And e2=exp1
        #binOpAnd

    | e1=exp1 Or e2=exp1
        #binOpOr

    | e1=exp1 op=exp0 e2=exp1
        #binOp

    | paramTypes=funTypeParams ThinArrow returnType=exp1
        #funType

    | blockBegin (es+=exp2 lineSep)* es+=exp2? blockEnd
        #block

    | define
        #e1Define

    | lambda
        #e1Lambda

    | function
        #e1Function

    | userType
        #e1UserType

    | lhs=exp1 Equal rhs=exp2
        #assign
    ;

define
    : lhs=pair Equal rhs=exp2
        // #define
    ;

lambda // a nameless function
    : LParen ( params += pair (Comma params += pair)* )? RParen
        (ThinArrow returnType = exp1)?
        FatArrow exp = exp2
    ;

function
    : name=exp0 lambda
    ;

userType
    : Data name=exp0 lineSep?
        (blockBegin
            (fields+=pair lineSep)* fields+=pair?
        blockEnd)?
        #data

    | Union name=exp0 lineSep?
        (blockBegin
            (alternatives+=exp2 lineSep)* alternatives+=exp2?
        blockEnd)?
    #union
    ;

exp2 // exp2s prevent if-exps from being used in the middle of an if-exp without parens
    : exp1
        #exp21

    | If e1=exp1 Then e2=exp1 Else e3=exp2
        #if
    ;

funTypeParams
    : exp0
        #funTypeParamExp
    | LParen exps RParen
        #funTypeParamExps
    ;

blockBegin
    : Indent
    | LBrace
    ;

blockEnd
    : Dedent
    | RBrace
    ;

lineSep
    : Newline
    | Semicolon
    ;

exps
    : exp2 (Comma exp2)*
    ;

// Would this be better as:
//  : (exp2 Comma)* exp2?

// Thoughts: it might be good if pairs themselves were exps,
// assuming this didn't cause any ambiguity,
// so that people can write what amount to macros to construct
// function signatures
pair
    // : exp0
        // #paramSingle // will be used for purity, e.g. getInput(~@) -> String
    : e1=exp0 e2=exp0
    ;

unit
    : define
        #unitDefine
    | function
        #unitFunction
    | userType
        #unitUserType
    | Namespace name=Name lineSep?
        (blockBegin
            (members+=exp2 lineSep)* members+=exp2?
        blockEnd)?
        #unitNamespace
    ;

ast
    : lineSep* (units+=unit lineSep+)* units+=unit?

    ;

