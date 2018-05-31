parser grammar RiplParser;

options { tokenVocab = RiplLexer; }

@header { package ripl.parser.antlr; }


// grammar
exp0
    : Name                  #name
    | VBln                  #bln
    | VInt                  #int
    | VFlt                  #flt
    | VStr                  #str

    | LParen e=exp2 RParen  #bracketExp

    | At                    #impure

    // Recover names of operator tokens to enable reference outside of binops
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

    : op=(Tilda | Minus | Not) e=exp1
        #unaryOp

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

    | LParen ( params += pair (Comma params += pair)* )? RParen
        (ThinArrow returnType = exp1)?
        FatArrow exp = exp2
        #fun

    | f=exp0 LParen args=exps? RParen
        #apply

    | e1=exp0 Period e2=exp0
        #select

    | blockBegin (es+=exp2 lineSep)* es+=exp2? blockEnd
        #block

    | Data name=exp0 lineSep? (blockBegin (fields+=pair lineSep)* fields+=pair blockEnd)?
        #data

    | e=exp0
        #exp10
    ;

exp2
    : If e1=exp1 Then e2=exp1 Else e3=exp2
        #if

    | a=exp1 Equal b=exp1
        #assign

    | exp1
        #exp21
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

pair
    // : exp0
        // #paramSingle // will be used for purity, e.g. getInput(~@) -> String
    : exp0 exp0
    ;

