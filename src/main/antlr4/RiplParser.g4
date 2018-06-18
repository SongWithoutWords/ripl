parser grammar RiplParser;

options { tokenVocab = RiplLexer; }

@header { package ripl.parser.antlr; }

exp
    // atoms

    // should probably rename 'name' to 'symbol'
    // (it's more general now)
    : Name
        #name
    | VBln
        #bln
    | VInt
        #int
    | VFlt
        #flt
    | VStr
        #str
    | At
        #world

    // composite
    | LParen (es+=exp Newline?)* RParen
        #sExp

    | Indent (es+=exp Newline)+ Dedent
        #iExp

    | e1=exp Period e2=exp
        #select

    | op=(Tilda | Chevron | Apostrophe) e=exp
        #unaryOp

    // Recover names of operator tokens to enable reference outside of operations
    ;
