{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module JavaletteLight where

import Language.LBNF(lbnf, dumpCode, bnfc)
import Language.LBNF.Compiletime
import qualified Language.LBNF.Grammar




bnfc [lbnf|

-- This is a new pragma. The rest of the grammar is original JL.
antiquote "[" ":" ":]" ;

-- Javalette Light: a simple subset of C, covering
-- programs with a single zero-argument function.
-- example: koe.jll
-- ordinary rules

Fun.      Prog     ::= Typ Ident "(" ")" "{" [Stm] "}" ;

SDecl.    Stm      ::= Typ Ident ";"  ;
SAss.     Stm      ::= Ident "=" Expr ";"  ;
SIncr.    Stm      ::= Ident "++" ";"  ;
SWhile.   Stm      ::= "while" "(" Expr ")" "{" [Stm] "}" ;

ELt.      Expr0     ::= Expr1 "<" Expr1 ;
EPlus.    Expr1     ::= Expr1 "+" Expr2 ;
ETimes.   Expr2     ::= Expr2 "*" Expr3 ;
EVar.     Expr3     ::= Ident ;
EInt.     Expr3     ::= Integer ;
EDouble.  Expr3     ::= Double ;

[].       [Stm]    ::= ;
(:).      [Stm]    ::= Stm [Stm] ;

-- coercions

_.        Stm      ::= Stm ";" ;

_.  Expr      ::= Expr0 ;
_.  Expr0     ::= Expr1 ;
_.  Expr1     ::= Expr2 ;
_.  Expr2     ::= Expr3 ;
_.  Expr3     ::= "(" Expr ")" ;

TInt.     Typ  ::= "int" ;
TDouble.  Typ  ::= "double" ;

-- pragmas

comment "/*" "*/" ;
comment "//" ;

entrypoints Prog, Stm, Expr ;
  |]


