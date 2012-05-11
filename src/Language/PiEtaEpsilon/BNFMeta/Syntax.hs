{-# LANGUAGE GADTs, KindSignatures, 
    TypeOperators, TemplateHaskell, QuasiQuotes #-}
module Language.PiEtaEpsilon.BNFMeta.Syntax where
--from bnfc-meta
import Language.LBNF(lbnf, dumpCode, bnfc)
import Language.LBNF.Compiletime
import qualified Language.LBNF.Grammar


bnfc [lbnf|

-- This is a new pragma. The rest of the grammar is original JL.
antiquote "[" ":" ":]" ;

EIdentityS.        Expr0     ::= "identityS" Expr1      ;
EIdentityP.        Expr1     ::= "identityP" Expr2      ;
ECommutativeS.     Expr2     ::= "commutativeS" Expr3        ;
ECommutativeP.     Expr3     ::= "commutativeP" Expr4         ;
EAssociativeS.     Expr4     ::= "associativeS" Expr5            ;
EAssociativeP.     Expr5     ::= "associativeP" Expr6            ;
ESplitS.           Expr6     ::= "splitS"   Expr7          ;
ESplitP.           Expr7     ::= "splitP"   Expr8            ;
EDistributiveZero. Expr8     ::= "distributiveZero"  Expr9             ;
EDistributivePlus. Expr9     ::= "distributivePlus" Expr10              ;
EEliminate.        Expr10    ::= "X"     Expr11            ;
EIntroduce.        Expr11    ::= "V"     Expr12           ;
ECompose.          Expr12    ::= Expr12 "." Expr13    ;
EPlus.             Expr13    ::= Expr13 "+" Expr14    ;
ETimes.            Expr14    ::= Expr14 "*" Expr15    ;
EId.               Expr15    ::= Ident                ;
EDouble.           Expr16    ::= Double;

_.  Expr      ::= Expr0        ;
_.  Expr0     ::= Expr1        ;
_.  Expr1     ::= Expr2        ;
_.  Expr2     ::= Expr3        ;
_.  Expr3     ::= Expr4        ;  
_.  Expr4     ::= Expr5        ;
_.  Expr5     ::= Expr6        ;
_.  Expr6     ::= Expr7        ;
_.  Expr7     ::= Expr8        ;
_.  Expr8     ::= Expr9        ;
_.  Expr9     ::= Expr10       ;
_.  Expr10    ::= Expr11       ;
_.  Expr11    ::= Expr12       ;
_.  Expr12    ::= Expr13       ;
_.  Expr13    ::= Expr14       ;
_.  Expr14    ::= Expr15       ;
_.  Expr15    ::= Expr16       ;
_.  Expr16    ::= "(" Expr ")" ;    


TDouble.  Typ  ::= "double" ;

-- pragmas

comment "/*" "*/" ;
comment "//" ;

entrypoints Expr ;
  |]