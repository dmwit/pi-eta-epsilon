{-# LANGUAGE GADTs, KindSignatures, 
    TypeOperators, TemplateHaskell, QuasiQuotes #-}
module Language.PiEtaEpsilon.BNFMeta.Term where
--from bnfc-meta
import Language.LBNF(lbnf, dumpCode, bnfc)
import Language.LBNF.Compiletime
import qualified Language.LBNF.Grammar


bnfc [lbnf|

-- This is a new pragma. The rest of the grammar is original JL.
antiquote "[" ":" ":]" ;

--Iso                                               
BIdentityS.        BaseIso0    ::= "<=+=>"              ;
BIdentityP.        BaseIso1    ::= "<=*=>"              ;
BCommutativeS.     BaseIso2    ::= "commutativeS"       ;
BCommutativeP.     BaseIso3    ::= "commutativeP"       ;
BAssociativeS.     BaseIso4    ::= "|+|+|"              ;
BAssociativeP.     BaseIso5    ::= "|*|*|"              ;
BSplitS.           BaseIso6    ::= "-+<"                ;
BSplitP.           BaseIso7    ::= "-*<"                ;
BDistributiveZero. BaseIso8    ::= "distributiveZero"   ;
BDistributivePlus. BaseIso9    ::= "distributivePlus"   ;
--
IEliminate.        Iso0   ::= "#"     BaseIso            ;
IIntroduce.        Iso1   ::= "'"     BaseIso            ;
--
--Terms
TDistribute.       Term0    ::= Term1 ";" Term1        ;
TPlus.             Term1    ::= Term1 "+" Term2        ;
TTimes.            Term2    ::= Term2 "*" Term3        ;
TBase.             Term3    ::= "<" Iso                ;
TId.               Term4    ::= Ident                  ;


_.              Term      ::= Term0        ;
_.              Term0     ::= Term1        ;
_.              Term1     ::= Term2        ;
_.              Term2     ::= Term3        ;
_.              Term3     ::= Term4        ;
_.              Term4     ::= "(" Term ")" ;

_.      Iso       ::= Iso0         ;
_.      Iso0      ::= Iso1         ;
_.      Iso1      ::= "(" Iso ")"         ;


_.  BaseIso   ::= BaseIso0        ;  
_.  BaseIso0  ::= BaseIso1        ;
_.  BaseIso1  ::= BaseIso2        ;
_.  BaseIso2  ::= BaseIso3        ;
_.  BaseIso3  ::= BaseIso4        ;
_.  BaseIso4  ::= BaseIso5        ;
_.  BaseIso5  ::= BaseIso6        ;
_.  BaseIso6  ::= BaseIso7        ;
_.  BaseIso7  ::= BaseIso8        ;
_.  BaseIso8  ::= BaseIso9        ;
_.  BaseIso9  ::= "(" BaseIso ")" ;






TDouble.  Typ  ::= "double" ;

-- pragmas

comment "/*" "*/" ;
comment "//" ;

entrypoints Term, Iso, BaseIso ;
  |]
  
