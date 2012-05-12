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
IEliminate.        Iso   ::= "X"     BaseIso          ;
IIntroduce.        Iso   ::= "V"     BaseIso          ;
--
--Terms
TCompose.          Term0    ::= Term1 "." Term1        ;
TPlus.             Term1    ::= Term1 "+" Term2        ;
TTimes.            Term2    ::= Term2 "*" Term3        ;
TBase.             Term3    ::= "@" Iso                ;
TId.               Term4    ::= Ident                  ;

_.  Term      ::= Term0        ;
_.  Term0     ::= Term1        ;
_.  Term1     ::= Term2        ;
_.  Term2     ::= Term3        ;
_.  Term3     ::= Term4        ;
_.  Term4     ::= "(" Term ")" ;    


TDouble.  Typ  ::= "double" ;

-- pragmas

comment "/*" "*/" ;
comment "//" ;

entrypoints Term, Iso, BaseIso ;
  |]
  
