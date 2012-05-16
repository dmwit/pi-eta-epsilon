{-# LANGUAGE GADTs, KindSignatures, 
    TypeOperators, TemplateHaskell, QuasiQuotes #-}
module Language.PiEtaEpsilon.BNFMeta.Pi where
--from bnfc-meta
import Language.LBNF(lbnf, dumpCode, bnfc)
import Language.LBNF.Compiletime
import qualified Language.LBNF.Grammar

bnfc [lbnf|

-- This is a new pragma. The rest of the grammar is original JL.
antiquote "[" ":" ":]" ;

--Pis

PDummy.            Pi0    ::= Pi1  "dummy"  Pi1       ;
PCompose.          Pi1    ::= Pi1  "."  Pi2       ;
PPlus.             Pi2    ::= Pi2  "+"  Pi3       ;
PTimes.            Pi3    ::= Pi3  "*"  Pi4       ;
PTrace.            Pi4    ::= "trace" Pi5         ;

PEliminate.        Pi5   ::= "#"     Pi6          ;
PIntroduce.        Pi6   ::= "'"     Pi7          ;
                                           
PIdentity.         Pi7    ::= "<=|=>"             ;
PCommute.          Pi8    ::= "commute"           ;
PAssociate.        Pi9    ::= "|||"               ;
PAnnihilate.       Pi10   ::= "x"                  ;
PDistribute.       Pi11   ::= "distribute"        ;
PFold.             Pi12   ::= "fold"              ;
PId.               Pi13    ::= "id"              ;
PIndent.            Pi14   ::= Ident               ;


_.              Pi      ::= Pi0        ;
_.              Pi0     ::= Pi1        ;
_.              Pi1     ::= Pi2        ;
_.              Pi2     ::= Pi3        ;
_.              Pi3     ::= Pi4        ;
_.              Pi4     ::= Pi5        ;
_.              Pi5     ::= Pi6        ;
_.              Pi6     ::= Pi7        ;
_.              Pi7     ::= Pi8        ;
_.              Pi8     ::= Pi9        ;
_.              Pi9     ::= Pi10        ;
_.              Pi10    ::= Pi11        ;
_.              Pi11    ::= Pi12        ;
_.              Pi12    ::= Pi13        ;
_.              Pi13    ::= Pi14        ;
_.              Pi14    ::= "(" Pi ")" ;

TDouble.  Typ  ::= "double" ;

-- pragmas

comment "/*" "*/" ;
comment "//" ;

entrypoints Pi ;
  |]
  
