{-# LANGUAGE GADTs, KindSignatures, 
    TypeOperators, TemplateHaskell, QuasiQuotes #-}
module Language.PiEtaEpsilon.BNFMeta.Value where
--from bnfc-meta
import Language.LBNF(lbnf, dumpCode, bnfc)
import Language.LBNF.Compiletime
import qualified Language.LBNF.Grammar


bnfc [lbnf|

-- This is a new pragma. The rest of the grammar is original JL.
antiquote "[" ":" ":]" ;

--Iso                                               
VTuple.       Value0    ::= "(" Value1 "," Value1 ")" ;
VLeft.        Value1    ::= "L" Value2               ;
VRight.       Value2    ::= "R" Value3               ;
VNegate.      Value3    ::= "-" Value4               ;
VReciprocate. Value4    ::= "/" Value5             ;
VUnit.        Value5    ::= "()"                      ;

_.  Value      ::= Value0        ;
_.  Value0     ::= Value1        ;
_.  Value1     ::= Value2        ;
_.  Value2     ::= Value3        ;
_.  Value3     ::= Value4        ;
_.  Value4     ::= Value5        ;
_.  Value5     ::= "(" Value ")" ;    


TDouble.  Typ  ::= "double" ;

-- pragmas

comment "/*" "*/" ;
comment "//" ;

entrypoints Value;
  |]
  
