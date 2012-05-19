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
VDummy.       Value0    ::= "&" Value1 "," Value1 "&" ;
VTuple.       Value1    ::= Value1 "," Value2  ;
VLeft.        Value2    ::= "L" Value3               ;
VRight.       Value3    ::= "R" Value4               ;
VNegate.      Value4    ::= "-" Value5               ;
VReciprocate. Value5    ::= "/" Value6             ;
VUnit.        Value6    ::= "1"                      ;

_.  Value      ::= Value0        ;
_.  Value0     ::= Value1        ;
_.  Value1     ::= Value2        ;
_.  Value2     ::= Value3        ;
_.  Value3     ::= Value4        ;
_.  Value4     ::= Value5        ;
_.  Value5     ::= Value6        ;
_.  Value6     ::= "(" Value ")" ;    


TDouble.  Typ  ::= "double" ;

-- pragmas

comment "/*" "*/" ;
comment "//" ;

entrypoints Value;
  |]
  
