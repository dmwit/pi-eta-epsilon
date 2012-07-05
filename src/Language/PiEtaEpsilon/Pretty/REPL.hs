{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}
module Language.PiEtaEpsilon.Pretty.REPL where

import Language.PiEtaEpsilon.Syntax
import Language.PiEtaEpsilon.Evaluator
import Control.Unification.Types   -- necessary to compile here!
import Control.Unification.IntVar  -- necessary to compile here!

import Data.Functor.Fixedpoint
import Prelude hiding (Left, Right)

import Language.PiEtaEpsilon.Pretty.Class

parens s = "(" ++ s ++ ")"
boxes  s = "[" ++ s ++ "]"

sepBy  s l r = unwords [ppr l, s, ppr r]

----------------------------------------Instances-----------------------------------------
instance PPrint ()      where ppr = const ""
instance PPrint IsoBase where 
    ppr IdentityS        = "<=+=>"
    ppr CommutativeS     = "x+x"
    ppr AssociativeS     = "|+|+|"
    ppr SplitS           = "-+<"
    ppr IdentityP        = "<=*=>"
    ppr CommutativeP     = "x*x"
    ppr AssociativeP     = "|*|*|"
    ppr SplitP           = "-*<"
    ppr DistributiveZero = "^0^"
    ppr DistributivePlus = "^+^"
         
instance PPrint Iso     where
	ppr (Eliminate b) = "# " ++ ppr b
	ppr (Introduce b) = "' " ++ ppr b

instance PPrint Term where
    ppr (x ::: y) = (ppr x) ++ " ; " ++ (ppr y)
    ppr (x :+: y) = (ppr x) ++ " + " ++ (ppr y)
    ppr (x :*: y) = (ppr x) ++ " * " ++ (ppr y)
    ppr (Base      x) = "< "    ++ (ppr x)
    ppr (Id         ) = "<=>" 

instance PPrint String where
    ppr = id
    
instance PPrint [String] where
    ppr = show

instance PPrint [UValue] where
     ppr (x:[]) = ppr x
     ppr (x:xs) = boxes $ sepBy ","  (ppr x) (fmap (ppr) xs)
     ppr []     = "[]"

instance (PPrint a, PPrint b, PPrint String) => PPrint (a,b) where
     ppr (x,y) = parens $ sepBy "," (ppr x) (ppr y)

instance PPrint UValue  where 
    ppr (UTerm Unit)              = "1"
    ppr (UTerm (Left        x  )) = parens $ "L " ++ ppr x
    ppr (UTerm (Right       x  )) = parens $ "R " ++ ppr x
    ppr (UTerm (Tuple       x y)) = parens $ ppr x ++ "," ++ ppr y 
    ppr (UTerm (Negate      x  )) = "- " ++ ppr x 
    ppr (UTerm (Reciprocate x  )) = "/ " ++ ppr x
    
    
    
    
    
    
    
    