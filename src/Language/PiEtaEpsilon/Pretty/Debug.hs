{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}
module Language.PiEtaEpsilon.Pretty.Debug where

import Language.PiEtaEpsilon.Syntax
import Language.PiEtaEpsilon.Evaluator
import Control.Unification.Types   -- necessary to compile here!
import Control.Unification.IntVar  -- necessary to compile here!

import Language.PiEtaEpsilon.Pretty.Class

import Data.Functor.Fixedpoint
import Prelude hiding (Left, Right)


{-
class Pretty a where
    ppr :: a -> String


instance Pretty Value where
    ppr (Fix Unit)             = "()"
    ppr (Fix (Left x))         = "L " ++ ppr x
    ppr (Fix (Right x))        = "R " ++ ppr x
    ppr (Fix (Tuple x y))      = "(" ++ ppr x ++ "," ++ ppr y ++ ")"
    ppr (Fix (Negate x))       = "- " ++ ppr x
    ppr (Fix (Reciprocate x))  = "/ " ++ ppr x
    
-}





parens s = "(" ++ s ++ ")"
boxes  s = "[" ++ s ++ "]"

sepBy  s l r = unwords [ppr l, s, ppr r]

----------------------------------------Instances-----------------------------------------
instance PPrint ()      where ppr = const ""
instance PPrint IsoBase where ppr = show
instance PPrint Iso     where
	ppr (Eliminate b) = "e" ++ ppr b
	ppr (Introduce b) = "i" ++ ppr b

instance PPrint Term where
	ppr (Base iso) = ppr iso
	ppr Id = "id"
	ppr (x ::: y) = parens $ sepBy ";" x y
	ppr (x :+: y) = parens $ sepBy "+" x y
	ppr (x :*: y) = parens $ sepBy "x" x y

{-
instance Pretty Type where
    ppr Zero = "0"
    ppr One  = "1"
    ppr (Sum     x y) = "(" ++ ppr x ++ " + " ++ ppr y ++ ")"
    ppr (Product x y) = "(" ++ ppr x ++ " * " ++ ppr y ++ ")"
    ppr (Negative   x) = "(" ++ " - " ++ ppr x ++ ")"
    ppr (Reciprocal x) = "(" ++ " / " ++ ppr x ++ ")"
-}

instance PPrint Type where
	ppr Zero = "0"
	ppr One  = "1"
	ppr (Sum      x y) = parens $ sepBy "+" x  y
	ppr (Product  x y) = parens $ sepBy "*" x  y
	ppr (Negative   y) = parens $ sepBy "-" () y
	ppr (Reciprocal y) = parens $ sepBy "/" () y

instance PPrint String where
    ppr = id
    
instance PPrint [String] where
    ppr = show

instance PPrint [UValue] where
     --ppr = error "ppr [a]"
     ppr (x:xs) = boxes $ sepBy ","  (ppr x) (fmap (ppr) xs)
     ppr []     = "[]"

instance (PPrint a, PPrint b, PPrint String) => PPrint (a,b) where
     ppr (x,y) = parens $ sepBy "," (ppr x) (ppr y)


instance PPrint UValue  where 
     ppr = show

instance PPrint Context where
	ppr = go id where
		go k  Box             = k "[]"
		go k (Fst      c t  ) = wrapl k c t ";"
		go k (Snd      t c  ) = wrapr k c t ";"
		go k (LSum     c t  ) = wrapl k c t "+"
		go k (RSum     t c  ) = wrapr k c t "+"
		go k (LProduct c t v) = wrapl k c t "x"
		go k (RProduct t v c) = wrapr k c t "x"
		wrapl k c t s = go (\s' -> concat ["(", k s' , " ", s, " ", ppr t, ")"]) c
		wrapr k c t s = go (\s' -> concat ["(", ppr t, " ", s, " ", k s' , ")"]) c



instance (Show (t (UTerm t IntVar))) => PPrint (IntBindingState t) where
     ppr  = show
 {-    
          concat
          [ "nextFreeVar: "
          , (nextFreeVar m)
 -}


-- This is an updated version I am playing with.
--
-- dday

instance PPrint MachineState where
	ppr m = concat
		[ if descending m then "|v|" else "|^|", " "
		, if forward m    then "|>|" else "|<|", " "
		, "term => ", ppr (term    m), ", "
		, "outp => ", ppr (output  m), ", "
		, "ctxt => ", ppr (context m)
		]

{-
instance PPrint MachineState where
	ppr m = concat
		[ if descending m then "<" else "["
		, ppr (term    m), ", "
		, ppr (output  m), ", "
		, ppr (context m)
		, if descending m then ">" else "]"
		, if forward m then "|>" else "<|"
		]
-}
