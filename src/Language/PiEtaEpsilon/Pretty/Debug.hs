{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}
module Language.PiEtaEpsilon.Pretty.Debug where
import Language.PiEtaEpsilon.Syntax    
import Data.Functor.Fixedpoint
import Prelude hiding (Left, Right)
{-
-------------------------------------------Class------------------------------------------
class Pretty a where
    ppr :: a -> String

----------------------------------------Instances-----------------------------------------
instance Pretty Type where
    ppr Zero = "0"
    ppr One  = "1"
    ppr (Sum     x y) = "(" ++ ppr x ++ " + " ++ ppr y ++ ")"
    ppr (Product x y) = "(" ++ ppr x ++ " * " ++ ppr y ++ ")"
    ppr (Negative   x) = "(" ++ " - " ++ ppr x ++ ")"
    ppr (Reciprocal x) = "(" ++ " / " ++ ppr x ++ ")"

instance Pretty Value where
    ppr (Fix Unit)             = "()"
    ppr (Fix (Left x))         = "L " ++ ppr x
    ppr (Fix (Right x))        = "R " ++ ppr x
    ppr (Fix (Tuple x y))      = "(" ++ ppr x ++ "," ++ ppr y ++ ")"
    ppr (Fix (Negate x))       = "- " ++ ppr x
    ppr (Fix (Reciprocate x))  = "/ " ++ ppr x
    
-}


class PPrint a where ppr :: a -> String

parens s = "(" ++ s ++ ")"
sepBy  s t t' = unwords [ppr t, s, ppr t']

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

instance PPrint Type where
	ppr Zero = "0"
	ppr One  = "1"
	ppr (Sum      x y) = parens $ sepBy "+" x  y
	ppr (Product  x y) = parens $ sepBy "*" x  y
	ppr (Negative   y) = parens $ sepBy "-" () y
	ppr (Reciprocal y) = parens $ sepBy "/" () y    