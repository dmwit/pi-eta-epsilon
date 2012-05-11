{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}
module Language.PiEtaEpsilon.Pretty.Debug where
import Language.PiEtaEpsilon.Syntax    
import Data.Functor.Fixedpoint
import Prelude hiding (Left, Right)

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