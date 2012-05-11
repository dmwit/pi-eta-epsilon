module Language.PiEtaEpsilon.Token where
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

parens     = T.parens haskell
reservedOp = T.reservedOp haskell

{-
data Tokens | Parens
            | One
            | Zero
            | Times
            | Add
            | Recip
            | Negate
            | Assoc
-}