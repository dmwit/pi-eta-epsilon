{-#LANGUAGE TemplateHaskell #-}

module Language.LBNF.Runtime(
  -- * Happy and Alex runtimes
  -- ord
  -- , listArray
  -- , (!)
  -- , Array
  -- , parseToQuoter
  ParseMonad(..)
  , err
  
  -- * Pretty printing runtimes
  , printTree
  , doc
  , concatD
  , Print(..)
  , prPrec
  , PrintPlain(..)

  -- * Quasi quoting runtimes
  --, Q
  --, BNFC_QQType, appEPAll, appEPAllL, fromLit, fromString, fromToken
  --, Lift (..)
  ) where



import Control.Monad (MonadPlus(..), liftM, foldM, (>=>))



import Data.Char



------------------
-- Lexing, Parsing
------------------

data ParseMonad a = Ok a | Bad String
  deriving (Read, Show, Eq, Ord)

instance Monad ParseMonad where
  return      = Ok
  fail        = Bad
  Ok a  >>= f = f a
  Bad s >>= f = Bad s

instance Functor ParseMonad where
  fmap = liftM

--instance MonadPlus ParseMonad where
--  mzero = Bad "Err.mzero"
--  mplus (Bad _) y = y
--  mplus x       _ = x

err :: (String -> a) -> ParseMonad a -> a
err e b = case b of 
    Bad s -> e s
    Ok x  -> x


-----------
-- PRINTING
-----------

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)
  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])


instance Print Double where
  prt _ x = doc (shows x)

newtype PrintPlain = MkPrintPlain String

instance Print PrintPlain where
  prt _ (MkPrintPlain s) = doc $ showString s