module Language.Haskell.TH.Hide(export) where
import Data.List(partition)
import Language.Haskell.TH

export :: [Name] -> [Dec] -> Q [Dec]
export el = buildClause el . partition whereable where
  whereable :: Dec -> Bool
  whereable d = case d of
    (FunD  _ _) -> True
    (ValD _ _ _) -> True
    (SigD _ _)   -> True
    (PragmaD _)  -> True
    _            -> False

buildClause el (wh,tl) = do
  v <- valD 
    (splitTup tupP [varP n | n <- el]) 
    (normalB $ splitTup tupE [varE n | n <- el])
    (map return wh)
  return $ v : tl

  
splitTup :: ([a] -> a) -> [a] -> a 
splitTup tup ls = case splitAt 60 ls of
  (_,[])       -> tup ls
  (first,rest) -> tup $ first ++ [splitTup tup rest]

