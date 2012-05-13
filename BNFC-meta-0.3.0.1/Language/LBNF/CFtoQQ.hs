{-# LANGUAGE TemplateHaskell #-}
module Language.LBNF.CFtoQQ(cf2qq) where

import Data.Char (toLower)

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax(lift)
import Language.Haskell.TH.Quote
-- import Language.Haskell.TH.Lift

import Language.LBNF.Compiletime(printTree, stringAq, parseToQuoter)
import Language.LBNF.CF(quoterName, CF, quoters, aqSyntax)

cf2qq :: CF -> Q [Dec]
cf2qq cf = do
  aqToken <- maybe (return []) (deriveAq cf) (aqSyntax cf)
  qqs <- sequence $ map mkQQ eps
  return $ aqToken ++ qqs
  where
    eps      = quoters cf


deriveAq cf (_,i,a) = do
    v <- newName "a"
    let nAqToken = mkName "AqToken"
        nAqFun   = mkName "global_aq"
    d <-funD nAqFun [clause [conP nAqToken [varP v]] (normalB $ aqDec (varE v)) []] 
    return $ [d] where
  aqDec v = 
    [| stringAq (drop $(lie) . reverse . drop $(lae) . reverse $ printTree $(v)) |]
  (lie, lae) = (lift $ length i + 1 ,lift $ length a + 1)

mkQQ s = funD qqName [clause [] (normalB qqe) []] where
  qqe  = [|parseToQuoter ($(varE qName) . $(varE tokName)) |]
  qqName = mkName $ quoterName s
  qName = mkName $ 'q':s
  tokName = mkName "myLexer"


