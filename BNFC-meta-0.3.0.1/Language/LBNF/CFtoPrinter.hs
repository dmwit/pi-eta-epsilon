{-# LANGUAGE TemplateHaskell #-}
{-
    BNF Converter: Pretty-printer generator
    Copyright (C) 2004  Author:  Aarne Ranta

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Language.LBNF.CFtoPrinter (cf2Printer) where

import Language.LBNF.CF
import Language.LBNF.Utils
import Language.LBNF.Runtime
import Data.List (intersperse)
import Data.Char(toLower)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

cf2Printer :: CF -> Q [Dec]
cf2Printer cf = sequence $ concat [
  if hasIdent cf then [identRule cf] else [],
  [ownPrintRule cf own | (own,_) <- tokenPragmas cf],
  rules cf
  ]

{-
showsPrintRule cf t = unlines $ [
  "instance Print " ++ t ++ " where",
  "  prt _ x = doc (shows x)",
  ifList cf t
  ]
-}

identRule cf = ownPrintRule cf "Ident"

ownPrintRule :: CF -> String -> DecQ
ownPrintRule cf own = do
  i <- newName "i"
  let 
    posn = if isPositionCat cf own 
      then conP (mkName own) [tupP [wildP, varP i]]
      else conP (mkName own) [varP i]
    body = normalB [|doc (showString $(varE i))|]
    prtc = funD ('prt) [clause [wildP, posn] body []]
  instanceD (cxt []) (appT (conT $ ''Print) $ conT $ mkName own) [prtc]
 
{-unlines $ [
  "instance Print " ++ own ++ " where",
  "  prt _ (" ++ own ++ posn ++ ") = doc (showString i)",
  ifList cf own
  ]
 where
   posn = if isPositionCat cf own then " (_,i)" else " i"
-}
-- copy and paste from CFtoTemplate

rules :: CF -> [Q Dec]
rules cf = 
  map (\(s,xs) -> case_fun s (map toArgs xs) (ifList cf s)) $ cf2data cf
 where 
   toArgs (cons,Left args) = ((cons, names (map (checkRes . var) args) (0 :: Int)), ruleOf cons)
   toArgs (cons,Right reg) = ((cons, names ["s"] (0 :: Int)), ruleOf cons)
   names [] _ = []
   names (x:xs) n
     | elem x xs = (x ++ show n) : names xs (n+1)
     | otherwise = x             : names xs n
   var ('[':xs)  = var (init xs) ++ "s"
   var "Ident"   = "id"
   var "Integer" = "n"
   var "String"  = "str"
   var "Char"    = "c"
   var "Double"  = "d"
   var xs        = map toLower xs
   checkRes s
        | elem s reservedHaskell = s ++ "'"
	| otherwise              = s
   reservedHaskell = ["case","class","data","default","deriving","do","else","if",
		      "import","in","infix","infixl","infixr","instance","let","module",
		      "newtype","of","then","type","where","as","qualified","hiding"]
   ruleOf s = maybe undefined id $ lookup s (rulesOfCF cf)

-- case_fun :: Cat -> [(Con,Rule)] -> Q Dec
case_fun cat xs lst =
 instanceD (cxt []) (appT (conT ''Print) $ conT $ mkName cat) $
  [newName "i" >>= \i -> newName "x" >>= prtc i] ++ lst where
    prtc i n = funD ('prt) [clause [varP i,varP n] (body) []] where
      body = normalB $ caseE (varE n) $
        map mtch xs
      mtch ((c,xx),r) = match 
        (conP (mkName c) [varP (mkName x)|x <- xx])
        (normalB 
          [| prPrec 
               $(varE i) 
               $(litE $ IntegerL $ toInteger $ precCat $ fst r) 
               $(mkRhs xx (snd r))
          |])
        []
  
{-
unlines [
  "instance Print" +++ cat +++ "where",
  "  prt i" +++ "e = case e of",
  unlines $ map (\ ((c,xx),r) -> 
    "   " ++ c +++ unwords xx +++ "->" +++ 
    "prPrec i" +++ show (precCat (fst r)) +++ mkRhs xx (snd r)) xs
  ]
-}

ifList :: CF -> String -> [DecQ]
ifList cf cat = mkListRule $ nil cat ++ one cat ++ cons cat where
  nil cat  = [(listP [],mkRhs [] its) | 
                            (f,(c,its)) <- rulesOfCF cf, isNilFun f , normCatOfList c == cat]
  one cat  = [(listP [varP $ mkName "x"], mkRhs ["x"] its) | 
                            (f,(c,its)) <- rulesOfCF cf, isOneFun f , normCatOfList c == cat]
  cons cat = [(conP '(:) [varP $ mkName "x",varP $ mkName "xs"], mkRhs ["x","xs"] its) | 
                            (f,(c,its)) <- rulesOfCF cf, isConsFun f , normCatOfList c == cat]
  mkListRule [] = []
  mkListRule rs = [do
    es <- newName "es"
    funD 'prtList [clause [varP es] (normalB $ caseE (varE es) $ map mtch rs) []]]
  mtch (p,e) = match p (normalB e) []


mkRhs :: [String] -> Either [Either String String] a -> ExpQ
mkRhs args (Left its) = [| concatD $(listE $ mk args its) |]
 where
  mk args (Left "#" : items)      = mk args items
  mk (arg:args) (Left c : items)  = prt' c (arg)        : mk args items
  mk args       (Right s : items) = [| doc (showString $(lift (s :: String))) |] : mk args items
  mk _ _ = []
  prt' :: String -> String -> ExpQ
  prt' c arg = [| prt $(lift $ precCat c) $(varE $ mkName arg) |]
mkRhs args (Right reg) = [|doc (showString $(varE $ mkName "s"))|]

  {-
 "(concatD [" ++ unwords (intersperse "," (mk args its)) ++ "])"
 where
  mk args (Left "#" : items)      = mk args items
  mk (arg:args) (Left c : items)  = (prt c +++ arg)        : mk args items
  mk args       (Right s : items) = ("doc (showString" +++ show s ++ ")") : mk args items
  mk _ _ = []
  prt c = "prt" +++ show (precCat c)
-}
