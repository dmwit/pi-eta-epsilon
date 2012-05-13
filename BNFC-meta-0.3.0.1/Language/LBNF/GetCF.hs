{-
    BNF Converter: Abstract syntax
    Copyright (C) 2004  Author: Markus Forsberg, Aarne Ranta

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


module Language.LBNF.GetCF where

import Control.Monad ( when )

import Language.LBNF.CF
import Language.LBNF.Utils
-- import Language.LBNF.ParBNF
import Language.LBNF.Grammar(pGrammar, tokens)

import Data.List(nub,partition)
import qualified Language.LBNF.Grammar as Abs
import Language.LBNF.Runtime
import Data.Char
import Language.LBNF.TypeChecker

type TempRHS = Either [Either String String] Reg
type TempRule = (Fun,(Cat,TempRHS))

getCF :: String -> (CF, [String])
getCF = getCFofG . pGrammar . tokens

getCFofG :: ParseMonad Abs.Grammar -> (CF, [String])
getCFofG g = (cf,msgs ++ msgs1) where

  (cf,msgs1) = ((exts,ruls2),msgs2)
  (ruls2,msgs2) = untag $ map (checkRule cf0) $ rulesOfCF cf0
  untag :: ([Either Rule String]) -> ([Rule],[String])
  untag ls = ([c | Left c <- ls], [r| Right r <- ls])
  -- isRule = either (const True) (const False)
  cf0 :: CF
  (cf0@(exts,_),msgs) = (revs . srt . conv $ g)
  srt :: [Either (Either Pragma TempRule) String] -> (CF, [String])
  srt rs = let 
       rules              = [fixRuleTokens n r | (n,Left (Right r)) <- zip [1..] rs]
       literals           = nub  [lit | Left xs <- map (snd . snd) rules,
                                   (Left lit) <- xs,
                                   elem lit specialCatsP]
       
       pragma             = [r | Left (Left r) <- rs]
       tokens             = [i | TokenReg i _ _ <- pragma]
       errors             = [s | Right s <- rs, not (null s)]
       (symbols,keywords) = partition notIdent reservedWords
       notIdent s         = null s || not (isIdentAlpha (head s)) || any (not . isIdentRest) s
       isIdentAlpha c     = isLatin1 c && isAlpha c
       isIdentRest c      = isIdentAlpha c || isDigit c || c == '_' || c == '\''
       reservedWords      = nub [t | (_,(_,Left its)) <- rules, Right t <- its] ++ 
         concatMap (reservedLiteralAQ [ (b,i,a) | AntiQuote b i a <- pragma ]) (literals ++ tokens)
       cats               = []
	    in (((pragma,(literals,symbols,keywords,cats)),rules),errors)
	    
  revs :: (CF, [String]) -> (CF, [String])
  revs (cf@((pragma,(literals,symbols,keywords,_)),rules),errors) =
    (((pragma,
       (literals,symbols,keywords,findAllReversibleCats (cf))),rules),errors)

fixRuleTokens :: Int -> TempRule -> Rule
fixRuleTokens n (f,(c,rhs)) = 
  (f,(c,either Left (\r -> Right (r,"RTL_"++show n)) rhs))



  

conv :: ParseMonad Abs.Grammar -> [Either (Either Pragma TempRule) String]
conv (Bad s)                 = [Right s]
conv (Ok (Abs.Grammar defs)) = map Left $ concatMap (transDef defs) defs

reservedLiteralAQ []        l = []
reservedLiteralAQ [(b,i,a)] l = [b ++ l]
reservedLiteralAQ _         l = error "multiple antiquote pragmas"

isAqLabel x = case x of
  (Abs.Aq s)    -> True
--  Abs.LabP Abs.Aq _    -> True
--  Abs.LabPF Abs.Aq _ _ -> True
--  Abs.LabF Abs.Aq _    -> True
--  _                    -> False

transDef :: [Abs.Def] -> Abs.Def -> [Either Pragma TempRule]
transDef defs x = case x of
-- Abs.Rule label cat items | isAqLabel label -> []
 Abs.Rule label cat items -> 
   [Right (transLabel label,(transCat cat, transRHS items))]
 Abs.Comment str               -> [Left $ CommentS str]
 Abs.Comments str0 str         -> [Left $ CommentM (str0,str)]
 Abs.Token ident reg           -> [Left $ TokenReg (transIdent ident) False reg]
 Abs.PosToken ident reg        -> [Left $ TokenReg (transIdent ident) True reg]
 Abs.Entryp idents             -> [Left $ EntryPoints (map transIdent idents)]
 Abs.Internal label cat items  -> 
   [Right (transLabel label,(transCat cat,(Left $ Left "#":(map transItem items))))]
 Abs.Separator size ident str -> map  Right $ separatorRules size ident str
 Abs.Terminator size ident str -> map  Right $ terminatorRules size ident str
 Abs.Coercions ident int -> map  (Right) $ coercionRules ident int
 Abs.Rules ident strs -> map (Right) $ ebnfRules ident strs
 Abs.Layout ss      -> [Left $ Layout ss]
 Abs.LayoutStop ss  -> [Left $ LayoutStop ss]
 Abs.LayoutTop      -> [Left $ LayoutTop]
 Abs.Derive ss      -> [Left $ Derive [s|Abs.Ident s<-ss]]
-- Abs.Function f xs e -> [Left $ FunDef (transIdent f) (map transArg xs) (transExp e)]
 Abs.AntiQuote b i a ->
   [Left $ AntiQuote b i a] 
   ++ [Left  $ TokenReg "AqToken" False $ aqToken i a]
   ++ aqRules (b,i,a) (getCats defs) where
   reg = aqToken a

aqToken :: String -> String -> Abs.Reg
aqToken i s@(c:cs) = Abs.RSeq (Abs.RSeqs i) $ Abs.RSeq (Abs.RStar $ foldr1 Abs.RAlt $ map clause prefixes) $ Abs.RSeqs s where
  prefixes = scanr (:) [c] . reverse $ cs
  clause (d:ds) = subclause (reverse ds) (Abs.RMinus Abs.RAny $ Abs.RChar d)
  subclause [] x = x
  subclause (e:es) x = Abs.RSeq (Abs.RChar e) (subclause es x)

getCats :: [Abs.Def] -> [Cat]
getCats = nub . concatMap (\x -> case x of
  Abs.Rule _ cat _     -> [transCat cat]
  Abs.Internal _ cat _ -> [transCat cat]
  _                    -> [])

aqRHS :: [Abs.Item] -> Cat
aqRHS xs = case filter filt xs of
  [Abs.NTerminal cat] -> transCat cat
  _ -> error "anti-quotation rules must have exactly one non-terminal"
  where
    filt x  =case x of
      Abs.Terminal str   -> False
      Abs.NTerminal cat  -> True


toks x = case x of
 Abs.Token (Abs.Ident ident) reg           -> [ident]
 Abs.PosToken (Abs.Ident ident) reg        -> [ident]
 _                                         -> []

aqRules :: (String,String,String) -> [String] -> [Either Pragma TempRule]
aqRules (b,i,a) = concatMap aqRule where
  aqRule cat = map Right [
      (aqFun,(cat, Left [Right b,Left "AqToken"])),
      (aqFun,(cat, Left [Right (b++normCat cat), Left "AqToken"]))
      ]

aqFun = "$global_aq"

-- addSpecials :: (String,String,String) -> [Either Pragma Rule] -> [Either Pragma Rule]
-- addSpecials (b,i,a) rs = rs ++ concatMap special literals where
  
--  special aqs@('A':'Q':'_':s) = map Right [(aqs,(aqs,[Left s])),
--    (renameAq s,(rename s, [Right b,Left "AqToken"])),
--    (renameAqt s,(rename s, [Right (b++s), Left "AqToken"]))
--    ]
--  rules              = [r | (Right r) <- rs]
--  literals           = nub  [lit | xs <- map (snd . snd) rules,
--    (Left lit) <- xs,
--    elem lit (map rename specialCatsP)]
-- \end{hack}



separatorRules :: Abs.MinimumSize -> Abs.Cat -> String -> [TempRule]
separatorRules size c s = if null s then terminatorRules size c s else ifEmpty [
  ("(:[])", (cs,Left [Left c'])),
  ("(:)",   (cs,Left [Left c', Right s, Left cs]))
  ]
 where 
   c' = transCat c
   cs = "[" ++ c' ++ "]"
   ifEmpty rs = if (size == Abs.MNonempty) then rs else (("[]", (cs,Left [])) : rs)

terminatorRules :: Abs.MinimumSize -> Abs.Cat -> String -> [TempRule]
terminatorRules size c s = [
  ifEmpty,
  ("(:)",   (cs,Left $ Left c' : s' [Left cs]))
  ]
 where 
   c' = transCat c
   cs = "[" ++ c' ++ "]"
   s' its = if null s then its else (Right s : its)
   ifEmpty = if (size == Abs.MNonempty) 
                then ("(:[])",(cs,Left $ [Left c'] ++ if null s then [] else [Right s]))
                else ("[]",   (cs,Left []))

coercionRules :: Abs.Ident -> Integer -> [TempRule]
coercionRules (Abs.Ident c) n = 
   ("_", (c,               Left [Left (c ++ "1")])) :
  [("_", (c ++ show (i-1), Left [Left (c ++ show i)])) | i <- [2..n]] ++
  [("_", (c ++ show n,     Left [Right "(", Left c, Right ")"]))]

  
ebnfRules :: Abs.Ident -> [Abs.RHS] -> [TempRule]
ebnfRules (Abs.Ident c) rhss = 
  [(mkFun k c rhs, (c, transRHS rhs)) | (k, rhs) <- zip [1 :: Int ..] rhss]
 where
   mkFun :: Int -> String -> Abs.RHS -> String
   mkFun k c i = case i of
     (Abs.RHS [Abs.Terminal s])  -> c' ++ "_" ++ mkName k s
     (Abs.RHS [Abs.NTerminal n]) -> c' ++ identCat (transCat n)
     _ -> c' ++ "_" ++ show k
   c' = c --- normCat c
   mkName k s = if all (\c -> isAlphaNum c || elem c "_'") s 
                   then s else show k


transRHS :: Abs.RHS -> TempRHS
transRHS (Abs.RHS its) = Left $ map transItem its
transRHS (Abs.TRHS r)  = Right r



transItem :: Abs.Item -> Either Cat String
transItem x = case x of
 Abs.Terminal str   -> Right str
 Abs.NTerminal cat  -> Left (transCat cat)

transCat :: Abs.Cat -> Cat
transCat x = case x of
 Abs.ListCat cat  -> "[" ++ (transCat cat) ++ "]"
 Abs.IdCat id     -> transIdent id

transLabel :: Abs.Label -> Fun
transLabel y = let g = transLabelId y in g
 where
   transLabelId x = case x of
     Abs.Id id     -> transIdent id
     Abs.Wild      -> "_"
     Abs.ListE     -> "[]"
     Abs.ListCons  -> "(:)"
     Abs.ListOne   -> "(:[])"
     Abs.Aq (Abs.JIdent i)     -> "$" ++ transIdent i
     Abs.Aq _     -> "$"
--   transProf (Abs.ProfIt bss as) = 
--     ([map fromInteger bs | Abs.Ints bs <- bss], map fromInteger as)

transIdent :: Abs.Ident -> String
transIdent x = case x of
 Abs.Ident str  -> str

transArg :: Abs.Arg -> String
transArg (Abs.Arg x) = transIdent x

transExp :: Abs.Exp -> Exp
transExp e = case e of
    Abs.App x es    -> App (transIdent x) (map transExp es)
    Abs.Var x	    -> App (transIdent x) []
    Abs.Cons e1 e2  -> cons e1 (transExp e2)
    Abs.List es	    -> foldr cons nil es
    Abs.LitInt x    -> LitInt x
    Abs.LitDouble x -> LitDouble x
    Abs.LitChar x   -> LitChar x
    Abs.LitString x -> LitString x
  where
    cons e1 e2 = App "(:)" [transExp e1, e2]
    nil	       = App "[]" []



