{-
    BNF Converter: Alex 2.0 Generator
    Copyright (C) 2004  Author:  Peter Gammie

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

-------------------------------------------------------------------
-- |
-- Module      :  CFtoAlex2
-- Copyright   :  (C)opyright 2003, {aarne,markus,peteg} at cs dot chalmers dot se
-- License     :  GPL (see COPYING for details)
-- 
-- Maintainer  :  {markus,aarne} at cs dot chalmers dot se
-- Stability   :  alpha
-- Portability :  Haskell98
--
-- Hacked version of @CFtoAlex@ to cope with Alex2.
--
-------------------------------------------------------------------
module Language.LBNF.CFtoAlex2 (abstractAlex, cf2alex2, concreteAlex) where

import Language.LBNF.CF
import Text.Alex.Quote
import Data.List

-- For RegToAlex, see below.
import Language.LBNF.Grammar(Reg(..), Ident(..))
import Data.Char

abstractAlex = compileAlex . parseAlex . cf2alex2

concreteAlex :: CF -> String
concreteAlex = parseAlex . cf2alex2

cf2alex2 ::CF -> String
cf2alex2 cf = 
  unlines $ concat $ intersperse [""] [
    cMacros,
    rMacros cf,
    restOfAlex False cf
   ]
   
cMacros :: [String]
cMacros = [
  "$l = [a-zA-Z\\192 - \\255] # [\\215 \\247]    -- isolatin1 letter FIXME",
  "$c = [A-Z\\192-\\221] # [\\215]    -- capital isolatin1 letter FIXME",
  "$s = [a-z\\222-\\255] # [\\247]    -- small isolatin1 letter FIXME",
  "$d = [0-9]                -- digit",
  "$i = [$l $d _ ']          -- identifier character",
  "$u = [.]          -- universal: any character"
  ]

rMacros :: CF -> [String]
rMacros cf = 
  let symbs = symbols cf
  in
  (if null symbs then [] else [
   "@rsyms =    -- symbols and non-identifier-like reserved words",
   "   " ++ unwords (intersperse "|" (map mkEsc symbs))
   ])
 where
  mkEsc = unwords . esc
  esc s = if null a then rest else show a : rest
      where (a,r) = span (\c -> isLatin1 c && isAlphaNum c) s
            rest = case r of
                       [] -> []
                       (c:xs) -> s : esc xs
                         where s = '\\':show (ord c)

restOfAlex :: Bool -> CF -> [String]
restOfAlex shareStrings cf = [
  ":-", 
  lexComments (comments cf),
  "$white+ ;",
  pTSpec (symbols cf),

  userDefTokenTypes,
  ident,

  ifC "String" ("\\\" ([$u # [\\\" \\\\ \\n]] | (\\\\ (\\\" | \\\\ | \\' | n | t)))* \\\"" ++
                  "{ tok (\\p s -> PT p (TL $ share $ unescapeInitTail s)) }"),
  ifC "Char"    "\\\' ($u # [\\\' \\\\] | \\\\ [\\\\ \\\' n t]) \\'  { tok (\\p s -> PT p (TC $ share s))  }",
  ifC "Integer" "$d+      { tok (\\p s -> PT p (TI $ share s))    }",
  ifC "Double"  "$d+ \\. $d+ (e (\\-)? $d+)? { tok (\\p s -> PT p (TD $ share s)) }",
  "",
  "{",
  "",
  "tok f p s = f p s",
  "",
  "share :: String -> String",
  "share = " ++ if shareStrings then "shareString" else "id",
  "",
  "data Tok =", 
  "   TS !String !Int    -- reserved words and symbols",
  " | TL !String         -- string literals", 
  " | TI !String         -- integer literals",
  " | TV !String         -- identifiers",
  " | TD !String         -- double precision float literals",
  " | TC !String         -- character literals",
  userDefTokenConstrs,
  " deriving (Eq,Show,Ord)",
  "",
  "data Token = ",
  "   PT  Posn Tok",
  " | Err Posn",
  "  deriving (Eq,Show,Ord)",
  "",
  "tokenPos (PT (Pn _ l _) _ :_) = \"line \" ++ show l", 
  "tokenPos (Err (Pn _ l _) :_) = \"line \" ++ show l", 
  "tokenPos _ = \"end of file\"",
  "",
  "posLineCol (Pn _ l c) = (l,c)",
  "mkPosToken t@(PT p _) = (posLineCol p, prToken t)",
  "",
  "prToken t = case t of", 
  "  PT _ (TS s _) -> s",
  "  PT _ (TI s) -> s",
  "  PT _ (TV s) -> s",
  "  PT _ (TD s) -> s",
  "  PT _ (TC s) -> s",
  userDefTokenPrint,  
  "  _ -> show t",
  "",
  "data BTree = N | B String Tok BTree BTree deriving (Show)",
  "",
  "eitherResIdent :: (String -> Tok) -> String -> Tok",
  "eitherResIdent tv s = treeFind resWords",
  "  where",
  "  treeFind N = tv s",
  "  treeFind (B a t left right) | s < a  = treeFind left",
  "                              | s > a  = treeFind right",
  "                              | s == a = t",
  "",
  "resWords = " ++ (show $ sorted2tree $ zip (sort resws) [1..]),
  "   where b s n = let bs = s",
  "                  in B bs (TS bs n)",
  "",
  "unescapeInitTail :: String -> String",
  "unescapeInitTail = unesc . tail where",
  "  unesc s = case s of",
  "    '\\\\':c:cs | elem c ['\\\"', '\\\\', '\\\''] -> c : unesc cs",
  "    '\\\\':'n':cs  -> '\\n' : unesc cs",
  "    '\\\\':'t':cs  -> '\\t' : unesc cs",
  "    '\"':[]    -> []",
  "    c:cs      -> c : unesc cs",
  "    _         -> []",
  "",
  "-------------------------------------------------------------------",
  "-- Alex wrapper code.",
  "-- A modified \"posn\" wrapper.",
  "-------------------------------------------------------------------",
  "",
  "",
  "alexStartPos :: Posn",
  "alexStartPos = Pn 0 1 1",
  "",
  "tokens :: String -> [Token]",
  "tokens str = go (alexStartPos, '\\n', [], str)",
  "    where",
  "      go :: AlexInput -> [Token]",
  "      go inp@(pos, _, _, str) =",
  "               case alexScan inp 0 of",
  "                AlexEOF                -> []",
  "                AlexError (pos, _, _, _)  -> [Err pos]",
  "                AlexSkip  inp' len     -> go inp'",
  "                AlexToken inp' len act -> act pos (take len str) : (go inp')",
  "",
  "",
  "alexInputPrevChar :: AlexInput -> Char",
  "alexInputPrevChar (p, c, _, s) = c",
  "}"
  ]
 where
   ifC cat s = if isUsedCat cf cat then s else ""
   lexComments ([],[])           = []    
   lexComments (xs,s1:ys) = '\"' : s1 ++ "\"" ++ " [.]* ; -- Toss single line comments\n" ++ lexComments (xs, ys)
   lexComments (([l1,l2],[r1,r2]):xs,[]) = concat $
					[
					('\"':l1:l2:"\" ([$u # \\"), -- FIXME quotes or escape?
					(l2:"] | \\"),
					(r1:" [$u # \\"),
					(r2:"])* (\""),
					(r1:"\")+ \""),
					(r2:"\" ; \n"),
					lexComments (xs, [])
					]
   lexComments ((_:xs),[]) = lexComments (xs,[]) 
---   lexComments (xs,(_:ys)) = lexComments (xs,ys) 

   -- tokens consisting of special symbols
   pTSpec [] = ""
   pTSpec _ = "@rsyms { tok (\\p s -> PT p (eitherResIdent (TV . share) s)) }"

   userDefTokenTypes = unlines $
     [printRegAlex exp ++
      " { tok (\\p s -> PT p (eitherResIdent (T_"  ++ name ++ " . share) s)) }"
      | (name,exp) <- toks]
   userDefTokenConstrs = unlines $
     [" | T_" ++ name ++ " !String" | (name,_) <- toks]
   userDefTokenPrint = unlines $
     ["  PT _ (T_" ++ name ++ " s) -> s" | (name,_) <- toks]
   toks = tokenPragmas cf ++ ruleTokens cf

   ident =
     "$l $i*   { tok (\\p s -> PT p (eitherResIdent (TV . share) s)) }" 
     --ifC "Ident"  "<ident>   ::= ^l ^i*   { ident  p = PT p . eitherResIdent TV }" 

   resws = reservedWords cf ++ symbols cf


data BTree = N | B String Int BTree BTree 

instance Show BTree where
    showsPrec _ N = showString "N"
    showsPrec n (B s k l r) = wrap (showString "b " . shows s  . showChar ' '. shows k  . showChar ' '
				    . showsPrec 1 l . showChar ' '
				    . showsPrec 1 r)
	where wrap f = if n > 0 then showChar '(' . f . showChar ')' else f

sorted2tree :: [(String,Int)] -> BTree
sorted2tree [] = N
sorted2tree xs = B x n (sorted2tree t1) (sorted2tree t2) where
  (t1,((x,n):t2)) = splitAt (length xs `div` 2) xs


-------------------------------------------------------------------
-- Inlined version of @RegToAlex@.
-- Syntax has changed...
-------------------------------------------------------------------

-- modified from pretty-printer generated by the BNF converter

-- the top-level printing method
printRegAlex :: Reg -> String
printRegAlex = render . prt 0

-- you may want to change render and parenth

render :: [String] -> String
render = rend 0
    where rend :: Int -> [String] -> String
	  rend i ss = case ss of
		        "["      :ts -> cons "["  $ rend i ts
			"("      :ts -> cons "("  $ rend i ts
			t  : "," :ts -> cons t    $ space "," $ rend i ts
		        t  : ")" :ts -> cons t    $ cons ")"  $ rend i ts
			t  : "]" :ts -> cons t    $ cons "]"  $ rend i ts
			t        :ts -> space t   $ rend i ts
			_            -> ""

	  cons s t  = s ++ t
	  new i s   = s
	  space t s = if null s then t else t ++ " " ++ s

parenth :: [String] -> [String]
parenth ss = ["("] ++ ss ++ [")"]

-- the printer class does the job
class Print a where
  prt :: Int -> a -> [String]
  prtList :: [a] -> [String]
  prtList = concat . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ c = if isAlphaNum c && isLatin1 c then [[c]] else ['\\':show (ord c)]
  prtList s = map (concat . prt 0) s

prPrec :: Int -> Int -> [String] -> [String]
prPrec i j = if j<i then parenth else id

instance Print Ident where
  prt _ (Ident i) = [i]

instance Print Reg where
  prt i e = case e of
   RSeq reg0 reg -> prPrec i 2 (concat [prt 2 reg0 , prt 3 reg])
   RAlt reg0 reg -> prPrec i 1 (concat [prt 1 reg0 , ["|"] , prt 2 reg])
   RMinus reg0 reg -> prPrec i 1 (concat [prt 2 reg0 , ["#"] , prt 2 reg])
   RStar reg -> prPrec i 3 (concat [prt 3 reg , ["*"]])
   RPlus reg -> prPrec i 3 (concat [prt 3 reg , ["+"]])
   ROpt reg  -> prPrec i 3 (concat [prt 3 reg , ["?"]])
   REps  -> prPrec i 3 (["/\\n"])
   RChar c -> prPrec i 3 (concat [prt 0 c])
   RAlts str -> prPrec i 3 (concat [["["],prt 0 str,["]"]])
   RSeqs str -> prPrec i 2 (concat (map (prt 0) str))
   RDigit  -> prPrec i 3 (concat [["$d"]])
   RLetter  -> prPrec i 3 (concat [["$l"]])
   RUpper  -> prPrec i 3 (concat [["$c"]])
   RLower  -> prPrec i 3 (concat [["$s"]])
   RAny  -> prPrec i 3 (concat [["$u"]])


