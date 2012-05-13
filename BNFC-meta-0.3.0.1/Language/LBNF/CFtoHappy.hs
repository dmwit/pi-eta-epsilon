{-
    BNF Converter: Happy Generator
    Copyright (C) 2004  Author:  Markus Forberg, Aarne Ranta

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

module Language.LBNF.CFtoHappy 
       (
       cf2Happy
       ,HappyMode(..)
       ,abstractHappy, concreteHappy
       )
        where

import Language.LBNF.CF
import Language.Haskell.TH(Dec,Q,Loc(..))

import Text.Happy.Quote
import Data.List (intersperse, sort)
import Data.Char


-- Type declarations

type Rules       = [Rul]
type Rul        = (NonTerminal,[(Rule,Pattern,Action)])
type NonTerminal = String
type Pattern     = [Either String String]
data Action      = MkAction (Maybe String) [(Bool,Cat,MetaVar)]
type MetaVar     = String



-- default naming


moduleName  = "HappyParser"
tokenName   = "Token"

appEPAllL = "appEPAllL myLocation "
appEPAll  = "appEPAll myLocation "
fromToken = "fromToken myLocation "
fromPositionToken = "fromPositionToken myLocation "
fromLitteral = "fromLit myLocation "

-- Happy mode

data HappyMode = Standard | GLR deriving Eq

abstractHappy :: Loc -> CF -> Q [Dec]
abstractHappy m = compileHappy' . parseHappyInfo . cf2Happy m where
  compileHappy' (c,i) = do
    happyWarn i
    compileHappy c

concreteHappy :: Loc -> CF -> String
concreteHappy m = parseHappy . cf2Happy m


-- generates happy code. 
cf2Happy :: Loc -> CF -> String
cf2Happy l cf
 = unlines 
    [declarations Standard (allEntryPoints cf),
     tokens (symbols cf ++ reservedWords cf),
     specialToks cf,
     delimiter,
     specialRules l cf,
     prRules l (rulesForHappy cf),
     finalize l cf]


-- The declarations of a happy file.
declarations :: HappyMode -> [NonTerminal] -> String
declarations mode ns = unlines 
                 [generateP ns,
          	  case mode of 
                    Standard -> "-- no lexer declaration"
                    GLR      -> "%lexer { myLexer } { Err _ }",
                  "%monad { ParseMonad }",
                  "%tokentype { " ++ tokenName ++ " }"]
   where generateP []     = []
	 generateP (n:ns) = concat ["%name p",n' ," ",n' ,"\n%name q",n' ," QQ_",n',"\n" ,generateP ns]
                               where n' = identCat n

-- The useless delimiter symbol.
delimiter :: String
delimiter = "\n%%\n"

-- Generate the list of tokens and their identifiers.
tokens :: [String] -> String
tokens toks = "%token \n" ++ prTokens (zip (sort toks) [1..])
 where prTokens []         = []
       prTokens ((t,k):tk) = " " ++ (convert t) ++ 
                             " { " ++ oneTok t k ++ " }\n" ++
                             prTokens tk
       oneTok t k = "PT _ (TS _ " ++ show k ++ ")"

-- Happy doesn't allow characters such as едц to occur in the happy file. This
-- is however not a restriction, just a naming paradigm in the happy source file.
convert :: String -> String
convert "\\" = concat ['\'':"\\\\","\'"]
convert xs   = concat ['\'':(escape xs),"\'"]
  where escape [] = []
	escape ('\'':xs) = '\\':'\'' : escape xs
	escape (x:xs) = x:escape xs

rulesForHappy :: CF -> Rules
rulesForHappy cf = map mkOne $ ruleGroups cf where
  mkOne (cat,rules) = constructRule cf rules cat

-- For every non-terminal, we construct a set of rules. A rule is a sequence of
-- terminals and non-terminals, and an action to be performed
-- As an optimization, a pair of list rules [C] ::= "" | C k [C]
-- is left-recursivized into [C] ::= "" | [C] C k.
-- This could be generalized to cover other forms of list rules.
constructRule :: CF -> [Rule] -> NonTerminal -> (NonTerminal,[(Rule,Pattern,Action)])
constructRule cf rules nt = (nt,[(r,p,generateAction nt (revF b r) m) | 
     r0 <- rules,
     let (b,r) = if isConsFun (funRule r0) && elem (valCat r0) revs 
                   then (True,revSepListRule r0) 
                 else (False,r0),
     let (p,m) = generatePatterns cf r])
 where
   revF b r = if b then ("flip " ++ funRule r) else (underscore $ funRule r)
   revs = reversibleCats cf
   underscore f | isDefinedRule f   = f ++ "_"
		| otherwise	    = f

-- Generates a string containing the semantic action.
-- An action can for example be: Sum $1 $2, that is, construct an AST
-- with the constructor Sum applied to the two metavariables $1 and $2.
generateAction :: NonTerminal -> (Fun) -> [(Bool,Cat,MetaVar)] -> Action
generateAction nt f ms = MkAction (if isCoercion f then Nothing else Just f) ms

-- Generate patterns and a set of metavariables indicating 
-- where in the pattern the non-terminal

generatePatterns :: CF -> Rule -> (Pattern,[(Bool,Cat,MetaVar)])
generatePatterns cf r = case rhsRule r of
  Left []   -> ([Right "{- empty -}"],[])
  Left its  -> ((map mkIt its), metas its) 
  Right (_,tok)   -> ([Right $ "L_" ++  tok],[(False,funRule r,"$1")])
 where
   mkIt i = case i of
     Left c -> Left c
     Right s -> Right $ convert s
   metas its = [revIf c ('$': show i) | (i,Left c) <- zip [1 ::Int ..] its]
   revIf c m = (not (isConsFun (funRule r)) && elem c revs,c,m) 
   revs = reversibleCats cf


-- We have now constructed the patterns and actions, 
-- so the only thing left is to merge them into one string.



prRules :: Loc -> Rules -> String
prRules l rs = unlines . map prOne $  rs
  where
    prOne (nt,[]) = ""
    prOne r@(nt,_) = 
      prTypeSig n (normCat nt) ++ prRule l r ++ 
      prTypeSig qqn "BNFC_QQType" ++ prRuleQ l r
        where qqn   = qqCat nt
              n     = identCat nt

qqCat = ("QQ_"++). identCat
    
qualify "" f     = f
qualify _ f@"[]" = f
qualify m  f     = m ++ "." ++ f

prTypeSig :: String -> String -> String
prTypeSig cat typ = unwords [cat, "::", "{", typ, "}\n"]

prRule :: Loc -> Rul -> String
prRule _ (_,[])           = ""
prRule m (nt,((_,p,a):ls))  = 
  unwords [identCat nt, ":" , prPattern p, "{", prAction a, "}", "\n" ++ pr ls] ++ "\n"
  where 
    pr [] = []
    pr ((_,p,a):ls) = 
      unlines [(concat $ intersperse " " ["  |", prPattern p, "{", prAction a , "}"])] ++ pr ls
    prAction :: Action -> String
    prAction (MkAction fun []) = maybe "" pf fun where
      pf f = f
    prAction (MkAction fun ms) = maybe (thrd $ head ms) pf fun where
      thrd (_,_,m) = m
      pf f 
       | isAqFun f = "% fail \"Can not parse anti-quoted expressions\""
       | otherwise 
         = f++" "++unwords ["("++(if b then "reverse $ " else "")++m1++")"|(b,c,m1) <- ms]
    


prRuleQ :: Loc -> Rul -> String
prRuleQ _ (_,[])           = ""
prRuleQ m (nt,((rul,p,a):ls))  = 
  unwords [qqCat nt, ":" , prPatternQ (isAqAction a) p, "{", prActionQ rul a, "}", "\n" ++ pr ls] ++ "\n"
  where 
    pr [] = []
    pr ((rulx,p,a):ls) = 
      unlines [(concat $ intersperse " " ["  |", prPatternQ (isAqAction a) p, "{", prActionQ rulx a , "}"])] ++ pr ls where
    prActionQ :: Rule -> Action -> String
    prActionQ rulz (MkAction fun []) = maybe "" pf fun where
          pf f = appEPAll ++" \"" ++ f++"\" []"
    prActionQ rulz (MkAction fun ms) = maybe (thrd $ head ms) pf fun where
            thrd (_,_,m) = m
            pf f 
              | isAqFun f = fun ++ " " ++ unwords (map (\(b,c,m) -> m) ms)
              | isTokenRule rulz = fromToken ++ "\""++f++"\" $1"
              | otherwise       = constr++" ["++
                 (concat $ intersperse "," [m1|(_,c,m1) <- ms])
                 ++ "]"       
              where 
                fun = case tail f of
                  [] | isTokenRule rulz -> "stringAq"
                     | otherwise        -> "printAq"
                  x  -> x
                constr = case f of
                  "flip (:)" -> appEPAllL
                  "(:)"      -> appEPAll ++"\":\" "
                  "(:[])"    -> appEPAllL
                  _          -> appEPAll ++"\""++f++"\" "       
          
          {-
        expspats 
          | isTokenRule rul = fromToken ++ "\""++f++"\" $1"
          | otherwise       = constr++" ["++
              (concat $ intersperse "," [m1|(_,c,m1) <- ms])
              ++ "]"

-}
isAqAction (MkAction mf _) = maybe False isAqFun mf

prPattern      = prPatternQ True
prPatternQ  aq = unwords . (map $ either (if aq then identCat else qqCat) id)


-- Finally, some haskell code.

finalize :: Loc -> CF -> String
finalize l cf = unlines $
   [
     "{",
     "\nhappyError :: [" ++ tokenName ++ "] -> ParseMonad a",
     "happyError ts =", 
     "  fail $ \"syntax error at \" ++ tokenPos ts ++ ",
     "  case ts of",
     "    [] -> []",
     "    [Err _] -> \" due to lexer error\"", 
     "    _ -> \" before \" ++ unwords (map prToken (take 4 ts))",
     "",
     "myLexer = " ++ (if hasLayout cf then "resolveLayout True . " else "") ++ "tokens",
     "",
     "myLocation = (\""++loc_package l++"\",\""++loc_module l++"\")",
     ""
   ] ++ definedRules cf ++ [ "}" ]

definedRules ((ps,_),_) = [ mkDef f xs e | FunDef f xs e <- ps ]
    where
	mkDef f xs e = unwords $ (f ++ "_") : xs' ++ ["=", show e']
	    where
		xs' = map (++"_") xs
		e'  = underscore e
	underscore (App x es)
	    | isLower $ head x	= App (x ++ "_") $ map underscore es
	    | otherwise		= App x $ map underscore es
	underscore e	      = e

-- aarne's modifs 8/1/2002:
-- Markus's modifs 11/02/2002

-- GF literals
specialToks :: CF -> String
specialToks cf = unlines $
		 (map aux (literals cf))
		  ++ ["L_err    { _ }"]
 where aux cat = 
        case cat of
          "Ident"  -> "L_ident  { PT _ (TV $$) }"
          "String" -> "L_quoted { PT _ (TL $$) }"
          "Integer" -> "L_integ  { PT _ (TI $$) }"
          "Double" -> "L_doubl  { PT _ (TD $$) }"
          "Char"   -> "L_charac { PT _ (TC $$) }"
          own      -> "L_" ++ own ++ " { PT _ (T_" ++ own ++ " " ++ posn ++ ") }"
         where
           posn = if isPositionCat cf cat then "_" else "$$"

specialRules :: Loc -> CF -> String
specialRules l cf = unlines $
                  map aux (typed_literals cf)
 where 
   -- m = loc_module l
   aux (fun,cat) =
     case cat of
--         "Ident"   -> "Ident   :: { (Ident }   : L_ident  { (Ident $1,fromToken \"Ident\" $1) }" 
--	 "String"  -> "String  :: { (String,BNFC_QQType) }  : L_quoted { fromString $1 }" -- FIXME: Why not read?
--	 "Integer" -> "Integer :: { (Integer,BNFC_QQType) } \nInteger : "++iaq++"L_integ  { fromLit (read $1) }"
--	 "Double"  -> "Double  :: { (Double,BNFC_QQType) }  : L_doubl  { fromLit (read $1) }"
--	 "Char"    -> "Char    :: { (Char,BNFC_QQType) }    : L_charac { fromLit (read $1) }"
--	 own       -> own ++ "    :: { (" ++ own ++ ",BNFC_QQType) } : L_" ++ own ++ 
--	   " { (" ++ own ++ " ("++ posn ++ "$1),fromToken \""++own++"\" $1)}"
      "Ident"   -> unlines
        [ "Ident    :: { Ident }             : L_ident  { Ident $1 }"
        , "QQ_Ident :: { BNFC_QQType }   : L_ident  { "++fromToken ++"\"Ident\" $1 }"
        ] ++ aqrule "Ident"
      "String"  -> unlines
        [ "String  :: { String }  : L_quoted { $1 }" 
        , "QQ_String :: { BNFC_QQType }"
        , "QQ_String : L_quoted  { fromString myLocation $1 }"
        ] ++ aqrule "String"
      "Integer" -> unlines
        [ "Integer    :: { Integer } : L_integ  { (read $1) :: Integer }"
        , "QQ_Integer :: { BNFC_QQType }"
        , "QQ_Integer : L_integ  { "++fromLitteral++"(read $1 :: Integer) }"
        ] ++ aqrule "Integer"
      "Double"  -> unlines
        [ "Double  :: { Double }  : L_doubl  { (read $1) :: Double }"
        , "QQ_Double :: { BNFC_QQType }"
        , "QQ_Double : L_doubl  { "++fromLitteral++" (read $1 :: Double) }"
        ] ++ aqrule "Double"
      "Char"    -> unlines
        [ "Char    :: { Char }    : L_charac { (read $1) :: Char }"
        , "QQ_Char :: { BNFC_QQType }"
        , "QQ_Char : L_charac  { "++fromLitteral++" (read $1 :: Char) }"
        ] ++ aqrule "Char"
      "AqToken" -> unlines
        ["AqToken    :: { AqToken }             : L_AqToken  { AqToken $1 }"]
      _         -> unlines
        [ cat ++ "    :: { " ++ cat ++ "} : L_" ++ fun ++ " { " ++ fun ++ " ("++ posn ++ "$1)}"
        , "QQ_"++cat ++ " :: { BNFC_QQType }"
        , "QQ_"++cat ++ " : L_" ++ fun ++ " {"++fromToken' ++" \""++ fun ++"\" ("++ posn ++ " $1 ) }"
        ] ++  aqrule cat
      where
         posn = if isPositionCat cf cat then "mkPosToken " else ""
         fromToken' = if isPositionCat cf cat then fromPositionToken else fromToken
         isPos = isPositionCat cf cat
   aqrule = maybe (const "") rule $ aqSyntax cf 
   rule (b,i,a) = twoRules where
     open     = "'"++b++"' " ++ body 
     closed t = "'"++b++t++"' " ++ body
     body     = "AqToken { global_aq $2 } "
     twoRules typ = "\n  | "++ open ++ "\n  | " ++ closed typ



