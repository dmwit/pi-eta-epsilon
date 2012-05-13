{-#LANGUAGE QuasiQuotes #-}
{-
    BNF Converter: Language definition
    Copyright (C) 2004  Author: Markus Forberg, Michael Pellauer, Aarne Ranta

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

import Language.LBNF
import Language.Haskell.TH(runQ)
import System.Directory (copyFile, doesFileExist)
import Control.Monad(when)

import System.FilePath

package = "BNFC-meta-0.2.1"

nfile = file ++ ".new.bak"
file = ".." </> "Language" </> "LBNF" </> "Grammar.hs"
bfile = file ++ ".bak"
bbfile = bfile ++ ".bak"

main = do
  c <- getCode ("Language.LBNF.Grammar") g
  doesFileExist bfile >>= \b -> when b $ copyFile bfile bbfile
  copyFile file bfile
  writeFile file $ c

revert = do
  copyFile file nfile
  doesFileExist bfile >>= \b -> when b $ copyFile bfile file
  doesFileExist bbfile >>= \b -> when b $ copyFile bbfile bfile




here = do
  c <- getCode ("Grammar") g
  writeFile "Grammar.hs" c


g = [lbnf|


-- A Grammar is a sequence of definitions

-- What is this
-- LGr.      LGrammar ::= [LDef] ;
-- DefAll.   LDef ::= Def ;
-- DefSome.  LDef ::= [Ident] ":" Def ;
-- LDefView. LDef ::= "views" [Ident] ;
-- separator LDef ";" ;

Grammar . Grammar ::= [Def] ;

separator Def ";" ;

[]  . [Item] ::= ;
(:) . [Item] ::= Item [Item] ;
--The rules of the grammar
Rule   . Def ::= Label "." Cat "::=" RHS ;
-- TRule  . Def ::= Label "*" Cat "::=" Reg ;

-- RegRHS.  RHS ::= Reg ;
RHS.   RHS ::= [Item] ;
TRHS.  RHS ::= "@" Reg ;
separator nonempty RHS "|" ;

-- Items
Terminal  . Item ::= String ;
NTerminal . Item ::= Cat ;

-- Categories
OptCat   . Cat  ::= "?" Cat1 ;
_        . Cat  ::= Cat1 ;
ListCat  . Cat1 ::= "[" Cat "]" ;
IdCat    . Cat1 ::= Ident ;

-- functional labels
Id       . Label ::= Ident ; 
Wild     . Label ::= "_" ; 
ListE    . Label ::= "[" "]" ;
ListCons . Label ::= "(" ":" ")" ; 
ListOne  . Label ::= "(" ":" "[" "]" ")" ;
Aq       . Label ::= "$" MIdent ;

-- Maybe Ident
JIdent   . MIdent ::= Ident ;
NIdent   . MIdent ::= ;


-- Pragmas
Comment  .  Def ::= "comment" String ;
Comments .  Def ::= "comment" String String ;
Internal .  Def ::= "internal" Label "." Cat "::=" [Item] ; 
Token.      Def ::= "token" Ident Reg ;
PosToken.   Def ::= "position" "token" Ident Reg ;
Entryp.     Def ::= "entrypoints" [Ident] ;
Separator.  Def ::= "separator" MinimumSize Cat String ;
Terminator. Def ::= "terminator" MinimumSize Cat String ;
Coercions.  Def ::= "coercions" Ident Integer ;
Rules.      Def ::= "rules" Ident "::=" [RHS] ;
Function.   Def ::= "define" Ident [Arg] "=" Exp ;
External.   Def ::= "external" Ident "=" HsTyp ;
AntiQuote.  Def ::= "antiquote" String String String ;
Derive.     Def ::= "derive" [Ident] ;


HsApp.      HsTyp  ::= HsTyp HsTyp1 ;
HsCon.      HsTyp1 ::= Ident ;
HsTup.      HsTyp1 ::= "(" [HsTyp] ")" ;
HsList.     HsTyp1 ::= "[" HsTyp "]" ;
-- _.          HsTyp1 ::= "(" HsTyp ")" ;

separator nonempty HsTyp ",";


Layout.     Def ::= "layout" [String] ;
LayoutStop. Def ::= "layout" "stop" [String] ;
LayoutTop.  Def ::= "layout" "toplevel" ;

Arg. Arg ::= Ident ;
separator Arg "" ;

-- Expressions
Cons.	    Exp  ::= Exp1 ":" Exp ;
App.	    Exp1 ::= Ident [Exp2] ;
Var.	    Exp2 ::= Ident ;
LitInt.	    Exp2 ::= Integer ;
LitChar.    Exp2 ::= Char ;
LitString.  Exp2 ::= String ;
LitDouble.  Exp2 ::= Double ;
List.	    Exp2 ::= "[" [Exp] "]" ;


coercions Exp 2;
separator nonempty Exp2 "" ;
separator Exp "," ;

separator nonempty String "," ;

-- List size condition
MNonempty.  MinimumSize ::= "nonempty" ;
MEmpty.     MinimumSize ::=  ;

-- regular expressions
RSeq.   Reg2 ::= Reg2 Reg3 ;
RAlt.   Reg1 ::= Reg1 "|" Reg2 ;
RMinus. Reg1 ::= Reg2 "-" Reg2 ;

RStar.  Reg3 ::= Reg3 "*" ;
RPlus.  Reg3 ::= Reg3 "+" ;
ROpt.   Reg3 ::= Reg3 "?" ;

REps.   Reg3 ::= "eps" ;

RChar.  Reg3 ::= Char ;           -- single character
RAlts.  Reg3 ::= "[" String "]" ; -- list of alternative characters
RSeqs.  Reg3 ::= "{" String "}" ; -- character sequence

RDigit.  Reg3 ::= "digit" ;
RLetter. Reg3 ::= "letter" ;
RUpper.  Reg3 ::= "upper" ;
RLower.  Reg3 ::= "lower" ;
RAny.    Reg3 ::= "char" ;

_. Reg  ::= Reg1 ;
_. Reg1 ::= Reg2 ;
_. Reg2 ::= Reg3 ;
_. Reg3 ::= "(" Reg ")" ;

-- list of categories in the entrypoint pragma
(:[]).  [Ident] ::= Ident ;
(:).    [Ident] ::= Ident "," [Ident] ;

-- comments in BNF source
comment "--" ;
comment "{-" "-}" ; 
|]
