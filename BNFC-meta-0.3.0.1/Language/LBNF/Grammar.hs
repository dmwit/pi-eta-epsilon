{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Language.LBNF.Grammar(Language.LBNF.Grammar.myLexer
  , Language.LBNF.Grammar.tokens
  , Language.LBNF.Grammar.pGrammar
  , Language.LBNF.Grammar.pListDef
  , Language.LBNF.Grammar.pListItem
  , Language.LBNF.Grammar.pDef
  , Language.LBNF.Grammar.pRHS
  , Language.LBNF.Grammar.pListRHS
  , Language.LBNF.Grammar.pItem
  , Language.LBNF.Grammar.pCat
  , Language.LBNF.Grammar.pCat1
  , Language.LBNF.Grammar.pLabel
  , Language.LBNF.Grammar.pMIdent
  , Language.LBNF.Grammar.pHsTyp
  , Language.LBNF.Grammar.pHsTyp1
  , Language.LBNF.Grammar.pListHsTyp
  , Language.LBNF.Grammar.pArg
  , Language.LBNF.Grammar.pListArg
  , Language.LBNF.Grammar.pExp
  , Language.LBNF.Grammar.pExp1
  , Language.LBNF.Grammar.pExp2
  , Language.LBNF.Grammar.pListExp2
  , Language.LBNF.Grammar.pListExp
  , Language.LBNF.Grammar.pListString
  , Language.LBNF.Grammar.pMinimumSize
  , Language.LBNF.Grammar.pReg2
  , Language.LBNF.Grammar.pReg1
  , Language.LBNF.Grammar.pReg3
  , Language.LBNF.Grammar.pReg
  , Language.LBNF.Grammar.pListIdent
  , Language.LBNF.Grammar.qGrammar
  , Language.LBNF.Grammar.qListDef
  , Language.LBNF.Grammar.qListItem
  , Language.LBNF.Grammar.qDef
  , Language.LBNF.Grammar.qRHS
  , Language.LBNF.Grammar.qListRHS
  , Language.LBNF.Grammar.qItem
  , Language.LBNF.Grammar.qCat
  , Language.LBNF.Grammar.qCat1
  , Language.LBNF.Grammar.qLabel
  , Language.LBNF.Grammar.qMIdent
  , Language.LBNF.Grammar.qHsTyp
  , Language.LBNF.Grammar.qHsTyp1
  , Language.LBNF.Grammar.qListHsTyp
  , Language.LBNF.Grammar.qArg
  , Language.LBNF.Grammar.qListArg
  , Language.LBNF.Grammar.qExp
  , Language.LBNF.Grammar.qExp1
  , Language.LBNF.Grammar.qExp2
  , Language.LBNF.Grammar.qListExp2
  , Language.LBNF.Grammar.qListExp
  , Language.LBNF.Grammar.qListString
  , Language.LBNF.Grammar.qMinimumSize
  , Language.LBNF.Grammar.qReg2
  , Language.LBNF.Grammar.qReg1
  , Language.LBNF.Grammar.qReg3
  , Language.LBNF.Grammar.qReg
  , Language.LBNF.Grammar.qListIdent
  , Language.LBNF.Grammar.grammar
  , Language.LBNF.Grammar.listDef
  , Language.LBNF.Grammar.listItem
  , Language.LBNF.Grammar.def
  , Language.LBNF.Grammar.rHS
  , Language.LBNF.Grammar.listRHS
  , Language.LBNF.Grammar.item
  , Language.LBNF.Grammar.cat
  , Language.LBNF.Grammar.cat1
  , Language.LBNF.Grammar.label
  , Language.LBNF.Grammar.mIdent
  , Language.LBNF.Grammar.hsTyp
  , Language.LBNF.Grammar.hsTyp1
  , Language.LBNF.Grammar.listHsTyp
  , Language.LBNF.Grammar.arg
  , Language.LBNF.Grammar.listArg
  , Language.LBNF.Grammar.exp
  , Language.LBNF.Grammar.exp1
  , Language.LBNF.Grammar.exp2
  , Language.LBNF.Grammar.listExp2
  , Language.LBNF.Grammar.listExp
  , Language.LBNF.Grammar.listString
  , Language.LBNF.Grammar.minimumSize
  , Language.LBNF.Grammar.reg2
  , Language.LBNF.Grammar.reg1
  , Language.LBNF.Grammar.reg3
  , Language.LBNF.Grammar.reg
  , Language.LBNF.Grammar.listIdent
  , Language.LBNF.Grammar.Grammar(..)
  , Language.LBNF.Grammar.Def(..)
  , Language.LBNF.Grammar.RHS(..)
  , Language.LBNF.Grammar.Item(..)
  , Language.LBNF.Grammar.Cat(..)
  , Language.LBNF.Grammar.Label(..)
  , Language.LBNF.Grammar.MIdent(..)
  , Language.LBNF.Grammar.HsTyp(..)
  , Language.LBNF.Grammar.Arg(..)
  , Language.LBNF.Grammar.Exp(..)
  , Language.LBNF.Grammar.MinimumSize(..)
  , Language.LBNF.Grammar.Reg(..)
  , Language.LBNF.Grammar.Ident(..)) where
import Language.LBNF.Compiletime
import Language.Haskell.TH (location, loc_package)

data Grammar = Grammar ([Def]) deriving (Show, Eq, Ord)
data Def
    = Rule Label Cat RHS
    | Comment String
    | Comments String String
    | Internal Label Cat ([Item])
    | Token Ident Reg
    | PosToken Ident Reg
    | Entryp ([Ident])
    | Separator MinimumSize Cat String
    | Terminator MinimumSize Cat String
    | Coercions Ident Integer
    | Rules Ident ([RHS])
    | Function Ident ([Arg]) Exp
    | External Ident HsTyp
    | AntiQuote String String String
    | Derive ([Ident])
    | Layout ([String])
    | LayoutStop ([String])
    | LayoutTop
    deriving (Show, Eq, Ord)
data RHS = RHS ([Item]) | TRHS Reg deriving (Show, Eq, Ord)
data Item
    = Terminal String | NTerminal Cat
    deriving (Show, Eq, Ord)
data Cat
    = OptCat Cat | ListCat Cat | IdCat Ident
    deriving (Show, Eq, Ord)
data Label
    = Id Ident | Wild | ListE | ListCons | ListOne | Aq MIdent
    deriving (Show, Eq, Ord)
data MIdent = JIdent Ident | NIdent deriving (Show, Eq, Ord)
data HsTyp
    = HsApp HsTyp HsTyp | HsCon Ident | HsTup ([HsTyp]) | HsList HsTyp
    deriving (Show, Eq, Ord)
data Arg = Arg Ident deriving (Show, Eq, Ord)
data Exp
    = Cons Exp Exp
    | App Ident ([Exp])
    | Var Ident
    | LitInt Integer
    | LitChar Char
    | LitString String
    | LitDouble Double
    | List ([Exp])
    deriving (Show, Eq, Ord)
data MinimumSize = MNonempty | MEmpty deriving (Show, Eq, Ord)
data Reg
    = RSeq Reg Reg
    | RAlt Reg Reg
    | RMinus Reg Reg
    | RStar Reg
    | RPlus Reg
    | ROpt Reg
    | REps
    | RChar Char
    | RAlts String
    | RSeqs String
    | RDigit
    | RLetter
    | RUpper
    | RLower
    | RAny
    deriving (Show, Eq, Ord)
newtype Ident = Ident String deriving (Show, Eq, Ord)
instance Print Ident
    where prt _ (Ident i_0) = doc (showString i_0)
instance Print Grammar
    where prt i_1 x_2 = case x_2 of
                                                  Grammar defs -> prPrec i_1 0 (concatD [prt 0 defs])
instance Print Def
    where prt i_3 x_4 = case x_4 of
                                                  Rule label
                                                       cat
                                                       rhs -> prPrec i_3 0 (concatD [prt 0 label,
                                                                                                                                 doc (showString "."),
                                                                                                                                 prt 0 cat,
                                                                                                                                 doc (showString "::="),
                                                                                                                                 prt 0 rhs])
                                                  Comment str -> prPrec i_3 0 (concatD [doc (showString "comment"),
                                                                                                                                    prt 0 str])
                                                  Comments str0
                                                           str -> prPrec i_3 0 (concatD [doc (showString "comment"),
                                                                                                                                     prt 0 str0,
                                                                                                                                     prt 0 str])
                                                  Internal label
                                                           cat
                                                           items -> prPrec i_3 0 (concatD [doc (showString "internal"),
                                                                                                                                       prt 0 label,
                                                                                                                                       doc (showString "."),
                                                                                                                                       prt 0 cat,
                                                                                                                                       doc (showString "::="),
                                                                                                                                       prt 0 items])
                                                  Token id
                                                        reg -> prPrec i_3 0 (concatD [doc (showString "token"),
                                                                                                                                  prt 0 id,
                                                                                                                                  prt 0 reg])
                                                  PosToken id
                                                           reg -> prPrec i_3 0 (concatD [doc (showString "position"),
                                                                                                                                     doc (showString "token"),
                                                                                                                                     prt 0 id,
                                                                                                                                     prt 0 reg])
                                                  Entryp ids -> prPrec i_3 0 (concatD [doc (showString "entrypoints"),
                                                                                                                                   prt 0 ids])
                                                  Separator minimumsize
                                                            cat
                                                            str -> prPrec i_3 0 (concatD [doc (showString "separator"),
                                                                                                                                      prt 0 minimumsize,
                                                                                                                                      prt 0 cat,
                                                                                                                                      prt 0 str])
                                                  Terminator minimumsize
                                                             cat
                                                             str -> prPrec i_3 0 (concatD [doc (showString "terminator"),
                                                                                                                                       prt 0 minimumsize,
                                                                                                                                       prt 0 cat,
                                                                                                                                       prt 0 str])
                                                  Coercions id
                                                            n -> prPrec i_3 0 (concatD [doc (showString "coercions"),
                                                                                                                                    prt 0 id,
                                                                                                                                    prt 0 n])
                                                  Rules id
                                                        rhss -> prPrec i_3 0 (concatD [doc (showString "rules"),
                                                                                                                                   prt 0 id,
                                                                                                                                   doc (showString "::="),
                                                                                                                                   prt 0 rhss])
                                                  Function id
                                                           args
                                                           exp -> prPrec i_3 0 (concatD [doc (showString "define"),
                                                                                                                                     prt 0 id,
                                                                                                                                     prt 0 args,
                                                                                                                                     doc (showString "="),
                                                                                                                                     prt 0 exp])
                                                  External id
                                                           hstyp -> prPrec i_3 0 (concatD [doc (showString "external"),
                                                                                                                                       prt 0 id,
                                                                                                                                       doc (showString "="),
                                                                                                                                       prt 0 hstyp])
                                                  AntiQuote str0
                                                            str1
                                                            str -> prPrec i_3 0 (concatD [doc (showString "antiquote"),
                                                                                                                                      prt 0 str0,
                                                                                                                                      prt 0 str1,
                                                                                                                                      prt 0 str])
                                                  Derive ids -> prPrec i_3 0 (concatD [doc (showString "derive"),
                                                                                                                                   prt 0 ids])
                                                  Layout strs -> prPrec i_3 0 (concatD [doc (showString "layout"),
                                                                                                                                    prt 0 strs])
                                                  LayoutStop strs -> prPrec i_3 0 (concatD [doc (showString "layout"),
                                                                                                                                        doc (showString "stop"),
                                                                                                                                        prt 0 strs])
                                                  LayoutTop -> prPrec i_3 0 (concatD [doc (showString "layout"),
                                                                                                                                  doc (showString "toplevel")])
          prtList es_5 = case es_5 of
                                                   [] -> concatD []
                                                   [x] -> concatD [prt 0 x]
                                                   (:) x
                                                                 xs -> concatD [prt 0 x,
                                                                                                      doc (showString ";"),
                                                                                                      prt 0 xs]
instance Print RHS
    where prt i_6 x_7 = case x_7 of
                                                  RHS items -> prPrec i_6 0 (concatD [prt 0 items])
                                                  TRHS reg -> prPrec i_6 0 (concatD [doc (showString "@"),
                                                                                                                                 prt 0 reg])
          prtList es_8 = case es_8 of
                                                   [x] -> concatD [prt 0 x]
                                                   (:) x
                                                                 xs -> concatD [prt 0 x,
                                                                                                      doc (showString "|"),
                                                                                                      prt 0 xs]
instance Print Item
    where prt i_9 x_10 = case x_10 of
                                                   Terminal str -> prPrec i_9 0 (concatD [prt 0 str])
                                                   NTerminal cat -> prPrec i_9 0 (concatD [prt 0 cat])
          prtList es_11 = case es_11 of
                                                    [] -> concatD []
                                                    (:) x
                                                                  xs -> concatD [prt 0 x,
                                                                                                       prt 0 xs]
instance Print Cat
    where prt i_12 x_13 = case x_13 of
                                                    OptCat cat -> prPrec i_12 0 (concatD [doc (showString "?"),
                                                                                                                                      prt 1 cat])
                                                    ListCat cat -> prPrec i_12 1 (concatD [doc (showString "["),
                                                                                                                                       prt 0 cat,
                                                                                                                                       doc (showString "]")])
                                                    IdCat id -> prPrec i_12 1 (concatD [prt 0 id])
instance Print Label
    where prt i_14 x_15 = case x_15 of
                                                    Id id -> prPrec i_14 0 (concatD [prt 0 id])
                                                    Wild -> prPrec i_14 0 (concatD [doc (showString "_")])
                                                    ListE -> prPrec i_14 0 (concatD [doc (showString "["),
                                                                                                                                 doc (showString "]")])
                                                    ListCons -> prPrec i_14 0 (concatD [doc (showString "("),
                                                                                                                                    doc (showString ":"),
                                                                                                                                    doc (showString ")")])
                                                    ListOne -> prPrec i_14 0 (concatD [doc (showString "("),
                                                                                                                                   doc (showString ":"),
                                                                                                                                   doc (showString "["),
                                                                                                                                   doc (showString "]"),
                                                                                                                                   doc (showString ")")])
                                                    Aq mident -> prPrec i_14 0 (concatD [doc (showString "$"),
                                                                                                                                     prt 0 mident])
instance Print MIdent
    where prt i_16 x_17 = case x_17 of
                                                    JIdent id -> prPrec i_16 0 (concatD [prt 0 id])
                                                    NIdent -> prPrec i_16 0 (concatD [])
instance Print HsTyp
    where prt i_18 x_19 = case x_19 of
                                                    HsApp hstyp0
                                                          hstyp -> prPrec i_18 0 (concatD [prt 0 hstyp0,
                                                                                                                                       prt 1 hstyp])
                                                    HsCon id -> prPrec i_18 1 (concatD [prt 0 id])
                                                    HsTup hstyps -> prPrec i_18 1 (concatD [doc (showString "("),
                                                                                                                                        prt 0 hstyps,
                                                                                                                                        doc (showString ")")])
                                                    HsList hstyp -> prPrec i_18 1 (concatD [doc (showString "["),
                                                                                                                                        prt 0 hstyp,
                                                                                                                                        doc (showString "]")])
          prtList es_20 = case es_20 of
                                                    [x] -> concatD [prt 0 x]
                                                    (:) x
                                                                  xs -> concatD [prt 0 x,
                                                                                                       doc (showString ","),
                                                                                                       prt 0 xs]
instance Print Arg
    where prt i_21 x_22 = case x_22 of
                                                    Arg id -> prPrec i_21 0 (concatD [prt 0 id])
          prtList es_23 = case es_23 of
                                                    [] -> concatD []
                                                    (:) x
                                                                  xs -> concatD [prt 0 x,
                                                                                                       prt 0 xs]
instance Print Exp
    where prt i_24 x_25 = case x_25 of
                                                    Cons exp0
                                                         exp -> prPrec i_24 0 (concatD [prt 1 exp0,
                                                                                                                                    doc (showString ":"),
                                                                                                                                    prt 0 exp])
                                                    App id
                                                        exps -> prPrec i_24 1 (concatD [prt 0 id,
                                                                                                                                    prt 2 exps])
                                                    Var id -> prPrec i_24 2 (concatD [prt 0 id])
                                                    LitInt n -> prPrec i_24 2 (concatD [prt 0 n])
                                                    LitChar c -> prPrec i_24 2 (concatD [prt 0 c])
                                                    LitString str -> prPrec i_24 2 (concatD [prt 0 str])
                                                    LitDouble d -> prPrec i_24 2 (concatD [prt 0 d])
                                                    List exps -> prPrec i_24 2 (concatD [doc (showString "["),
                                                                                                                                     prt 0 exps,
                                                                                                                                     doc (showString "]")])
          prtList es_26 = case es_26 of
                                                    [] -> concatD []
                                                    [x] -> concatD [prt 2 x]
                                                    [x] -> concatD [prt 0 x]
                                                    (:) x
                                                                  xs -> concatD [prt 2 x,
                                                                                                       prt 2 xs]
                                                    (:) x
                                                                  xs -> concatD [prt 0 x,
                                                                                                       doc (showString ","),
                                                                                                       prt 0 xs]
instance Print MinimumSize
    where prt i_27 x_28 = case x_28 of
                                                    MNonempty -> prPrec i_27 0 (concatD [doc (showString "nonempty")])
                                                    MEmpty -> prPrec i_27 0 (concatD [])
instance Print Reg
    where prt i_29 x_30 = case x_30 of
                                                    RSeq reg0
                                                         reg -> prPrec i_29 2 (concatD [prt 2 reg0,
                                                                                                                                    prt 3 reg])
                                                    RAlt reg0
                                                         reg -> prPrec i_29 1 (concatD [prt 1 reg0,
                                                                                                                                    doc (showString "|"),
                                                                                                                                    prt 2 reg])
                                                    RMinus reg0
                                                           reg -> prPrec i_29 1 (concatD [prt 2 reg0,
                                                                                                                                      doc (showString "-"),
                                                                                                                                      prt 2 reg])
                                                    RStar reg -> prPrec i_29 3 (concatD [prt 3 reg,
                                                                                                                                     doc (showString "*")])
                                                    RPlus reg -> prPrec i_29 3 (concatD [prt 3 reg,
                                                                                                                                     doc (showString "+")])
                                                    ROpt reg -> prPrec i_29 3 (concatD [prt 3 reg,
                                                                                                                                    doc (showString "?")])
                                                    REps -> prPrec i_29 3 (concatD [doc (showString "eps")])
                                                    RChar c -> prPrec i_29 3 (concatD [prt 0 c])
                                                    RAlts str -> prPrec i_29 3 (concatD [doc (showString "["),
                                                                                                                                     prt 0 str,
                                                                                                                                     doc (showString "]")])
                                                    RSeqs str -> prPrec i_29 3 (concatD [doc (showString "{"),
                                                                                                                                     prt 0 str,
                                                                                                                                     doc (showString "}")])
                                                    RDigit -> prPrec i_29 3 (concatD [doc (showString "digit")])
                                                    RLetter -> prPrec i_29 3 (concatD [doc (showString "letter")])
                                                    RUpper -> prPrec i_29 3 (concatD [doc (showString "upper")])
                                                    RLower -> prPrec i_29 3 (concatD [doc (showString "lower")])
                                                    RAny -> prPrec i_29 3 (concatD [doc (showString "char")])
grammar = Language.LBNF.Compiletime.parseToQuoter (qGrammar . myLexer)
listDef = Language.LBNF.Compiletime.parseToQuoter (qListDef . myLexer)
listItem = Language.LBNF.Compiletime.parseToQuoter (qListItem . myLexer)
def = Language.LBNF.Compiletime.parseToQuoter (qDef . myLexer)
rHS = Language.LBNF.Compiletime.parseToQuoter (qRHS . myLexer)
listRHS = Language.LBNF.Compiletime.parseToQuoter (qListRHS . myLexer)
item = Language.LBNF.Compiletime.parseToQuoter (qItem . myLexer)
cat = Language.LBNF.Compiletime.parseToQuoter (qCat . myLexer)
cat1 = Language.LBNF.Compiletime.parseToQuoter (qCat1 . myLexer)
label = Language.LBNF.Compiletime.parseToQuoter (qLabel . myLexer)
mIdent = Language.LBNF.Compiletime.parseToQuoter (qMIdent . myLexer)
hsTyp = Language.LBNF.Compiletime.parseToQuoter (qHsTyp . myLexer)
hsTyp1 = Language.LBNF.Compiletime.parseToQuoter (qHsTyp1 . myLexer)
listHsTyp = Language.LBNF.Compiletime.parseToQuoter (qListHsTyp . myLexer)
arg = Language.LBNF.Compiletime.parseToQuoter (qArg . myLexer)
listArg = Language.LBNF.Compiletime.parseToQuoter (qListArg . myLexer)
exp = Language.LBNF.Compiletime.parseToQuoter (qExp . myLexer)
exp1 = Language.LBNF.Compiletime.parseToQuoter (qExp1 . myLexer)
exp2 = Language.LBNF.Compiletime.parseToQuoter (qExp2 . myLexer)
listExp2 = Language.LBNF.Compiletime.parseToQuoter (qListExp2 . myLexer)
listExp = Language.LBNF.Compiletime.parseToQuoter (qListExp . myLexer)
listString = Language.LBNF.Compiletime.parseToQuoter (qListString . myLexer)
minimumSize = Language.LBNF.Compiletime.parseToQuoter (qMinimumSize . myLexer)
reg2 = Language.LBNF.Compiletime.parseToQuoter (qReg2 . myLexer)
reg1 = Language.LBNF.Compiletime.parseToQuoter (qReg1 . myLexer)
reg3 = Language.LBNF.Compiletime.parseToQuoter (qReg3 . myLexer)
reg = Language.LBNF.Compiletime.parseToQuoter (qReg . myLexer)
listIdent = Language.LBNF.Compiletime.parseToQuoter (qListIdent . myLexer)
alex_base :: Array Int Int
alex_base = listArray (0,63) [-8,74,320,-55,-32,448,694,47,822,950,1078,1206,1334,1462,1575,0,1703,0,1816,0,1929,0,175,0,549,0,1994,2250,2186,0,0,2299,2555,2673,2737,0,2993,84,2929,0,0,2994,-37,76,92,3240,2561,3496,3432,0,3678,3924,0,141,-36,-35,0,-33,4141,0,0,129,3203,283]

alex_table :: Array Int Int
alex_table = listArray (0,4396) [0,53,53,53,53,53,56,60,44,50,6,63,63,63,63,63,63,63,63,63,63,0,0,0,53,3,45,0,56,0,0,33,56,56,56,56,56,54,56,0,61,61,61,61,61,61,61,61,61,61,57,56,0,56,0,56,56,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,56,-1,56,4,56,0,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,55,56,56,45,2,0,0,0,45,62,62,62,62,62,62,62,62,62,62,0,0,0,0,0,4,63,63,63,63,63,63,63,63,63,63,53,53,53,53,53,0,0,4,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,53,0,43,45,61,61,61,61,61,61,61,61,61,61,46,0,0,0,0,0,0,45,0,0,0,0,52,45,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,28,11,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,16,15,15,15,14,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,63,63,63,63,63,63,63,63,63,63,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,28,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,51,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,28,11,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,16,15,15,15,14,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,47,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,34,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,28,11,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,16,15,15,15,14,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,36,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,32,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,27,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,9,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,10,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,22,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,24,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,26,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,38,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,41,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,-1,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,0,58,58,58,58,58,58,58,58,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,32,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,34,10,23,23,23,23,23,23,23,23,23,23,23,23,23,23,24,13,17,17,17,18,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,48,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,62,62,62,62,62,62,62,62,62,0,0,0,0,0,0,0,0,0,0,0,0,0,59,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,37,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,36,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,38,9,25,25,25,25,25,25,25,25,25,25,25,25,25,25,26,12,19,19,19,20,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,47,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,48,5,40,40,40,40,40,40,40,40,40,40,40,40,40,40,41,8,30,30,30,31,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,28,11,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,16,15,15,15,14,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,58,0,0,0,0,0,0,0,0,58,58,58,58,58,58,58,58,58,58,0,0,0,0,0,0,0,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,0,0,0,0,58,0,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,46,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,4396) [-1,9,10,11,12,13,61,39,45,45,45,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,32,58,34,-1,36,-1,-1,39,40,41,42,43,44,45,46,-1,48,49,50,51,52,53,54,55,56,57,58,59,-1,61,-1,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,10,93,39,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,34,45,-1,-1,-1,39,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,92,48,49,50,51,52,53,54,55,56,57,9,10,11,12,13,-1,-1,110,-1,-1,-1,-1,-1,116,-1,-1,-1,-1,-1,-1,-1,-1,-1,32,-1,46,92,48,49,50,51,52,53,54,55,56,57,195,-1,-1,-1,-1,-1,-1,110,-1,-1,-1,-1,125,116,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,45,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,125,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,45,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,10,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,39,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,-1,184,185,186,187,188,189,190,191,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,92,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,34,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,101,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,92,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,45,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,195,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,63) [-1,6,6,-1,-1,-1,6,-1,-1,-1,-1,-1,-1,-1,21,21,-1,23,23,25,25,29,29,35,35,39,39,6,6,6,40,40,4,4,4,4,45,-1,45,45,49,49,-1,-1,-1,45,-1,50,50,50,50,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,63) [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_4))],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_6))],[(AlexAcc (alex_action_7))],[(AlexAcc (alex_action_8))],[(AlexAcc (alex_action_8))]]
alex_action_3 =  tok (\p s -> PT p (eitherResIdent (TV . share) s)) 
alex_action_4 =  tok (\p s -> PT p (eitherResIdent (TV . share) s)) 
alex_action_5 =  tok (\p s -> PT p (TL $ share $ unescapeInitTail s)) 
alex_action_6 =  tok (\p s -> PT p (TC $ share s))  
alex_action_7 =  tok (\p s -> PT p (TI $ share s))    
alex_action_8 =  tok (\p s -> PT p (TD $ share s)) 


tok f p s = f p s

share :: String -> String
share = id

data Tok =
   TS !String !Int    -- reserved words and symbols
 | TL !String         -- string literals
 | TI !String         -- integer literals
 | TV !String         -- identifiers
 | TD !String         -- double precision float literals
 | TC !String         -- character literals

 deriving (Eq,Show,Ord)

data Token = 
   PT  Posn Tok
 | Err Posn
  deriving (Eq,Show,Ord)

tokenPos (PT (Pn _ l _) _ :_) = "line " ++ show l
tokenPos (Err (Pn _ l _) :_) = "line " ++ show l
tokenPos _ = "end of file"

posLineCol (Pn _ l c) = (l,c)
mkPosToken t@(PT p _) = (posLineCol p, prToken t)

prToken t = case t of
  PT _ (TS s _) -> s
  PT _ (TI s) -> s
  PT _ (TV s) -> s
  PT _ (TD s) -> s
  PT _ (TC s) -> s

  _ -> show t

data BTree = N | B String Tok BTree BTree deriving (Show)

eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) | s < a  = treeFind left
                              | s > a  = treeFind right
                              | s == a = t

resWords = b "define" 22 (b ";" 11 (b "," 6 (b ")" 3 (b "(" 2 (b "$" 1 N N) N) (b "+" 5 (b "*" 4 N N) N)) (b ":" 9 (b "." 8 (b "-" 7 N N) N) (b "::=" 10 N N))) (b "_" 17 (b "@" 14 (b "?" 13 (b "=" 12 N N) N) (b "]" 16 (b "[" 15 N N) N)) (b "coercions" 20 (b "char" 19 (b "antiquote" 18 N N) N) (b "comment" 21 N N)))) (b "position" 33 (b "internal" 28 (b "entrypoints" 25 (b "digit" 24 (b "derive" 23 N N) N) (b "external" 27 (b "eps" 26 N N) N)) (b "lower" 31 (b "letter" 30 (b "layout" 29 N N) N) (b "nonempty" 32 N N))) (b "toplevel" 39 (b "stop" 36 (b "separator" 35 (b "rules" 34 N N) N) (b "token" 38 (b "terminator" 37 N N) N)) (b "|" 42 (b "{" 41 (b "upper" 40 N N) N) (b "}" 43 N N))))
   where b s n = let bs = s
                  in B bs (TS bs n)

unescapeInitTail :: String -> String
unescapeInitTail = unesc . tail where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '"':[]    -> []
    c:cs      -> c : unesc cs
    _         -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------


alexStartPos :: Posn
alexStartPos = Pn 0 1 1

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : (go inp')


alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, _, s) = c

alexIndexInt16OffAddr arr off = arr ! off
alexIndexInt32OffAddr arr off = arr ! off
quickIndex arr i = arr ! i
-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (sc)
  = alexScanUser undefined input (sc)

alexScanUser user input (sc)
  = case alex_scan_tkn user input (0) input sc AlexNone of
	(AlexNone, input') ->
		case alexGetByte input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input'

	(AlexLastSkip input'' len, _) ->



		AlexSkip input'' len

	(AlexLastAcc k input''' len, _) ->



		AlexToken input''' len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
	new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		(base) = alexIndexInt32OffAddr alex_base s
		((ord_c)) = fromIntegral c
		(offset) = (base + ord_c)
		(check)  = alexIndexInt16OffAddr alex_check offset
		
		(new_s) = if (offset >= (0)) && (check == ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
	case new_s of 
	    (-1) -> (new_acc, input)
		-- on an error, we want to keep the input *before* the
		-- character that failed, not after.
    	    _ -> alex_scan_tkn user orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
			new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (len)
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (len)
	check_accs (AlexAccPred a predx : rest)
	   | predx user orig_input (len) input
	   = AlexLastAcc a input (len)
	check_accs (AlexAccSkipPred predx : rest)
	   | predx user orig_input (len) input
	   = AlexLastSkip input (len)
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

instance Functor AlexLastAcc where
    fmap f AlexNone = AlexNone
    fmap f (AlexLastAcc x y z) = AlexLastAcc (f x) y z
    fmap f (AlexLastSkip x y) = AlexLastSkip x y

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

alexPrevCharMatches f _ input _ _ = f (alexInputPrevChar input)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user _ _ input = 
     case alex_scan_tkn user input (0) input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (i) = i

{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

-- parser produced by Happy 

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn59 (String)
	| HappyAbsSyn60 (BNFC_QQType)
	| HappyAbsSyn61 (Ident)
	| HappyAbsSyn63 (Integer)
	| HappyAbsSyn65 (Char)
	| HappyAbsSyn67 (Double)
	| HappyAbsSyn69 (Grammar)
	| HappyAbsSyn71 ([Def])
	| HappyAbsSyn73 ([Item])
	| HappyAbsSyn75 (Def)
	| HappyAbsSyn77 (RHS)
	| HappyAbsSyn79 ([RHS])
	| HappyAbsSyn81 (Item)
	| HappyAbsSyn83 (Cat)
	| HappyAbsSyn87 (Label)
	| HappyAbsSyn89 (MIdent)
	| HappyAbsSyn91 (HsTyp)
	| HappyAbsSyn95 ([HsTyp])
	| HappyAbsSyn97 (Arg)
	| HappyAbsSyn99 ([Arg])
	| HappyAbsSyn101 (Exp)
	| HappyAbsSyn107 ([Exp])
	| HappyAbsSyn111 ([String])
	| HappyAbsSyn113 (MinimumSize)
	| HappyAbsSyn115 (Reg)
	| HappyAbsSyn123 ([Ident])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277,
 action_278,
 action_279,
 action_280,
 action_281,
 action_282,
 action_283,
 action_284,
 action_285,
 action_286,
 action_287,
 action_288,
 action_289,
 action_290,
 action_291,
 action_292,
 action_293,
 action_294,
 action_295,
 action_296,
 action_297,
 action_298,
 action_299,
 action_300,
 action_301,
 action_302,
 action_303,
 action_304,
 action_305,
 action_306,
 action_307,
 action_308,
 action_309,
 action_310,
 action_311,
 action_312,
 action_313,
 action_314,
 action_315,
 action_316,
 action_317,
 action_318,
 action_319,
 action_320,
 action_321,
 action_322,
 action_323,
 action_324,
 action_325,
 action_326,
 action_327,
 action_328,
 action_329,
 action_330,
 action_331,
 action_332,
 action_333,
 action_334,
 action_335,
 action_336,
 action_337,
 action_338,
 action_339,
 action_340,
 action_341,
 action_342,
 action_343,
 action_344,
 action_345,
 action_346,
 action_347,
 action_348,
 action_349,
 action_350,
 action_351,
 action_352,
 action_353,
 action_354,
 action_355,
 action_356,
 action_357,
 action_358,
 action_359,
 action_360,
 action_361,
 action_362,
 action_363,
 action_364,
 action_365,
 action_366,
 action_367,
 action_368,
 action_369,
 action_370,
 action_371,
 action_372,
 action_373,
 action_374,
 action_375,
 action_376,
 action_377,
 action_378,
 action_379,
 action_380,
 action_381,
 action_382,
 action_383,
 action_384,
 action_385,
 action_386,
 action_387,
 action_388,
 action_389,
 action_390,
 action_391,
 action_392,
 action_393,
 action_394,
 action_395,
 action_396,
 action_397,
 action_398,
 action_399,
 action_400,
 action_401,
 action_402,
 action_403,
 action_404,
 action_405,
 action_406,
 action_407,
 action_408,
 action_409,
 action_410,
 action_411,
 action_412,
 action_413,
 action_414,
 action_415,
 action_416,
 action_417,
 action_418,
 action_419,
 action_420,
 action_421,
 action_422,
 action_423,
 action_424,
 action_425,
 action_426,
 action_427,
 action_428,
 action_429,
 action_430,
 action_431,
 action_432,
 action_433,
 action_434,
 action_435,
 action_436,
 action_437,
 action_438,
 action_439,
 action_440,
 action_441,
 action_442 :: () => Int -> ({-HappyReduction (ParseMonad) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (ParseMonad) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (ParseMonad) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (ParseMonad) HappyAbsSyn)

happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149,
 happyReduce_150,
 happyReduce_151,
 happyReduce_152,
 happyReduce_153,
 happyReduce_154,
 happyReduce_155,
 happyReduce_156,
 happyReduce_157,
 happyReduce_158,
 happyReduce_159,
 happyReduce_160,
 happyReduce_161,
 happyReduce_162,
 happyReduce_163,
 happyReduce_164,
 happyReduce_165,
 happyReduce_166,
 happyReduce_167,
 happyReduce_168,
 happyReduce_169,
 happyReduce_170,
 happyReduce_171,
 happyReduce_172,
 happyReduce_173,
 happyReduce_174,
 happyReduce_175,
 happyReduce_176,
 happyReduce_177,
 happyReduce_178,
 happyReduce_179,
 happyReduce_180,
 happyReduce_181,
 happyReduce_182,
 happyReduce_183,
 happyReduce_184,
 happyReduce_185,
 happyReduce_186,
 happyReduce_187,
 happyReduce_188,
 happyReduce_189,
 happyReduce_190,
 happyReduce_191,
 happyReduce_192,
 happyReduce_193,
 happyReduce_194,
 happyReduce_195,
 happyReduce_196,
 happyReduce_197,
 happyReduce_198,
 happyReduce_199,
 happyReduce_200,
 happyReduce_201,
 happyReduce_202,
 happyReduce_203,
 happyReduce_204,
 happyReduce_205,
 happyReduce_206,
 happyReduce_207,
 happyReduce_208,
 happyReduce_209,
 happyReduce_210,
 happyReduce_211,
 happyReduce_212,
 happyReduce_213,
 happyReduce_214,
 happyReduce_215,
 happyReduce_216,
 happyReduce_217,
 happyReduce_218,
 happyReduce_219,
 happyReduce_220,
 happyReduce_221,
 happyReduce_222,
 happyReduce_223,
 happyReduce_224,
 happyReduce_225,
 happyReduce_226,
 happyReduce_227,
 happyReduce_228,
 happyReduce_229,
 happyReduce_230,
 happyReduce_231,
 happyReduce_232,
 happyReduce_233,
 happyReduce_234,
 happyReduce_235,
 happyReduce_236,
 happyReduce_237,
 happyReduce_238,
 happyReduce_239,
 happyReduce_240,
 happyReduce_241,
 happyReduce_242,
 happyReduce_243,
 happyReduce_244,
 happyReduce_245,
 happyReduce_246,
 happyReduce_247,
 happyReduce_248,
 happyReduce_249 :: () => ({-HappyReduction (ParseMonad) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (ParseMonad) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (ParseMonad) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (ParseMonad) HappyAbsSyn)

action_0 (125) = happyShift action_179
action_0 (126) = happyShift action_180
action_0 (139) = happyShift action_181
action_0 (141) = happyShift action_182
action_0 (142) = happyShift action_229
action_0 (144) = happyShift action_230
action_0 (145) = happyShift action_231
action_0 (146) = happyShift action_232
action_0 (147) = happyShift action_233
action_0 (149) = happyShift action_234
action_0 (151) = happyShift action_235
action_0 (152) = happyShift action_236
action_0 (153) = happyShift action_237
action_0 (157) = happyShift action_238
action_0 (158) = happyShift action_239
action_0 (159) = happyShift action_240
action_0 (161) = happyShift action_241
action_0 (162) = happyShift action_242
action_0 (169) = happyShift action_63
action_0 (61) = happyGoto action_177
action_0 (69) = happyGoto action_251
action_0 (71) = happyGoto action_252
action_0 (75) = happyGoto action_248
action_0 (87) = happyGoto action_228
action_0 _ = happyReduce_68

action_1 (125) = happyShift action_173
action_1 (126) = happyShift action_174
action_1 (139) = happyShift action_175
action_1 (141) = happyShift action_176
action_1 (142) = happyShift action_213
action_1 (144) = happyShift action_214
action_1 (145) = happyShift action_215
action_1 (146) = happyShift action_216
action_1 (147) = happyShift action_217
action_1 (149) = happyShift action_218
action_1 (151) = happyShift action_219
action_1 (152) = happyShift action_220
action_1 (153) = happyShift action_221
action_1 (157) = happyShift action_222
action_1 (158) = happyShift action_223
action_1 (159) = happyShift action_224
action_1 (161) = happyShift action_225
action_1 (162) = happyShift action_226
action_1 (169) = happyShift action_60
action_1 (62) = happyGoto action_171
action_1 (70) = happyGoto action_249
action_1 (72) = happyGoto action_250
action_1 (76) = happyGoto action_246
action_1 (88) = happyGoto action_212
action_1 _ = happyReduce_71

action_2 (125) = happyShift action_179
action_2 (126) = happyShift action_180
action_2 (139) = happyShift action_181
action_2 (141) = happyShift action_182
action_2 (142) = happyShift action_229
action_2 (144) = happyShift action_230
action_2 (145) = happyShift action_231
action_2 (146) = happyShift action_232
action_2 (147) = happyShift action_233
action_2 (149) = happyShift action_234
action_2 (151) = happyShift action_235
action_2 (152) = happyShift action_236
action_2 (153) = happyShift action_237
action_2 (157) = happyShift action_238
action_2 (158) = happyShift action_239
action_2 (159) = happyShift action_240
action_2 (161) = happyShift action_241
action_2 (162) = happyShift action_242
action_2 (169) = happyShift action_63
action_2 (61) = happyGoto action_177
action_2 (71) = happyGoto action_247
action_2 (75) = happyGoto action_248
action_2 (87) = happyGoto action_228
action_2 _ = happyReduce_68

action_3 (125) = happyShift action_173
action_3 (126) = happyShift action_174
action_3 (139) = happyShift action_175
action_3 (141) = happyShift action_176
action_3 (142) = happyShift action_213
action_3 (144) = happyShift action_214
action_3 (145) = happyShift action_215
action_3 (146) = happyShift action_216
action_3 (147) = happyShift action_217
action_3 (149) = happyShift action_218
action_3 (151) = happyShift action_219
action_3 (152) = happyShift action_220
action_3 (153) = happyShift action_221
action_3 (157) = happyShift action_222
action_3 (158) = happyShift action_223
action_3 (159) = happyShift action_224
action_3 (161) = happyShift action_225
action_3 (162) = happyShift action_226
action_3 (169) = happyShift action_60
action_3 (62) = happyGoto action_171
action_3 (72) = happyGoto action_245
action_3 (76) = happyGoto action_246
action_3 (88) = happyGoto action_212
action_3 _ = happyReduce_71

action_4 (73) = happyGoto action_244
action_4 _ = happyReduce_74

action_5 (74) = happyGoto action_243
action_5 _ = happyReduce_76

action_6 (125) = happyShift action_179
action_6 (126) = happyShift action_180
action_6 (139) = happyShift action_181
action_6 (141) = happyShift action_182
action_6 (142) = happyShift action_229
action_6 (144) = happyShift action_230
action_6 (145) = happyShift action_231
action_6 (146) = happyShift action_232
action_6 (147) = happyShift action_233
action_6 (149) = happyShift action_234
action_6 (151) = happyShift action_235
action_6 (152) = happyShift action_236
action_6 (153) = happyShift action_237
action_6 (157) = happyShift action_238
action_6 (158) = happyShift action_239
action_6 (159) = happyShift action_240
action_6 (161) = happyShift action_241
action_6 (162) = happyShift action_242
action_6 (169) = happyShift action_63
action_6 (61) = happyGoto action_177
action_6 (75) = happyGoto action_227
action_6 (87) = happyGoto action_228
action_6 _ = happyFail

action_7 (125) = happyShift action_173
action_7 (126) = happyShift action_174
action_7 (139) = happyShift action_175
action_7 (141) = happyShift action_176
action_7 (142) = happyShift action_213
action_7 (144) = happyShift action_214
action_7 (145) = happyShift action_215
action_7 (146) = happyShift action_216
action_7 (147) = happyShift action_217
action_7 (149) = happyShift action_218
action_7 (151) = happyShift action_219
action_7 (152) = happyShift action_220
action_7 (153) = happyShift action_221
action_7 (157) = happyShift action_222
action_7 (158) = happyShift action_223
action_7 (159) = happyShift action_224
action_7 (161) = happyShift action_225
action_7 (162) = happyShift action_226
action_7 (169) = happyShift action_60
action_7 (62) = happyGoto action_171
action_7 (76) = happyGoto action_211
action_7 (88) = happyGoto action_212
action_7 _ = happyFail

action_8 (138) = happyShift action_208
action_8 (73) = happyGoto action_205
action_8 (77) = happyGoto action_210
action_8 _ = happyReduce_74

action_9 (138) = happyShift action_204
action_9 (74) = happyGoto action_201
action_9 (78) = happyGoto action_209
action_9 _ = happyReduce_76

action_10 (138) = happyShift action_208
action_10 (73) = happyGoto action_205
action_10 (77) = happyGoto action_206
action_10 (79) = happyGoto action_207
action_10 _ = happyReduce_74

action_11 (138) = happyShift action_204
action_11 (74) = happyGoto action_201
action_11 (78) = happyGoto action_202
action_11 (80) = happyGoto action_203
action_11 _ = happyReduce_76

action_12 (137) = happyShift action_194
action_12 (139) = happyShift action_188
action_12 (168) = happyShift action_57
action_12 (169) = happyShift action_63
action_12 (59) = happyGoto action_198
action_12 (61) = happyGoto action_186
action_12 (81) = happyGoto action_199
action_12 (83) = happyGoto action_200
action_12 (85) = happyGoto action_193
action_12 _ = happyFail

action_13 (137) = happyShift action_191
action_13 (139) = happyShift action_185
action_13 (168) = happyShift action_106
action_13 (169) = happyShift action_60
action_13 (60) = happyGoto action_195
action_13 (62) = happyGoto action_183
action_13 (82) = happyGoto action_196
action_13 (84) = happyGoto action_197
action_13 (86) = happyGoto action_190
action_13 _ = happyFail

action_14 (137) = happyShift action_194
action_14 (139) = happyShift action_188
action_14 (169) = happyShift action_63
action_14 (61) = happyGoto action_186
action_14 (83) = happyGoto action_192
action_14 (85) = happyGoto action_193
action_14 _ = happyFail

action_15 (137) = happyShift action_191
action_15 (139) = happyShift action_185
action_15 (169) = happyShift action_60
action_15 (62) = happyGoto action_183
action_15 (84) = happyGoto action_189
action_15 (86) = happyGoto action_190
action_15 _ = happyFail

action_16 (139) = happyShift action_188
action_16 (169) = happyShift action_63
action_16 (61) = happyGoto action_186
action_16 (85) = happyGoto action_187
action_16 _ = happyFail

action_17 (139) = happyShift action_185
action_17 (169) = happyShift action_60
action_17 (62) = happyGoto action_183
action_17 (86) = happyGoto action_184
action_17 _ = happyFail

action_18 (125) = happyShift action_179
action_18 (126) = happyShift action_180
action_18 (139) = happyShift action_181
action_18 (141) = happyShift action_182
action_18 (169) = happyShift action_63
action_18 (61) = happyGoto action_177
action_18 (87) = happyGoto action_178
action_18 _ = happyFail

action_19 (125) = happyShift action_173
action_19 (126) = happyShift action_174
action_19 (139) = happyShift action_175
action_19 (141) = happyShift action_176
action_19 (169) = happyShift action_60
action_19 (62) = happyGoto action_171
action_19 (88) = happyGoto action_172
action_19 _ = happyFail

action_20 (169) = happyShift action_63
action_20 (61) = happyGoto action_169
action_20 (89) = happyGoto action_170
action_20 _ = happyReduce_147

action_21 (169) = happyShift action_60
action_21 (62) = happyGoto action_167
action_21 (90) = happyGoto action_168
action_21 _ = happyReduce_149

action_22 (91) = happyGoto action_166
action_22 _ = happyFail

action_23 (92) = happyGoto action_165
action_23 _ = happyFail

action_24 (126) = happyShift action_163
action_24 (139) = happyShift action_164
action_24 (169) = happyShift action_63
action_24 (61) = happyGoto action_161
action_24 (93) = happyGoto action_162
action_24 _ = happyFail

action_25 (126) = happyShift action_159
action_25 (139) = happyShift action_160
action_25 (169) = happyShift action_60
action_25 (62) = happyGoto action_157
action_25 (94) = happyGoto action_158
action_25 _ = happyFail

action_26 (91) = happyGoto action_155
action_26 (95) = happyGoto action_156
action_26 _ = happyFail

action_27 (92) = happyGoto action_153
action_27 (96) = happyGoto action_154
action_27 _ = happyFail

action_28 (169) = happyShift action_63
action_28 (61) = happyGoto action_151
action_28 (97) = happyGoto action_152
action_28 _ = happyFail

action_29 (169) = happyShift action_60
action_29 (62) = happyGoto action_149
action_29 (98) = happyGoto action_150
action_29 _ = happyFail

action_30 (99) = happyGoto action_148
action_30 _ = happyReduce_164

action_31 (100) = happyGoto action_147
action_31 _ = happyReduce_166

action_32 (126) = happyShift action_131
action_32 (139) = happyShift action_132
action_32 (168) = happyShift action_57
action_32 (169) = happyShift action_63
action_32 (170) = happyShift action_133
action_32 (171) = happyShift action_93
action_32 (172) = happyShift action_134
action_32 (59) = happyGoto action_122
action_32 (61) = happyGoto action_123
action_32 (63) = happyGoto action_124
action_32 (65) = happyGoto action_125
action_32 (67) = happyGoto action_126
action_32 (101) = happyGoto action_146
action_32 (103) = happyGoto action_128
action_32 (105) = happyGoto action_129
action_32 _ = happyFail

action_33 (126) = happyShift action_118
action_33 (139) = happyShift action_119
action_33 (168) = happyShift action_106
action_33 (169) = happyShift action_60
action_33 (170) = happyShift action_120
action_33 (171) = happyShift action_78
action_33 (172) = happyShift action_121
action_33 (60) = happyGoto action_109
action_33 (62) = happyGoto action_110
action_33 (64) = happyGoto action_111
action_33 (66) = happyGoto action_112
action_33 (68) = happyGoto action_113
action_33 (102) = happyGoto action_145
action_33 (104) = happyGoto action_115
action_33 (106) = happyGoto action_116
action_33 _ = happyFail

action_34 (126) = happyShift action_131
action_34 (139) = happyShift action_132
action_34 (168) = happyShift action_57
action_34 (169) = happyShift action_63
action_34 (170) = happyShift action_133
action_34 (171) = happyShift action_93
action_34 (172) = happyShift action_134
action_34 (59) = happyGoto action_122
action_34 (61) = happyGoto action_123
action_34 (63) = happyGoto action_124
action_34 (65) = happyGoto action_125
action_34 (67) = happyGoto action_126
action_34 (103) = happyGoto action_144
action_34 (105) = happyGoto action_129
action_34 _ = happyFail

action_35 (126) = happyShift action_118
action_35 (139) = happyShift action_119
action_35 (168) = happyShift action_106
action_35 (169) = happyShift action_60
action_35 (170) = happyShift action_120
action_35 (171) = happyShift action_78
action_35 (172) = happyShift action_121
action_35 (60) = happyGoto action_109
action_35 (62) = happyGoto action_110
action_35 (64) = happyGoto action_111
action_35 (66) = happyGoto action_112
action_35 (68) = happyGoto action_113
action_35 (104) = happyGoto action_143
action_35 (106) = happyGoto action_116
action_35 _ = happyFail

action_36 (126) = happyShift action_131
action_36 (139) = happyShift action_132
action_36 (168) = happyShift action_57
action_36 (169) = happyShift action_63
action_36 (170) = happyShift action_133
action_36 (171) = happyShift action_93
action_36 (172) = happyShift action_134
action_36 (59) = happyGoto action_122
action_36 (61) = happyGoto action_138
action_36 (63) = happyGoto action_124
action_36 (65) = happyGoto action_125
action_36 (67) = happyGoto action_126
action_36 (105) = happyGoto action_142
action_36 _ = happyFail

action_37 (126) = happyShift action_118
action_37 (139) = happyShift action_119
action_37 (168) = happyShift action_106
action_37 (169) = happyShift action_60
action_37 (170) = happyShift action_120
action_37 (171) = happyShift action_78
action_37 (172) = happyShift action_121
action_37 (60) = happyGoto action_109
action_37 (62) = happyGoto action_135
action_37 (64) = happyGoto action_111
action_37 (66) = happyGoto action_112
action_37 (68) = happyGoto action_113
action_37 (106) = happyGoto action_141
action_37 _ = happyFail

action_38 (126) = happyShift action_131
action_38 (139) = happyShift action_132
action_38 (168) = happyShift action_57
action_38 (169) = happyShift action_63
action_38 (170) = happyShift action_133
action_38 (171) = happyShift action_93
action_38 (172) = happyShift action_134
action_38 (59) = happyGoto action_122
action_38 (61) = happyGoto action_138
action_38 (63) = happyGoto action_124
action_38 (65) = happyGoto action_125
action_38 (67) = happyGoto action_126
action_38 (105) = happyGoto action_139
action_38 (107) = happyGoto action_140
action_38 _ = happyFail

action_39 (126) = happyShift action_118
action_39 (139) = happyShift action_119
action_39 (168) = happyShift action_106
action_39 (169) = happyShift action_60
action_39 (170) = happyShift action_120
action_39 (171) = happyShift action_78
action_39 (172) = happyShift action_121
action_39 (60) = happyGoto action_109
action_39 (62) = happyGoto action_135
action_39 (64) = happyGoto action_111
action_39 (66) = happyGoto action_112
action_39 (68) = happyGoto action_113
action_39 (106) = happyGoto action_136
action_39 (108) = happyGoto action_137
action_39 _ = happyFail

action_40 (126) = happyShift action_131
action_40 (139) = happyShift action_132
action_40 (168) = happyShift action_57
action_40 (169) = happyShift action_63
action_40 (170) = happyShift action_133
action_40 (171) = happyShift action_93
action_40 (172) = happyShift action_134
action_40 (59) = happyGoto action_122
action_40 (61) = happyGoto action_123
action_40 (63) = happyGoto action_124
action_40 (65) = happyGoto action_125
action_40 (67) = happyGoto action_126
action_40 (101) = happyGoto action_127
action_40 (103) = happyGoto action_128
action_40 (105) = happyGoto action_129
action_40 (109) = happyGoto action_130
action_40 _ = happyReduce_194

action_41 (126) = happyShift action_118
action_41 (139) = happyShift action_119
action_41 (168) = happyShift action_106
action_41 (169) = happyShift action_60
action_41 (170) = happyShift action_120
action_41 (171) = happyShift action_78
action_41 (172) = happyShift action_121
action_41 (60) = happyGoto action_109
action_41 (62) = happyGoto action_110
action_41 (64) = happyGoto action_111
action_41 (66) = happyGoto action_112
action_41 (68) = happyGoto action_113
action_41 (102) = happyGoto action_114
action_41 (104) = happyGoto action_115
action_41 (106) = happyGoto action_116
action_41 (110) = happyGoto action_117
action_41 _ = happyReduce_197

action_42 (168) = happyShift action_57
action_42 (59) = happyGoto action_107
action_42 (111) = happyGoto action_108
action_42 _ = happyFail

action_43 (168) = happyShift action_106
action_43 (60) = happyGoto action_104
action_43 (112) = happyGoto action_105
action_43 _ = happyFail

action_44 (156) = happyShift action_103
action_44 (113) = happyGoto action_102
action_44 _ = happyReduce_205

action_45 (156) = happyShift action_101
action_45 (114) = happyGoto action_100
action_45 _ = happyReduce_207

action_46 (126) = happyShift action_84
action_46 (139) = happyShift action_85
action_46 (143) = happyShift action_86
action_46 (148) = happyShift action_87
action_46 (150) = happyShift action_88
action_46 (154) = happyShift action_89
action_46 (155) = happyShift action_90
action_46 (164) = happyShift action_91
action_46 (165) = happyShift action_92
action_46 (171) = happyShift action_93
action_46 (65) = happyGoto action_79
action_46 (115) = happyGoto action_99
action_46 (119) = happyGoto action_82
action_46 _ = happyFail

action_47 (126) = happyShift action_69
action_47 (139) = happyShift action_70
action_47 (143) = happyShift action_71
action_47 (148) = happyShift action_72
action_47 (150) = happyShift action_73
action_47 (154) = happyShift action_74
action_47 (155) = happyShift action_75
action_47 (164) = happyShift action_76
action_47 (165) = happyShift action_77
action_47 (171) = happyShift action_78
action_47 (66) = happyGoto action_64
action_47 (116) = happyGoto action_98
action_47 (120) = happyGoto action_67
action_47 _ = happyFail

action_48 (126) = happyShift action_84
action_48 (139) = happyShift action_85
action_48 (143) = happyShift action_86
action_48 (148) = happyShift action_87
action_48 (150) = happyShift action_88
action_48 (154) = happyShift action_89
action_48 (155) = happyShift action_90
action_48 (164) = happyShift action_91
action_48 (165) = happyShift action_92
action_48 (171) = happyShift action_93
action_48 (65) = happyGoto action_79
action_48 (115) = happyGoto action_80
action_48 (117) = happyGoto action_97
action_48 (119) = happyGoto action_82
action_48 _ = happyFail

action_49 (126) = happyShift action_69
action_49 (139) = happyShift action_70
action_49 (143) = happyShift action_71
action_49 (148) = happyShift action_72
action_49 (150) = happyShift action_73
action_49 (154) = happyShift action_74
action_49 (155) = happyShift action_75
action_49 (164) = happyShift action_76
action_49 (165) = happyShift action_77
action_49 (171) = happyShift action_78
action_49 (66) = happyGoto action_64
action_49 (116) = happyGoto action_65
action_49 (118) = happyGoto action_96
action_49 (120) = happyGoto action_67
action_49 _ = happyFail

action_50 (126) = happyShift action_84
action_50 (139) = happyShift action_85
action_50 (143) = happyShift action_86
action_50 (148) = happyShift action_87
action_50 (150) = happyShift action_88
action_50 (154) = happyShift action_89
action_50 (155) = happyShift action_90
action_50 (164) = happyShift action_91
action_50 (165) = happyShift action_92
action_50 (171) = happyShift action_93
action_50 (65) = happyGoto action_79
action_50 (119) = happyGoto action_95
action_50 _ = happyFail

action_51 (126) = happyShift action_69
action_51 (139) = happyShift action_70
action_51 (143) = happyShift action_71
action_51 (148) = happyShift action_72
action_51 (150) = happyShift action_73
action_51 (154) = happyShift action_74
action_51 (155) = happyShift action_75
action_51 (164) = happyShift action_76
action_51 (165) = happyShift action_77
action_51 (171) = happyShift action_78
action_51 (66) = happyGoto action_64
action_51 (120) = happyGoto action_94
action_51 _ = happyFail

action_52 (126) = happyShift action_84
action_52 (139) = happyShift action_85
action_52 (143) = happyShift action_86
action_52 (148) = happyShift action_87
action_52 (150) = happyShift action_88
action_52 (154) = happyShift action_89
action_52 (155) = happyShift action_90
action_52 (164) = happyShift action_91
action_52 (165) = happyShift action_92
action_52 (171) = happyShift action_93
action_52 (65) = happyGoto action_79
action_52 (115) = happyGoto action_80
action_52 (117) = happyGoto action_81
action_52 (119) = happyGoto action_82
action_52 (121) = happyGoto action_83
action_52 _ = happyFail

action_53 (126) = happyShift action_69
action_53 (139) = happyShift action_70
action_53 (143) = happyShift action_71
action_53 (148) = happyShift action_72
action_53 (150) = happyShift action_73
action_53 (154) = happyShift action_74
action_53 (155) = happyShift action_75
action_53 (164) = happyShift action_76
action_53 (165) = happyShift action_77
action_53 (171) = happyShift action_78
action_53 (66) = happyGoto action_64
action_53 (116) = happyGoto action_65
action_53 (118) = happyGoto action_66
action_53 (120) = happyGoto action_67
action_53 (122) = happyGoto action_68
action_53 _ = happyFail

action_54 (169) = happyShift action_63
action_54 (61) = happyGoto action_61
action_54 (123) = happyGoto action_62
action_54 _ = happyFail

action_55 (169) = happyShift action_60
action_55 (62) = happyGoto action_58
action_55 (124) = happyGoto action_59
action_55 _ = happyFail

action_56 (168) = happyShift action_57
action_56 _ = happyFail

action_57 _ = happyReduce_56

action_58 (130) = happyShift action_348
action_58 _ = happyReduce_248

action_59 (174) = happyAccept
action_59 _ = happyFail

action_60 _ = happyReduce_59

action_61 (130) = happyShift action_347
action_61 _ = happyReduce_246

action_62 (174) = happyAccept
action_62 _ = happyFail

action_63 _ = happyReduce_58

action_64 _ = happyReduce_235

action_65 (126) = happyShift action_69
action_65 (131) = happyShift action_346
action_65 (139) = happyShift action_70
action_65 (143) = happyShift action_71
action_65 (148) = happyShift action_72
action_65 (150) = happyShift action_73
action_65 (154) = happyShift action_74
action_65 (155) = happyShift action_75
action_65 (164) = happyShift action_76
action_65 (165) = happyShift action_77
action_65 (171) = happyShift action_78
action_65 (66) = happyGoto action_64
action_65 (120) = happyGoto action_330
action_65 _ = happyReduce_217

action_66 (166) = happyShift action_332
action_66 _ = happyReduce_245

action_67 (128) = happyShift action_336
action_67 (129) = happyShift action_337
action_67 (137) = happyShift action_338
action_67 _ = happyReduce_211

action_68 (174) = happyAccept
action_68 _ = happyFail

action_69 (126) = happyShift action_69
action_69 (139) = happyShift action_70
action_69 (143) = happyShift action_71
action_69 (148) = happyShift action_72
action_69 (150) = happyShift action_73
action_69 (154) = happyShift action_74
action_69 (155) = happyShift action_75
action_69 (164) = happyShift action_76
action_69 (165) = happyShift action_77
action_69 (171) = happyShift action_78
action_69 (66) = happyGoto action_64
action_69 (116) = happyGoto action_65
action_69 (118) = happyGoto action_66
action_69 (120) = happyGoto action_67
action_69 (122) = happyGoto action_345
action_69 _ = happyFail

action_70 (168) = happyShift action_106
action_70 (60) = happyGoto action_344
action_70 _ = happyFail

action_71 _ = happyReduce_242

action_72 _ = happyReduce_238

action_73 _ = happyReduce_234

action_74 _ = happyReduce_239

action_75 _ = happyReduce_241

action_76 _ = happyReduce_240

action_77 (168) = happyShift action_106
action_77 (60) = happyGoto action_343
action_77 _ = happyFail

action_78 _ = happyReduce_63

action_79 _ = happyReduce_222

action_80 (126) = happyShift action_84
action_80 (131) = happyShift action_342
action_80 (139) = happyShift action_85
action_80 (143) = happyShift action_86
action_80 (148) = happyShift action_87
action_80 (150) = happyShift action_88
action_80 (154) = happyShift action_89
action_80 (155) = happyShift action_90
action_80 (164) = happyShift action_91
action_80 (165) = happyShift action_92
action_80 (171) = happyShift action_93
action_80 (65) = happyGoto action_79
action_80 (119) = happyGoto action_329
action_80 _ = happyReduce_214

action_81 (166) = happyShift action_331
action_81 _ = happyReduce_244

action_82 (128) = happyShift action_333
action_82 (129) = happyShift action_334
action_82 (137) = happyShift action_335
action_82 _ = happyReduce_209

action_83 (174) = happyAccept
action_83 _ = happyFail

action_84 (126) = happyShift action_84
action_84 (139) = happyShift action_85
action_84 (143) = happyShift action_86
action_84 (148) = happyShift action_87
action_84 (150) = happyShift action_88
action_84 (154) = happyShift action_89
action_84 (155) = happyShift action_90
action_84 (164) = happyShift action_91
action_84 (165) = happyShift action_92
action_84 (171) = happyShift action_93
action_84 (65) = happyGoto action_79
action_84 (115) = happyGoto action_80
action_84 (117) = happyGoto action_81
action_84 (119) = happyGoto action_82
action_84 (121) = happyGoto action_341
action_84 _ = happyFail

action_85 (168) = happyShift action_57
action_85 (59) = happyGoto action_340
action_85 _ = happyFail

action_86 _ = happyReduce_229

action_87 _ = happyReduce_225

action_88 _ = happyReduce_221

action_89 _ = happyReduce_226

action_90 _ = happyReduce_228

action_91 _ = happyReduce_227

action_92 (168) = happyShift action_57
action_92 (59) = happyGoto action_339
action_92 _ = happyFail

action_93 _ = happyReduce_62

action_94 (128) = happyShift action_336
action_94 (129) = happyShift action_337
action_94 (137) = happyShift action_338
action_94 (174) = happyAccept
action_94 _ = happyFail

action_95 (128) = happyShift action_333
action_95 (129) = happyShift action_334
action_95 (137) = happyShift action_335
action_95 (174) = happyAccept
action_95 _ = happyFail

action_96 (166) = happyShift action_332
action_96 (174) = happyAccept
action_96 _ = happyFail

action_97 (166) = happyShift action_331
action_97 (174) = happyAccept
action_97 _ = happyFail

action_98 (126) = happyShift action_69
action_98 (139) = happyShift action_70
action_98 (143) = happyShift action_71
action_98 (148) = happyShift action_72
action_98 (150) = happyShift action_73
action_98 (154) = happyShift action_74
action_98 (155) = happyShift action_75
action_98 (164) = happyShift action_76
action_98 (165) = happyShift action_77
action_98 (171) = happyShift action_78
action_98 (174) = happyAccept
action_98 (66) = happyGoto action_64
action_98 (120) = happyGoto action_330
action_98 _ = happyFail

action_99 (126) = happyShift action_84
action_99 (139) = happyShift action_85
action_99 (143) = happyShift action_86
action_99 (148) = happyShift action_87
action_99 (150) = happyShift action_88
action_99 (154) = happyShift action_89
action_99 (155) = happyShift action_90
action_99 (164) = happyShift action_91
action_99 (165) = happyShift action_92
action_99 (171) = happyShift action_93
action_99 (174) = happyAccept
action_99 (65) = happyGoto action_79
action_99 (119) = happyGoto action_329
action_99 _ = happyFail

action_100 (174) = happyAccept
action_100 _ = happyFail

action_101 _ = happyReduce_206

action_102 (174) = happyAccept
action_102 _ = happyFail

action_103 _ = happyReduce_204

action_104 (130) = happyShift action_328
action_104 _ = happyReduce_202

action_105 (174) = happyAccept
action_105 _ = happyFail

action_106 _ = happyReduce_57

action_107 (130) = happyShift action_327
action_107 _ = happyReduce_200

action_108 (174) = happyAccept
action_108 _ = happyFail

action_109 _ = happyReduce_186

action_110 (126) = happyShift action_118
action_110 (139) = happyShift action_119
action_110 (168) = happyShift action_106
action_110 (169) = happyShift action_60
action_110 (170) = happyShift action_120
action_110 (171) = happyShift action_78
action_110 (172) = happyShift action_121
action_110 (60) = happyGoto action_109
action_110 (62) = happyGoto action_135
action_110 (64) = happyGoto action_111
action_110 (66) = happyGoto action_112
action_110 (68) = happyGoto action_113
action_110 (106) = happyGoto action_136
action_110 (108) = happyGoto action_326
action_110 _ = happyReduce_183

action_111 _ = happyReduce_184

action_112 _ = happyReduce_185

action_113 _ = happyReduce_187

action_114 (130) = happyShift action_325
action_114 _ = happyReduce_198

action_115 (133) = happyShift action_324
action_115 _ = happyReduce_171

action_116 _ = happyReduce_175

action_117 (174) = happyAccept
action_117 _ = happyFail

action_118 (126) = happyShift action_118
action_118 (139) = happyShift action_119
action_118 (168) = happyShift action_106
action_118 (169) = happyShift action_60
action_118 (170) = happyShift action_120
action_118 (171) = happyShift action_78
action_118 (172) = happyShift action_121
action_118 (60) = happyGoto action_109
action_118 (62) = happyGoto action_110
action_118 (64) = happyGoto action_111
action_118 (66) = happyGoto action_112
action_118 (68) = happyGoto action_113
action_118 (102) = happyGoto action_323
action_118 (104) = happyGoto action_115
action_118 (106) = happyGoto action_116
action_118 _ = happyFail

action_119 (126) = happyShift action_118
action_119 (139) = happyShift action_119
action_119 (168) = happyShift action_106
action_119 (169) = happyShift action_60
action_119 (170) = happyShift action_120
action_119 (171) = happyShift action_78
action_119 (172) = happyShift action_121
action_119 (60) = happyGoto action_109
action_119 (62) = happyGoto action_110
action_119 (64) = happyGoto action_111
action_119 (66) = happyGoto action_112
action_119 (68) = happyGoto action_113
action_119 (102) = happyGoto action_114
action_119 (104) = happyGoto action_115
action_119 (106) = happyGoto action_116
action_119 (110) = happyGoto action_322
action_119 _ = happyReduce_197

action_120 _ = happyReduce_61

action_121 _ = happyReduce_65

action_122 _ = happyReduce_179

action_123 (126) = happyShift action_131
action_123 (139) = happyShift action_132
action_123 (168) = happyShift action_57
action_123 (169) = happyShift action_63
action_123 (170) = happyShift action_133
action_123 (171) = happyShift action_93
action_123 (172) = happyShift action_134
action_123 (59) = happyGoto action_122
action_123 (61) = happyGoto action_138
action_123 (63) = happyGoto action_124
action_123 (65) = happyGoto action_125
action_123 (67) = happyGoto action_126
action_123 (105) = happyGoto action_139
action_123 (107) = happyGoto action_321
action_123 _ = happyReduce_176

action_124 _ = happyReduce_177

action_125 _ = happyReduce_178

action_126 _ = happyReduce_180

action_127 (130) = happyShift action_320
action_127 _ = happyReduce_195

action_128 (133) = happyShift action_319
action_128 _ = happyReduce_169

action_129 _ = happyReduce_173

action_130 (174) = happyAccept
action_130 _ = happyFail

action_131 (126) = happyShift action_131
action_131 (139) = happyShift action_132
action_131 (168) = happyShift action_57
action_131 (169) = happyShift action_63
action_131 (170) = happyShift action_133
action_131 (171) = happyShift action_93
action_131 (172) = happyShift action_134
action_131 (59) = happyGoto action_122
action_131 (61) = happyGoto action_123
action_131 (63) = happyGoto action_124
action_131 (65) = happyGoto action_125
action_131 (67) = happyGoto action_126
action_131 (101) = happyGoto action_318
action_131 (103) = happyGoto action_128
action_131 (105) = happyGoto action_129
action_131 _ = happyFail

action_132 (126) = happyShift action_131
action_132 (139) = happyShift action_132
action_132 (168) = happyShift action_57
action_132 (169) = happyShift action_63
action_132 (170) = happyShift action_133
action_132 (171) = happyShift action_93
action_132 (172) = happyShift action_134
action_132 (59) = happyGoto action_122
action_132 (61) = happyGoto action_123
action_132 (63) = happyGoto action_124
action_132 (65) = happyGoto action_125
action_132 (67) = happyGoto action_126
action_132 (101) = happyGoto action_127
action_132 (103) = happyGoto action_128
action_132 (105) = happyGoto action_129
action_132 (109) = happyGoto action_317
action_132 _ = happyReduce_194

action_133 _ = happyReduce_60

action_134 _ = happyReduce_64

action_135 _ = happyReduce_183

action_136 (126) = happyShift action_118
action_136 (139) = happyShift action_119
action_136 (168) = happyShift action_106
action_136 (169) = happyShift action_60
action_136 (170) = happyShift action_120
action_136 (171) = happyShift action_78
action_136 (172) = happyShift action_121
action_136 (60) = happyGoto action_109
action_136 (62) = happyGoto action_135
action_136 (64) = happyGoto action_111
action_136 (66) = happyGoto action_112
action_136 (68) = happyGoto action_113
action_136 (106) = happyGoto action_136
action_136 (108) = happyGoto action_316
action_136 _ = happyReduce_192

action_137 (174) = happyAccept
action_137 _ = happyFail

action_138 _ = happyReduce_176

action_139 (126) = happyShift action_131
action_139 (139) = happyShift action_132
action_139 (168) = happyShift action_57
action_139 (169) = happyShift action_63
action_139 (170) = happyShift action_133
action_139 (171) = happyShift action_93
action_139 (172) = happyShift action_134
action_139 (59) = happyGoto action_122
action_139 (61) = happyGoto action_138
action_139 (63) = happyGoto action_124
action_139 (65) = happyGoto action_125
action_139 (67) = happyGoto action_126
action_139 (105) = happyGoto action_139
action_139 (107) = happyGoto action_315
action_139 _ = happyReduce_190

action_140 (174) = happyAccept
action_140 _ = happyFail

action_141 (174) = happyAccept
action_141 _ = happyFail

action_142 (174) = happyAccept
action_142 _ = happyFail

action_143 (174) = happyAccept
action_143 _ = happyFail

action_144 (174) = happyAccept
action_144 _ = happyFail

action_145 (174) = happyAccept
action_145 _ = happyFail

action_146 (174) = happyAccept
action_146 _ = happyFail

action_147 (169) = happyShift action_60
action_147 (174) = happyAccept
action_147 (62) = happyGoto action_149
action_147 (98) = happyGoto action_314
action_147 _ = happyFail

action_148 (169) = happyShift action_63
action_148 (174) = happyAccept
action_148 (61) = happyGoto action_151
action_148 (97) = happyGoto action_313
action_148 _ = happyFail

action_149 _ = happyReduce_163

action_150 (174) = happyAccept
action_150 _ = happyFail

action_151 _ = happyReduce_162

action_152 (174) = happyAccept
action_152 _ = happyFail

action_153 (126) = happyShift action_159
action_153 (130) = happyShift action_312
action_153 (139) = happyShift action_160
action_153 (169) = happyShift action_60
action_153 (62) = happyGoto action_157
action_153 (94) = happyGoto action_306
action_153 _ = happyReduce_160

action_154 (174) = happyAccept
action_154 _ = happyFail

action_155 (126) = happyShift action_163
action_155 (130) = happyShift action_311
action_155 (139) = happyShift action_164
action_155 (169) = happyShift action_63
action_155 (61) = happyGoto action_161
action_155 (93) = happyGoto action_305
action_155 _ = happyReduce_158

action_156 (174) = happyAccept
action_156 _ = happyFail

action_157 _ = happyReduce_155

action_158 (174) = happyAccept
action_158 _ = happyFail

action_159 (92) = happyGoto action_153
action_159 (96) = happyGoto action_310
action_159 _ = happyFail

action_160 (92) = happyGoto action_309
action_160 _ = happyFail

action_161 _ = happyReduce_152

action_162 (174) = happyAccept
action_162 _ = happyFail

action_163 (91) = happyGoto action_155
action_163 (95) = happyGoto action_308
action_163 _ = happyFail

action_164 (91) = happyGoto action_307
action_164 _ = happyFail

action_165 (126) = happyShift action_159
action_165 (139) = happyShift action_160
action_165 (169) = happyShift action_60
action_165 (174) = happyAccept
action_165 (62) = happyGoto action_157
action_165 (94) = happyGoto action_306
action_165 _ = happyFail

action_166 (126) = happyShift action_163
action_166 (139) = happyShift action_164
action_166 (169) = happyShift action_63
action_166 (174) = happyAccept
action_166 (61) = happyGoto action_161
action_166 (93) = happyGoto action_305
action_166 _ = happyFail

action_167 _ = happyReduce_148

action_168 (174) = happyAccept
action_168 _ = happyFail

action_169 _ = happyReduce_146

action_170 (174) = happyAccept
action_170 _ = happyFail

action_171 _ = happyReduce_140

action_172 (174) = happyAccept
action_172 _ = happyFail

action_173 (169) = happyShift action_60
action_173 (62) = happyGoto action_167
action_173 (90) = happyGoto action_304
action_173 _ = happyReduce_149

action_174 (133) = happyShift action_303
action_174 _ = happyFail

action_175 (140) = happyShift action_302
action_175 _ = happyFail

action_176 _ = happyReduce_141

action_177 _ = happyReduce_134

action_178 (174) = happyAccept
action_178 _ = happyFail

action_179 (169) = happyShift action_63
action_179 (61) = happyGoto action_169
action_179 (89) = happyGoto action_301
action_179 _ = happyReduce_147

action_180 (133) = happyShift action_300
action_180 _ = happyFail

action_181 (140) = happyShift action_299
action_181 _ = happyFail

action_182 _ = happyReduce_135

action_183 _ = happyReduce_133

action_184 (174) = happyAccept
action_184 _ = happyFail

action_185 (137) = happyShift action_191
action_185 (139) = happyShift action_185
action_185 (169) = happyShift action_60
action_185 (62) = happyGoto action_183
action_185 (84) = happyGoto action_298
action_185 (86) = happyGoto action_190
action_185 _ = happyFail

action_186 _ = happyReduce_131

action_187 (174) = happyAccept
action_187 _ = happyFail

action_188 (137) = happyShift action_194
action_188 (139) = happyShift action_188
action_188 (169) = happyShift action_63
action_188 (61) = happyGoto action_186
action_188 (83) = happyGoto action_297
action_188 (85) = happyGoto action_193
action_188 _ = happyFail

action_189 (174) = happyAccept
action_189 _ = happyFail

action_190 _ = happyReduce_129

action_191 (139) = happyShift action_185
action_191 (169) = happyShift action_60
action_191 (62) = happyGoto action_183
action_191 (86) = happyGoto action_296
action_191 _ = happyFail

action_192 (174) = happyAccept
action_192 _ = happyFail

action_193 _ = happyReduce_127

action_194 (139) = happyShift action_188
action_194 (169) = happyShift action_63
action_194 (61) = happyGoto action_186
action_194 (85) = happyGoto action_295
action_194 _ = happyFail

action_195 _ = happyReduce_124

action_196 (174) = happyAccept
action_196 _ = happyFail

action_197 _ = happyReduce_125

action_198 _ = happyReduce_122

action_199 (174) = happyAccept
action_199 _ = happyFail

action_200 _ = happyReduce_123

action_201 (137) = happyShift action_191
action_201 (139) = happyShift action_185
action_201 (168) = happyShift action_106
action_201 (169) = happyShift action_60
action_201 (60) = happyGoto action_195
action_201 (62) = happyGoto action_183
action_201 (82) = happyGoto action_256
action_201 (84) = happyGoto action_197
action_201 (86) = happyGoto action_190
action_201 _ = happyReduce_116

action_202 (166) = happyShift action_294
action_202 _ = happyReduce_120

action_203 (174) = happyAccept
action_203 _ = happyFail

action_204 (126) = happyShift action_69
action_204 (139) = happyShift action_70
action_204 (143) = happyShift action_71
action_204 (148) = happyShift action_72
action_204 (150) = happyShift action_73
action_204 (154) = happyShift action_74
action_204 (155) = happyShift action_75
action_204 (164) = happyShift action_76
action_204 (165) = happyShift action_77
action_204 (171) = happyShift action_78
action_204 (66) = happyGoto action_64
action_204 (116) = happyGoto action_65
action_204 (118) = happyGoto action_66
action_204 (120) = happyGoto action_67
action_204 (122) = happyGoto action_293
action_204 _ = happyFail

action_205 (137) = happyShift action_194
action_205 (139) = happyShift action_188
action_205 (168) = happyShift action_57
action_205 (169) = happyShift action_63
action_205 (59) = happyGoto action_198
action_205 (61) = happyGoto action_186
action_205 (81) = happyGoto action_255
action_205 (83) = happyGoto action_200
action_205 (85) = happyGoto action_193
action_205 _ = happyReduce_114

action_206 (166) = happyShift action_292
action_206 _ = happyReduce_118

action_207 (174) = happyAccept
action_207 _ = happyFail

action_208 (126) = happyShift action_84
action_208 (139) = happyShift action_85
action_208 (143) = happyShift action_86
action_208 (148) = happyShift action_87
action_208 (150) = happyShift action_88
action_208 (154) = happyShift action_89
action_208 (155) = happyShift action_90
action_208 (164) = happyShift action_91
action_208 (165) = happyShift action_92
action_208 (171) = happyShift action_93
action_208 (65) = happyGoto action_79
action_208 (115) = happyGoto action_80
action_208 (117) = happyGoto action_81
action_208 (119) = happyGoto action_82
action_208 (121) = happyGoto action_291
action_208 _ = happyFail

action_209 (174) = happyAccept
action_209 _ = happyFail

action_210 (174) = happyAccept
action_210 _ = happyFail

action_211 (174) = happyAccept
action_211 _ = happyFail

action_212 (132) = happyShift action_290
action_212 _ = happyFail

action_213 (168) = happyShift action_106
action_213 (60) = happyGoto action_289
action_213 _ = happyFail

action_214 (169) = happyShift action_60
action_214 (62) = happyGoto action_288
action_214 _ = happyFail

action_215 (168) = happyShift action_106
action_215 (60) = happyGoto action_287
action_215 _ = happyFail

action_216 (169) = happyShift action_60
action_216 (62) = happyGoto action_286
action_216 _ = happyFail

action_217 (169) = happyShift action_60
action_217 (62) = happyGoto action_58
action_217 (124) = happyGoto action_285
action_217 _ = happyFail

action_218 (169) = happyShift action_60
action_218 (62) = happyGoto action_58
action_218 (124) = happyGoto action_284
action_218 _ = happyFail

action_219 (169) = happyShift action_60
action_219 (62) = happyGoto action_283
action_219 _ = happyFail

action_220 (125) = happyShift action_173
action_220 (126) = happyShift action_174
action_220 (139) = happyShift action_175
action_220 (141) = happyShift action_176
action_220 (169) = happyShift action_60
action_220 (62) = happyGoto action_171
action_220 (88) = happyGoto action_282
action_220 _ = happyFail

action_221 (160) = happyShift action_280
action_221 (163) = happyShift action_281
action_221 (168) = happyShift action_106
action_221 (60) = happyGoto action_104
action_221 (112) = happyGoto action_279
action_221 _ = happyFail

action_222 (162) = happyShift action_278
action_222 _ = happyFail

action_223 (169) = happyShift action_60
action_223 (62) = happyGoto action_277
action_223 _ = happyFail

action_224 (156) = happyShift action_101
action_224 (114) = happyGoto action_276
action_224 _ = happyReduce_207

action_225 (156) = happyShift action_101
action_225 (114) = happyGoto action_275
action_225 _ = happyReduce_207

action_226 (169) = happyShift action_60
action_226 (62) = happyGoto action_274
action_226 _ = happyFail

action_227 (174) = happyAccept
action_227 _ = happyFail

action_228 (132) = happyShift action_273
action_228 _ = happyFail

action_229 (168) = happyShift action_57
action_229 (59) = happyGoto action_272
action_229 _ = happyFail

action_230 (169) = happyShift action_63
action_230 (61) = happyGoto action_271
action_230 _ = happyFail

action_231 (168) = happyShift action_57
action_231 (59) = happyGoto action_270
action_231 _ = happyFail

action_232 (169) = happyShift action_63
action_232 (61) = happyGoto action_269
action_232 _ = happyFail

action_233 (169) = happyShift action_63
action_233 (61) = happyGoto action_61
action_233 (123) = happyGoto action_268
action_233 _ = happyFail

action_234 (169) = happyShift action_63
action_234 (61) = happyGoto action_61
action_234 (123) = happyGoto action_267
action_234 _ = happyFail

action_235 (169) = happyShift action_63
action_235 (61) = happyGoto action_266
action_235 _ = happyFail

action_236 (125) = happyShift action_179
action_236 (126) = happyShift action_180
action_236 (139) = happyShift action_181
action_236 (141) = happyShift action_182
action_236 (169) = happyShift action_63
action_236 (61) = happyGoto action_177
action_236 (87) = happyGoto action_265
action_236 _ = happyFail

action_237 (160) = happyShift action_263
action_237 (163) = happyShift action_264
action_237 (168) = happyShift action_57
action_237 (59) = happyGoto action_107
action_237 (111) = happyGoto action_262
action_237 _ = happyFail

action_238 (162) = happyShift action_261
action_238 _ = happyFail

action_239 (169) = happyShift action_63
action_239 (61) = happyGoto action_260
action_239 _ = happyFail

action_240 (156) = happyShift action_103
action_240 (113) = happyGoto action_259
action_240 _ = happyReduce_205

action_241 (156) = happyShift action_103
action_241 (113) = happyGoto action_258
action_241 _ = happyReduce_205

action_242 (169) = happyShift action_63
action_242 (61) = happyGoto action_257
action_242 _ = happyFail

action_243 (137) = happyShift action_191
action_243 (139) = happyShift action_185
action_243 (168) = happyShift action_106
action_243 (169) = happyShift action_60
action_243 (174) = happyAccept
action_243 (60) = happyGoto action_195
action_243 (62) = happyGoto action_183
action_243 (82) = happyGoto action_256
action_243 (84) = happyGoto action_197
action_243 (86) = happyGoto action_190
action_243 _ = happyFail

action_244 (137) = happyShift action_194
action_244 (139) = happyShift action_188
action_244 (168) = happyShift action_57
action_244 (169) = happyShift action_63
action_244 (174) = happyAccept
action_244 (59) = happyGoto action_198
action_244 (61) = happyGoto action_186
action_244 (81) = happyGoto action_255
action_244 (83) = happyGoto action_200
action_244 (85) = happyGoto action_193
action_244 _ = happyFail

action_245 (174) = happyAccept
action_245 _ = happyFail

action_246 (135) = happyShift action_254
action_246 _ = happyReduce_72

action_247 (174) = happyAccept
action_247 _ = happyFail

action_248 (135) = happyShift action_253
action_248 _ = happyReduce_69

action_249 (174) = happyAccept
action_249 _ = happyFail

action_250 _ = happyReduce_67

action_251 (174) = happyAccept
action_251 _ = happyFail

action_252 _ = happyReduce_66

action_253 (125) = happyShift action_179
action_253 (126) = happyShift action_180
action_253 (139) = happyShift action_181
action_253 (141) = happyShift action_182
action_253 (142) = happyShift action_229
action_253 (144) = happyShift action_230
action_253 (145) = happyShift action_231
action_253 (146) = happyShift action_232
action_253 (147) = happyShift action_233
action_253 (149) = happyShift action_234
action_253 (151) = happyShift action_235
action_253 (152) = happyShift action_236
action_253 (153) = happyShift action_237
action_253 (157) = happyShift action_238
action_253 (158) = happyShift action_239
action_253 (159) = happyShift action_240
action_253 (161) = happyShift action_241
action_253 (162) = happyShift action_242
action_253 (169) = happyShift action_63
action_253 (61) = happyGoto action_177
action_253 (71) = happyGoto action_412
action_253 (75) = happyGoto action_248
action_253 (87) = happyGoto action_228
action_253 _ = happyReduce_68

action_254 (125) = happyShift action_173
action_254 (126) = happyShift action_174
action_254 (139) = happyShift action_175
action_254 (141) = happyShift action_176
action_254 (142) = happyShift action_213
action_254 (144) = happyShift action_214
action_254 (145) = happyShift action_215
action_254 (146) = happyShift action_216
action_254 (147) = happyShift action_217
action_254 (149) = happyShift action_218
action_254 (151) = happyShift action_219
action_254 (152) = happyShift action_220
action_254 (153) = happyShift action_221
action_254 (157) = happyShift action_222
action_254 (158) = happyShift action_223
action_254 (159) = happyShift action_224
action_254 (161) = happyShift action_225
action_254 (162) = happyShift action_226
action_254 (169) = happyShift action_60
action_254 (62) = happyGoto action_171
action_254 (72) = happyGoto action_411
action_254 (76) = happyGoto action_246
action_254 (88) = happyGoto action_212
action_254 _ = happyReduce_71

action_255 _ = happyReduce_75

action_256 _ = happyReduce_77

action_257 (126) = happyShift action_84
action_257 (139) = happyShift action_85
action_257 (143) = happyShift action_86
action_257 (148) = happyShift action_87
action_257 (150) = happyShift action_88
action_257 (154) = happyShift action_89
action_257 (155) = happyShift action_90
action_257 (164) = happyShift action_91
action_257 (165) = happyShift action_92
action_257 (171) = happyShift action_93
action_257 (65) = happyGoto action_79
action_257 (115) = happyGoto action_80
action_257 (117) = happyGoto action_81
action_257 (119) = happyGoto action_82
action_257 (121) = happyGoto action_410
action_257 _ = happyFail

action_258 (137) = happyShift action_194
action_258 (139) = happyShift action_188
action_258 (169) = happyShift action_63
action_258 (61) = happyGoto action_186
action_258 (83) = happyGoto action_409
action_258 (85) = happyGoto action_193
action_258 _ = happyFail

action_259 (137) = happyShift action_194
action_259 (139) = happyShift action_188
action_259 (169) = happyShift action_63
action_259 (61) = happyGoto action_186
action_259 (83) = happyGoto action_408
action_259 (85) = happyGoto action_193
action_259 _ = happyFail

action_260 (134) = happyShift action_407
action_260 _ = happyFail

action_261 (169) = happyShift action_63
action_261 (61) = happyGoto action_406
action_261 _ = happyFail

action_262 _ = happyReduce_93

action_263 (168) = happyShift action_57
action_263 (59) = happyGoto action_107
action_263 (111) = happyGoto action_405
action_263 _ = happyFail

action_264 _ = happyReduce_95

action_265 (132) = happyShift action_404
action_265 _ = happyFail

action_266 (136) = happyShift action_403
action_266 _ = happyFail

action_267 _ = happyReduce_84

action_268 _ = happyReduce_92

action_269 (99) = happyGoto action_402
action_269 _ = happyReduce_164

action_270 (168) = happyShift action_57
action_270 (59) = happyGoto action_401
action_270 _ = happyReduce_79

action_271 (170) = happyShift action_133
action_271 (63) = happyGoto action_400
action_271 _ = happyFail

action_272 (168) = happyShift action_57
action_272 (59) = happyGoto action_399
action_272 _ = happyFail

action_273 (137) = happyShift action_194
action_273 (139) = happyShift action_188
action_273 (169) = happyShift action_63
action_273 (61) = happyGoto action_186
action_273 (83) = happyGoto action_398
action_273 (85) = happyGoto action_193
action_273 _ = happyFail

action_274 (126) = happyShift action_69
action_274 (139) = happyShift action_70
action_274 (143) = happyShift action_71
action_274 (148) = happyShift action_72
action_274 (150) = happyShift action_73
action_274 (154) = happyShift action_74
action_274 (155) = happyShift action_75
action_274 (164) = happyShift action_76
action_274 (165) = happyShift action_77
action_274 (171) = happyShift action_78
action_274 (66) = happyGoto action_64
action_274 (116) = happyGoto action_65
action_274 (118) = happyGoto action_66
action_274 (120) = happyGoto action_67
action_274 (122) = happyGoto action_397
action_274 _ = happyFail

action_275 (137) = happyShift action_191
action_275 (139) = happyShift action_185
action_275 (169) = happyShift action_60
action_275 (62) = happyGoto action_183
action_275 (84) = happyGoto action_396
action_275 (86) = happyGoto action_190
action_275 _ = happyFail

action_276 (137) = happyShift action_191
action_276 (139) = happyShift action_185
action_276 (169) = happyShift action_60
action_276 (62) = happyGoto action_183
action_276 (84) = happyGoto action_395
action_276 (86) = happyGoto action_190
action_276 _ = happyFail

action_277 (134) = happyShift action_394
action_277 _ = happyFail

action_278 (169) = happyShift action_60
action_278 (62) = happyGoto action_393
action_278 _ = happyFail

action_279 _ = happyReduce_111

action_280 (168) = happyShift action_106
action_280 (60) = happyGoto action_104
action_280 (112) = happyGoto action_392
action_280 _ = happyFail

action_281 _ = happyReduce_113

action_282 (132) = happyShift action_391
action_282 _ = happyFail

action_283 (136) = happyShift action_390
action_283 _ = happyFail

action_284 _ = happyReduce_102

action_285 _ = happyReduce_110

action_286 (100) = happyGoto action_389
action_286 _ = happyReduce_166

action_287 (168) = happyShift action_106
action_287 (60) = happyGoto action_388
action_287 _ = happyReduce_97

action_288 (170) = happyShift action_120
action_288 (64) = happyGoto action_387
action_288 _ = happyFail

action_289 (168) = happyShift action_106
action_289 (60) = happyGoto action_386
action_289 _ = happyFail

action_290 (137) = happyShift action_191
action_290 (139) = happyShift action_185
action_290 (169) = happyShift action_60
action_290 (62) = happyGoto action_183
action_290 (84) = happyGoto action_385
action_290 (86) = happyGoto action_190
action_290 _ = happyFail

action_291 _ = happyReduce_115

action_292 (138) = happyShift action_208
action_292 (73) = happyGoto action_205
action_292 (77) = happyGoto action_206
action_292 (79) = happyGoto action_384
action_292 _ = happyReduce_74

action_293 _ = happyReduce_117

action_294 (138) = happyShift action_204
action_294 (74) = happyGoto action_201
action_294 (78) = happyGoto action_202
action_294 (80) = happyGoto action_383
action_294 _ = happyReduce_76

action_295 _ = happyReduce_126

action_296 _ = happyReduce_128

action_297 (140) = happyShift action_382
action_297 _ = happyFail

action_298 (140) = happyShift action_381
action_298 _ = happyFail

action_299 _ = happyReduce_136

action_300 (127) = happyShift action_379
action_300 (139) = happyShift action_380
action_300 _ = happyFail

action_301 _ = happyReduce_139

action_302 _ = happyReduce_142

action_303 (127) = happyShift action_377
action_303 (139) = happyShift action_378
action_303 _ = happyFail

action_304 _ = happyReduce_145

action_305 _ = happyReduce_150

action_306 _ = happyReduce_151

action_307 (126) = happyShift action_163
action_307 (139) = happyShift action_164
action_307 (140) = happyShift action_376
action_307 (169) = happyShift action_63
action_307 (61) = happyGoto action_161
action_307 (93) = happyGoto action_305
action_307 _ = happyFail

action_308 (127) = happyShift action_375
action_308 _ = happyFail

action_309 (126) = happyShift action_159
action_309 (139) = happyShift action_160
action_309 (140) = happyShift action_374
action_309 (169) = happyShift action_60
action_309 (62) = happyGoto action_157
action_309 (94) = happyGoto action_306
action_309 _ = happyFail

action_310 (127) = happyShift action_373
action_310 _ = happyFail

action_311 (91) = happyGoto action_155
action_311 (95) = happyGoto action_372
action_311 _ = happyFail

action_312 (92) = happyGoto action_153
action_312 (96) = happyGoto action_371
action_312 _ = happyFail

action_313 _ = happyReduce_165

action_314 _ = happyReduce_167

action_315 _ = happyReduce_191

action_316 _ = happyReduce_193

action_317 (140) = happyShift action_370
action_317 _ = happyFail

action_318 (127) = happyShift action_369
action_318 _ = happyFail

action_319 (126) = happyShift action_131
action_319 (139) = happyShift action_132
action_319 (168) = happyShift action_57
action_319 (169) = happyShift action_63
action_319 (170) = happyShift action_133
action_319 (171) = happyShift action_93
action_319 (172) = happyShift action_134
action_319 (59) = happyGoto action_122
action_319 (61) = happyGoto action_123
action_319 (63) = happyGoto action_124
action_319 (65) = happyGoto action_125
action_319 (67) = happyGoto action_126
action_319 (101) = happyGoto action_368
action_319 (103) = happyGoto action_128
action_319 (105) = happyGoto action_129
action_319 _ = happyFail

action_320 (126) = happyShift action_131
action_320 (139) = happyShift action_132
action_320 (168) = happyShift action_57
action_320 (169) = happyShift action_63
action_320 (170) = happyShift action_133
action_320 (171) = happyShift action_93
action_320 (172) = happyShift action_134
action_320 (59) = happyGoto action_122
action_320 (61) = happyGoto action_123
action_320 (63) = happyGoto action_124
action_320 (65) = happyGoto action_125
action_320 (67) = happyGoto action_126
action_320 (101) = happyGoto action_127
action_320 (103) = happyGoto action_128
action_320 (105) = happyGoto action_129
action_320 (109) = happyGoto action_367
action_320 _ = happyReduce_194

action_321 _ = happyReduce_172

action_322 (140) = happyShift action_366
action_322 _ = happyFail

action_323 (127) = happyShift action_365
action_323 _ = happyFail

action_324 (126) = happyShift action_118
action_324 (139) = happyShift action_119
action_324 (168) = happyShift action_106
action_324 (169) = happyShift action_60
action_324 (170) = happyShift action_120
action_324 (171) = happyShift action_78
action_324 (172) = happyShift action_121
action_324 (60) = happyGoto action_109
action_324 (62) = happyGoto action_110
action_324 (64) = happyGoto action_111
action_324 (66) = happyGoto action_112
action_324 (68) = happyGoto action_113
action_324 (102) = happyGoto action_364
action_324 (104) = happyGoto action_115
action_324 (106) = happyGoto action_116
action_324 _ = happyFail

action_325 (126) = happyShift action_118
action_325 (139) = happyShift action_119
action_325 (168) = happyShift action_106
action_325 (169) = happyShift action_60
action_325 (170) = happyShift action_120
action_325 (171) = happyShift action_78
action_325 (172) = happyShift action_121
action_325 (60) = happyGoto action_109
action_325 (62) = happyGoto action_110
action_325 (64) = happyGoto action_111
action_325 (66) = happyGoto action_112
action_325 (68) = happyGoto action_113
action_325 (102) = happyGoto action_114
action_325 (104) = happyGoto action_115
action_325 (106) = happyGoto action_116
action_325 (110) = happyGoto action_363
action_325 _ = happyReduce_197

action_326 _ = happyReduce_174

action_327 (168) = happyShift action_57
action_327 (59) = happyGoto action_107
action_327 (111) = happyGoto action_362
action_327 _ = happyFail

action_328 (168) = happyShift action_106
action_328 (60) = happyGoto action_104
action_328 (112) = happyGoto action_361
action_328 _ = happyFail

action_329 (128) = happyShift action_333
action_329 (129) = happyShift action_334
action_329 (137) = happyShift action_335
action_329 _ = happyReduce_208

action_330 (128) = happyShift action_336
action_330 (129) = happyShift action_337
action_330 (137) = happyShift action_338
action_330 _ = happyReduce_210

action_331 (126) = happyShift action_84
action_331 (139) = happyShift action_85
action_331 (143) = happyShift action_86
action_331 (148) = happyShift action_87
action_331 (150) = happyShift action_88
action_331 (154) = happyShift action_89
action_331 (155) = happyShift action_90
action_331 (164) = happyShift action_91
action_331 (165) = happyShift action_92
action_331 (171) = happyShift action_93
action_331 (65) = happyGoto action_79
action_331 (115) = happyGoto action_360
action_331 (119) = happyGoto action_82
action_331 _ = happyFail

action_332 (126) = happyShift action_69
action_332 (139) = happyShift action_70
action_332 (143) = happyShift action_71
action_332 (148) = happyShift action_72
action_332 (150) = happyShift action_73
action_332 (154) = happyShift action_74
action_332 (155) = happyShift action_75
action_332 (164) = happyShift action_76
action_332 (165) = happyShift action_77
action_332 (171) = happyShift action_78
action_332 (66) = happyGoto action_64
action_332 (116) = happyGoto action_359
action_332 (120) = happyGoto action_67
action_332 _ = happyFail

action_333 _ = happyReduce_218

action_334 _ = happyReduce_219

action_335 _ = happyReduce_220

action_336 _ = happyReduce_231

action_337 _ = happyReduce_232

action_338 _ = happyReduce_233

action_339 (167) = happyShift action_358
action_339 _ = happyFail

action_340 (140) = happyShift action_357
action_340 _ = happyFail

action_341 (127) = happyShift action_356
action_341 _ = happyFail

action_342 (126) = happyShift action_84
action_342 (139) = happyShift action_85
action_342 (143) = happyShift action_86
action_342 (148) = happyShift action_87
action_342 (150) = happyShift action_88
action_342 (154) = happyShift action_89
action_342 (155) = happyShift action_90
action_342 (164) = happyShift action_91
action_342 (165) = happyShift action_92
action_342 (171) = happyShift action_93
action_342 (65) = happyGoto action_79
action_342 (115) = happyGoto action_355
action_342 (119) = happyGoto action_82
action_342 _ = happyFail

action_343 (167) = happyShift action_354
action_343 _ = happyFail

action_344 (140) = happyShift action_353
action_344 _ = happyFail

action_345 (127) = happyShift action_352
action_345 _ = happyFail

action_346 (126) = happyShift action_69
action_346 (139) = happyShift action_70
action_346 (143) = happyShift action_71
action_346 (148) = happyShift action_72
action_346 (150) = happyShift action_73
action_346 (154) = happyShift action_74
action_346 (155) = happyShift action_75
action_346 (164) = happyShift action_76
action_346 (165) = happyShift action_77
action_346 (171) = happyShift action_78
action_346 (66) = happyGoto action_64
action_346 (116) = happyGoto action_351
action_346 (120) = happyGoto action_67
action_346 _ = happyFail

action_347 (169) = happyShift action_63
action_347 (61) = happyGoto action_61
action_347 (123) = happyGoto action_350
action_347 _ = happyFail

action_348 (169) = happyShift action_60
action_348 (62) = happyGoto action_58
action_348 (124) = happyGoto action_349
action_348 _ = happyFail

action_349 _ = happyReduce_249

action_350 _ = happyReduce_247

action_351 (126) = happyShift action_69
action_351 (139) = happyShift action_70
action_351 (143) = happyShift action_71
action_351 (148) = happyShift action_72
action_351 (150) = happyShift action_73
action_351 (154) = happyShift action_74
action_351 (155) = happyShift action_75
action_351 (164) = happyShift action_76
action_351 (165) = happyShift action_77
action_351 (171) = happyShift action_78
action_351 (66) = happyGoto action_64
action_351 (120) = happyGoto action_330
action_351 _ = happyReduce_216

action_352 _ = happyReduce_243

action_353 _ = happyReduce_236

action_354 _ = happyReduce_237

action_355 (126) = happyShift action_84
action_355 (139) = happyShift action_85
action_355 (143) = happyShift action_86
action_355 (148) = happyShift action_87
action_355 (150) = happyShift action_88
action_355 (154) = happyShift action_89
action_355 (155) = happyShift action_90
action_355 (164) = happyShift action_91
action_355 (165) = happyShift action_92
action_355 (171) = happyShift action_93
action_355 (65) = happyGoto action_79
action_355 (119) = happyGoto action_329
action_355 _ = happyReduce_213

action_356 _ = happyReduce_230

action_357 _ = happyReduce_223

action_358 _ = happyReduce_224

action_359 (126) = happyShift action_69
action_359 (139) = happyShift action_70
action_359 (143) = happyShift action_71
action_359 (148) = happyShift action_72
action_359 (150) = happyShift action_73
action_359 (154) = happyShift action_74
action_359 (155) = happyShift action_75
action_359 (164) = happyShift action_76
action_359 (165) = happyShift action_77
action_359 (171) = happyShift action_78
action_359 (66) = happyGoto action_64
action_359 (120) = happyGoto action_330
action_359 _ = happyReduce_215

action_360 (126) = happyShift action_84
action_360 (139) = happyShift action_85
action_360 (143) = happyShift action_86
action_360 (148) = happyShift action_87
action_360 (150) = happyShift action_88
action_360 (154) = happyShift action_89
action_360 (155) = happyShift action_90
action_360 (164) = happyShift action_91
action_360 (165) = happyShift action_92
action_360 (171) = happyShift action_93
action_360 (65) = happyGoto action_79
action_360 (119) = happyGoto action_329
action_360 _ = happyReduce_212

action_361 _ = happyReduce_203

action_362 _ = happyReduce_201

action_363 _ = happyReduce_199

action_364 _ = happyReduce_170

action_365 _ = happyReduce_189

action_366 _ = happyReduce_188

action_367 _ = happyReduce_196

action_368 _ = happyReduce_168

action_369 _ = happyReduce_182

action_370 _ = happyReduce_181

action_371 _ = happyReduce_161

action_372 _ = happyReduce_159

action_373 _ = happyReduce_156

action_374 _ = happyReduce_157

action_375 _ = happyReduce_153

action_376 _ = happyReduce_154

action_377 _ = happyReduce_143

action_378 (140) = happyShift action_432
action_378 _ = happyFail

action_379 _ = happyReduce_137

action_380 (140) = happyShift action_431
action_380 _ = happyFail

action_381 _ = happyReduce_132

action_382 _ = happyReduce_130

action_383 _ = happyReduce_121

action_384 _ = happyReduce_119

action_385 (134) = happyShift action_430
action_385 _ = happyFail

action_386 (168) = happyShift action_106
action_386 (60) = happyGoto action_429
action_386 _ = happyFail

action_387 _ = happyReduce_105

action_388 _ = happyReduce_98

action_389 (136) = happyShift action_428
action_389 (169) = happyShift action_60
action_389 (62) = happyGoto action_149
action_389 (98) = happyGoto action_314
action_389 _ = happyFail

action_390 (92) = happyGoto action_427
action_390 _ = happyFail

action_391 (137) = happyShift action_191
action_391 (139) = happyShift action_185
action_391 (169) = happyShift action_60
action_391 (62) = happyGoto action_183
action_391 (84) = happyGoto action_426
action_391 (86) = happyGoto action_190
action_391 _ = happyFail

action_392 _ = happyReduce_112

action_393 (126) = happyShift action_69
action_393 (139) = happyShift action_70
action_393 (143) = happyShift action_71
action_393 (148) = happyShift action_72
action_393 (150) = happyShift action_73
action_393 (154) = happyShift action_74
action_393 (155) = happyShift action_75
action_393 (164) = happyShift action_76
action_393 (165) = happyShift action_77
action_393 (171) = happyShift action_78
action_393 (66) = happyGoto action_64
action_393 (116) = happyGoto action_65
action_393 (118) = happyGoto action_66
action_393 (120) = happyGoto action_67
action_393 (122) = happyGoto action_425
action_393 _ = happyFail

action_394 (138) = happyShift action_204
action_394 (74) = happyGoto action_201
action_394 (78) = happyGoto action_202
action_394 (80) = happyGoto action_424
action_394 _ = happyReduce_76

action_395 (168) = happyShift action_106
action_395 (60) = happyGoto action_423
action_395 _ = happyFail

action_396 (168) = happyShift action_106
action_396 (60) = happyGoto action_422
action_396 _ = happyFail

action_397 _ = happyReduce_100

action_398 (134) = happyShift action_421
action_398 _ = happyFail

action_399 (168) = happyShift action_57
action_399 (59) = happyGoto action_420
action_399 _ = happyFail

action_400 _ = happyReduce_87

action_401 _ = happyReduce_80

action_402 (136) = happyShift action_419
action_402 (169) = happyShift action_63
action_402 (61) = happyGoto action_151
action_402 (97) = happyGoto action_313
action_402 _ = happyFail

action_403 (91) = happyGoto action_418
action_403 _ = happyFail

action_404 (137) = happyShift action_194
action_404 (139) = happyShift action_188
action_404 (169) = happyShift action_63
action_404 (61) = happyGoto action_186
action_404 (83) = happyGoto action_417
action_404 (85) = happyGoto action_193
action_404 _ = happyFail

action_405 _ = happyReduce_94

action_406 (126) = happyShift action_84
action_406 (139) = happyShift action_85
action_406 (143) = happyShift action_86
action_406 (148) = happyShift action_87
action_406 (150) = happyShift action_88
action_406 (154) = happyShift action_89
action_406 (155) = happyShift action_90
action_406 (164) = happyShift action_91
action_406 (165) = happyShift action_92
action_406 (171) = happyShift action_93
action_406 (65) = happyGoto action_79
action_406 (115) = happyGoto action_80
action_406 (117) = happyGoto action_81
action_406 (119) = happyGoto action_82
action_406 (121) = happyGoto action_416
action_406 _ = happyFail

action_407 (138) = happyShift action_208
action_407 (73) = happyGoto action_205
action_407 (77) = happyGoto action_206
action_407 (79) = happyGoto action_415
action_407 _ = happyReduce_74

action_408 (168) = happyShift action_57
action_408 (59) = happyGoto action_414
action_408 _ = happyFail

action_409 (168) = happyShift action_57
action_409 (59) = happyGoto action_413
action_409 _ = happyFail

action_410 _ = happyReduce_82

action_411 _ = happyReduce_73

action_412 _ = happyReduce_70

action_413 _ = happyReduce_86

action_414 _ = happyReduce_85

action_415 _ = happyReduce_88

action_416 _ = happyReduce_83

action_417 (134) = happyShift action_440
action_417 _ = happyFail

action_418 (126) = happyShift action_163
action_418 (139) = happyShift action_164
action_418 (169) = happyShift action_63
action_418 (61) = happyGoto action_161
action_418 (93) = happyGoto action_305
action_418 _ = happyReduce_90

action_419 (126) = happyShift action_131
action_419 (139) = happyShift action_132
action_419 (168) = happyShift action_57
action_419 (169) = happyShift action_63
action_419 (170) = happyShift action_133
action_419 (171) = happyShift action_93
action_419 (172) = happyShift action_134
action_419 (59) = happyGoto action_122
action_419 (61) = happyGoto action_123
action_419 (63) = happyGoto action_124
action_419 (65) = happyGoto action_125
action_419 (67) = happyGoto action_126
action_419 (101) = happyGoto action_439
action_419 (103) = happyGoto action_128
action_419 (105) = happyGoto action_129
action_419 _ = happyFail

action_420 _ = happyReduce_91

action_421 (138) = happyShift action_208
action_421 (73) = happyGoto action_205
action_421 (77) = happyGoto action_438
action_421 _ = happyReduce_74

action_422 _ = happyReduce_104

action_423 _ = happyReduce_103

action_424 _ = happyReduce_106

action_425 _ = happyReduce_101

action_426 (134) = happyShift action_437
action_426 _ = happyFail

action_427 (126) = happyShift action_159
action_427 (139) = happyShift action_160
action_427 (169) = happyShift action_60
action_427 (62) = happyGoto action_157
action_427 (94) = happyGoto action_306
action_427 _ = happyReduce_108

action_428 (126) = happyShift action_118
action_428 (139) = happyShift action_119
action_428 (168) = happyShift action_106
action_428 (169) = happyShift action_60
action_428 (170) = happyShift action_120
action_428 (171) = happyShift action_78
action_428 (172) = happyShift action_121
action_428 (60) = happyGoto action_109
action_428 (62) = happyGoto action_110
action_428 (64) = happyGoto action_111
action_428 (66) = happyGoto action_112
action_428 (68) = happyGoto action_113
action_428 (102) = happyGoto action_436
action_428 (104) = happyGoto action_115
action_428 (106) = happyGoto action_116
action_428 _ = happyFail

action_429 _ = happyReduce_109

action_430 (138) = happyShift action_204
action_430 (74) = happyGoto action_201
action_430 (78) = happyGoto action_435
action_430 _ = happyReduce_76

action_431 (127) = happyShift action_434
action_431 _ = happyFail

action_432 (127) = happyShift action_433
action_432 _ = happyFail

action_433 _ = happyReduce_144

action_434 _ = happyReduce_138

action_435 _ = happyReduce_96

action_436 _ = happyReduce_107

action_437 (74) = happyGoto action_442
action_437 _ = happyReduce_76

action_438 _ = happyReduce_78

action_439 _ = happyReduce_89

action_440 (73) = happyGoto action_441
action_440 _ = happyReduce_74

action_441 (137) = happyShift action_194
action_441 (139) = happyShift action_188
action_441 (168) = happyShift action_57
action_441 (169) = happyShift action_63
action_441 (59) = happyGoto action_198
action_441 (61) = happyGoto action_186
action_441 (81) = happyGoto action_255
action_441 (83) = happyGoto action_200
action_441 (85) = happyGoto action_193
action_441 _ = happyReduce_81

action_442 (137) = happyShift action_191
action_442 (139) = happyShift action_185
action_442 (168) = happyShift action_106
action_442 (169) = happyShift action_60
action_442 (60) = happyGoto action_195
action_442 (62) = happyGoto action_183
action_442 (82) = happyGoto action_256
action_442 (84) = happyGoto action_197
action_442 (86) = happyGoto action_190
action_442 _ = happyReduce_99

happyReduce_56 = happySpecReduce_1  59 happyReduction_56
happyReduction_56 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn59
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  60 happyReduction_57
happyReduction_57 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn60
		 (fromString myLocation happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  61 happyReduction_58
happyReduction_58 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn61
		 (Ident happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  62 happyReduction_59
happyReduction_59 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn60
		 (fromToken myLocation "Ident" happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  63 happyReduction_60
happyReduction_60 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn63
		 ((read happy_var_1) :: Integer
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  64 happyReduction_61
happyReduction_61 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn60
		 (fromLit myLocation (read happy_var_1 :: Integer)
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  65 happyReduction_62
happyReduction_62 (HappyTerminal (PT _ (TC happy_var_1)))
	 =  HappyAbsSyn65
		 ((read happy_var_1) :: Char
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  66 happyReduction_63
happyReduction_63 (HappyTerminal (PT _ (TC happy_var_1)))
	 =  HappyAbsSyn60
		 (fromLit myLocation  (read happy_var_1 :: Char)
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  67 happyReduction_64
happyReduction_64 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn67
		 ((read happy_var_1) :: Double
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  68 happyReduction_65
happyReduction_65 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn60
		 (fromLit myLocation  (read happy_var_1 :: Double)
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  69 happyReduction_66
happyReduction_66 (HappyAbsSyn71  happy_var_1)
	 =  HappyAbsSyn69
		 (Grammar (happy_var_1)
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  70 happyReduction_67
happyReduction_67 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Grammar"  [happy_var_1]
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_0  71 happyReduction_68
happyReduction_68  =  HappyAbsSyn71
		 ([]
	)

happyReduce_69 = happySpecReduce_1  71 happyReduction_69
happyReduction_69 (HappyAbsSyn75  happy_var_1)
	 =  HappyAbsSyn71
		 ((:[]) (happy_var_1)
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  71 happyReduction_70
happyReduction_70 (HappyAbsSyn71  happy_var_3)
	_
	(HappyAbsSyn75  happy_var_1)
	 =  HappyAbsSyn71
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_0  72 happyReduction_71
happyReduction_71  =  HappyAbsSyn60
		 (appEPAll myLocation  "[]" []
	)

happyReduce_72 = happySpecReduce_1  72 happyReduction_72
happyReduction_72 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  72 happyReduction_73
happyReduction_73 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_0  73 happyReduction_74
happyReduction_74  =  HappyAbsSyn73
		 ([]
	)

happyReduce_75 = happySpecReduce_2  73 happyReduction_75
happyReduction_75 (HappyAbsSyn81  happy_var_2)
	(HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn73
		 (flip (:) (happy_var_1) (happy_var_2)
	)
happyReduction_75 _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_0  74 happyReduction_76
happyReduction_76  =  HappyAbsSyn60
		 (appEPAll myLocation  "[]" []
	)

happyReduce_77 = happySpecReduce_2  74 happyReduction_77
happyReduction_77 (HappyAbsSyn60  happy_var_2)
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAllL myLocation  [happy_var_1,happy_var_2]
	)
happyReduction_77 _ _  = notHappyAtAll 

happyReduce_78 = happyReduce 5 75 happyReduction_78
happyReduction_78 ((HappyAbsSyn77  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn83  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn87  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn75
		 (Rule (happy_var_1) (happy_var_3) (happy_var_5)
	) `HappyStk` happyRest

happyReduce_79 = happySpecReduce_2  75 happyReduction_79
happyReduction_79 (HappyAbsSyn59  happy_var_2)
	_
	 =  HappyAbsSyn75
		 (Comment (happy_var_2)
	)
happyReduction_79 _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  75 happyReduction_80
happyReduction_80 (HappyAbsSyn59  happy_var_3)
	(HappyAbsSyn59  happy_var_2)
	_
	 =  HappyAbsSyn75
		 (Comments (happy_var_2) (happy_var_3)
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happyReduce 6 75 happyReduction_81
happyReduction_81 ((HappyAbsSyn73  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn83  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn87  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn75
		 (Internal (happy_var_2) (happy_var_4) (reverse $ happy_var_6)
	) `HappyStk` happyRest

happyReduce_82 = happySpecReduce_3  75 happyReduction_82
happyReduction_82 (HappyAbsSyn115  happy_var_3)
	(HappyAbsSyn61  happy_var_2)
	_
	 =  HappyAbsSyn75
		 (Token (happy_var_2) (happy_var_3)
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happyReduce 4 75 happyReduction_83
happyReduction_83 ((HappyAbsSyn115  happy_var_4) `HappyStk`
	(HappyAbsSyn61  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn75
		 (PosToken (happy_var_3) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_84 = happySpecReduce_2  75 happyReduction_84
happyReduction_84 (HappyAbsSyn123  happy_var_2)
	_
	 =  HappyAbsSyn75
		 (Entryp (happy_var_2)
	)
happyReduction_84 _ _  = notHappyAtAll 

happyReduce_85 = happyReduce 4 75 happyReduction_85
happyReduction_85 ((HappyAbsSyn59  happy_var_4) `HappyStk`
	(HappyAbsSyn83  happy_var_3) `HappyStk`
	(HappyAbsSyn113  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn75
		 (Separator (happy_var_2) (happy_var_3) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_86 = happyReduce 4 75 happyReduction_86
happyReduction_86 ((HappyAbsSyn59  happy_var_4) `HappyStk`
	(HappyAbsSyn83  happy_var_3) `HappyStk`
	(HappyAbsSyn113  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn75
		 (Terminator (happy_var_2) (happy_var_3) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_87 = happySpecReduce_3  75 happyReduction_87
happyReduction_87 (HappyAbsSyn63  happy_var_3)
	(HappyAbsSyn61  happy_var_2)
	_
	 =  HappyAbsSyn75
		 (Coercions (happy_var_2) (happy_var_3)
	)
happyReduction_87 _ _ _  = notHappyAtAll 

happyReduce_88 = happyReduce 4 75 happyReduction_88
happyReduction_88 ((HappyAbsSyn79  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn61  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn75
		 (Rules (happy_var_2) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_89 = happyReduce 5 75 happyReduction_89
happyReduction_89 ((HappyAbsSyn101  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn99  happy_var_3) `HappyStk`
	(HappyAbsSyn61  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn75
		 (Function (happy_var_2) (reverse $ happy_var_3) (happy_var_5)
	) `HappyStk` happyRest

happyReduce_90 = happyReduce 4 75 happyReduction_90
happyReduction_90 ((HappyAbsSyn91  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn61  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn75
		 (External (happy_var_2) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_91 = happyReduce 4 75 happyReduction_91
happyReduction_91 ((HappyAbsSyn59  happy_var_4) `HappyStk`
	(HappyAbsSyn59  happy_var_3) `HappyStk`
	(HappyAbsSyn59  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn75
		 (AntiQuote (happy_var_2) (happy_var_3) (happy_var_4)
	) `HappyStk` happyRest

happyReduce_92 = happySpecReduce_2  75 happyReduction_92
happyReduction_92 (HappyAbsSyn123  happy_var_2)
	_
	 =  HappyAbsSyn75
		 (Derive (happy_var_2)
	)
happyReduction_92 _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_2  75 happyReduction_93
happyReduction_93 (HappyAbsSyn111  happy_var_2)
	_
	 =  HappyAbsSyn75
		 (Layout (happy_var_2)
	)
happyReduction_93 _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_3  75 happyReduction_94
happyReduction_94 (HappyAbsSyn111  happy_var_3)
	_
	_
	 =  HappyAbsSyn75
		 (LayoutStop (happy_var_3)
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_2  75 happyReduction_95
happyReduction_95 _
	_
	 =  HappyAbsSyn75
		 (LayoutTop
	)

happyReduce_96 = happyReduce 5 76 happyReduction_96
happyReduction_96 ((HappyAbsSyn60  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn60  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn60  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (appEPAll myLocation "Rule"  [happy_var_1,happy_var_3,happy_var_5]
	) `HappyStk` happyRest

happyReduce_97 = happySpecReduce_2  76 happyReduction_97
happyReduction_97 (HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Comment"  [happy_var_2]
	)
happyReduction_97 _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_3  76 happyReduction_98
happyReduction_98 (HappyAbsSyn60  happy_var_3)
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Comments"  [happy_var_2,happy_var_3]
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happyReduce 6 76 happyReduction_99
happyReduction_99 ((HappyAbsSyn60  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn60  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn60  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (appEPAll myLocation "Internal"  [happy_var_2,happy_var_4,happy_var_6]
	) `HappyStk` happyRest

happyReduce_100 = happySpecReduce_3  76 happyReduction_100
happyReduction_100 (HappyAbsSyn60  happy_var_3)
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Token"  [happy_var_2,happy_var_3]
	)
happyReduction_100 _ _ _  = notHappyAtAll 

happyReduce_101 = happyReduce 4 76 happyReduction_101
happyReduction_101 ((HappyAbsSyn60  happy_var_4) `HappyStk`
	(HappyAbsSyn60  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (appEPAll myLocation "PosToken"  [happy_var_3,happy_var_4]
	) `HappyStk` happyRest

happyReduce_102 = happySpecReduce_2  76 happyReduction_102
happyReduction_102 (HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Entryp"  [happy_var_2]
	)
happyReduction_102 _ _  = notHappyAtAll 

happyReduce_103 = happyReduce 4 76 happyReduction_103
happyReduction_103 ((HappyAbsSyn60  happy_var_4) `HappyStk`
	(HappyAbsSyn60  happy_var_3) `HappyStk`
	(HappyAbsSyn60  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (appEPAll myLocation "Separator"  [happy_var_2,happy_var_3,happy_var_4]
	) `HappyStk` happyRest

happyReduce_104 = happyReduce 4 76 happyReduction_104
happyReduction_104 ((HappyAbsSyn60  happy_var_4) `HappyStk`
	(HappyAbsSyn60  happy_var_3) `HappyStk`
	(HappyAbsSyn60  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (appEPAll myLocation "Terminator"  [happy_var_2,happy_var_3,happy_var_4]
	) `HappyStk` happyRest

happyReduce_105 = happySpecReduce_3  76 happyReduction_105
happyReduction_105 (HappyAbsSyn60  happy_var_3)
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Coercions"  [happy_var_2,happy_var_3]
	)
happyReduction_105 _ _ _  = notHappyAtAll 

happyReduce_106 = happyReduce 4 76 happyReduction_106
happyReduction_106 ((HappyAbsSyn60  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn60  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (appEPAll myLocation "Rules"  [happy_var_2,happy_var_4]
	) `HappyStk` happyRest

happyReduce_107 = happyReduce 5 76 happyReduction_107
happyReduction_107 ((HappyAbsSyn60  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn60  happy_var_3) `HappyStk`
	(HappyAbsSyn60  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (appEPAll myLocation "Function"  [happy_var_2,happy_var_3,happy_var_5]
	) `HappyStk` happyRest

happyReduce_108 = happyReduce 4 76 happyReduction_108
happyReduction_108 ((HappyAbsSyn60  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn60  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (appEPAll myLocation "External"  [happy_var_2,happy_var_4]
	) `HappyStk` happyRest

happyReduce_109 = happyReduce 4 76 happyReduction_109
happyReduction_109 ((HappyAbsSyn60  happy_var_4) `HappyStk`
	(HappyAbsSyn60  happy_var_3) `HappyStk`
	(HappyAbsSyn60  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (appEPAll myLocation "AntiQuote"  [happy_var_2,happy_var_3,happy_var_4]
	) `HappyStk` happyRest

happyReduce_110 = happySpecReduce_2  76 happyReduction_110
happyReduction_110 (HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Derive"  [happy_var_2]
	)
happyReduction_110 _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_2  76 happyReduction_111
happyReduction_111 (HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Layout"  [happy_var_2]
	)
happyReduction_111 _ _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_3  76 happyReduction_112
happyReduction_112 (HappyAbsSyn60  happy_var_3)
	_
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "LayoutStop"  [happy_var_3]
	)
happyReduction_112 _ _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_2  76 happyReduction_113
happyReduction_113 _
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation  "LayoutTop" []
	)

happyReduce_114 = happySpecReduce_1  77 happyReduction_114
happyReduction_114 (HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn77
		 (RHS (reverse $ happy_var_1)
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_2  77 happyReduction_115
happyReduction_115 (HappyAbsSyn115  happy_var_2)
	_
	 =  HappyAbsSyn77
		 (TRHS (happy_var_2)
	)
happyReduction_115 _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  78 happyReduction_116
happyReduction_116 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "RHS"  [happy_var_1]
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_2  78 happyReduction_117
happyReduction_117 (HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "TRHS"  [happy_var_2]
	)
happyReduction_117 _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  79 happyReduction_118
happyReduction_118 (HappyAbsSyn77  happy_var_1)
	 =  HappyAbsSyn79
		 ((:[]) (happy_var_1)
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_3  79 happyReduction_119
happyReduction_119 (HappyAbsSyn79  happy_var_3)
	_
	(HappyAbsSyn77  happy_var_1)
	 =  HappyAbsSyn79
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_119 _ _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  80 happyReduction_120
happyReduction_120 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_3  80 happyReduction_121
happyReduction_121 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_121 _ _ _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1  81 happyReduction_122
happyReduction_122 (HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn81
		 (Terminal (happy_var_1)
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  81 happyReduction_123
happyReduction_123 (HappyAbsSyn83  happy_var_1)
	 =  HappyAbsSyn81
		 (NTerminal (happy_var_1)
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_1  82 happyReduction_124
happyReduction_124 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Terminal"  [happy_var_1]
	)
happyReduction_124 _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_1  82 happyReduction_125
happyReduction_125 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "NTerminal"  [happy_var_1]
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_2  83 happyReduction_126
happyReduction_126 (HappyAbsSyn83  happy_var_2)
	_
	 =  HappyAbsSyn83
		 (OptCat (happy_var_2)
	)
happyReduction_126 _ _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_1  83 happyReduction_127
happyReduction_127 (HappyAbsSyn83  happy_var_1)
	 =  HappyAbsSyn83
		 (happy_var_1
	)
happyReduction_127 _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_2  84 happyReduction_128
happyReduction_128 (HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "OptCat"  [happy_var_2]
	)
happyReduction_128 _ _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_1  84 happyReduction_129
happyReduction_129 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_1
	)
happyReduction_129 _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_3  85 happyReduction_130
happyReduction_130 _
	(HappyAbsSyn83  happy_var_2)
	_
	 =  HappyAbsSyn83
		 (ListCat (happy_var_2)
	)
happyReduction_130 _ _ _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_1  85 happyReduction_131
happyReduction_131 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn83
		 (IdCat (happy_var_1)
	)
happyReduction_131 _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_3  86 happyReduction_132
happyReduction_132 _
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "ListCat"  [happy_var_2]
	)
happyReduction_132 _ _ _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_1  86 happyReduction_133
happyReduction_133 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "IdCat"  [happy_var_1]
	)
happyReduction_133 _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_1  87 happyReduction_134
happyReduction_134 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn87
		 (Id (happy_var_1)
	)
happyReduction_134 _  = notHappyAtAll 

happyReduce_135 = happySpecReduce_1  87 happyReduction_135
happyReduction_135 _
	 =  HappyAbsSyn87
		 (Wild
	)

happyReduce_136 = happySpecReduce_2  87 happyReduction_136
happyReduction_136 _
	_
	 =  HappyAbsSyn87
		 (ListE
	)

happyReduce_137 = happySpecReduce_3  87 happyReduction_137
happyReduction_137 _
	_
	_
	 =  HappyAbsSyn87
		 (ListCons
	)

happyReduce_138 = happyReduce 5 87 happyReduction_138
happyReduction_138 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn87
		 (ListOne
	) `HappyStk` happyRest

happyReduce_139 = happySpecReduce_2  87 happyReduction_139
happyReduction_139 (HappyAbsSyn89  happy_var_2)
	_
	 =  HappyAbsSyn87
		 (Aq (happy_var_2)
	)
happyReduction_139 _ _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_1  88 happyReduction_140
happyReduction_140 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Id"  [happy_var_1]
	)
happyReduction_140 _  = notHappyAtAll 

happyReduce_141 = happySpecReduce_1  88 happyReduction_141
happyReduction_141 _
	 =  HappyAbsSyn60
		 (appEPAll myLocation  "Wild" []
	)

happyReduce_142 = happySpecReduce_2  88 happyReduction_142
happyReduction_142 _
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation  "ListE" []
	)

happyReduce_143 = happySpecReduce_3  88 happyReduction_143
happyReduction_143 _
	_
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation  "ListCons" []
	)

happyReduce_144 = happyReduce 5 88 happyReduction_144
happyReduction_144 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (appEPAll myLocation  "ListOne" []
	) `HappyStk` happyRest

happyReduce_145 = happySpecReduce_2  88 happyReduction_145
happyReduction_145 (HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Aq"  [happy_var_2]
	)
happyReduction_145 _ _  = notHappyAtAll 

happyReduce_146 = happySpecReduce_1  89 happyReduction_146
happyReduction_146 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn89
		 (JIdent (happy_var_1)
	)
happyReduction_146 _  = notHappyAtAll 

happyReduce_147 = happySpecReduce_0  89 happyReduction_147
happyReduction_147  =  HappyAbsSyn89
		 (NIdent
	)

happyReduce_148 = happySpecReduce_1  90 happyReduction_148
happyReduction_148 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "JIdent"  [happy_var_1]
	)
happyReduction_148 _  = notHappyAtAll 

happyReduce_149 = happySpecReduce_0  90 happyReduction_149
happyReduction_149  =  HappyAbsSyn60
		 (appEPAll myLocation  "NIdent" []
	)

happyReduce_150 = happySpecReduce_2  91 happyReduction_150
happyReduction_150 (HappyAbsSyn91  happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn91
		 (HsApp (happy_var_1) (happy_var_2)
	)
happyReduction_150 _ _  = notHappyAtAll 

happyReduce_151 = happySpecReduce_2  92 happyReduction_151
happyReduction_151 (HappyAbsSyn60  happy_var_2)
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "HsApp"  [happy_var_1,happy_var_2]
	)
happyReduction_151 _ _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_1  93 happyReduction_152
happyReduction_152 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn91
		 (HsCon (happy_var_1)
	)
happyReduction_152 _  = notHappyAtAll 

happyReduce_153 = happySpecReduce_3  93 happyReduction_153
happyReduction_153 _
	(HappyAbsSyn95  happy_var_2)
	_
	 =  HappyAbsSyn91
		 (HsTup (happy_var_2)
	)
happyReduction_153 _ _ _  = notHappyAtAll 

happyReduce_154 = happySpecReduce_3  93 happyReduction_154
happyReduction_154 _
	(HappyAbsSyn91  happy_var_2)
	_
	 =  HappyAbsSyn91
		 (HsList (happy_var_2)
	)
happyReduction_154 _ _ _  = notHappyAtAll 

happyReduce_155 = happySpecReduce_1  94 happyReduction_155
happyReduction_155 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "HsCon"  [happy_var_1]
	)
happyReduction_155 _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_3  94 happyReduction_156
happyReduction_156 _
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "HsTup"  [happy_var_2]
	)
happyReduction_156 _ _ _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_3  94 happyReduction_157
happyReduction_157 _
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "HsList"  [happy_var_2]
	)
happyReduction_157 _ _ _  = notHappyAtAll 

happyReduce_158 = happySpecReduce_1  95 happyReduction_158
happyReduction_158 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn95
		 ((:[]) (happy_var_1)
	)
happyReduction_158 _  = notHappyAtAll 

happyReduce_159 = happySpecReduce_3  95 happyReduction_159
happyReduction_159 (HappyAbsSyn95  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn95
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_159 _ _ _  = notHappyAtAll 

happyReduce_160 = happySpecReduce_1  96 happyReduction_160
happyReduction_160 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_160 _  = notHappyAtAll 

happyReduce_161 = happySpecReduce_3  96 happyReduction_161
happyReduction_161 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_161 _ _ _  = notHappyAtAll 

happyReduce_162 = happySpecReduce_1  97 happyReduction_162
happyReduction_162 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn97
		 (Arg (happy_var_1)
	)
happyReduction_162 _  = notHappyAtAll 

happyReduce_163 = happySpecReduce_1  98 happyReduction_163
happyReduction_163 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Arg"  [happy_var_1]
	)
happyReduction_163 _  = notHappyAtAll 

happyReduce_164 = happySpecReduce_0  99 happyReduction_164
happyReduction_164  =  HappyAbsSyn99
		 ([]
	)

happyReduce_165 = happySpecReduce_2  99 happyReduction_165
happyReduction_165 (HappyAbsSyn97  happy_var_2)
	(HappyAbsSyn99  happy_var_1)
	 =  HappyAbsSyn99
		 (flip (:) (happy_var_1) (happy_var_2)
	)
happyReduction_165 _ _  = notHappyAtAll 

happyReduce_166 = happySpecReduce_0  100 happyReduction_166
happyReduction_166  =  HappyAbsSyn60
		 (appEPAll myLocation  "[]" []
	)

happyReduce_167 = happySpecReduce_2  100 happyReduction_167
happyReduction_167 (HappyAbsSyn60  happy_var_2)
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAllL myLocation  [happy_var_1,happy_var_2]
	)
happyReduction_167 _ _  = notHappyAtAll 

happyReduce_168 = happySpecReduce_3  101 happyReduction_168
happyReduction_168 (HappyAbsSyn101  happy_var_3)
	_
	(HappyAbsSyn101  happy_var_1)
	 =  HappyAbsSyn101
		 (Cons (happy_var_1) (happy_var_3)
	)
happyReduction_168 _ _ _  = notHappyAtAll 

happyReduce_169 = happySpecReduce_1  101 happyReduction_169
happyReduction_169 (HappyAbsSyn101  happy_var_1)
	 =  HappyAbsSyn101
		 (happy_var_1
	)
happyReduction_169 _  = notHappyAtAll 

happyReduce_170 = happySpecReduce_3  102 happyReduction_170
happyReduction_170 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Cons"  [happy_var_1,happy_var_3]
	)
happyReduction_170 _ _ _  = notHappyAtAll 

happyReduce_171 = happySpecReduce_1  102 happyReduction_171
happyReduction_171 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_1
	)
happyReduction_171 _  = notHappyAtAll 

happyReduce_172 = happySpecReduce_2  103 happyReduction_172
happyReduction_172 (HappyAbsSyn107  happy_var_2)
	(HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn101
		 (App (happy_var_1) (happy_var_2)
	)
happyReduction_172 _ _  = notHappyAtAll 

happyReduce_173 = happySpecReduce_1  103 happyReduction_173
happyReduction_173 (HappyAbsSyn101  happy_var_1)
	 =  HappyAbsSyn101
		 (happy_var_1
	)
happyReduction_173 _  = notHappyAtAll 

happyReduce_174 = happySpecReduce_2  104 happyReduction_174
happyReduction_174 (HappyAbsSyn60  happy_var_2)
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "App"  [happy_var_1,happy_var_2]
	)
happyReduction_174 _ _  = notHappyAtAll 

happyReduce_175 = happySpecReduce_1  104 happyReduction_175
happyReduction_175 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_1
	)
happyReduction_175 _  = notHappyAtAll 

happyReduce_176 = happySpecReduce_1  105 happyReduction_176
happyReduction_176 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn101
		 (Var (happy_var_1)
	)
happyReduction_176 _  = notHappyAtAll 

happyReduce_177 = happySpecReduce_1  105 happyReduction_177
happyReduction_177 (HappyAbsSyn63  happy_var_1)
	 =  HappyAbsSyn101
		 (LitInt (happy_var_1)
	)
happyReduction_177 _  = notHappyAtAll 

happyReduce_178 = happySpecReduce_1  105 happyReduction_178
happyReduction_178 (HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn101
		 (LitChar (happy_var_1)
	)
happyReduction_178 _  = notHappyAtAll 

happyReduce_179 = happySpecReduce_1  105 happyReduction_179
happyReduction_179 (HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn101
		 (LitString (happy_var_1)
	)
happyReduction_179 _  = notHappyAtAll 

happyReduce_180 = happySpecReduce_1  105 happyReduction_180
happyReduction_180 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn101
		 (LitDouble (happy_var_1)
	)
happyReduction_180 _  = notHappyAtAll 

happyReduce_181 = happySpecReduce_3  105 happyReduction_181
happyReduction_181 _
	(HappyAbsSyn107  happy_var_2)
	_
	 =  HappyAbsSyn101
		 (List (happy_var_2)
	)
happyReduction_181 _ _ _  = notHappyAtAll 

happyReduce_182 = happySpecReduce_3  105 happyReduction_182
happyReduction_182 _
	(HappyAbsSyn101  happy_var_2)
	_
	 =  HappyAbsSyn101
		 (happy_var_2
	)
happyReduction_182 _ _ _  = notHappyAtAll 

happyReduce_183 = happySpecReduce_1  106 happyReduction_183
happyReduction_183 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "Var"  [happy_var_1]
	)
happyReduction_183 _  = notHappyAtAll 

happyReduce_184 = happySpecReduce_1  106 happyReduction_184
happyReduction_184 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "LitInt"  [happy_var_1]
	)
happyReduction_184 _  = notHappyAtAll 

happyReduce_185 = happySpecReduce_1  106 happyReduction_185
happyReduction_185 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "LitChar"  [happy_var_1]
	)
happyReduction_185 _  = notHappyAtAll 

happyReduce_186 = happySpecReduce_1  106 happyReduction_186
happyReduction_186 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "LitString"  [happy_var_1]
	)
happyReduction_186 _  = notHappyAtAll 

happyReduce_187 = happySpecReduce_1  106 happyReduction_187
happyReduction_187 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "LitDouble"  [happy_var_1]
	)
happyReduction_187 _  = notHappyAtAll 

happyReduce_188 = happySpecReduce_3  106 happyReduction_188
happyReduction_188 _
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "List"  [happy_var_2]
	)
happyReduction_188 _ _ _  = notHappyAtAll 

happyReduce_189 = happySpecReduce_3  106 happyReduction_189
happyReduction_189 _
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (happy_var_2
	)
happyReduction_189 _ _ _  = notHappyAtAll 

happyReduce_190 = happySpecReduce_1  107 happyReduction_190
happyReduction_190 (HappyAbsSyn101  happy_var_1)
	 =  HappyAbsSyn107
		 ((:[]) (happy_var_1)
	)
happyReduction_190 _  = notHappyAtAll 

happyReduce_191 = happySpecReduce_2  107 happyReduction_191
happyReduction_191 (HappyAbsSyn107  happy_var_2)
	(HappyAbsSyn101  happy_var_1)
	 =  HappyAbsSyn107
		 ((:) (happy_var_1) (happy_var_2)
	)
happyReduction_191 _ _  = notHappyAtAll 

happyReduce_192 = happySpecReduce_1  108 happyReduction_192
happyReduction_192 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_192 _  = notHappyAtAll 

happyReduce_193 = happySpecReduce_2  108 happyReduction_193
happyReduction_193 (HappyAbsSyn60  happy_var_2)
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_2]
	)
happyReduction_193 _ _  = notHappyAtAll 

happyReduce_194 = happySpecReduce_0  109 happyReduction_194
happyReduction_194  =  HappyAbsSyn107
		 ([]
	)

happyReduce_195 = happySpecReduce_1  109 happyReduction_195
happyReduction_195 (HappyAbsSyn101  happy_var_1)
	 =  HappyAbsSyn107
		 ((:[]) (happy_var_1)
	)
happyReduction_195 _  = notHappyAtAll 

happyReduce_196 = happySpecReduce_3  109 happyReduction_196
happyReduction_196 (HappyAbsSyn107  happy_var_3)
	_
	(HappyAbsSyn101  happy_var_1)
	 =  HappyAbsSyn107
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_196 _ _ _  = notHappyAtAll 

happyReduce_197 = happySpecReduce_0  110 happyReduction_197
happyReduction_197  =  HappyAbsSyn60
		 (appEPAll myLocation  "[]" []
	)

happyReduce_198 = happySpecReduce_1  110 happyReduction_198
happyReduction_198 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_198 _  = notHappyAtAll 

happyReduce_199 = happySpecReduce_3  110 happyReduction_199
happyReduction_199 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_199 _ _ _  = notHappyAtAll 

happyReduce_200 = happySpecReduce_1  111 happyReduction_200
happyReduction_200 (HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn111
		 ((:[]) (happy_var_1)
	)
happyReduction_200 _  = notHappyAtAll 

happyReduce_201 = happySpecReduce_3  111 happyReduction_201
happyReduction_201 (HappyAbsSyn111  happy_var_3)
	_
	(HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn111
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_201 _ _ _  = notHappyAtAll 

happyReduce_202 = happySpecReduce_1  112 happyReduction_202
happyReduction_202 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_202 _  = notHappyAtAll 

happyReduce_203 = happySpecReduce_3  112 happyReduction_203
happyReduction_203 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_203 _ _ _  = notHappyAtAll 

happyReduce_204 = happySpecReduce_1  113 happyReduction_204
happyReduction_204 _
	 =  HappyAbsSyn113
		 (MNonempty
	)

happyReduce_205 = happySpecReduce_0  113 happyReduction_205
happyReduction_205  =  HappyAbsSyn113
		 (MEmpty
	)

happyReduce_206 = happySpecReduce_1  114 happyReduction_206
happyReduction_206 _
	 =  HappyAbsSyn60
		 (appEPAll myLocation  "MNonempty" []
	)

happyReduce_207 = happySpecReduce_0  114 happyReduction_207
happyReduction_207  =  HappyAbsSyn60
		 (appEPAll myLocation  "MEmpty" []
	)

happyReduce_208 = happySpecReduce_2  115 happyReduction_208
happyReduction_208 (HappyAbsSyn115  happy_var_2)
	(HappyAbsSyn115  happy_var_1)
	 =  HappyAbsSyn115
		 (RSeq (happy_var_1) (happy_var_2)
	)
happyReduction_208 _ _  = notHappyAtAll 

happyReduce_209 = happySpecReduce_1  115 happyReduction_209
happyReduction_209 (HappyAbsSyn115  happy_var_1)
	 =  HappyAbsSyn115
		 (happy_var_1
	)
happyReduction_209 _  = notHappyAtAll 

happyReduce_210 = happySpecReduce_2  116 happyReduction_210
happyReduction_210 (HappyAbsSyn60  happy_var_2)
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "RSeq"  [happy_var_1,happy_var_2]
	)
happyReduction_210 _ _  = notHappyAtAll 

happyReduce_211 = happySpecReduce_1  116 happyReduction_211
happyReduction_211 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_1
	)
happyReduction_211 _  = notHappyAtAll 

happyReduce_212 = happySpecReduce_3  117 happyReduction_212
happyReduction_212 (HappyAbsSyn115  happy_var_3)
	_
	(HappyAbsSyn115  happy_var_1)
	 =  HappyAbsSyn115
		 (RAlt (happy_var_1) (happy_var_3)
	)
happyReduction_212 _ _ _  = notHappyAtAll 

happyReduce_213 = happySpecReduce_3  117 happyReduction_213
happyReduction_213 (HappyAbsSyn115  happy_var_3)
	_
	(HappyAbsSyn115  happy_var_1)
	 =  HappyAbsSyn115
		 (RMinus (happy_var_1) (happy_var_3)
	)
happyReduction_213 _ _ _  = notHappyAtAll 

happyReduce_214 = happySpecReduce_1  117 happyReduction_214
happyReduction_214 (HappyAbsSyn115  happy_var_1)
	 =  HappyAbsSyn115
		 (happy_var_1
	)
happyReduction_214 _  = notHappyAtAll 

happyReduce_215 = happySpecReduce_3  118 happyReduction_215
happyReduction_215 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "RAlt"  [happy_var_1,happy_var_3]
	)
happyReduction_215 _ _ _  = notHappyAtAll 

happyReduce_216 = happySpecReduce_3  118 happyReduction_216
happyReduction_216 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "RMinus"  [happy_var_1,happy_var_3]
	)
happyReduction_216 _ _ _  = notHappyAtAll 

happyReduce_217 = happySpecReduce_1  118 happyReduction_217
happyReduction_217 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_1
	)
happyReduction_217 _  = notHappyAtAll 

happyReduce_218 = happySpecReduce_2  119 happyReduction_218
happyReduction_218 _
	(HappyAbsSyn115  happy_var_1)
	 =  HappyAbsSyn115
		 (RStar (happy_var_1)
	)
happyReduction_218 _ _  = notHappyAtAll 

happyReduce_219 = happySpecReduce_2  119 happyReduction_219
happyReduction_219 _
	(HappyAbsSyn115  happy_var_1)
	 =  HappyAbsSyn115
		 (RPlus (happy_var_1)
	)
happyReduction_219 _ _  = notHappyAtAll 

happyReduce_220 = happySpecReduce_2  119 happyReduction_220
happyReduction_220 _
	(HappyAbsSyn115  happy_var_1)
	 =  HappyAbsSyn115
		 (ROpt (happy_var_1)
	)
happyReduction_220 _ _  = notHappyAtAll 

happyReduce_221 = happySpecReduce_1  119 happyReduction_221
happyReduction_221 _
	 =  HappyAbsSyn115
		 (REps
	)

happyReduce_222 = happySpecReduce_1  119 happyReduction_222
happyReduction_222 (HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn115
		 (RChar (happy_var_1)
	)
happyReduction_222 _  = notHappyAtAll 

happyReduce_223 = happySpecReduce_3  119 happyReduction_223
happyReduction_223 _
	(HappyAbsSyn59  happy_var_2)
	_
	 =  HappyAbsSyn115
		 (RAlts (happy_var_2)
	)
happyReduction_223 _ _ _  = notHappyAtAll 

happyReduce_224 = happySpecReduce_3  119 happyReduction_224
happyReduction_224 _
	(HappyAbsSyn59  happy_var_2)
	_
	 =  HappyAbsSyn115
		 (RSeqs (happy_var_2)
	)
happyReduction_224 _ _ _  = notHappyAtAll 

happyReduce_225 = happySpecReduce_1  119 happyReduction_225
happyReduction_225 _
	 =  HappyAbsSyn115
		 (RDigit
	)

happyReduce_226 = happySpecReduce_1  119 happyReduction_226
happyReduction_226 _
	 =  HappyAbsSyn115
		 (RLetter
	)

happyReduce_227 = happySpecReduce_1  119 happyReduction_227
happyReduction_227 _
	 =  HappyAbsSyn115
		 (RUpper
	)

happyReduce_228 = happySpecReduce_1  119 happyReduction_228
happyReduction_228 _
	 =  HappyAbsSyn115
		 (RLower
	)

happyReduce_229 = happySpecReduce_1  119 happyReduction_229
happyReduction_229 _
	 =  HappyAbsSyn115
		 (RAny
	)

happyReduce_230 = happySpecReduce_3  119 happyReduction_230
happyReduction_230 _
	(HappyAbsSyn115  happy_var_2)
	_
	 =  HappyAbsSyn115
		 (happy_var_2
	)
happyReduction_230 _ _ _  = notHappyAtAll 

happyReduce_231 = happySpecReduce_2  120 happyReduction_231
happyReduction_231 _
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "RStar"  [happy_var_1]
	)
happyReduction_231 _ _  = notHappyAtAll 

happyReduce_232 = happySpecReduce_2  120 happyReduction_232
happyReduction_232 _
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "RPlus"  [happy_var_1]
	)
happyReduction_232 _ _  = notHappyAtAll 

happyReduce_233 = happySpecReduce_2  120 happyReduction_233
happyReduction_233 _
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "ROpt"  [happy_var_1]
	)
happyReduction_233 _ _  = notHappyAtAll 

happyReduce_234 = happySpecReduce_1  120 happyReduction_234
happyReduction_234 _
	 =  HappyAbsSyn60
		 (appEPAll myLocation  "REps" []
	)

happyReduce_235 = happySpecReduce_1  120 happyReduction_235
happyReduction_235 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation "RChar"  [happy_var_1]
	)
happyReduction_235 _  = notHappyAtAll 

happyReduce_236 = happySpecReduce_3  120 happyReduction_236
happyReduction_236 _
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "RAlts"  [happy_var_2]
	)
happyReduction_236 _ _ _  = notHappyAtAll 

happyReduce_237 = happySpecReduce_3  120 happyReduction_237
happyReduction_237 _
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (appEPAll myLocation "RSeqs"  [happy_var_2]
	)
happyReduction_237 _ _ _  = notHappyAtAll 

happyReduce_238 = happySpecReduce_1  120 happyReduction_238
happyReduction_238 _
	 =  HappyAbsSyn60
		 (appEPAll myLocation  "RDigit" []
	)

happyReduce_239 = happySpecReduce_1  120 happyReduction_239
happyReduction_239 _
	 =  HappyAbsSyn60
		 (appEPAll myLocation  "RLetter" []
	)

happyReduce_240 = happySpecReduce_1  120 happyReduction_240
happyReduction_240 _
	 =  HappyAbsSyn60
		 (appEPAll myLocation  "RUpper" []
	)

happyReduce_241 = happySpecReduce_1  120 happyReduction_241
happyReduction_241 _
	 =  HappyAbsSyn60
		 (appEPAll myLocation  "RLower" []
	)

happyReduce_242 = happySpecReduce_1  120 happyReduction_242
happyReduction_242 _
	 =  HappyAbsSyn60
		 (appEPAll myLocation  "RAny" []
	)

happyReduce_243 = happySpecReduce_3  120 happyReduction_243
happyReduction_243 _
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (happy_var_2
	)
happyReduction_243 _ _ _  = notHappyAtAll 

happyReduce_244 = happySpecReduce_1  121 happyReduction_244
happyReduction_244 (HappyAbsSyn115  happy_var_1)
	 =  HappyAbsSyn115
		 (happy_var_1
	)
happyReduction_244 _  = notHappyAtAll 

happyReduce_245 = happySpecReduce_1  122 happyReduction_245
happyReduction_245 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_1
	)
happyReduction_245 _  = notHappyAtAll 

happyReduce_246 = happySpecReduce_1  123 happyReduction_246
happyReduction_246 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn123
		 ((:[]) (happy_var_1)
	)
happyReduction_246 _  = notHappyAtAll 

happyReduce_247 = happySpecReduce_3  123 happyReduction_247
happyReduction_247 (HappyAbsSyn123  happy_var_3)
	_
	(HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn123
		 ((:) (happy_var_1) (happy_var_3)
	)
happyReduction_247 _ _ _  = notHappyAtAll 

happyReduce_248 = happySpecReduce_1  124 happyReduction_248
happyReduction_248 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAllL myLocation  [happy_var_1]
	)
happyReduction_248 _  = notHappyAtAll 

happyReduce_249 = happySpecReduce_3  124 happyReduction_249
happyReduction_249 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (appEPAll myLocation ":"  [happy_var_1,happy_var_3]
	)
happyReduction_249 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 174 174 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 125;
	PT _ (TS _ 2) -> cont 126;
	PT _ (TS _ 3) -> cont 127;
	PT _ (TS _ 4) -> cont 128;
	PT _ (TS _ 5) -> cont 129;
	PT _ (TS _ 6) -> cont 130;
	PT _ (TS _ 7) -> cont 131;
	PT _ (TS _ 8) -> cont 132;
	PT _ (TS _ 9) -> cont 133;
	PT _ (TS _ 10) -> cont 134;
	PT _ (TS _ 11) -> cont 135;
	PT _ (TS _ 12) -> cont 136;
	PT _ (TS _ 13) -> cont 137;
	PT _ (TS _ 14) -> cont 138;
	PT _ (TS _ 15) -> cont 139;
	PT _ (TS _ 16) -> cont 140;
	PT _ (TS _ 17) -> cont 141;
	PT _ (TS _ 18) -> cont 142;
	PT _ (TS _ 19) -> cont 143;
	PT _ (TS _ 20) -> cont 144;
	PT _ (TS _ 21) -> cont 145;
	PT _ (TS _ 22) -> cont 146;
	PT _ (TS _ 23) -> cont 147;
	PT _ (TS _ 24) -> cont 148;
	PT _ (TS _ 25) -> cont 149;
	PT _ (TS _ 26) -> cont 150;
	PT _ (TS _ 27) -> cont 151;
	PT _ (TS _ 28) -> cont 152;
	PT _ (TS _ 29) -> cont 153;
	PT _ (TS _ 30) -> cont 154;
	PT _ (TS _ 31) -> cont 155;
	PT _ (TS _ 32) -> cont 156;
	PT _ (TS _ 33) -> cont 157;
	PT _ (TS _ 34) -> cont 158;
	PT _ (TS _ 35) -> cont 159;
	PT _ (TS _ 36) -> cont 160;
	PT _ (TS _ 37) -> cont 161;
	PT _ (TS _ 38) -> cont 162;
	PT _ (TS _ 39) -> cont 163;
	PT _ (TS _ 40) -> cont 164;
	PT _ (TS _ 41) -> cont 165;
	PT _ (TS _ 42) -> cont 166;
	PT _ (TS _ 43) -> cont 167;
	PT _ (TL happy_dollar_dollar) -> cont 168;
	PT _ (TV happy_dollar_dollar) -> cont 169;
	PT _ (TI happy_dollar_dollar) -> cont 170;
	PT _ (TC happy_dollar_dollar) -> cont 171;
	PT _ (TD happy_dollar_dollar) -> cont 172;
	_ -> cont 173;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => ParseMonad a -> (a -> ParseMonad b) -> ParseMonad b
happyThen = (>>=)
happyReturn :: () => a -> ParseMonad a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> ParseMonad a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> ParseMonad a
happyError' = happyError

pGrammar tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn69 z -> happyReturn z; _other -> notHappyAtAll })

qGrammar tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pListDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn71 z -> happyReturn z; _other -> notHappyAtAll })

qListDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pListItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn73 z -> happyReturn z; _other -> notHappyAtAll })

qListItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn75 z -> happyReturn z; _other -> notHappyAtAll })

qDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pRHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn77 z -> happyReturn z; _other -> notHappyAtAll })

qRHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pListRHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn79 z -> happyReturn z; _other -> notHappyAtAll })

qListRHS tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_12 tks) (\x -> case x of {HappyAbsSyn81 z -> happyReturn z; _other -> notHappyAtAll })

qItem tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_13 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pCat tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_14 tks) (\x -> case x of {HappyAbsSyn83 z -> happyReturn z; _other -> notHappyAtAll })

qCat tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_15 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pCat1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_16 tks) (\x -> case x of {HappyAbsSyn83 z -> happyReturn z; _other -> notHappyAtAll })

qCat1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_17 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pLabel tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_18 tks) (\x -> case x of {HappyAbsSyn87 z -> happyReturn z; _other -> notHappyAtAll })

qLabel tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_19 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pMIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_20 tks) (\x -> case x of {HappyAbsSyn89 z -> happyReturn z; _other -> notHappyAtAll })

qMIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_21 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pHsTyp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_22 tks) (\x -> case x of {HappyAbsSyn91 z -> happyReturn z; _other -> notHappyAtAll })

qHsTyp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_23 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pHsTyp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_24 tks) (\x -> case x of {HappyAbsSyn91 z -> happyReturn z; _other -> notHappyAtAll })

qHsTyp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_25 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pListHsTyp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_26 tks) (\x -> case x of {HappyAbsSyn95 z -> happyReturn z; _other -> notHappyAtAll })

qListHsTyp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_27 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_28 tks) (\x -> case x of {HappyAbsSyn97 z -> happyReturn z; _other -> notHappyAtAll })

qArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_29 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pListArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_30 tks) (\x -> case x of {HappyAbsSyn99 z -> happyReturn z; _other -> notHappyAtAll })

qListArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_31 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_32 tks) (\x -> case x of {HappyAbsSyn101 z -> happyReturn z; _other -> notHappyAtAll })

qExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_33 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pExp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_34 tks) (\x -> case x of {HappyAbsSyn101 z -> happyReturn z; _other -> notHappyAtAll })

qExp1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_35 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_36 tks) (\x -> case x of {HappyAbsSyn101 z -> happyReturn z; _other -> notHappyAtAll })

qExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_37 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pListExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_38 tks) (\x -> case x of {HappyAbsSyn107 z -> happyReturn z; _other -> notHappyAtAll })

qListExp2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_39 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pListExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_40 tks) (\x -> case x of {HappyAbsSyn107 z -> happyReturn z; _other -> notHappyAtAll })

qListExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_41 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pListString tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_42 tks) (\x -> case x of {HappyAbsSyn111 z -> happyReturn z; _other -> notHappyAtAll })

qListString tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_43 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pMinimumSize tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_44 tks) (\x -> case x of {HappyAbsSyn113 z -> happyReturn z; _other -> notHappyAtAll })

qMinimumSize tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_45 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pReg2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_46 tks) (\x -> case x of {HappyAbsSyn115 z -> happyReturn z; _other -> notHappyAtAll })

qReg2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_47 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pReg1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_48 tks) (\x -> case x of {HappyAbsSyn115 z -> happyReturn z; _other -> notHappyAtAll })

qReg1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_49 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pReg3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_50 tks) (\x -> case x of {HappyAbsSyn115 z -> happyReturn z; _other -> notHappyAtAll })

qReg3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_51 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pReg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_52 tks) (\x -> case x of {HappyAbsSyn115 z -> happyReturn z; _other -> notHappyAtAll })

qReg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_53 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

pListIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_54 tks) (\x -> case x of {HappyAbsSyn123 z -> happyReturn z; _other -> notHappyAtAll })

qListIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_55 tks) (\x -> case x of {HappyAbsSyn60 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> ParseMonad a
happyError ts =
  fail $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map prToken (take 4 ts))

myLexer = tokens

myLocation = ($(fmap loc_package location >>= lift),"Language.LBNF.Grammar")

{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates\\GenericTemplate.hs" #-}








{-# LINE 49 "templates\\GenericTemplate.hs" #-}

{-# LINE 59 "templates\\GenericTemplate.hs" #-}

{-# LINE 68 "templates\\GenericTemplate.hs" #-}


-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:\n
notHappyAtAll = error "Internal Happy error\n"
-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
