{-# LANGUAGE TupleSections, MultiParamTypeClasses  #-}
module Language.PiEtaEpsilon.Parser.Term where
import Language.PiEtaEpsilon.Parser.Classes
import Language.PiEtaEpsilon.Token
import Language.PiEtaEpsilon.Syntax
import Text.Parsec   
import Text.Parsec.Expr
import Control.Monad.Reader
import Prelude hiding (negate, Left, Right)
import Language.PiEtaEpsilon.BNFMeta.Value hiding (Value)
import qualified Language.PiEtaEpsilon.BNFMeta.Term as M
import Data.Functor.Fixedpoint
import qualified Language.LBNF.Grammar as G
import Language.Haskell.TH.Quote
import Control.Monad


instance To M.Term Term where
    to (M.TBase          x) = Base     (to x)
    to (M.TId             ) = Id -- I could also make a To Ident String instance       
    to (M.TCompose     x y) = (to x) ::: (to y)       
    to (M.TPlus        x y) = (to x) :+: (to y)      
    to (M.TTimes       x y) = (to x) :*: (to y)


instance To M.Iso Iso where
    to (M.IEliminate x) = Eliminate (to x)
    to (M.IIntroduce x) = Introduce (to x)
    
instance To M.BaseIso IsoBase where
    to M.BIdentityS        = IdentityS          
    to M.BIdentityP        = IdentityP       
    to M.BCommutativeS     = CommutativeS    
    to M.BCommutativeP     = CommutativeP    
    to M.BAssociativeS     = AssociativeS    
    to M.BAssociativeP     = AssociativeP    
    to M.BSplitS           = SplitS          
    to M.BSplitP           = SplitP          
    to M.BDistributiveZero = DistributiveZero
    to M.BDistributivePlus = DistributivePlus


{-
pIsoBase =  pIdentityS
        <|> pIdentityP
        <|> pCommutativeS
        <|> pCommutativeP
        <|> pAssociativeS
        <|> pAssociativeP
        <|> pSplitS
        <|> pSplitP

pIdentityS    = return IdentityS    << string "<=+=>" 
pIdentityP    = return IdentityP    << string "<=*=>" 
pCommutativeS = return CommutativeS << string "x+x"   
pCommutativeP = return CommutativeP << string "x*x"   
pAssociativeS = return AssociativeS << string "|+|+|" 
pAssociativeP = return AssociativeP << string "|*|*|" 
pSplitS       = return SplitS       << string ">+-"   
pSplitP       = return SplitP       << string ">*-"   


pIso =  pEliminate 
    <|> pIntroduce 

pEliminate = (return . Eliminate) =<< pIsoBase << spaces << string "#"
pIntroduce = (return . Introduce) =<< pIsoBase << spaces << string "'"

pTerm =  pIsoBase
	 <|> pId
	 <|> pCompose
	 <|> pPlus
	 <|> pTimes

pIsoBase = (return . Base) =<< pIso << spaces << string "<"
pId      = return Id << string "<=>"
binaryTerm term token = do
    l <- expr
    spaces
    string token
    spaces 
    r <- expr
    return $ term l r

pCompose = binaryTerm Compose "."    
pPlus    = binaryTerm Plus    "+"
pTimes   = binaryTerm Plus    "*"

expr = parens pTerm <|> pTerm
-}