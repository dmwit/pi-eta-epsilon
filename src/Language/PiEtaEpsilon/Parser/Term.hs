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


instance To M.Term Term where
    to (M.TBase     x   ) = Base     (to x)
    to (M.TId       (M.Ident x)) = Id -- I could also make a To Ident String instance       
    to (M.TDistribute x y ) = (to x) ::: (to y)       
    to (M.TPlus       x y ) = (to x) :+: (to y)      
    to (M.TTimes      x y ) = (to x) :*: (to y)


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
-- isomorphisms {{{2
data IsoBase
	= IdentityS | CommutativeS | AssociativeS | SplitS
	| IdentityP | CommutativeP | AssociativeP | SplitP
	| DistributiveZero | DistributivePlus
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
-}

{-
pIsoBase =  pIdentityS
        <|> pIdentityP
        <|> pCommutativeS
        <|> pCommutativeP
        <|> pAssociativeS
        <|> pAssociativeP
        <|> pSplitS
        <|> pSplitP

pPlus = undefined
pZero = undefined

pTimes = undefined 
pOne   = undefined

pIdentityS = do
    permute (,) <$$> pPlus <||> pZero
    return IdentityS
    
pIdentityP = do
    permute (,) <$$> pTimes <||> pOne
    return IdentityP    
    
pCommutativeS = do
    string "X"
    return CommutativeS
    
pCommutativeP = do
    string "X"
    return CommutativeP
    
pAssociativeS = do
   string "]"
   return AssociativeS
    
pAssociativeP = do
    string "["
    return AssociativeP
    
pSplitS = do
    string "\"
    return 
    
pSplitP       = do

{-
data Iso
	= Eliminate IsoBase
	| Introduce IsoBase
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
-}

pIso =  pEliminate 
    <|> pIntroduce 

pEliminate = undefined
pIntroduce = undefined

{-
-- Term {{{2
data Term
	= Base Iso
	| Id
	| Term ::: Term
	| Term :+: Term
	| Term :*: Term
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-}

expr =  pIsoBase
	<|> pId
	<|> pElement
	<|> pPlus
	<|> pTimes
	
pIsoBase = undefined
pId      = undefined
pElement = undefined
pPlus    = undefined
pTimes   = undefined
-}
