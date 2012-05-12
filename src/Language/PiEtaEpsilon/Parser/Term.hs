{-# LANGUAGE TupleSections  #-}
import Language.PiEtaEpsilon.Parser.Term where
import Language.PiEtaEpsilon.Syntax
import Language.PiEtaEpsilon.Token
import Language.PiEtaEpsilon.Syntax
import Text.Parsec   
import Text.Parsec.Expr
import Control.Monad.Reader
import Prelude hiding (negate)
import Language.PiEtaEpsilon.Parser.Type





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
