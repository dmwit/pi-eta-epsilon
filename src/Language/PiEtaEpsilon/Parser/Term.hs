{-# LANGUAGE TupleSections  #-}
import Language.PiEtaEpsilon.Parser.Term where
import Language.PiEtaEpsilon.Token
import Language.PiEtaEpsilon.Syntax
import Text.Parsec   
import Text.Parsec.Expr
import Control.Monad.Reader
import Prelude hiding (negate, Left, Right)
import Language.PiEtaEpsilon.BNFMeta.Value hiding (Value)
import qualified Language.PiEtaEpsilon.BNFMeta.Value as M
import Data.Functor.Fixedpoint
import qualified Language.LBNF.Grammar as G
import Language.Haskell.TH.Quote

--TODO
--1.) Make functional
class To a b | a -> b where
    to :: a -> b 

class From a b | a -> b where
    from :: a -> b

instance To M.Term Term where
    to (TBase     x   ) = Base     (to x)
    to (TId       (Ident x)) = Id  x -- I could also make a To Ident String instance       
    to (TCompose  x y ) = Compose  (to x) (to y)       
    to (TPlus     x y ) = Plus     (to x) (to y)      
    to (TTimes    x y ) = Times    (to x) (to y)






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
