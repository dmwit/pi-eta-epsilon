{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, TypeSynonymInstances, 
    FunctionalDependencies, FlexibleInstances, TemplateHaskell, QuasiQuotes #-}
module Language.PiEtaEpsilon.Parser.Value (To, From) where
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
    to :: b -> a 
    
class From a b | a -> b where
    from :: b -> a

instance To Value M.Value where
    to (VTuple       x y) = Fix $ Tuple (to x) (to y)
    to (VLeft        x  ) = Fix $ Left        (to x)       
    to (VRight       x  ) = Fix $ Right       (to x)       
    to (VNegate      x  ) = Fix $ Negate      (to x)      
    to (VReciprocate x  ) = Fix $ Reciprocate (to x)
    to (VUnit           ) = Fix Unit       


 


---------------------------------------------Value----------------------------------------------
{-
valueExpr = expr

expr = parens expr'
    <|> expr'
    
expr' =  pUnit
	 <|> pLeft        
	 <|> pRight       
	 <|> pTuple       
	 <|> pNegate      
	 <|> pReciprocate
	
pUnit = do
    string "()" 
    return unit
     
pLeft = do
    string "L"
    t <- expr 
    return $ left t
        
pRight = do
    string "R"
    t <- expr
    return $ right t 

pTuple = parens $ 
  do 
    t <- expr
    spaces 
    char ','
    spaces
    s <- expr
    return $ tuple t s
       
pNegate = do
    char '-'
    spaces
    t <- expr
    return $ negate t
       
pReciprocate = do
    char '/'
    spaces
    t <- expr
    return $ reciprocate t
    
-}    
    
    
    
    
    
    
    
    
    