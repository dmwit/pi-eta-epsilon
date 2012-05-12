{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.PiEtaEpsilon.Parser.Value (valueExpr) where
import Language.PiEtaEpsilon.Token
import Language.PiEtaEpsilon.Syntax
import Text.Parsec   
import Text.Parsec.Expr
import Control.Monad.Reader
import Prelude hiding (negate)


---------------------------------------------Value----------------------------------------------

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
    
    
    
    
    
    
    
    
    
    
    