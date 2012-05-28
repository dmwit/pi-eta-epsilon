{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, TypeSynonymInstances, 
    FunctionalDependencies, FlexibleInstances, TemplateHaskell, QuasiQuotes #-}
module Language.PiEtaEpsilon.Parser.Value (toP) where
import Language.PiEtaEpsilon.Token
import Language.PiEtaEpsilon.Syntax
import Text.Parsec   
import Text.Parsec.Expr
import Control.Monad.Reader
import Prelude hiding (negate, Left, Right)
import Language.PiEtaEpsilon.BNFMeta.Value hiding (Value, pValue)
import qualified Language.PiEtaEpsilon.BNFMeta.Value as M
import Data.Functor.Fixedpoint
import qualified Language.LBNF.Grammar as G
import Language.Haskell.TH.Quote
import Language.PiEtaEpsilon.Parser.Classes

instance To M.Value Value where
    to (VTuple       x y) = Fix $ Tuple (to x) (to y)
    to (VLeft        x  ) = Fix $ Left        (to x)       
    to (VRight       x  ) = Fix $ Right       (to x)       
    to (VNegate      x  ) = Fix $ Negate      (to x)      
    to (VReciprocate x  ) = Fix $ Reciprocate (to x)
    to (VUnit           ) = Fix Unit       

toP :: (Particle a) => Value -> a
toP (Fix ( Tuple x y)) = tuple  (toP x) (toP y)
toP (Fix ( Left         x)) = left   (toP x)
toP (Fix ( Right        x)) = right  (toP x)
toP (Fix ( Negate       x)) = negate (toP x)
toP (Fix ( Reciprocate  x)) = reciprocate (toP x)
toP (Fix Unit)                  = unit

{-
---------------------------------------------Value----------------------------------------------
expr = parens pValue
    <|> pValue
    
pValue =  pUnit
	 <|> pLeft        
	 <|> pRight       
	 <|> pTuple       
	 <|> pNegate      
	 <|> pReciprocate
	
pUnit = do
    string "u" 
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
    
    
    
    
    
    
    
    
    