{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.PiEtaEpsilon.Parser where
import Language.PiEtaEpsilon.Syntax 
import Text.Parsec   
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

parens     = T.parens haskell
reservedOp = T.reservedOp haskell

parseType = runParser pType () ""

pZero = do
    spaces 
    char '0'
    spaces 
    return Zero
pOne  = do 
    spaces 
    char '1'
    spaces 
    return One

pType    = spaces >> buildExpressionParser table pTypeTerm
          <?> "type"

pTypeTerm =  parens pType 
         <|> pZero
         <|> pOne
         <?> "simple type"

table   = [ 
            [prefix "-" Negative, prefix "/" Reciprocal ],
            [binary "*" Product AssocLeft],
            [binary "+" Sum AssocLeft]
          ]
      
binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })


