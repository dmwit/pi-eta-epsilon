module Language.PiEtaEpsilon.Interactive.StatementParser (
    Statement (..),
    pStatement
    ) where
import Text.Parsec
import Language.PiEtaEpsilon.Syntax hiding (Left, Right)
import Language.PiEtaEpsilon.Evaluator
import Control.Monad.Identity
import Language.PiEtaEpsilon.Parser.Term
import Language.PiEtaEpsilon.Parser.Value
import Data.List.Split
import Language.LBNF.Runtime
import Language.PiEtaEpsilon.Parser.Classes

toUV = toP . to

data Statement = Stmt_eval Term UValue
               | Stmt_let String Term
               | Stmt_empty
   
pStatement :: String -> Either String Statement    
pStatement input = do
    [termString, valueString] <- case splitOn "$" input of
        [t, v] -> return [t, v]
        _ -> Left "Statement parser error! Did you forget a '$' between the term and the value?"
    term <- case parseTerm termString of
                Language.LBNF.Runtime.Ok  x -> return x
                Bad m -> Left m
    value <- case parseValue valueString of
                Language.LBNF.Runtime.Ok  x -> return x
                Bad m -> Left m
    return $ Stmt_eval (to term) (toUV value)
{-
pStatement :: String -> Either String Statement    
pStatement st = case runParser statementP () "" st of
    Right x -> Right x
    Left  x -> Left $ show x

statementP = do 
    toParsec parseTerm 
    toParsec parseValue

toParsec p = updateParserState (updateStateWith p)
    
updateStateWith p (State input pos x) = State newInput newPos x where
    newInput = undefined
    newPos   = undefined
-}