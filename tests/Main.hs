{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes #-}
module Main where
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import Debug.Trace.Helpers
import Debug.Trace
import Test.QuickCheck.Checkers
import Data.List
import Data.Generics.Uniplate.Data
import Control.Applicative ((<$>))    
import Control.Monad.Error
import Language.PiEtaEpsilon
import Debug.Trace


--main = quickCheck $ roundTrip pprType   (fromRight . parseType)

main = defaultMain tests

mkParserTest f input expected = case f input of
        Prelude.Right x -> x @?= expected
        Prelude.Left  x -> assertFailure $ show x 

testParseType = mkParserTest parseType

fromRight :: (Show a) => Either a b -> b
fromRight (Prelude.Right x) = x
fromRight (Prelude.Left x)         = error $ "okay here's what's wrong " ++ show x

roundTrip f g x = if (g $ f x) == x 
                    then True
                    else trace (show x) False

roundTripBack f g x = result where
        y = f x
        result = if (f $ g y) == y
                    then True
                    else trace (show x) False 
  

sp' :: (x -> b -> c) -> (y -> z -> b) -> x -> y -> z ->  c
sp' f g x y z = f x $ g y z 
     
sp f g (x, y, z) = sp' f g x y z
                             
tests = [
            testGroup "Type Parser" $ concat [
                    map (sp testCase testParseType) [
                        ("test_pZero"       , "0",     Zero),
                        ("test_pOne"        , "1",     One),
                        ("test_pSum"        , "1 + 0", Sum        One Zero),
                        ("test_pProduct"    , "1 * 0", Product    One Zero),
                        ("test_pNegative"   , "- 1"  , Negative   One),
                        ("test_pReciprocal" , "/ 1"  , Reciprocal One)
                    ], 
                    --properities
                    [
                        testProperty "a ppr type is a parsed type"   $ roundTrip     pprType (fromRight . parseType), 
                        testProperty "a parsed string is a ppr type" $ roundTripBack pprType (fromRight . parseType)
                    ]
                ],
                    
            testGroup "Type QuasiQuoter" [
                testCase "test_expression_0" $ [typ| (1 + 0) * (1 * 1)|] @?= Product (Sum One Zero) (Product One One) 
            ]
        ]
        
    
    
    
    
      
