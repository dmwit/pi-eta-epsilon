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



main = defaultMain tests

mkParserTest f input expected = case f input of
        Prelude.Right x -> x @?= expected
        Prelude.Left  x -> assertFailure $ show x 

testParseType = mkParserTest parseType


tests = [
            testGroup "Type Parser" [
                testCase "test_pZero"       $ testParseType "0"       Zero,
                testCase "test_pOne"        $ testParseType "1"       One,
                testCase "test_pSum"        $ testParseType "1 + 0" $ Sum        One Zero,
                testCase "test_pProduct"    $ testParseType "1 * 0" $ Product    One Zero,
                testCase "test_pNegative"   $ testParseType "- 1"   $ Negative   One,
                testCase "test_pReciprocal" $ testParseType "/ 1"   $ Reciprocal One
            ],
            testGroup "Type QuasiQuoter" [
                testCase "test_expression_0" $ [typ| (1 + 0) * (1 * 1)|] @?= Product (Sum One Zero) (Product One One) 
            ]
        ]
        
    
    
    
    
      