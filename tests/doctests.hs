module Main where

import Test.DocTest

main :: IO ()
main = doctest [
                 "--optghc=-packageghc"
               , "--optghc=-isrc"
               , "--optghc=-idist/build/autogen/"
               , "--optghc=-optP-include"
               , "--optghc=-optPdist/build/autogen/cabal_macros.h"
               , "src/Language/PiEtaEpsilon/Examples.hs"
               , "src/Language/PiEtaEpsilon/Syntax.hs"
               ]

