{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, TypeSynonymInstances, 
    FunctionalDependencies, FlexibleInstances, TemplateHaskell, QuasiQuotes #-}
module Language.PiEtaEpsilon.Parser.Classes where

class To a b | a -> b where
    to :: a -> b 

class From a b | a -> b where
    from :: a -> b