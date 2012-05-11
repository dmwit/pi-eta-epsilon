-- boilerplate {{{1
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, TypeSynonymInstances #-}
module Language.PiEtaEpsilon.Syntax where
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer hiding (Product(..), Sum(..))
import Control.Unification
import Data.Foldable
import Data.Functor.Fixedpoint
import Data.Traversable
import Prelude hiding (Either(..), negate)
import GHC.Generics hiding ((:*:))
import Data.Data
import Data.Typeable
import Test.QuickCheck

-- types {{{1
-- Type {{{2
data Type
	= Zero
	| One
	| Sum        Type Type
	| Product    Type Type
	| Negative   Type
	| Reciprocal Type
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- values and unification variables {{{2
data ValueF t
	= Unit
	| Left        t
	| Right       t
	| Tuple       t t
	| Negate      t
	| Reciprocate t
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable)
type Value = Fix ValueF

-- isomorphisms {{{2
data IsoBase
	= IdentityS | CommutativeS | AssociativeS | SplitS
	| IdentityP | CommutativeP | AssociativeP | SplitP
	| DistributiveZero | DistributivePlus
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Iso
	= Eliminate IsoBase
	| Introduce IsoBase
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- Term {{{2
data Term
	= Base Iso
	| Id
	| Term ::: Term
	| Term :+: Term
	| Term :*: Term
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- convenience names for Values {{{1
class Particle a where
	unit :: a
	left, right, negate, reciprocate :: a -> a
	tuple :: a -> a -> a

instance Particle (Fix ValueF) where
	unit        = Fix Unit
	left        = Fix . Left
	right       = Fix . Right
	negate      = Fix . Negate
	reciprocate = Fix . Reciprocate
	tuple t1 t2 = Fix (Tuple t1 t2)

instance Particle (UTerm ValueF v) where
	unit        = UTerm Unit
	left        = UTerm . Left
	right       = UTerm . Right
	negate      = UTerm . Negate
	reciprocate = UTerm . Reciprocate
	tuple t1 t2 = UTerm (Tuple t1 t2)

-- doesn't need to be part of a type class yet, I guess
var = UVar

-- arbitrary instances {{{1
----------------------------------------------------------------------------------------------------
----------                          Arbitrary Instances                                  -----------
----------------------------------------------------------------------------------------------------

instance Arbitrary Type where
    arbitrary = sized arb' where
        arb' size = do
            let maxChoice = if size == (0 :: Int) then 1 :: Int else 5
            c <- choose(0, maxChoice)
            case c of
                0 -> return Zero
                1 -> return One
                2 -> Sum        <$> arb' (size `div` 2) <*> arb' (size `div` 2)
                3 -> Product    <$> arb' (size `div` 2) <*> arb' (size `div` 2)
                4 -> Negative   <$> arb' (size - 1)
                5 -> Reciprocal <$> arb' (size - 1)

    shrink Zero           = []
    shrink One            = []
    shrink (Sum x y)      = [x, y]
    shrink (Product x y)  = [x, y]
    shrink (Negative x)   = [x]
    shrink (Reciprocal x) = [x]

instance Arbitrary Value where
    arbitrary = sized arb' where
        arb' size = do
            let maxChoice = if size == 0 then 1 else 5
            c <- choose(0 :: Int, maxChoice)
            case c of
                0 -> return unit
                1 -> left   <$>  arb' (size - 1)
                2 -> right  <$>  arb' (size - 1)
                3 -> tuple  <$>  arb' (size `div` 2) <*> arb' (size `div` 2)
                4 -> negate <$>  arb' (size - 1)
                5 -> reciprocate <$> arb' (size - 1)
                         
        shrink (Fix Unit)            = []
        shrink (Fix (Left x))        = [x]
        shrink (Fix (Right x))       = [x]
        shrink (Fix (Tuple x y))     = [x, y]
        shrink (Fix (Negate x))      = [x]
        shrink (Fix (Reciprocate x)) = [x]

        
        
        
        
        
        
        
        
        
        
        
        


