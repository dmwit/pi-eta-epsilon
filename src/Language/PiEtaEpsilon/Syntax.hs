{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module Language.PiEtaEpsilon.Syntax where
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer hiding (Product(..), Sum(..))
import Prelude hiding (Either(..))
import GHC.Generics hiding ((:*:))
import Data.Data
import Data.Typeable
import Test.QuickCheck

data Type
	= Zero
	| One
	| Sum        Type Type
	| Product    Type Type
	| Negative   Type
	| Reciprocal Type
	deriving(Eq, Ord, Show, Read, Data, Typeable, Generic)

data Value
	= Unit
	| Left        Value
	| Right       Value
	| Tuple       Value Value
	| Negate      Value
	| Reciprocate Value
	| UnificationVariable Int Type
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data IsoBase
	= IdentityS Type | CommutativeS Type Type | AssociativeS Type Type Type
	| IdentityP Type | CommutativeP Type Type | AssociativeP Type Type Type
	| DistributiveZero Type
	| DistributivePlus Type Type Type
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Iso
	= Eliminate IsoBase
	| Introduce IsoBase
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Term
	= Base Iso
	| Id Type
	| Term ::: Term
	| Term :+: Term
	| Term :*: Term
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

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
            let maxChoice = if size == 0 then 1 else 6
            c <- choose(0 :: Int, maxChoice)
            case c of
                0 -> return Unit
                1 -> Left   <$>  arb' (size - 1) 
                2 -> Right  <$>  arb' (size - 1) 
                3 -> Tuple  <$>  arb' (size `div` 2) <*> arb' (size `div` 2)
                4 -> Negate <$>  arb' (size - 1) 
                5 -> Reciprocate <$> arb' (size - 1) 
                6 -> UnificationVariable <$> arbitrary <*> arbitrary


------------------------------------------------------------------------------------
----                              PrettyPrint                               --------
------------------------------------------------------------------------------------
pprType :: Type -> String
pprType Zero = "0"
pprType One  = "1"
pprType (Sum     x y) = "(" ++ pprType x ++ " + " ++ pprType y ++ ")"
pprType (Product x y) = "(" ++ pprType x ++ " * " ++ pprType y ++ ")"
pprType (Negative   x) = "(" ++ " - " ++ pprType x ++ ")"
pprType (Reciprocal x) = "(" ++ " / " ++ pprType x ++ ")"
