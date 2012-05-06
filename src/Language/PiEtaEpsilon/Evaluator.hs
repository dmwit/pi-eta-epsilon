{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving #-}
module Language.PiEtaEpsilon.Evaluator where

import Language.PiEtaEpsilon.Syntax
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Logic
import Control.Monad.Identity
import Control.Monad.Trans.Identity
import Control.Unification
import Control.Unification.IntVar
import Prelude hiding (Either(..))
import GHC.Generics hiding ((:*:))

type UValue = UTerm ValueF IntVar

data Context
	= Box
	| Fst  Context Term | Snd  Term Context
	| LSum Context Term | RSum Term Context
	| LProduct Context Term UValue | RProduct Term UValue Context
	deriving (Show)

data MachineState = MachineState
	{ forward     :: Bool
	, descending  :: Bool
	, term        :: Term
	, output      :: UValue
	, context     :: Context
	} deriving (Show)

newtype PEET m a = PEET { unPEET :: IntBindingT ValueF (LogicT m) a }
type PEE = PEET Identity
deriving instance Functor     (PEET m)
deriving instance Monad       (PEET m)
deriving instance Applicative (PEET m)
deriving instance Alternative (PEET m)
deriving instance BindingMonad ValueF IntVar (PEET m)
instance MonadTrans PEET where lift m = PEET (lift (lift m))
instance MonadError (UnificationFailure ValueF IntVar) (PEET m) where
	throwError _ = empty -- throw away worlds where something doesn't unify
	catchError   = error "catchError undefined for the PEET monad"

runPEET :: Monad m => PEET m a -> m [a]
runPEET = observeAllT . evalIntBindingT . unPEET

runPEE :: PEE a -> [a]
runPEE = runIdentity . runPEET

instance Unifiable ValueF where
	zipMatch  Unit               Unit              = Just  Unit
	zipMatch (Left        a   ) (Left        b   ) = Just (Left        (a, b)         )
	zipMatch (Right       a   ) (Right       b   ) = Just (Right       (a, b)         )
	zipMatch (Tuple       a a') (Tuple       b b') = Just (Tuple       (a, b) (a', b'))
	zipMatch (Negate      a   ) (Negate      b   ) = Just (Negate      (a, b)         )
	zipMatch (Reciprocate a   ) (Reciprocate b   ) = Just (Reciprocate (a, b)         )
	zipMatch _ _ = Nothing

adjointIso :: Iso -> Iso
adjointIso (Eliminate b) = Introduce b
adjointIso (Introduce b) = Eliminate b

adjoint :: Term -> Term
adjoint (Base iso)  = Base (adjointIso iso)
adjoint (Id   t  )  = Id t
adjoint (t1 ::: t2) = adjoint t2 ::: adjoint t1
adjoint (t1 :+: t2) = adjoint t1 :+: adjoint t2
adjoint (t1 :*: t2) = adjoint t1 :*: adjoint t2

equate :: (MonadError (UnificationFailure t v) m, BindingMonad t v m) => UTerm t v -> UTerm t v -> m (UTerm t v)
equate t t' = runIdentityT (t =:= t')

newVariable :: BindingMonad t v m => m (UTerm t' v)
newVariable = var <$> freeVar

evalIso :: Iso -> UValue -> PEET m UValue
evalIso (Eliminate (IdentityS t)) v = newVariable >>= \v' -> equate v (right v') >> return v'
evalIso (Introduce (IdentityS t)) v = return (right v)
evalIso (Eliminate (CommutativeS t1 t2)) v =
	    (newVariable >>= \v' -> equate v (left  v') >> return (right v'))
	<|> (newVariable >>= \v' -> equate v (right v') >> return (left  v'))
evalIso (Introduce (CommutativeS t1 t2)) v = evalIso (Eliminate (CommutativeS t2 t1)) v
evalIso (Eliminate (AssociativeS t1 t2 t3)) v =
	    (newVariable >>= \v1 -> equate v (left         v1 ) >> return (left (left  v1)))
	<|> (newVariable >>= \v2 -> equate v (right (left  v2)) >> return (left (right v2)))
	<|> (newVariable >>= \v3 -> equate v (right (right v3)) >> return (right       v3 ))
evalIso (Introduce (AssociativeS t1 t2 t3)) v =
	    (newVariable >>= \v1 -> equate v (left (left  v1)) >> return (left         v1 ))
	<|> (newVariable >>= \v2 -> equate v (left (right v2)) >> return (right (left  v2)))
	<|> (newVariable >>= \v3 -> equate v (right       v3 ) >> return (right (right v3)))
evalIso (Eliminate (IdentityP t)) v = newVariable >>= \v' -> equate v (tuple unit v') >> return v'
evalIso (Introduce (IdentityP t)) v = return (tuple unit v)
evalIso (Eliminate (CommutativeP t1 t2)) v = do
	v1 <- newVariable
	v2 <- newVariable
	equate v (tuple v1 v2)
	return (tuple v2 v1)
evalIso (Introduce (CommutativeP t1 t2)) v = evalIso (Eliminate (CommutativeP t2 t1)) v
evalIso (Eliminate (AssociativeP t1 t2 t3)) v = do
	v1 <- newVariable
	v2 <- newVariable
	v3 <- newVariable
	equate v (tuple v1 (tuple v2 v3))
	return (tuple (tuple v1 v2) v3)
evalIso (Introduce (AssociativeP t1 t2 t3)) v = do
	v1 <- newVariable
	v2 <- newVariable
	v3 <- newVariable
	equate v (tuple (tuple v1 v2) v3)
	return (tuple v1 (tuple v2 v3))
evalIso (Introduce (DistributiveZero t)) v = empty
evalIso (Eliminate (DistributiveZero t)) v = empty
evalIso (Eliminate (DistributivePlus t1 t2 t3)) v = newVariable >>= \v3 ->
	    (newVariable >>= \v1 -> equate v (tuple (left  v1) v3) >> return (left  (tuple v1 v3)))
	<|> (newVariable >>= \v2 -> equate v (tuple (right v2) v3) >> return (right (tuple v2 v3)))
evalIso (Introduce (DistributivePlus t1 t2 t3)) v = newVariable >>= \v3 ->
	    (newVariable >>= \v1 -> equate v (left  (tuple v1 v3)) >> return (tuple (left  v1) v3))
	<|> (newVariable >>= \v2 -> equate v (right (tuple v2 v3)) >> return (tuple (right v2) v3))

initialize :: Term -> UValue -> MachineState
initialize t v = MachineState {
	forward = True,
	descending = True,
	term = t,
	output = v,
	context = Box
	}

isFinal :: MachineState -> Bool
isFinal (MachineState { forward = True, descending = False, context = Box }) = True
isFinal _ = False

stepEval :: MachineState -> PEET m MachineState
stepEval m@(MachineState { forward = True, descending = True }) = case term m of
	Base iso  -> do
		v <- evalIso iso (output m)
		return m { descending = False, output = v }
	Id     t  -> return m { descending = False }
	t1 ::: t2 -> return m { term = t1, context = Fst (context m) t2 }
	t1 :+: t2 -> newVariable >>= \v ->
		    (equate v (left  (output m)) >> return m { term = t1, output = v, context = LSum (context m) t2 })
		<|> (equate v (right (output m)) >> return m { term = t2, output = v, context = RSum t1 (context m) })
	t1 :*: t2 -> do
		v1 <- newVariable
		v2 <- newVariable
		equate (tuple v1 v2) (output m)
		return m { term = t1, output = v1, context = LProduct (context m) t2 v2 }
stepEval m@(MachineState { forward = True, descending = False }) = case context m of
	Box -> empty
	Fst  cxt t -> return m { descending = True, term = t, context = Snd (term m) cxt }
	Snd  t cxt -> return m { term = term m ::: t, context = cxt }
	LSum cxt t -> return m { term = term m :+: t, output = left  (output m), context = cxt }
	RSum t cxt -> return m { term = t :+: term m, output = right (output m), context = cxt }
	LProduct cxt t v -> return m { descending = True, term = t, output = v, context = RProduct (term m) (output m) cxt }
	RProduct t v cxt -> return m { term = t :*: term m, output = tuple v (output m), context = cxt }
stepEval m@(MachineState { forward = False, descending = True }) = case context m of
	Box -> empty
	Fst  cxt t -> return m { term = term m ::: t, context = cxt }
	Snd  t cxt -> return m { descending = False, term = t, context = Fst cxt (term m) }
	LSum cxt t -> return m { term = term m :+: t, output = left  (output m), context = cxt }
	RSum t cxt -> return m { term = t :+: term m, output = right (output m), context = cxt }
	LProduct cxt t v -> return m { term = term m :*: t, output = tuple (output m) v, context = cxt }
	RProduct t v cxt -> return m { descending = False, term = t, output = v, context = LProduct cxt (term m) (output m) }
stepEval m@(MachineState { forward = False, descending = False }) = case term m of
	Base iso  -> do
		v <- evalIso (adjointIso iso) (output m)
		return m { descending = True, output = v }
	Id     t  -> return m { descending = True }
	t1 ::: t2 -> return m { term = t2, context = Snd t1 (context m) }
	t1 :+: t2 -> newVariable >>= \v ->
		    (equate v (left  (output m)) >> return m { term = t1, output = v, context = LSum (context m) t2 })
		<|> (equate v (right (output m)) >> return m { term = t2, output = v, context = RSum t1 (context m) })
	t1 :*: t2 -> do
		v1 <- newVariable
		v2 <- newVariable
		equate (tuple v1 v2) (output m)
		return m { term = t2, output = v2, context = RProduct t1 v1 (context m) }

eval :: MachineState -> PEET m MachineState
eval m
	| isFinal m = freeze (output m) >>= \v -> return m { output = v }
	| otherwise = stepEval m >>= eval
	where freeze = runIdentityT . applyBindings

topLevel :: Term -> UValue -> [UValue]
topLevel t v = map output . runPEE . eval $ initialize t v
