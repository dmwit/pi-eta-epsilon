-- boilerplate {{{1
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving #-}
module Language.PiEtaEpsilon.Evaluator where

import Language.PiEtaEpsilon.Syntax
import Language.PiEtaEpsilon.Pretty.Debug
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Logic
import Control.Monad.Identity
import Control.Monad.Trans.Identity
import Control.Unification
import Control.Unification.IntVar
import Prelude hiding (Either(..), negate)
import GHC.Generics hiding ((:*:))

-- types {{{1
-- UValue, Context, MachineState {{{2
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

-- PEET {{{2
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

-- unification for UValues {{{1
instance Unifiable ValueF where
	zipMatch  Unit               Unit              = Just  Unit
	zipMatch (Left        a   ) (Left        b   ) = Just (Left        (a, b)         )
	zipMatch (Right       a   ) (Right       b   ) = Just (Right       (a, b)         )
	zipMatch (Tuple       a a') (Tuple       b b') = Just (Tuple       (a, b) (a', b'))
	zipMatch (Negate      a   ) (Negate      b   ) = Just (Negate      (a, b)         )
	zipMatch (Reciprocate a   ) (Reciprocate b   ) = Just (Reciprocate (a, b)         )
	zipMatch _ _ = Nothing

-- evaluation {{{1
-- misc {{{2
newVariable :: BindingMonad t v m => m (UTerm t' v)
newVariable = var <$> freeVar

-- The type signatures are scarier than the implementations.  The basic idea is
-- to assert that the v argument has the head form given by f, extract the
-- holes in the head given by f, and apply the new head form given by f' to the
-- values in the holes.  For example, "transform1 left right v" turns (Left v)
-- into (Right v) and fails on any value that definitely doesn't have "Left" at
-- the front.
transform0 f f' v = runIdentityT (v =:= f) >> return f'
transform1 f f' v = newVariable >>= \v' -> transform0 (f v') (f' v') v
transform2 f f' v = newVariable >>= \v' -> transform1 (f v') (f' v') v
transform3 f f' v = newVariable >>= \v' -> transform2 (f v') (f' v') v

tripleL, tripleR :: Particle a => a -> a -> a -> a
tripleL v1 v2 v3 = tuple (tuple v1 v2) v3
tripleR v1 v2 v3 = tuple v1 (tuple v2 v3)

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

-- evaluation of isomorphisms {{{2
evalIso :: Iso -> UValue -> PEET m UValue
evalIso (Eliminate IdentityS   ) v = transform1 right id v
evalIso (Introduce IdentityS   ) v = transform1 id right v
evalIso (Eliminate CommutativeS) v = transform1 left right v <|> transform1 right left v
evalIso (Introduce CommutativeS) v = transform1 left right v <|> transform1 right left v
evalIso (Eliminate AssociativeS) v =
	    transform1  left           (left . left ) v
	<|> transform1 (right . left ) (left . right) v
	<|> transform1 (right . right)  right         v
evalIso (Introduce AssociativeS) v =
	    transform1 (left . left )  left           v
	<|> transform1 (left . right) (right . left ) v
	<|> transform1  right         (right . right) v
evalIso (Eliminate SplitS      ) v = error "the impossible happened: stepEval passed an eta+ off to evalIso"
evalIso (Introduce SplitS      ) v = error "the impossible happened: stepEval passed an epsilon+ off to evalIso"
evalIso (Eliminate IdentityP   ) v = transform1 (tuple unit) id v
evalIso (Introduce IdentityP   ) v = transform1 id (tuple unit) v
evalIso (Eliminate CommutativeP) v = transform2 tuple (flip tuple) v
evalIso (Introduce CommutativeP) v = transform2 tuple (flip tuple) v
evalIso (Eliminate AssociativeP) v = transform3 tripleR tripleL v
evalIso (Introduce AssociativeP) v = transform3 tripleL tripleR v
evalIso (Eliminate SplitP      ) v = newVariable >>= \v' -> transform0 unit (tuple (reciprocate v') v') v
evalIso (Introduce SplitP      ) v = newVariable >>= \v' -> transform0 (tuple (reciprocate v') v') unit v
evalIso (Eliminate DistributiveZero) v = empty
evalIso (Introduce DistributiveZero) v = empty
evalIso (Eliminate DistributivePlus) v =
	    transform2 (\v1 v3 -> tuple (left  v1) v3) (\v1 v3 -> left  (tuple v1 v3)) v
	<|> transform2 (\v2 v3 -> tuple (right v2) v3) (\v2 v3 -> right (tuple v2 v3)) v
evalIso (Introduce DistributivePlus) v =
	    transform2 (\v1 v3 -> left  (tuple v1 v3)) (\v1 v3 -> tuple (left  v1) v3) v
	<|> transform2 (\v2 v3 -> right (tuple v2 v3)) (\v2 v3 -> tuple (right v2) v3) v

-- evaluation of terms {{{2
stepEval :: MachineState -> PEET m MachineState
stepEval m@(MachineState { forward = True, descending = True }) = case term m of
	Base (Eliminate SplitS) -> empty
	Base (Introduce SplitS) -> do
		v <-     transform1 right (left . negate) (output m)
		     <|> transform1 (left . negate) right (output m)
		return m { output = v, forward = False }
	Base iso  -> do
		v <- evalIso iso (output m)
		return m { descending = False, output = v }
	Id        -> return m { descending = False }
	t1 ::: t2 -> return m { term = t1, context = Fst (context m) t2 }
	t1 :+: t2 -> transform1 left  (\v     -> m { term = t1, output = v , context = LSum (context m) t2        }) (output m)
	         <|> transform1 right (\v     -> m { term = t2, output = v , context = RSum t1 (context m)        }) (output m)
	t1 :*: t2 -> transform2 tuple (\v1 v2 -> m { term = t1, output = v1, context = LProduct (context m) t2 v2 }) (output m)
stepEval m@(MachineState { forward = True, descending = False }) = case context m of
	Box -> empty
	Fst  cxt t -> return m { descending = True, term = t, context = Snd (term m) cxt }
	Snd  t cxt -> return m { term = t ::: term m, context = cxt }
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
	Base (Eliminate SplitS) -> do
		v <-     transform1 right (left . negate) (output m)
		     <|> transform1 (left . negate) right (output m)
		return m { output = v, forward = True }
	Base (Introduce SplitS) -> empty
	Base iso  -> do
		v <- evalIso (adjointIso iso) (output m)
		return m { descending = True, output = v }
	Id        -> return m { descending = True }
	t1 ::: t2 -> return m { term = t2, context = Snd t1 (context m) }
	t1 :+: t2 -> transform1 left  (\v     -> m { term = t1, output = v , context = LSum (context m) t2        }) (output m)
	         <|> transform1 right (\v     -> m { term = t2, output = v , context = RSum t1 (context m)        }) (output m)
	t1 :*: t2 -> transform2 tuple (\v1 v2 -> m { term = t2, output = v2, context = RProduct t1 v1 (context m) }) (output m)

-- drivers {{{2
eval :: MachineState -> PEET m MachineState
eval m
	| isFinal m = freeze (output m) >>= \v -> return m { output = v }
	| otherwise = stepEval m >>= eval
	where freeze = runIdentityT . applyBindings

topLevel :: Term -> UValue -> [UValue]
topLevel t v = map output . runPEE . eval $ initialize t v

nSteps :: Term -> UValue -> Int -> [(MachineState, IntBindingState ValueF)]
nSteps t v n = observeAll . runIntBindingT . unPEET $ do
	m <- (iterate (stepEval >=>) return !! n) (initialize t v)
	v <- runIdentityT . applyBindings . output $ m
	return m { output = v }

-- pretty printer {{{1
instance PPrint UValue  where ppr = show
instance PPrint Context where
	ppr = go id where
		go k  Box             = k "[]"
		go k (Fst      c t  ) = wrapl k c t ";"
		go k (Snd      t c  ) = wrapr k c t ";"
		go k (LSum     c t  ) = wrapl k c t "+"
		go k (RSum     t c  ) = wrapr k c t "+"
		go k (LProduct c t v) = wrapl k c t "x"
		go k (RProduct t v c) = wrapr k c t "x"
		wrapl k c t s = go (\s' -> concat ["(", k s' , " ", s, " ", ppr t, ")"]) c
		wrapr k c t s = go (\s' -> concat ["(", ppr t, " ", s, " ", k s' , ")"]) c

instance PPrint MachineState where
	ppr m = concat
		[ if descending m then "<" else "["
		, ppr (term    m), ", "
		, ppr (output  m), ", "
		, ppr (context m)
		, if descending m then ">" else "]"
		, if forward m then "|>" else "<|"
		]
