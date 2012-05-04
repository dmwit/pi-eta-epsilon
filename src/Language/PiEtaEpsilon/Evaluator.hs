module Language.PiEtaEpsilon.Evaluator where
import Language.PiEtaEpsilon.Syntax
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer hiding (Product(..), Sum(..))
import Prelude hiding (Either(..))
import GHC.Generics hiding ((:*:))


data Context = Box | Fst Context Term | Snd Term Context | LProduct Context Term Value | RProduct Term Value Context | LSum Context Term | RSum Term Context deriving (Eq, Ord, Show, Read)
data MachineState = MachineState
	{ forward :: Bool
	, descending :: Bool
	, variable :: Int
	, term :: Term
	, output :: Value
	, context :: Context
	, constraints :: Constraints
	} deriving (Eq, Ord, Show, Read)
type Constraints = [(Value, Value)]
type IsoM = StateT Int (WriterT Constraints [])

adjointIso :: Iso -> Iso
adjointIso (Eliminate b) = Introduce b
adjointIso (Introduce b) = Eliminate b

adjoint :: Term -> Term
adjoint (Base iso)  = Base (adjointIso iso)
adjoint (Id   t  )  = Id t
adjoint (t1 ::: t2) = adjoint t2 ::: adjoint t1
adjoint (t1 :+: t2) = adjoint t1 :+: adjoint t2
adjoint (t1 :*: t2) = adjoint t1 :*: adjoint t2

piType :: Term -> (Type, Type)
piType (Base (Eliminate (IdentityS t              ))) = (Sum Zero t, t)
piType (Base (Eliminate (CommutativeS t1 t2       ))) = (Sum t1 t2, Sum t2 t1)
piType (Base (Eliminate (AssociativeS t1 t2 t3    ))) = (Sum t1 (Sum t2 t3), Sum (Sum t1 t2) t3)
piType (Base (Eliminate (IdentityP t              ))) = (Product One t, t)
piType (Base (Eliminate (CommutativeP t1 t2       ))) = (Product t1 t2, Product t2 t1)
piType (Base (Eliminate (AssociativeP t1 t2 t3    ))) = (Product t1 (Product t2 t3), Product (Product t1 t2) t3)
piType (Base (Eliminate (DistributiveZero t       ))) = (Product Zero t, Zero)
piType (Base (Eliminate (DistributivePlus t1 t2 t3))) = (Product (Sum t1 t2) t3, Sum (Product t1 t3) (Product t2 t3))
piType (Base other) = swap (piType (adjoint (Base other))) where swap ~(a,b) = (b,a)
piType (Id t) = (t, t)
piType (t1 ::: t2) = (ti1, to2) where
	(ti1, to1) = piType t1
	(ti2, to2) = piType t2
	-- assume to1 == ti2
piType (t1 :+: t2) = (Sum ti1 ti2, Sum to1 to2) where
	(ti1, to1) = piType t1
	(ti2, to2) = piType t2
piType (t1 :*: t2) = (Product ti1 ti2, Product to1 to2) where
	(ti1, to1) = piType t2
	(ti2, to2) = piType t2

domainType   = fst . piType
codomainType = snd . piType

runIsoM :: IsoM a -> [(a, Constraints)]
runIsoM = runWriterT . flip evalStateT 0

newVariable :: Type -> IsoM Value
newVariable t = do
	next <- get
	put (next+1)
	return (UnificationVariable next t)

equate :: Value -> Value -> IsoM ()
equate Unit            Unit             = return ()
equate (Left  v)       (Left  v')       = equate v v'
equate (Right v)       (Right v')       = equate v v'
equate (Tuple v1 v2)   (Tuple v1' v2')  = equate v1 v1' >> equate v2 v2'
equate (Negate v)      (Negate v')      = equate v v'
equate (Reciprocate v) (Reciprocate v') = equate v v'
equate v@(UnificationVariable {}) v'    = tell [(v, v')]
equate v v'@(UnificationVariable {})    = tell [(v', v)]
equate _               _                = lift (lift [])

evalIso :: Iso -> Value -> IsoM Value
evalIso (Eliminate (IdentityS t)) v = newVariable t >>= \v' -> equate v (Right v') >> return v'
evalIso (Introduce (IdentityS t)) v = return (Right v)
evalIso (Eliminate (CommutativeS t1 t2)) v =
	    (newVariable t1 >>= \v' -> equate v (Left  v') >> return (Right v'))
	<|> (newVariable t2 >>= \v' -> equate v (Right v') >> return (Left  v'))
evalIso (Introduce (CommutativeS t1 t2)) v = evalIso (Eliminate (CommutativeS t2 t1)) v
evalIso (Eliminate (AssociativeS t1 t2 t3)) v =
	    (newVariable t1 >>= \v1 -> equate v (Left         v1 ) >> return (Left (Left  v1)))
	<|> (newVariable t2 >>= \v2 -> equate v (Right (Left  v2)) >> return (Left (Right v2)))
	<|> (newVariable t3 >>= \v3 -> equate v (Right (Right v3)) >> return (Right       v3 ))
evalIso (Introduce (AssociativeS t1 t2 t3)) v =
	    (newVariable t1 >>= \v1 -> equate v (Left (Left  v1)) >> return (Left         v1 ))
	<|> (newVariable t2 >>= \v2 -> equate v (Left (Right v2)) >> return (Right (Left  v2)))
	<|> (newVariable t3 >>= \v3 -> equate v (Right       v3 ) >> return (Right (Right v3)))
evalIso (Eliminate (IdentityP t)) v = newVariable t >>= \v' -> equate v (Tuple Unit v') >> return v'
evalIso (Introduce (IdentityP t)) v = return (Tuple Unit v)
evalIso (Eliminate (CommutativeP t1 t2)) v = do
	v1 <- newVariable t1
	v2 <- newVariable t2
	equate v (Tuple v1 v2)
	return (Tuple v2 v1)
evalIso (Introduce (CommutativeP t1 t2)) v = evalIso (Eliminate (CommutativeP t2 t1)) v
evalIso (Eliminate (AssociativeP t1 t2 t3)) v = do
	v1 <- newVariable t1
	v2 <- newVariable t2
	v3 <- newVariable t3
	equate v (Tuple v1 (Tuple v2 v3))
	return (Tuple (Tuple v1 v2) v3)
evalIso (Introduce (AssociativeP t1 t2 t3)) v = do
	v1 <- newVariable t1
	v2 <- newVariable t2
	v3 <- newVariable t3
	equate v (Tuple (Tuple v1 v2) v3)
	return (Tuple v1 (Tuple v2 v3))
evalIso (Introduce (DistributiveZero t)) v = lift (lift [])
evalIso (Eliminate (DistributiveZero t)) v = lift (lift [])
evalIso (Eliminate (DistributivePlus t1 t2 t3)) v = newVariable t3 >>= \v3 ->
	    (newVariable t1 >>= \v1 -> equate v (Tuple (Left  v1) v3) >> return (Left  (Tuple v1 v3)))
	<|> (newVariable t2 >>= \v2 -> equate v (Tuple (Right v2) v3) >> return (Right (Tuple v2 v3)))
evalIso (Introduce (DistributivePlus t1 t2 t3)) v = newVariable t3 >>= \v3 ->
	    (newVariable t1 >>= \v1 -> equate v (Left  (Tuple v1 v3)) >> return (Tuple (Left  v1) v3))
	<|> (newVariable t2 >>= \v2 -> equate v (Right (Tuple v2 v3)) >> return (Tuple (Right v2) v3))

initialize :: Term -> Value -> [MachineState]
initialize t v = [MachineState {
	forward = True,
	descending = True,
	variable = 0,
	term = t,
	output = v,
	context = Box,
	constraints = []
	}]

isFinal :: MachineState -> Bool
isFinal (MachineState { forward = True, descending = False, context = Box }) = True
isFinal _ = False

solveConstraints :: (Value, Constraints) -> [(Value, Constraints)]
solveConstraints = undefined

stepConstraints :: MachineState -> MachineState
stepConstraints = undefined

stepEval :: MachineState -> [MachineState]
stepEval m@(MachineState { forward = True, descending = True }) = case term m of
	Base iso  ->
		[ m { descending = False, variable = v, output = o, constraints = cs ++ constraints m }
		| ((o, v), cs) <- runWriterT (runStateT (evalIso iso (output m)) (variable m))
		]
	Id     t  -> [m { descending = False }]
	t1 ::: t2 -> [m { term = t1, context = Fst (context m) t2 }]
	t1 :+: t2 -> let v = UnificationVariable (variable m) in
		[ m { variable = variable m + 1, term = t1, output = v (domainType t1), context = LSum (context m) t2, constraints = (v (domainType t1), Left  (output m)) : constraints m }
		, m { variable = variable m + 1, term = t2, output = v (domainType t2), context = RSum t1 (context m), constraints = (v (domainType t2), Right (output m)) : constraints m }
		]
	t1 :*: t2 -> [m { variable = variable m + 2, term = t1, output = v1, context = LProduct (context m) t2 v2, constraints = (Tuple v1 v2, output m) : constraints m }] where
		v1 = UnificationVariable (variable m    ) (domainType t1)
		v2 = UnificationVariable (variable m + 1) (domainType t2)
stepEval m@(MachineState { forward = True, descending = False }) = case context m of
	Box -> []
	Fst  cxt t -> [m { descending = True, term = t, context = Snd (term m) cxt }]
	Snd  t cxt -> [m { term = term m ::: t, context = cxt }]
	LSum cxt t -> [m { term = term m :+: t, output = Left  (output m), context = cxt }]
	RSum t cxt -> [m { term = t :+: term m, output = Right (output m), context = cxt }]
	LProduct cxt t v -> [m { descending = True, term = t, output = v, context = RProduct (term m) (output m) cxt }]
	RProduct t v cxt -> [m { term = t :*: term m, output = Tuple v (output m), context = cxt }]
stepEval m@(MachineState { forward = False, descending = True }) = case context m of
	Box -> []
	Fst  cxt t -> [m { term = term m ::: t, context = cxt }]
	Snd  t cxt -> [m { descending = False, term = t, context = Fst cxt (term m) }]
	LSum cxt t -> [m { term = term m :+: t, output = Left  (output m), context = cxt }]
	RSum t cxt -> [m { term = t :+: term m, output = Right (output m), context = cxt }]
	LProduct cxt t v -> [m { term = term m :*: t, output = Tuple (output m) v, context = cxt }]
	RProduct t v cxt -> [m { descending = False, term = t, output = v, context = LProduct cxt (term m) (output m) }]
stepEval m@(MachineState { forward = False, descending = False }) = case term m of
	Base iso  ->
		[ m { descending = True, variable = v, output = o, constraints = cs ++ constraints m }
		| ((o, v), cs) <- runWriterT (runStateT (evalIso (adjointIso iso) (output m)) (variable m))
		]
	Id     t  -> [m { descending = True }]
	t1 ::: t2 -> [m { term = t2, context = Snd t1 (context m) }]
	t1 :+: t2 -> let v = UnificationVariable (variable m) in
		[ m { variable = variable m + 1, term = t1, output = v (codomainType t1), context = LSum (context m) t2, constraints = (v (codomainType t1), Left  (output m)) : constraints m }
		, m { variable = variable m + 1, term = t2, output = v (codomainType t2), context = RSum t1 (context m), constraints = (v (codomainType t2), Right (output m)) : constraints m }
		]
	t1 :*: t2 -> [m { variable = variable m + 2, term = t2, output = v2, context = RProduct t1 v1 (context m), constraints = (Tuple v1 v2, output m) : constraints m }] where
		v1 = UnificationVariable (variable m    ) (codomainType t1)
		v2 = UnificationVariable (variable m + 1) (codomainType t2)