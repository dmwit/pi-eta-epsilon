module Language.PiEtaEpsilon.Evaluator where
import Language.PiEtaEpsilon.Syntax
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer hiding (Product(..), Sum(..))
import Prelude hiding (Either(..))
import GHC.Generics hiding ((:*:))

data Context = Box | Fst Context Term | Snd Term Context | LProduct Context Term Value | RProduct Term Value Context | LSum Context Term | RSum Term Context deriving (Eq, Ord, Show, Read)
data MachineState = MachineState {
	forward :: Bool,
	descending :: Bool,
	variable :: Int,
	term :: Term,
	context :: Context,
	output :: [(Value, Constraints)]
	} deriving (Eq, Ord, Show, Read)
type Constraints = [(Value, Value)]
type IsoM = StateT Int (WriterT Constraints [])

adjoint :: Term -> Term
adjoint (Base (Eliminate b)) = Base (Introduce b)
adjoint (Base (Introduce b)) = Base (Eliminate b)
adjoint (Id t) = Id t
adjoint (t1 ::: t2) = adjoint t2 ::: adjoint t1
adjoint (t1 :+: t2) = adjoint t1 :+: adjoint t2
adjoint (t1 :*: t2) = adjoint t1 :*: adjoint t2

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

initialize :: Term -> Value -> MachineState
initialize t v = MachineState {
	forward = True,
	descending = True,
	variable = 0,
	term = t,
	context = Box,
	output = [(v, [])]
	}

solveConstraints :: (Value, Constraints) -> [(Value, Constraints)]
solveConstraints = undefined

stepEval :: MachineState -> MachineState
stepEval = undefined

stepConstraints :: MachineState -> MachineState
stepConstraints = undefined