{-# LANGUAGE FlexibleContexts #-}
module Language.PiEtaEpsilon.ConstraintSolver where

import Language.PiEtaEpsilon.Syntax
import Control.Monad.Writer
import Data.Graph.Inductive.Graph
import Data.List
import Prelude hiding (Left, Right)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Prelude  as P

type Constraints = [(Value, Value)]

-- | explode general equalities down to equalities of the form @UnificationVariable n t = v@
equate :: MonadWriter Constraints m => Value -> Value -> m ()
equate Unit            Unit             = return ()
equate (Left  v)       (Left  v')       = equate v v'
equate (Right v)       (Right v')       = equate v v'
equate (Tuple v1 v2)   (Tuple v1' v2')  = equate v1 v1' >> equate v2 v2'
equate (Negate v)      (Negate v')      = equate v v'
equate (Reciprocate v) (Reciprocate v') = equate v v'
equate v@(UnificationVariable {}) v'    = tell [(v , v')]
equate v v'@(UnificationVariable {})    = tell [(v', v )]
equate v               v'               = fail $ "variable heads don't match: " ++ show v ++ " and " ++ show v'

class NodeSource a where getNodes :: a -> [LNode Type]
class EdgeSource a where getEdges :: a -> [Edge]

instance NodeSource Value where
	getNodes Unit            = []
	getNodes (Left  v)       = getNodes v
	getNodes (Right v)       = getNodes v
	getNodes (Tuple v1 v2)   = getNodes v1 ++ getNodes v2
	getNodes (Negate v)      = getNodes v
	getNodes (Reciprocate v) = getNodes v
	getNodes (UnificationVariable n t) = [(n,t)]

instance (NodeSource a, NodeSource b) => NodeSource (a,b) where getNodes (a,b) = getNodes a ++ getNodes b
instance (NodeSource a, NodeSource b) => EdgeSource (a,b) where
	getEdges (a,b) = do
		(na, ta) <- getNodes a
		(nb, tb) <- getNodes b
		return (na, nb)

instance NodeSource a => NodeSource [a] where getNodes = M.toList . M.fromList . concatMap getNodes
instance EdgeSource a => EdgeSource [a] where getEdges = S.toList . S.fromList . concatMap getEdges

getGraph :: (NodeSource a, EdgeSource a, Graph gr) => a -> gr Type ()
getGraph a = mkGraph (getNodes a) [(na, nb, ()) | (na, nb) <- getEdges a]

-- | remove all constraints of the form UnificationVariable n t = UnificationVariable n' t' (and n /= n')
substitute :: Constraints -> Constraints
substitute cs = undefined where
	(simplifications, structural) = partition (uncurry simple) cs

	-- just assume that t == t', hope this doesn't bite us later
	simple (UnificationVariable n t) (UnificationVariable n' t') = n /= n'
	simple _ _ = False
