module Language.PiEtaEpsilon.Examples where

import Prelude hiding ((||))
import Language.PiEtaEpsilon
import Language.PiEtaEpsilon.Pretty.Debug
import Language.PiEtaEpsilon.Parser
import Language.PiEtaEpsilon.Syntax
import Language.PiEtaEpsilon.Evaluator

import Control.Monad.Error.Class   -- to make types easier to digest
import Control.Unification.Types   -- ...
import Control.Unification.IntVar  -- ...


-- |
-- >>> adjoint notIso
-- Base (Introduce CommutativeS)
notIso = Base (Eliminate CommutativeS)


-- |
-- >>> topLevel (Base (Introduce IdentityS)) unit
-- [Right Unit]

-- |
-- >>> topLevel (Base (Introduce IdentityP)) unit
-- [Tuple Unit Unit]

-- |
-- >>> topLevel (Id) unit
-- [Unit]


-- |
-- >>> ppr $ fst $ head $ (nSteps (Id) unit) 0
-- "|v| |>| term => id, outp => Unit, ctxt => []"
--
-- |
-- >>> ppr $ fst $ head $ (nSteps (Id) unit) 1
-- "|^| |>| term => id, outp => Unit, ctxt => []"
--
-- |
-- >>> ppr $ fst $ head $ (nSteps (Id) unit) 2
-- "*** Exception: Prelude.head: empty list
-- 
-- 


-- in the paper, this is:
--         (zeroi)  ;              ;  (zeroe         )
--                                    ^^^^^^^^^^^^^^^^ -- b -> (0 + b)
--         ^^^^^^^ -- (0 + b) -> b

traceS f = prepareS ::: (Id :+: f) ::: adjoint prepareS 
traceP f = prepareP ::: (Id :*: f) ::: adjoint prepareP 

-- |
-- >>> adjoint prepareP
-- Base (Eliminate AssociativeP) ::: ((Base (Introduce SplitP) :+: Id) ::: Base (Eliminate IdentityP))
prepareP = Base (Introduce IdentityP) ::: (Base (Eliminate SplitP) :+: Id) ::: Base (Introduce AssociativeP)

-- |
-- >>> adjoint prepareS
-- Base (Eliminate AssociativeS) ::: ((Base (Introduce SplitS) :+: Id) ::: Base (Eliminate IdentityS))
prepareS = Base (Introduce IdentityS) ::: (Base (Eliminate SplitS) :+: Id) ::: Base (Introduce AssociativeS)




-- *
--   Examples from Pi
--   ================
--
--   All of the following are things I have tried to implement in dmwit's machine, but I found in
--      Pi.hs, the language for Pi.


inot = undefined

cond = undefined

controlled = undefined

cnot = controlled inot

toffoli = undefined

fredkin = undefined

peres = undefined

fullAdder = undefined


-- * Some Handy Swaps, etc

sw = undefined

sw2 = undefined

hide_unit = undefined

-- * Simple primitives on inductive types
addSub1 = undefined


-- * Iterating a list of nats

iter_ls_nat = undefined

-- * Isomorphisms over lists

ireverse = undefined

shuffle = undefined

-- * Iterating on a nat

iter_nat = undefined
iter_nat_i = undefined

evenOdd = undefined

addSubN = undefined

mult = undefined

fshuf = undefined

collect_garbage = undefined

fact = undefined

-- * Infinite loops
iso_inc = undefined
inc = undefined
dec = adjoint undefined

introF = undefined
introT = undefined

deleteF = undefined
deleteT = undefined

introZ = undefined
deleteZ = undefined

int2bool = undefined
bool2int = undefined

zero = undefined

add = undefined
mult' = undefined
cons = undefined
car = undefined
nil = undefined

introNilR = undefined
deleteNilR = undefined

duplicate = undefined

fact' = undefined

-- * Some interesting divergent functions (partial bijections)
omega0 = undefined
omega0_partial_id = undefined

omega0_id = undefined

omega1 = undefined
omega1_bool = undefined
omega1_unit = undefined


