module Language.PiEtaEpsilon.Examples where

import Prelude hiding ((||))
import Language.PiEtaEpsilon
import Language.PiEtaEpsilon.Parser
import Language.PiEtaEpsilon.Syntax
import Language.PiEtaEpsilon.Evaluator


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

-- >>> topLevel (Id) unit
-- [Unit]

-- >>> ppr $  fst $ head $ (nSteps (Id) unit) 0
-- "<id, Unit, []>|>"

-- |
-- >>> ppr $  fst $ head $ (nSteps (Id) unit) 1
-- "[id, Unit, []]|>"

-- |
-- >>> ppr $  fst $ head $ (nSteps (Id) unit) 2
-- "*** Exception: Prelude.head: empty list

-- |
-- >>> ppr $  fst $ head $ (nSteps (Id) unit) 0
-- "<id, Unit, []>|>"

-- |
-- >>> ppr $  fst $ head $ (nSteps (Id) unit) 1
-- "[id, Unit, []]|>"


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


