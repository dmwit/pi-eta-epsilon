module Language.PiEtaEpsilon.Examples where

import Language.PiEtaEpsilon.Syntax

notIso = Base (Eliminate CommutativeS)
traceS f = prepare ::: (Id :+: f) ::: adjoint prepare where prepare = Base (Introduce IdentityS) ::: (Base (Eliminate SplitS) :+: Id) ::: Base (Introduce AssociativeS)
traceP f = prepare ::: (Id :*: f) ::: adjoint prepare where prepare = Base (Introduce IdentityP) ::: (Base (Eliminate SplitP) :+: Id) ::: Base (Introduce AssociativeP)
