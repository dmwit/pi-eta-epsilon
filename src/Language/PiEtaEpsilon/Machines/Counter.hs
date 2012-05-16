{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Language.PiEtaEpsilon.Machines.Counter where
import Language.PiEtaEpsilon.BNFMeta.Term

counter = [term| right trace . distribute . first unfold . trace n m |]