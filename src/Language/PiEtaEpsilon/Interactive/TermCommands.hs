{-# LANGUAGE GADTs, KindSignatures, 
    TypeOperators, TemplateHaskell, QuasiQuotes #-}
module Language.PiEtaEpsilon.Interactive.TermCommands where


data Path = PLeft     --::= "l"    
          | PRight    --::= "r"    
          | PDown     --::= "d"    
          | PUp       --::= "u"    
          
type PathList = [Path]
          
data TermCmd = TDelete          --::= "del"  
             | TSet    Term     --::= "set" 
             | TInsert Term     --::= "ins"  
             | TMove   PathList --::= "mv"   

-- make the parser

-- make the evaluator

data State = State {
        _location :: PathList,
        _term     :: Term
    }
        
