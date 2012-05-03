module Language.PiEtaEpsilon.QuasiQuoter where
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Data.Typeable
import Data.Data
import Language.PiEtaEpsilon.Parser

typ :: QuasiQuoter
typ = QuasiQuoter quoteExprExp quoteExprPat undefined undefined

--quoteExprExp :: (Data s, Show s,  Typeable s) => SymParser () s -> String -> ExpQ
quoteExprExp r =  do  
    loc <- location
    parsed_expr <- case parseType r of
                    Left err  -> fail $ (show err ++ " at file " ++ loc_filename loc 
                              ++ " at line " ++ (show $ snd $ loc_start loc) ++ 
                              " at col " ++ (show $ snd $ loc_start loc))
                    Right e   -> return e
    dataToExpQ (const Nothing) $ parsed_expr
    
quoteExprPat = undefined

{- TODO    
data MetaType = MZero 
              | MOne 
              | MSum Type Type 
              | MProduct Type Type 
              | MNegative Type
              | MReciprocal Type 
              | AntiType String
              | AntiOne  ()
              | AntiZero Void
             deriving (Eq, Ord, Show, Read)
-}

      
