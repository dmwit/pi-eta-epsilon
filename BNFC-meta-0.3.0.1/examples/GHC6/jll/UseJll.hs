{-# LANGUAGE QuasiQuotes #-}
import JavaletteLight
import Language.LBNF.Runtime -- overloaded pretty-printing function
import Prelude hiding (exp)

{- This Javalette Light program is parsed at compile time, 
and replaced by it's abstract syntax representation.
The 'holes' in square brackets are anti-quoted Haskell 
expression. 

The QuasiQuoter prog is generated from the grammar in JavaletteLight.hs
(it corresponds to the category Prog).
-}


prg x v e = [$prog|
int f() {
 int a; 
 [:SWhile (EInt 10 :: Expr) [x]:]
 int a;
 int [:v:];
 int tmp;
 while (n < [Expr:e:]) {
   n = n + 1;
   tmp = a + b;
   a = b;
   b = tmp;
 }
}
|] 

st v = [$stm| [:v:] = 1; |]
pr = prg (st (Ident "n")) (Ident "n") [$expr|n|]
main = putStr $ printTree pr


eval vs = eval' where
  eval' [$expr| [:a:] < [:b:]  |] = if eval' a < eval' b then 1 else 0
  eval' [$expr| [:a:] + [:b:]  |] = eval' a + eval' b
  eval' [$expr| [:a:] * [:b:]  |] = eval' a * eval' b
  eval' [$expr| [Integer: n:]  |] = fromInteger n
  eval' [$expr| [Double:  n:]  |] = n
  eval' [$expr| [Ident:   v:]  |] = varval v
  varval v = maybe (error $ "undefined variable" ++ printTree v) id $ flip lookup vs v

h = eval [(Ident "a",5)] [$expr|a+2+3*4|]
