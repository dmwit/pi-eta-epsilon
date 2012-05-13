{-#LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Language.LBNF.Compiletime(
  -- * Happy and Alex runtimes
  HappyStk(..)
  , utf8Encode
  , Posn(Pn)
  , AlexInput
  , alexGetByte
  , ord
  , listArray
  , (!)
  , Array  
  -- * Pretty printing runtimes
  , printTree
  , doc
  , concatD
  , Print(..)
  , prPrec
  , PrintPlain(..)

  -- * Quasi quoting runtimes
  , parseToQuoter, parseToMonQuoter
  , ParseMonad(..)
  , errq
  , Q
  , BNFC_QQType(..), appEPAll, appEPAllL, fromString, fromLit, fromToken, fromPositionToken
  , Lift (..)
  , printAq
  , stringAq
  
  ) where

import Language.LBNF.Runtime
import Text.Happy.Quote(HappyStk(..))
import Data.Array(listArray, (!), Array)
import Data.Char
import qualified Data.Bits
import Data.Word(Word8)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Control.Monad ((>=>),liftM)
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse

data Posn = Pn !Int !Int !Int
      deriving (Eq, Show,Ord)

type AlexInput = (Posn,     -- current position,
                  Char,         -- previous char
                  [Word8],       -- pending bytes on current char
                  String)       -- current input string

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,c,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c 
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]



-- import qualified Language.Haskell.Exts.Parser as Hs


data BNFC_QQType = 
  QQApp (String,LocType) [BNFC_QQType] | 
  QQAq (Q Exp, Q Pat) |
  QQList [BNFC_QQType] |
  QQLit Lit |
  QQPosT (Int,Int) (String,LocType) String


type LocType = (String,String)


errq :: (String -> a) -> ParseMonad a -> Q a
errq e = return . err e



-- appEAll :: [TH_Exp] -> TH_Exp
appEPAll :: LocType -> String -> [BNFC_QQType] -> BNFC_QQType
appEPAll loc s l = QQApp (s,loc) l


appEPAllL :: LocType -> [BNFC_QQType] -> BNFC_QQType
appEPAllL loc l = QQList l


class Literal a where
  lit :: a -> Lit
  
instance Literal Double where
  lit = RationalL . toRational

instance Literal Integer where
  lit = IntegerL

instance Literal Char where
  lit = CharL

class IsChar a where
  toChar :: a -> Char
instance IsChar Char where
  toChar = id

instance IsChar a => Literal [a] where
  lit = StringL . map toChar

fromLit :: Literal a => LocType -> a -> BNFC_QQType
fromLit l a = QQLit $ lit a


fromString l s = fromLit l s -- (litE $ StringL s,litP $ StringL s)


fromToken l t s = QQApp (t,l) [QQLit $ lit s]
--    (
--    appE (mkGName l t >>= conE)(litE $ StringL s), 
--    mkGName l t >>= flip conP [litP $ StringL s]
--    )

fromPositionToken :: LocType -> String -> ((Int,Int),String) -> BNFC_QQType
fromPositionToken l t v@(pos,s) = QQPosT pos (t,l) s


qualify "" f     = f
qualify _ f@"[]" = f
qualify _ f@":"  = f
qualify m  f     = m ++ "." ++ f

-- Dynamic names
-- mkGName :: LocType -> String -> Q Name
-- mkGName (p,m) s = return $ mkName $ qualify m s 

-- Static names
mkGName (p,m) ":" = return $ mkName ":"
mkGName (p,m) "[]" = return $ mkName "[]"
mkGName (p,m) n = return $ Name (mkOccName n) $ 
    NameG DataName (mkPkgName $ p) (mkModName $ m)



parseToQuoter :: (String -> ParseMonad BNFC_QQType) -> QuasiQuoter
parseToQuoter p = QuasiQuoter {
  quoteExp = handle . p >=> toQExp,
  quotePat = handle . p >=> toQPat
  }

parseToMonQuoter :: (String -> ParseMonad BNFC_QQType) -> QuasiQuoter
parseToMonQuoter p = QuasiQuoter {
  quoteExp = handle . p >=> toQMExp,
  quotePat = handle . p >=> toQPat
  }

 -- {quoteExp = fst . handle . p, quotePat = snd . handle . p}

toQExp :: BNFC_QQType -> Q Exp
toQExp qq = case qq of
  QQApp (s,l) qs         -> do 
    const <- mkGName l s 
    foldl appE (conE const) (map toQExp qs)
  QQAq p                 -> fst p
  QQList qs              -> mapM toQExp qs >>= \qs' -> case qs' of
    [ListE es, e]          -> listE (map return $ es ++ [e])
    [ConE _,e]             -> listE $ [return e]
    a                      -> listE $ map return a
  QQLit l                -> litE l
  QQPosT pos (t,l) s     -> do
    constr <- mkGName l t 
    appE (conE constr) (lift (pos,s))

toQMExp :: BNFC_QQType -> Q Exp
toQMExp qq = case qq of
  QQApp (s,l) qs         -> do 
    const <- mkGName l s 
    foldl mAppE (returnE $ conE const) (map toQMExp qs)
  QQAq p                 -> fst p
  QQList qs              -> mapM toQMExp qs >>= \qs' -> case qs' of
    [ListE es, e]          -> sequenceE $ listE (map return $ es ++ [e])
    [ConE _,e]             -> sequenceE $ listE $ [return e]
    a                      -> sequenceE $ listE $ map return a
  QQLit l                -> returnE $ litE l
  QQPosT pos (t,l) s     -> do
    constr <- mkGName l t 
    returnE $ appE (conE constr) (lift (pos,s))

returnE = appE (varE 'return)
sequenceE = appE (varE 'sequence)

mAppE :: Q Exp -> Q Exp -> Q Exp
mAppE mf ma = [| $mf >>= flip liftM $ma |]




toQPat :: BNFC_QQType -> Q Pat
toQPat qq = case qq of
  QQApp (s,l) qs         -> do 
    const <- mkGName l s 
    conP const (map toQPat qs)
  QQAq p                 -> snd p
  QQList qs              -> mapM toQPat qs >>= \qs' -> case qs' of
    [p,ListP ps]           -> listP $ map return $ p : ps
    [x]                    -> listP [return x] 
  QQLit l                -> litP l
  QQPosT (p1,p2) (t,l) s -> mkGName l t >>= flip conP 
      [tupP [
        tupP [litP $ IntegerL $ toInteger p1, litP $ IntegerL $ toInteger p2],
        litP (lit s)
        ]]



printAq :: Print a => a -> BNFC_QQType
printAq a = stringAq $ printTree a

stringAq :: String -> BNFC_QQType
stringAq s = QQAq (
  either error return . parseExp $ s, 
  either error return . parsePat $ s)


handle :: ParseMonad BNFC_QQType -> Q BNFC_QQType
handle (Bad s) = fail s
handle (Ok a)  = return a

  
