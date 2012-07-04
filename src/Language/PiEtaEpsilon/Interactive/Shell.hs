{-# LANGUAGE MultiParamTypeClasses #-}
{-
 -   The Pee Shell, an interactive environment for evaluating pure untyped pee terms.
 -   Copyright (C) 2005-2007, Robert Dockins
 -
 -   This program is free software; you can redistribute it and/or modify
 -   it under the terms of the GNU General Public License as published by
 -   the Free Software Foundation; either version 2 of the License, or
 -   (at your option) any later version.
 -
 -   This program is distributed in the hope that it will be useful,
 -   but WITHOUT ANY WARRANTY; without even the implied warranty of
 -   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 -   GNU General Public License for more details.
 -
 -   You should have received a copy of the GNU General Public License
 -   along with this program; if not, write to the Free Software
 -   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 -}


module Language.PiEtaEpsilon.Interactive.Shell where

import Control.Monad.Trans
import System.IO
import Data.List (isPrefixOf)

import Language.PiEtaEpsilon.Evaluator
import Language.PiEtaEpsilon.Syntax hiding (Left, Right)
import Language.PiEtaEpsilon.Parser.Term
import Language.PiEtaEpsilon.Interactive.Version

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec (runParser)

import Language.PiEtaEpsilon.Pretty.REPL
import Language.PiEtaEpsilon.Pretty.Class
import System.Console.Shell
import System.Console.Shell.ShellMonad
--import System.Console.Shell.Backend.Readline
import System.Console.Shell.Backend.Haskeline
import Language.PiEtaEpsilon.Interactive.StatementParser
import Language.LBNF.Runtime
import Data.Default

defaultBackend = haskelineBackend

type Bindings = Map.Map String Term



-------------------------------------------------------
-- Define types to allow completion of let-bound names

completeLetBindings :: PeeShellState -> String -> IO [String]
completeLetBindings st prefix =
    return . filter (prefix `isPrefixOf`) . Map.keys . letBindings $ st

data LetBinding = LetBinding

instance Completion LetBinding PeeShellState where
  complete _         = completeLetBindings
  completableLabel _ = "<name>"

----------------------------------------------------------
-- Define the shell state

-- | Keeps track of all the state that is needed for the
--   operation of the pee shell.

data PeeShellState =
  PeeShellState
  { letBindings :: Map.Map String Term
                             -- ^ All \"let\" bindings currently in scope
  , forwards    :: Bool      -- ^ Which direction should we evaluate
  , histFile    :: Maybe String -- ^ A file for command history
  }
  deriving(Show, Eq)


-- | Default settings for all elements of shell state.
initialShellState =
  PeeShellState
  { letBindings = Map.empty
  , forwards    = True
  , histFile    = Nothing
  }

-----------------------------------------------------------------
-- Main entry point to the shell

-- | Run an interactive shell starting with the
--   given shell state and returning the final,
--   possibly modified, state.
peeShell :: PeeShellState -> IO PeeShellState
peeShell init = do
    let
      desc =
         (mkShellDescription commands evaluate)
         { defaultCompletions = Just completeLetBindings
         , historyFile        = histFile init
         , greetingText       = Just (versionInfo ++ shellMessage)
         , secondaryPrompt    = Just $ \_ -> return "] "
         }
    runShell desc defaultBackend init



-----------------------------------------------------------------
-- read definitions from a file
-- A few things to note. Since there is no subsitution, I completely ignore
-- the bindings, because we don't perform any subsitution
readDefinitionFile :: Bindings
                   -> String
                   -> IO (Bindings)
readDefinitionFile = error "readDefinitionFile"
--readDefinitionFile _ file = do
--    str  <- openFile file ReadMode >>= hGetContents
--    --TODO add back the comment stripping
--    case parseTerm str of
--        Bad err -> fail (show err)
--        Ok  b' -> return b'

----------------------------------------------------------------
-- Definition of all the shell commands

commands :: [ShellCommand PeeShellState]
commands =
  [ exitCommand "quit"
  , exitCommand "exit"
  , helpCommand "help"
  , toggle "reverse"   "Toggle the direction mode"  forwards (\x st -> st { forwards = x })

  , cmd "showall"    showBindings    "Show all let bindings"
  , cmd "show"       showBinding     "Show a let binding"
  , cmd "load"       loadDefFile     "Load definitions from a file"
  , cmd "clear"      clearBindings   "Clear all let bindings"

  , cmd "nowarranty" (shellPutInfo noWarranty)  "Print the warranty disclaimer"
  , cmd "gpl"        (shellPutInfo gpl)         "Print the GNU GPLv2, under which this software is licensed"
  , cmd "version"    (shellPutInfo versionInfo) "Print version info"
  ]


showBinding :: Completable LetBinding -> Sh PeeShellState ()
showBinding (Completable name) = do
    st <- getShellSt
    case Map.lookup name (letBindings st) of
        Nothing -> shellPutErrLn  $ concat ["'",name,"' not bound"]
        Just t  -> shellPutInfoLn $ concat [name," = ", ppr t]

showBindings :: Sh PeeShellState ()
showBindings = do
   st <- getShellSt
   shellPutStrLn $
     Map.foldWithKey
       (\name t x -> concat [name," = ", ppr t,"\n",x])
       ""
       (letBindings st)

clearBindings :: Sh PeeShellState ()
clearBindings = modifyShellSt (\st -> st{ letBindings = Map.empty })

loadDefFile :: File -> Sh PeeShellState ()
loadDefFile = error "loadDefFile"
--loadDefFile (File path) = do
--   st <- getShellSt
--   newBinds <- liftIO $ readDefinitionFile path
--   putShellSt st{ letBindings = newBinds }


----------------------------------------------------------------
-- Normal statement evaluation

evaluate :: String -> Sh PeeShellState ()
evaluate str = do
  case reverse str of
   '@':_ -> shellSpecial (ShellContinueLine (init str))
   _ -> do
      st <- getShellSt
      case pStatement str of
        Left err   -> shellPutErrLn ("parse error " ++ show err)
        Right stmt ->
          case stmt of
           Stmt_eval expr value -> evalExpr expr value
           Stmt_let nm expr     -> modifyShellSt (\st -> st{ letBindings = Map.insert nm expr (letBindings st) })
           Stmt_empty           -> return ()


evalExpr :: Term -> UValue -> Sh PeeShellState ()
evalExpr t v = getShellSt >>= \st -> eval t v st
 where
   eval t' v' st' = do
      let z = topLevelWithState (shellStateToMachineState st') t' v'
      shellPutStrLn $ ppr z

shellStateToMachineState :: PeeShellState -> MachineState
shellStateToMachineState (PeeShellState _ forwards _) = def {forward = forwards }

