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


module Language.PiEtaEpsilon.Interactive.CmdLine
( peeCmdLine
) where

import Data.IORef
import Numeric
import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Map as Map
import System.IO
import System.Exit
import System.Console.GetOpt
import Text.ParserCombinators.Parsec (runParser)

import Language.PiEtaEpsilon.Interactive.Shell
import Language.PiEtaEpsilon.Interactive.Version
import Language.PiEtaEpsilon.Interactive.StatementParser
import Language.PiEtaEpsilon.Syntax hiding (Left, Right)
import Language.PiEtaEpsilon.Evaluator
import Debug.Trace.Utils
import Language.PiEtaEpsilon.Pretty.REPL
import Language.PiEtaEpsilon.Pretty.Class


-----------------------------------------------------------------------
-- Main entry point for the command line tool
main = peeCmdLine []

-- | Parse command line options and run the pee shell
peeCmdLine :: [String] -> IO ()
peeCmdLine argv =
   do st <- parseCmdLine argv
      case (cmd_print st) of
         PrintNothing    -> doCmdLine st
         PrintHelp       -> putStr (printUsage usageNotes)
         PrintVersion    -> putStr versionInfo
         PrintNoWarranty -> putStr noWarranty
         PrintGPL        -> putStr gpl


doCmdLine :: PeeCmdLineState -> IO ()
doCmdLine st =
   case (cmd_input st) of
       Just expr -> evalInput st expr
       Nothing ->
           if (cmd_stdin st)
              then evalStdin st
              else runShell st

--------------------------------------------------------------
-- Holds important values parsed from the command line

data PeeCmdLineState
   = PeeCmdLineState
     { cmd_reverse  :: Bool
     , cmd_stdin    :: Bool
     , cmd_input    :: Maybe String
     , cmd_binds    :: Bindings
     , cmd_print    :: PrintWhat
     , cmd_history  :: Maybe String
     }

initialCmdLineState =
  PeeCmdLineState
  { cmd_reverse  = True
  , cmd_stdin    = False
  , cmd_input    = Nothing
  , cmd_binds    = Map.empty
  , cmd_print   = PrintNothing
  , cmd_history = Just "pee.history"
  }

data PrintWhat
   = PrintNothing
   | PrintVersion
   | PrintHelp
   | PrintNoWarranty
   | PrintGPL

-------------------------------------------------------------
-- Set up the command line options

data PeeCmdLineArgs
  = Reverse
  | ReadStdIn
  | Program String
  | Print PrintWhat
  | History String
  | NoHistory

options :: [OptDescr PeeCmdLineArgs]
options =
  [ Option ['r']     ["reverse"]     (NoArg Reverse)              "perform evaluation in reverse"
  , Option ['s']     ["stdin"]       (NoArg ReadStdIn)               "read from standard in"
  , Option ['e']     ["program"]     (ReqArg Program "PROGRAM")      "evaluate statements from command line"
  , Option ['h','?'] ["help"]        (NoArg (Print PrintHelp))       "print this message"
  , Option ['v']     ["version"]     (NoArg (Print PrintVersion))    "print version information"
  , Option ['g']     ["gpl"]         (NoArg (Print PrintGPL))        "print the GNU GPLv2, under which this software is licensed"
  , Option ['w']     ["nowarranty"]  (NoArg (Print PrintNoWarranty)) "print the warranty disclamer"
  , Option ['w']     ["history"]     (ReqArg History "HISTORY_FILE")  "set the command history file (default: 'pee.history')"
  , Option ['q']     ["nohistory"]   (NoArg NoHistory)                "disable command history file"

  ]



-----------------------------------------------------------------
-- Parser for the command line
-- yeah, I know its ugly

parseCmdLine :: [String] -> IO PeeCmdLineState
parseCmdLine argv =
   case getOpt RequireOrder options argv of
     (opts,files,[]) -> (foldl (>>=) (return initialCmdLineState) $ map applyFlag opts) >>= \st ->
                        (foldl (>>=) (return st)                  $ map loadDefs files)

     (_,_,errs)      -> fail (errMsg errs)

  where errMsg errs = printUsage (concat (intersperse "\n" errs))

        applyFlag :: PeeCmdLineArgs -> PeeCmdLineState -> IO PeeCmdLineState
        applyFlag Reverse               st = return st{ cmd_reverse  = True }
        applyFlag ReadStdIn             st = return st{ cmd_stdin   = True }
        applyFlag NoHistory             st = return st{ cmd_history = Nothing }
        applyFlag (History nm)          st = return st{ cmd_history = Just nm }
        applyFlag (Print printWhat)     st = return st{ cmd_print   = printWhat }
        applyFlag (Program pgm)         st = case cmd_input st of
                                                Nothing -> return st{ cmd_input = Just pgm }
                                                _       -> fail (errMsg ["'-e' option may only occur once"])

-----------------------------------------------------------------------
-- Actually run the shell

mapToShellState :: PeeCmdLineState -> PeeShellState
mapToShellState st =
  initialShellState
  { letBindings = cmd_binds st
  , forwards    = cmd_reverse st
  , histFile    = cmd_history st
  }

runShell :: PeeCmdLineState -> IO ()
runShell st = do
--   putStrLn versionInfo
--   putStrLn shellMessage
   peeShell (mapToShellState st)
   return ()



--------------------------------------------------------------------------
-- For dealing with input from stdin or the command line

evalStdin :: PeeCmdLineState -> IO ()
evalStdin st = hGetContents stdin >>= evalInput st

evalInput :: PeeCmdLineState -> String -> IO ()
evalInput st expr = do
    exitCode <- newIORef ExitSuccess
    case pStatement expr of
       Left msg    -> fail (show msg)
       Right stmt -> evalStmt exitCode st stmt 
    code <- readIORef exitCode
    exitWith code

setSucc :: IORef ExitCode -> IO ()
setSucc ec = writeIORef ec ExitSuccess

setFail :: IORef ExitCode -> IO ()
setFail ec = writeIORef ec (ExitFailure 100)

evalStmt :: IORef ExitCode -> PeeCmdLineState -> Statement -> IO PeeCmdLineState
evalStmt ec st (Stmt_eval t v)     = evalTerm st t v >> setSucc ec >> return st
--evalStmt ec st (Stmt_isEq t1 t2) = compareTerms ec st t1 t2 >> return st
evalStmt ec st (Stmt_let name t) = setSucc ec >> return st{ cmd_binds = Map.insert name t (cmd_binds st) }
evalStmt ec st (Stmt_empty)      = setSucc ec >> return st


evalTerm :: PeeCmdLineState -> Term -> UValue -> IO ()
evalTerm st t v = putStrLn . ppr $ topLevelWithState (cmdLineStateToMachineState st) 
        (traceItNote "term2" t) (traceItNote "value2" v)

cmdLineStateToMachineState = error "cmdLineStateToMachineState"

compareTerms :: IORef ExitCode
            -> PeeCmdLineState
            -> Term
            -> Term
            -> IO ()
compareTerms = error "compareTerms"
--compareTerms ec st t1 t2 = do
--  if normalEq (cmd_binds st) t1 t2
--     then putStrLn "equal"     >> setSucc ec
--     else putStrLn "not equal" >> setFail ec


-------------------------------------------------------------------------
-- Read definitions from a file

loadDefs :: FilePath -> PeeCmdLineState -> IO PeeCmdLineState
loadDefs = error "loadDefs not defined"
--loadDefs path st = do
--     let parseSt = PeeParseState (cmd_cps st) (cmd_extsyn st)
--    binds <- readDefinitionFile parseSt (cmd_binds st) path
--     return st{ cmd_binds = Map.union binds (cmd_binds st) }


-----------------------------------------------------------------------
-- Printing stuff

printUsage :: String -> String
printUsage str = unlines
   [ ""
   , ""
   , usageInfo "usage: peeShell {<option>} [{<file>}]\n" options
   , ""
   , ""
   ,str
   , ""
   ]

usageNotes :: String
usageNotes = unlines
   [ "Any files listed after the options will be parsed as a series of"
   , "\"let\" definitions, which will be in scope when the shell starts"
   , "(or when the -e expression is evaluated)"
   ]
