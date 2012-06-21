-- (C) Copyright Chris Banks 2011-2012

-- This file is part of The Continuous Pi-calculus Workbench (CPiWB). 

--     CPiWB is free software: you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation, either version 3 of the License, or
--     (at your option) any later version.

--     CPiWB is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.

--     You should have received a copy of the GNU General Public License
--     along with CPiWB.  If not, see <http://www.gnu.org/licenses/>.

import CpiLib
import CpiParser
import CpiSemantics
import CpiODE
import CpiPlot

import System.Console.Haskeline
import Control.Monad.State

import qualified Data.List as L


-- Some configurables:
welcome = "\nWelcome to the Continuous Pi-calculus Workbench (CPiWB).\n"
          ++"Type \"help\" for help.\n"
prompt = "CPiWB:> "

-- Our environment will be a stack of the Haskeline,
-- State transformer (of CPi Definitions), and IO monads:
type Environment = InputT (StateT Env IO)

-- Main function:
main :: IO ()
main = do putStrLn welcome;
          evalStateT (runInputT defaultSettings loop) []
              where 
                loop :: Environment ()
                loop = do input <- getInputLine prompt
                          case input of
                            Nothing -> return ()
                            Just "" -> loop
                            Just "quit" -> return ()
                            Just i -> do doCommand i;
                                         loop
-- TODO: command autocomplete (see Haskeline docs).
--       can we use the command map?

doCommand :: String -> Environment ()
doCommand cmdln = let cmd = head $ words cmdln in
                  case (lookup cmd commands) of
                    Nothing -> say "Try again."
                    Just x  -> (cmdFn x) cmdln

---------------
-- Command map:
---------------

-- TODO: document how to add new commands

data CmdRec = CmdRec {cmdFn::String->Environment (),
                      cmdHelp::(String,String)}

commands :: [(String,CmdRec)]
commands = [("help",
             CmdRec {cmdFn = helpCmd,
                     cmdHelp = helpTextHelp}),
            ("quit",
             CmdRec {cmdFn = undefined,
                     cmdHelp = helpTextQuit}),
            ("load",
             CmdRec {cmdFn = loadCmd,
                     cmdHelp = helpTextLoad}),
            ("env",
             CmdRec {cmdFn = envCmd,
                     cmdHelp = helpTextEnv}),
            ("clear",
             CmdRec {cmdFn = clearCmd,
                     cmdHelp = helpTextClear}),
            ("species",
             CmdRec {cmdFn = speciesCmd,
                     cmdHelp = helpTextSpecies}),
            ("process",
             CmdRec {cmdFn = processCmd,
                     cmdHelp = helpTextProcess}),
            ("trans",
             CmdRec {cmdFn = transCmd,
                     cmdHelp = helpTextTrans}),
            ("odes",
             CmdRec {cmdFn = odesCmd,
                     cmdHelp = helpTextOdes}),
            ("plot",
             CmdRec {cmdFn = plotCmd,
                     cmdHelp = helpTextPlot}),
            ("plotfile",
             CmdRec {cmdFn = plotFileCmd,
                     cmdHelp = helpTextPlotFile})]

-- TODO: * delete a specific defn cmd
--       * network cmd (need to parameterise in syntax first)
--      ** semantics/equivalences/model checking/ODE/trans.graph/etc...

---------------------
-- Command Functions:
---------------------

-- help Command
helpCmd :: String -> Environment ()
helpCmd x 
    | not(null(param x)) 
        = case (lookup (param x) commands) of
            Nothing -> say $ "Sorry no help on \""++x++"\"."
            Just r -> let (c,d) = cmdHelp r in
                      say $ "\n"++c++"\n\t"++d++"\n"
    | otherwise
        = say $ "\nThe available commands are:\n"
          ++"\n"++prettyList (map (\(x,_) -> x) commands)++"\n\n"
          ++"Type \"help <command>\" for help on a specific command.\n"

-- load Command
loadCmd :: String -> Environment ()
loadCmd x = do say $ "Loading: "++(param x);
               f <- getFile (param x);
               case parseFile f of
                 Left err -> say $ "Parse error at:\n"++(show err)
                 Right ds -> do putEnv ds;
                                say "Done. Type \"env\" to view."
-- TODO: maybe a flag to append to Env rather than overwrite?

-- env Command
envCmd :: String -> Environment ()
envCmd _ = do s <- getEnv;
              say $ prettys $ L.sort s

-- species Command
speciesCmd :: String -> Environment ()
speciesCmd x = case parseDefn x of
                 Left err -> say $ "Parse error at:\n"++(show err)
                 Right x  -> do addEnv x;
                                say $ pretty x

-- process Command
processCmd :: String -> Environment ()
processCmd = speciesCmd

-- clear Command
clearCmd :: String -> Environment ()
clearCmd _ = putEnv []

-- trans Command
transCmd :: String -> Environment ()
transCmd x = do env <- getEnv;
                case lookupProcName env (param x) of
                  Nothing   -> say $ "Process \""++(param x)
                               ++"\" is not in the Environment."
                  Just proc -> do let mts = processMTS env proc;
                                  say $ pretty mts

-- odes Command
odesCmd :: String -> Environment ()
odesCmd x = do env <- getEnv
               case lookupProcName env (param x) of
                 Nothing   -> say $ "Process \""++(param x)
                              ++"\" is not in the Environment."
                 Just proc -> do let mts = processMTS env proc
                                 let proc' = wholeProc env proc mts
                                 let dpdt = dPdt' env mts proc'
                                 say $ prettyODE env dpdt
                                 -- TODO: replace prettyMap with something
                                 --   that prints the ODEs nicely.

-- plot Command
plotCmd :: String -> Environment ()
plotCmd x = do env <- getEnv;
               let args = words x
               -- TODO: properly parse the command!
               --       and have some defaults?
               let res = read(args!!4)
               let start = read(args!!2)
               let end = read(args!!3)
               case lookupProcName env (args!!1) of
                 Nothing   -> say $ "Process \""++(args!!1)
                              ++"\" is not in the Environment."
                 Just proc -> do let mts = processMTS env proc
                                 let proc' = wholeProc env proc mts
                                 let dpdt = dPdt' env mts proc'
                                 let odes = xdot env dpdt
                                 let inits = initials env proc' dpdt
                                 let ts = timePoints res (start,end)
                                 let solns = solveODE env proc' dpdt (res,(start,end))
                                 let ss = speciesIn env dpdt
                                 let ss' = speciesInProc proc
                                 lift$lift$plotTimeSeriesFiltered ts solns ss ss'

-- plotFile Command
plotFileCmd :: String -> Environment ()
plotFileCmd x = do env <- getEnv;
                   let args = words x
                   -- TODO: properly parse the command!
                   --       and have some defaults?
                   let res = read(args!!4)
                   let start = read(args!!2)
                   let end = read(args!!3)
                   let file = args!!5
                   case lookupProcName env (args!!1) of
                     Nothing   -> say $ "Process \""++(args!!1)
                                  ++"\" is not in the Environment."
                     Just proc -> do let mts = processMTS env proc
                                     let proc' = wholeProc env proc mts
                                     let dpdt = dPdt' env mts proc'
                                     let odes = xdot env dpdt
                                     let inits = initials env proc' dpdt
                                     let ts = timePoints res (start,end)
                                     let solns = solveODE env proc' dpdt (res,(start,end))
                                     let ss = speciesIn env dpdt
                                     let ss' = speciesInProc proc
                                     lift$lift$plotTimeSeriesToFileFiltered ts solns ss ss' file
                                 


----------------------
-- Command help texts:
----------------------

helpTextHelp = ("help <command>","Shows help on a specific command.")
helpTextQuit = ("quit","Quits the session, same as Ctrl+D")
helpTextLoad = ("load <filename>","Loads a CPi definition file.")
helpTextEnv = ("env","Shows the contents of the current environment.")
helpTextSpecies = ("species <definition>","Adds a species definition to the "
                   ++"current environment.")
helpTextClear = ("clear","Clears the environment.")
helpTextProcess = ("process <definition>","Adds a process definition to the "
                   ++"environment.")
helpTextTrans = ("trans <process>","Shows the transitions of a process.")
helpTextOdes = ("odes <process>","Shoes the ODEs for a process.")
helpTextPlot = ("plot <process> <start> <end> <points>","Plots the time series of a process for the given interval [start,end] with the given number of time points.")
helpTextPlotFile = ("plotfile <process> <start> <end> <points> <file>","Plots the time series of a process for the given interval [start,end] with the given number of time points to a PDF")

---------------------
-- Utility functions:
---------------------

-- Say something to the user:
say = outputStrLn

-- Get the Environment state:
getEnv :: Environment Env
getEnv = lift get

-- Write the Environment state:
putEnv :: Env -> Environment ()
putEnv = lift . put

-- Add to the Environment state:
addEnv :: Definition -> Environment ()
addEnv x = do env <- getEnv;
              putEnv (x:env)

-- Read in a file:
getFile :: FilePath -> Environment String
getFile = lift . lift . readFile

-- get the parameters from a command line:
params :: String -> [String]
params cmdln = tail(words cmdln)
-- just the first:
param :: String -> String
param cmdln = let ps = params cmdln in
              case ps of
                []     -> []
                (p:ps) -> p

