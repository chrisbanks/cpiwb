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

import CPi.Lib
import CPi.Parser
import CPi.Semantics
import CPi.ODE
import CPi.Plot
import CPi.Logic
import CPi.Matlab
import CPi.Signals

import System.Console.Haskeline
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import qualified Data.List as L
import qualified Control.Exception as X

import Debug.Trace(trace)


-- Some configurables:
welcome = "\nWelcome to the Continuous Pi-calculus Workbench (CPiWB).\n"
          ++"Type \"help\" for help.\n"
prompt = "CPiWB:> "

-- Our environment will be a stack of the Haskeline,
-- State transformer (of CPi Definitions), and IO monads:
type Environment = InputT (StateT Env IO)
type Formulae = [Formula]

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
                     cmdHelp = helpTextPlotFile}),
            ("check",
             CmdRec {cmdFn = checkCmd,
                     cmdHelp = helpTextCheck}),
            ("check2",
             CmdRec {cmdFn = check2Cmd,
                     cmdHelp = helpTextCheck2}),
            ("plotall",
             CmdRec {cmdFn = plotAllCmd,
                     cmdHelp = helpTextPlotAll}),
            ("plotoctave",
             CmdRec {cmdFn = plotOctaveCmd,
                     cmdHelp = helpTextPlotOctave}),
            ("plotonly",
             CmdRec {cmdFn = plotOnlyCmd,
                     cmdHelp = helpTextPlotOnly}),
            ("phasePlot2",
             CmdRec {cmdFn = phase2Cmd,
                     cmdHelp = helpTextPhase2}),
            ("matlab",
             CmdRec {cmdFn = matlabCmd,
                     cmdHelp = helpTextMatlab}),
            ("evolve",
             CmdRec {cmdFn = evolveCmd,
                     cmdHelp = helpTextEvolve}),
            ("derivs",
             CmdRec {cmdFn = plotDerivsCmd,
                     cmdHelp = helpTextDerivs})]

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
                                 let dpdt = dPdt env mts proc
                                 say $ prettyODE env dpdt

-- plot Command
plotCmd :: String -> Environment ()
plotCmd = plotOctaveCmd
{-plotCmd x = do env <- getEnv;
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
                                 let dpdt = dPdt env mts proc
                                 let ts = (res,(start,end))
                                 let ts' = timePoints ts
                                 let solns = solveODE env proc mts dpdt ts
                                 let ss = speciesIn env dpdt
                                 let ss' = speciesInProc proc
                                 lift$lift$plotTimeSeriesFiltered ts' solns ss ss'
-}

-- phase2 command
phase2Cmd :: String -> Environment ()
phase2Cmd x = do env <- getEnv;
                 let args = words x
                 let res = read(args!!6)
                 let start = read(args!!4)
                 let end = read(args!!5)
                 let s1' = args!!2
                 let s2' = args!!3
                 case lookupProcName env (args!!1) of
                   Nothing   -> say $ "Process \""++(args!!1)
                                ++"\" is not in the Environment."
                   Just proc -> do let mts = processMTS env proc
                                   let dpdt = dPdt env mts proc
                                   let ts = (res,(start,end))
                                   let ts' = timePoints ts
                                   let solns = solveODEoctave env proc mts dpdt ts
                                   let ss = speciesIn env dpdt
                                   let ss' = (lookupSpecName env s1', lookupSpecName env s2')
                                   case ss' of
                                     (Just s1,Just s2) -> lift$lift$phasePlot2 ts' solns ss (s1,s2)
                                     otherwise -> say $ "Species "++s1'++" or "++s2'
                                                  ++" is not in the Environment."

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
                                     let dpdt = dPdt env mts proc
                                     let ts = (res,(start,end))
                                     let ts' = timePoints ts
                                     let solns = solveODEoctave env proc mts dpdt ts
                                     let ss = speciesIn env dpdt
                                     let ss' = speciesInProc proc
                                     lift$lift$plotTimeSeriesToFileFiltered ts' solns ss ss' file

-- plotAll Command
-- Plot all species (inc complexes)
plotAllCmd :: String -> Environment ()
plotAllCmd x = do env <- getEnv;
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
                                    let dpdt = dPdt env mts proc
                                    let ts = (res,(start,end))
                                    let ts' = timePoints ts
                                    let solns = solveODEoctave env proc mts dpdt ts
                                    let ss = speciesIn env dpdt
                                    lift$lift$plotTimeSeries ts' solns ss

-- plot using Octave solver Command
plotOctaveCmd :: String -> Environment ()
plotOctaveCmd x = do env <- getEnv;
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
                                       let dpdt = dPdt env mts proc
                                       let ts = (res,(start,end))
                                       let ts' = timePoints ts
                                       let solns = solveODEoctave env proc mts dpdt ts
                                       let ss = speciesIn env dpdt
                                       let ss' = speciesInProc proc
                                       lift$lift$plotTimeSeriesFiltered ts' solns ss ss'

-- plot only the specified species
plotOnlyCmd :: String -> Environment ()
plotOnlyCmd x = do env <- getEnv;
                   let args = words x
                   -- TODO: properly parse the command!
                   --       and have some defaults?
                   let res = read(args!!4)
                   let start = read(args!!2)
                   let end = read(args!!3)
                   let err x = X.throw $ CpiException $
                               "Species \""++x++"\" is not in the Environment."
                   let onlyss = map 
                                (\x->maybe (err x) id (lookupSpecName env x)) 
                                (drop 5 args)
                   case lookupProcName env (args!!1) of
                     Nothing   -> say $ "Process \""++(args!!1)
                                  ++"\" is not in the Environment."
                     Just proc -> do let mts = processMTS env proc
                                     let dpdt = dPdt env mts proc
                                     let ts = (res,(start,end))
                                     let ts' = timePoints ts
                                     let solns = solveODEoctave env proc 
                                                                mts dpdt ts
                                     let ss = speciesIn env dpdt
                                     lift$lift$
                                         plotTimeSeriesFiltered ts' solns 
                                                                ss onlyss

-- plot concentration derivatives command:
plotDerivsCmd :: String -> Environment ()
plotDerivsCmd x = do env <- getEnv;
                     let args = words x
                     -- TODO: properly parse the command!
                     --       and have some defaults?
                     let res = read(args!!4)
                     let start = read(args!!2)
                     let end = read(args!!3)
                     let err x = X.throw $ CpiException $
                                 "Species \""++x++"\" is not in the Environment."
                     let onlyss = map 
                                  (\x->maybe (err x) id (lookupSpecName env x)) 
                                  (drop 5 args)
                     case lookupProcName env (args!!1) of
                       Nothing   -> say $ "Process \""++(args!!1)
                                    ++"\" is not in the Environment."
                       Just proc -> do let mts = processMTS env proc
                                       let dpdt = dPdt env mts proc
                                       let ts = (res,(start,end))
                                       let ts' = timePoints ts
                                       let solns = solveODEoctave env proc 
                                                       mts dpdt ts
                                       let ds = derivs env dpdt ts solns
                                       let ss = speciesIn env dpdt
                                       lift$lift$
                                           plotTimeSeriesFiltered ts' ds
                                                                  ss onlyss


-- check command:
checkCmd :: String -> Environment ()
checkCmd x = do env <- getEnv
                let args = words x
                case lookupProcName env (args!!1) of
                  Nothing -> say $ "Process \""++(args!!1)
                             ++"\" is not in the Environment."
                  Just p  -> case parseFormula (unwords(drop 2 args)) of
                               Left err -> say $ "Formula parse error:\n" 
                                           ++ (show err)
                               Right f  -> let f' = reconcileSpecs env f
                                           in say $ show $ 
                                              modelCheck env 
                                                         solveODE 
                                                         Nothing 
                                                         p 
                                                         (500,(0,(simTime f'))) 
                                                         f'

check2Cmd :: String -> Environment ()
check2Cmd x = do env <- getEnv
                 let args = words x
                 case lookupProcName env (args!!1) of
                   Nothing -> say $ "Process \""++(args!!1)
                              ++"\" is not in the Environment."
                   Just p  -> case parseFormula (unwords(drop 2 args)) of
                                Left err -> say $ "Formula parse error:\n" 
                                            ++ (show err)
                                Right f  -> let f' = reconcileSpecs env f
                                            in say $ show $
                                               modelCheckSig env 
                                                            solveODEoctave 
                                                            Nothing 
                                                            p 
                                                            (500,(0,(simTime f')))
                                                            f'
-- MATLAB command - produce MATLAB script for ODEs.
matlabCmd :: String -> Environment ()
matlabCmd x = do env <- getEnv
                 let args = words x
                     start = read(args!!2)
                     end = read(args!!3)
                     res = read(args!!4)
                 case lookupProcName env (args!!1) of
                   Nothing -> say $ "Process \""++(args!!1)
                              ++"\" is not in the Environment."
                   Just p -> let mts = processMTS env p
                                 p' = dPdt env mts p
                                 ts = (res,(start,end))
                             in putFile (args!!5) $ matlabScript env p mts p' ts

-- Evolve command
evolveCmd :: String -> Environment ()
evolveCmd x = do env <- getEnv
                 let args = words x
                     start = read(args!!2)
                     end = read(args!!3)
                     res = read(args!!4)
                     newPname = (args!!5)
                 case lookupProcName env (args!!1) of
                   Nothing -> say $ "Process \""++(args!!1)
                              ++"\" is not in the Environment."
                   Just p -> let mts = processMTS env p
                                 p' = dPdt env mts p
                                 ts = (res,(start,end))
                                 newP = (evolveProcess env p mts p' 
                                                       ts solveODEoctave)
                             in do addEnv $ ProcessDef newPname newP
                                   say $ newPname ++ " = " 
                                           ++ (pretty newP)

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
helpTextCheck = ("check <process> <formula>","Model checker -- checks the process satisfies the formula")
helpTextCheck2 = ("check2 <process> <formula>","Model checker -- checks the process satisfies the formula using the formula rewriting algorithm with relative time bounds.")
helpTextPlotAll = ("plotall <process> <start> <end> <points>","Plots the time series of a process for the given interval [start,end] with the given number of time points, including all species defined and generated complexes.")
helpTextPlotOctave = ("plotoctave <process> <start> <end> <points>","Plots the time series of a process for the given interval [start,end] with the given number of time points, using GNU Octave to solve the ODEs.")
helpTextPhase2 = ("phasePlot2 <process>  <species> <species> <start> <end> <points>","Plots the (2-dimensional) phase diagram for two species, for the given interval [start,end] with the given number of time points.")
helpTextMatlab = ("matlab <process> <start> <end> <points> <filename>","Writes the MATLAB code for the ODEs to a file.")
helpTextEvolve = ("evolve <process> <start> <end> <points> <new process>","Gives a new process corresponding to the given process evolved to its end point.")
helpTextPlotOnly = ("plotonly <process> <start> <end> <points> <species...>","Plots the time series of a process for the given interval [start,end] with the given number of time points, plotting only the given (space separated) list of species.")
helpTextDerivs = ("derivs <process> <start> <end> <points> <species...>","Plots the concentration derivatives of a process for the given interval [start,end] with the given number of time points, plotting only the given (space separated) list of species.")

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

-- Write a file:
putFile :: FilePath -> String -> Environment ()
putFile f s = lift $ lift $ writeFile f s

-- get the parameters from a command line:
params :: String -> [String]
params cmdln = tail(words cmdln)
-- just the first:
param :: String -> String
param cmdln = let ps = params cmdln in
              case ps of
                []     -> []
                (p:ps) -> p

