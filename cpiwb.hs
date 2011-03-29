-- (C) Copyright Chris Banks 2011

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

import System.Console.Haskeline

import qualified Data.List as L

welcome = "\nWelcome to the Continuous Pi-calculus Workbench (CPiWB).\n"
          ++"Type \"help\" for help.\n"
prompt = "CPiWB:> "

main :: IO ()
main = do putStrLn welcome;
          runInputT defaultSettings loop
              where 
                loop :: InputT IO ()
                loop = do input <- getInputLine prompt
                          case input of
                            Nothing -> return ()
                            Just "" -> loop
                            Just "quit" -> return ()
                            Just i -> do doCommand i;
                                         loop
-- TODO: command autocomplete (see Haskeline docs).
--       can we use the command map?

doCommand :: String -> InputT IO ()
doCommand cmdln = let cmd = head(words cmdln)
                      params = tail(words cmdln)
                  in
                    case (lookup cmd commands) of
                      Nothing -> outputStrLn "Try again."
                      Just x  -> outputStrLn ((cmdFn x) params)

---------------
-- Command map:
---------------

data CmdRec = CmdRec {cmdFn::[String]->String,cmdHelp::String}

commands :: [(String,CmdRec)]
commands = [("help",CmdRec {
                         cmdFn = helpCmd,
                         cmdHelp = helpTextHelp}),
            ("load",CmdRec {
                         cmdFn = loadCmd,
                         cmdHelp = helpTextLoad})]

cmdList = "\n"++L.concat(L.intersperse "\n" (map (\(x,_) -> x) commands))++"\n\n"

---------------------
-- Command Functions:
---------------------

loadCmd :: [String] -> String
loadCmd (x:_) = "Loading: "++x

helpCmd :: [String] -> String
helpCmd (x:_) = case (lookup x commands) of
                  Nothing -> "Sorry no help on \""++x++"\"."
                  Just r -> cmdHelp r
helpCmd [] = "\nThe available commands are:\n"
             ++cmdList
             ++"Type \"help <command>\" for help on a specific command.\n"

----------------------
-- Command help texts:
----------------------

helpTextHelp = "\nhelp <command>\n\tShows help on a specific command.\n"
helpTextLoad = "\nload <filename>\n\tLoads a CPi definition file.\n"

