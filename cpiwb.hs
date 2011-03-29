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

welcome = "\nWelcome to the Continuous Pi-calculus Workbench (CPiWB).\n"
prompt = "CPi:> "

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
                            Just i -> do command i;
                                         loop
-- NOTE: Haskeline gives us a very nice REPL with history
--       and the possibility for autocomplete.

command :: String -> InputT IO ()
command cmdln = let cmd = head(words cmdln)
                    params = tail(words cmdln)
                in
                case cmd of
                  "help" -> outputStrLn (help params)
                  "load" -> outputStrLn ("Loading: "++(unwords params))
                  _      -> outputStrLn "Try again."

help :: [String] -> String
help ("load":_) = "\nload <filename>\n\tLoads a CPi definition file.\n"
help x = "Sorry, no help for: "++(unwords x)
-- TODO: maybe we want a map of help topics, so we can list topics
--       and find responses more easily.
