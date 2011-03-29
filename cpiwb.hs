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

main = do putStrLn welcome;
          putStr prompt;
          input <- getLine; -- (1)
          print input -- TODO: deal with the input.

-- TODO: (1) - find a better way of getting input than this
--             e.g. doesn't deal with backspace and what if we want
--             command history/autocomplete etc.

welcome = "\nWelcome to the Continuous Pi-calculus Workbench (CPiWB).\n"
prompt = "CPi:> "