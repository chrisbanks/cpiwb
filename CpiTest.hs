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

module CpiTest where

import CpiLib
import CpiParser
import CpiSemantics

import Text.ParserCombinators.Parsec
import System.IO

-- Parser Test harnesses:
tParse :: (Pretty a) => Parser a -> String -> IO ()
tParse p input = case (parse p "" input) of
                      Left err -> do putStr "parse error at";
                                     print err
                      Right x -> print (pretty x)

tParse' :: (Show a) => Parser a -> String -> IO ()
tParse' p input = case (parse p "" input) of
                      Left err -> do putStr "parse error at";
                                     print err
                      Right x -> print x

-- -- Test file input:

tFile x = do f <- readFile x
             tParse' pDefinitionLines f
