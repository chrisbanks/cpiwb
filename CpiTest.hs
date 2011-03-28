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
--     along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

module CpiTest where

import CpiLib
import CpiParser

import Text.ParserCombinators.Parsec
import IO

-- Parser Test harnesses:
testParse :: (Expr a) => Parser a -> String -> IO ()
testParse p input = case (parse p "" input) of
                      Left err -> do putStr "parse error at";
                                     print err
                      Right x -> print (pretty x)

testParse' :: (Expr a) => Parser a -> String -> IO ()
testParse' p input = case (parse p "" input) of
                      Left err -> do putStr "parse error at";
                                     print err
                      Right x -> print x

-- Test file input:

-- getRight (Right x) = x

-- testFile x = do file <- openFile x ReadMode;
--                 contents <- hGetContents file;
--                 result <- parse pDefinitionLines contents;
--                 result' <- getRight result;
--                 print result'
                