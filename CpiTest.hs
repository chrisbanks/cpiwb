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

-------------------------------
-- Tests for transition system:
-------------------------------

tTrans = do file <- readFile "testEnzyme.cpi"
            let defns = (\(Right x) -> x)(parse pDefinitionLines "" file)
            print defns

-- Test trans of Def/Nil
tTrans' = trans [tcSpecP0] (MTS []) tcP

-- Test trans of singleton Sum of Tau.P()
tTrans'2 = trans [tcSpecP0] (MTS []) tcSum1TauP

-- Test trans of Sum of Tau.P() + Tau.Q()
tTrans'3 = trans [tcSpecP0,tcSpecQ0] (MTS []) tcSum2TauPQ

tLookupDef = lookupDef [tcSpecP0] (tcP) 

------------------
-- Test constants:
------------------

-- tau@<0.5>.P():
tcSum1TauP = Sum [((Tau "0.5"),tcP)]
--  tau@<0.5>.P() + tau@<0.5>.Q()
tcSum2TauPQ = Sum [((Tau "0.5"),tcP),((Tau "0.6"),tcQ)]
-- P()
tcP = Def "P" []
-- Q()
tcQ = Def "Q" []
-- species P() = 0
tcSpecP0 = SpeciesDef "P" [] Nil
-- species Q() = 0
tcSpecQ0 = SpeciesDef "Q" [] Nil

