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

-- Test transition system for enzyme example:
tTrans = do file <- readFile "testEnzyme.cpi"
            let defns = (\(Right x) -> x)(parse pDefinitionLines "" file)
            -- transitions of the species individually:
            let mts = trans defns (MTS []) (Def "S" ["s"])
            let mts' = trans defns mts (Def "P" [])
            let mts'' = trans defns mts' (Def "E" ["e"])
            putStrLn $ pretty mts''
            -- transitions of the species in parallel (with taus)
            let pr = Par [(Def "S" ["s"]),(Def "E" ["e"]),(Def "P" [])]
            let mts2 = trans defns (MTS []) pr
            putStrLn $ pretty mts2
            -- full graph:
            let mtsfull = calcMTS defns mts2
            putStrLn $ pretty mtsfull

-- test recursive species
tTransRec = do let defns = (\(Right x)->x)(parse pDefinitionLines "" "species P() = tau<1>.P();")
               let mts = trans defns (MTS []) (Def "P" [])
               let mts' = calcMTS defns mts
               putStrLn $ pretty mts'

-- test infinite species
tTransInf = do let defns = (\(Right x)->x)(parse pDefinitionLines "" "species P() = tau<1>.(P()|P());")
               let mts = trans defns (MTS []) (Def "P" [])
               let mts' = calcMTS defns mts
               putStrLn $ pretty mts'

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

