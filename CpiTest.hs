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

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module CpiTest where

import CpiLib
import CpiParser
import CpiSemantics
import CpiODE
import CpiLogic
--import CpiPlot

import Text.ParserCombinators.Parsec
import System.IO
import Data.List as L
import qualified Data.Map as Map

import qualified Numeric.GSL as GSL
import qualified Numeric.LinearAlgebra as LA
import qualified Graphics.Plot as Plot


-----------------------------------
-- Parser tests:
-----------------------------------

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

-- Load an Env for testing:
tEnv x = do f <- readFile x
            case parse pDefinitionLines "" f of
              Left err -> return []
              Right x -> return x

tProc env x = maybe tEmptyProc id (lookupProcName env x)

tEmptyProc = Process [] (AffNet [])

tSpec x = case (parse pSpecies "" x) of
            Left err -> error $ show err
            Right x -> return x

-------------------------------
-- Tests for transition system:
-------------------------------

-- Test transition system for enzyme example:
{-tTrans = do file <- readFile "testEnzyme.cpi"
            let defns = (\(Right x) -> x)(parse pDefinitionLines "" file)
            putStrLn $ concat $ map (\x->(pretty x)++"\n") defns
            -- transitions of the species individually:
            let mts = trans defns (MTS []) (Def "S" ["s"])
            let mts' = trans defns mts (Def "P" [])
            let mts'' = trans defns mts' (Def "E" ["e"])
            -- let mts''' = trans defns mts'' (Def "I" ["i"])
            putStrLn "Initial species transtions:\n"
            putStrLn $ pretty mts''
            -- find resultant complexes (pseudoapps):
            let compxs = appls mts''
            putStrLn "Complexes formed (pseudoapplications):\n"
            putStrLn $ concat $ map (\x->(pretty x)++"\n") compxs
            -- find transitions for the complexes and add to MTS
            let finalmts = transs defns mts'' compxs
            putStrLn "Multi-transition system:\n"
            putStrLn $ pretty finalmts
            -- calculate the closure of the MTS
            let fixedmts = fixMTS defns finalmts
            putStrLn "Closed MTS:\n"
            putStrLn $ pretty fixedmts
-}

-- Some constants for playing with the Enzyme example:
tEnzDefs = do file <- readFile "testEnzyme.cpi"
              return ((\(Right x) -> x)(parse pDefinitionLines "" file))

tEnzPi = Process [(Def "S" ["s"],"1.0"),(Def "E" ["e"],"0.1"),(Def "P" [],"0.0")] (AffNet [Aff (("e","s"),"1.0")])

tEnzPi' = Process [(Def "S" ["s"],"1.0"),(Def "E" ["e"],"0.5"),(Def "P" [],"0.0"),((New (AffNet [Aff (("a","t"),"1.0"),Aff (("a","u"),"0.5")]) (Par [Sum [(Comm "a" [] [],Def "E" ["e"])],Sum [(Comm "t" [] [],Def "P" []),(Comm "u" [] [],Def "S" ["s"])]])),"0.0")] (AffNet [Aff (("e","s"),"1.0")])

-- Test get full MTS of a process:
tPTrans = do file <- readFile "testEnzyme.cpi"
             let defns = (\(Right x) -> x)(parse pDefinitionLines "" file)
             let pi = Process [(Def "S" ["s"],"1.0"),(Def "E" ["e"],"0.1"),(Def "P" [],"0.0")] (AffNet [Aff (("e","s"),"1.0")])
             let mts = processMTS defns pi
             putStrLn $ (prettys defns)++"\nTransitions:\n"++(pretty mts)

-- test recursive species
tTransRec = do let defns = (\(Right x)->x)(parse pDefinitionLines "" "species P() = tau<1>.P();")
               let mts = trans defns (MTS []) (Def "P" [])
               putStrLn $ pretty mts

-- test infinite species
tTransInf = do let defns = (\(Right x)->x)(parse pDefinitionLines "" "species P() = tau<1>.(P()|P());")
               let mts = trans defns (MTS []) (Def "P" [])
               putStrLn $ pretty mts

-- Test trans of Def/Nil
tTrans' = trans [tcSpecP0] (MTS []) tcP

-- Test trans of singleton Sum of Tau.P()
tTrans'2 = trans [tcSpecP0] (MTS []) tcSum1TauP

-- Test trans of Sum of Tau.P() + Tau.Q()
tTrans'3 = trans [tcSpecP0,tcSpecQ0] (MTS []) tcSum2TauPQ

tLookupDef = lookupDef [tcSpecP0] (tcP) 

-- Test tensor:
tTensor = do env <- tEnzDefs
             let net = AffNet [Aff (("e","s"),"1.0")]
             let e = Process [(Def "E" ["e"],"0.1")] net
             let s = Process [(Def "S" ["s"],"1.0")] net
             print $ tensor env net (partial env e) (partial env s)

-- Test tensor':
{-tTensor' = do env <- tEnzDefs
              let net = AffNet [Aff (("e","s"),"1.0")]
              let e = Process [(Def "E" ["e"],"0.1")] net
              let s = Process [(Def "S" ["s"],"1.0")] net
              putStrLn $ prettyODE env $ tensor' env net (partial' env e) (partial' env s)
-}

-- Test symbolic dPdt:
{-tdPdt = do env <- tEnzDefs
           let pi = tEnzPi'
           putStrLn $ prettyODE env $ dPdt' env pi
-}

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

-- tests for some struct.cong. rules
tcSs = Def "S" ["s"]
tcSsa = Def "S" ["s","a"]
tcXx = Def "X" ["x"]
tcNet0 = AffNet []
tcNet1 = AffNet [Aff (("s","s'"),"1")]
tcNet2 = AffNet [Aff (("a","b"),"1")]
tcNet3 = AffNet [Aff (("s","s'"),"1"),Aff (("x","y"),"1")]
tcNNS = New tcNet1 (New tcNet2 tcSs)
tcNNSsa = New tcNet1 (New tcNet2 tcSsa)
tcNSX = New tcNet1 (Par [tcXx,tcSs])
tcN3SX = New tcNet3 (Par [tcXx,tcSs])
tcN0SX = New tcNet0 (Par [tcXx,tcSs])

tcC1 = ConcBase (Par [tcXx,tcSs]) ["a"] ["s"]
tcC2 = ConcPar tcC1 [tcQ,tcP]

tcNestPar = Par [tcQ, tcP,Par [tcXx,Par [tcSs,Nil]]]


-----------------------------------
-- Tests for new fixmts
-----------------------------------

tNPs = do env <- tEnv "models/ddos.cpi"
          let pi = tProc env "Pi"
              net' (Process _ net) = net
              net = net' pi
              ss' (Process ss _) = ss
              ss = ss' pi
              initmts = transs env (MTS []) (map fst ss)
          return $ newPrimes env initmts

tFixmts = do env <- tEnv "models/ddos.cpi"
             let pi = tProc env "Pi"
             return $ processMTS env pi                 

-----------------------------------
-- ODE solver tests
-----------------------------------


-- xdot t [x,v] = [v, -0.95*x - 0.1*v]
txdot t [e,s,p,c] = [(-1.0)*1.1*e*s + (-1.0)*0.9*c + 0.5*c,
                    (-1.0)*1.1*e*s + 0.9*c,
                    (-1.0)*0.5*p + 0.5*c,
                    1.1*e*s + (-1.0)*0.9*c + (-1.0)*0.5*c]
txdot _ _ = undefined
ts = LA.linspace 250 (0,25)
-- sol = GSL.odeSolve xdot [10,0] ts
sol = GSL.odeSolve txdot [0.4,2.3,0,0] ts
tODE = Plot.mplot (ts : LA.toColumns sol)

{-tXdot = do env <- tEnzDefs
           let pi = tEnzPi'
           let x = xdot env (dPdt' env pi)
           -- return $ x ts [1.0,0,0.5,0]
           let sol' = GSL.odeSolve x [1.0,0,0.5,0] ts
           Plot.mplot (ts : LA.toColumns sol')
-}

tSeries = do env <- tEnv "testEnzyme.cpi"
             let pi = tProc env "Pi"
             let mts = processMTS env pi
             let pi' = wholeProc env pi mts
             let dpdt = dPdt' env mts pi'
             let odes = xdot env dpdt
             let inits = initials env pi' dpdt
             let ts = timePoints 250 (0,25)
             let soln = solveODE env pi' dpdt (250,(0,25))
             let ss = speciesIn env dpdt
             return $ timeSeries ts soln ss


-------------------------
-- Model checker tests:
-------------------------

tModelCheck src p f = do env <- tEnv src
                         let pi = tProc env p
                         return $ modelCheck env solveODE Nothing pi (250,(0,25)) f

tModelCheckDP src p f = do env <- tEnv src
                           let pi = tProc env p
                           return $ modelCheckDP env solveODE Nothing pi (250,(0,25)) f

tModelCheckHy src p f = do env <- tEnv src
                           let pi = tProc env p
                           return $ modelCheckHy env solveODE Nothing pi (250,(0,25)) f

--
-- Some contrived formulae for benchmarking:
-- (Use on the testGT.cpi model)
--
-- F(S<0.1)
tF1 = Pos (0,infty) (ValLT (Conc (Def "S" ["s"])) (R 0.1))

-- test gaurantee (introduce an inhibitor)
inhib = Process [(Def "I" ["i"],"2.0")] (AffNet [Aff (("e","i"),"2.0")])
tF2 = Gtee inhib (Neg tF1)

-- G(E>0.001) enzyme never runs out
tF3 = Nec (0,infty) (ValGT (Conc (Def "E" ["e"])) (R 0.001))
tFn3 = Neg tF3

-- still true with inhibitor:
tF4 = Gtee inhib tF3

-- test nested guarantee (re-solves for every time-point):
tF5 = Nec (0,infty) (Gtee inhib tF3)

-- test nested TL
-- G(F(S<0.1))
tF6 = Nec (0,infty) tF1
-- G(F(G(E>0.001)))
tF7 = Nec (0,infty) $ Pos (0,infty) tF3
-- G(F(G(Inhib|>(E>0.001))))
tF8 = Nec (0,infty) $ Pos (0,infty) tF4

-- should be faster in Hy than in DP:
tF9 = Pos (0,infty) (ValGT (Conc (Def "P" [])) (R 0.5))

tF10 = Nec (0,infty) tF9


--------------------------
-- Graph plotting tests:
--------------------------

{-
tPlot = plot ts dims
    where
      ts = [0.0,0.05..60.0] -- ::[Double]
      {-dims = [("Times 2",map (*2) ts),
              ("Times 3",map (*3) ts)
             ]-}
      dims = [("Sin x + " ++ show x, map sin (map (+x) ts)) | x<-[0..5]]

tPlotTimeSeries = do env <- tEnv "models/testEnzyme"
                     let pi = tProc env "Pi"
                     let mts = processMTS env pi
                     let pi' = wholeProc env pi mts
                     let dpdt = dPdt' env mts pi'
                     let odes = xdot env dpdt
                     let inits = initials env pi' dpdt
                     let ts = timePoints 250 (0,25)
                     let soln = solveODE env pi' dpdt (250,(0,25))
                     let ss = speciesIn env dpdt
                     plotTimeSeries ts soln ss
-}

---------------------------------
-- Testing solver with Jacobain:
---------------------------------

{-tSolveMAPKwithJac = do env <- tEnv "models/mapk.cpi"
                       let pi = tProc env "MAPK"
                           mts = processMTS env pi
                           pi' = wholeProc env pi mts
                           dpdt = dPdt' env mts pi'
                           odes = xdot env dpdt
                           jacob = jac env dpdt
                           inits = initials env pi' dpdt
                           ts = timePoints 800 (0,80)
                           soln = solveODE' odes jacob inits ts
                       return soln

tSolveMAPKwithoutJac = do env <- tEnv "models/mapk.cpi"
                          let pi = tProc env "MAPK"
                              mts = processMTS env pi
                              pi' = wholeProc env pi mts
                              dpdt = dPdt' env mts pi'
                              odes = xdot env dpdt
                              inits = initials env pi' dpdt
                              ts = timePoints 800 (0,80)
                              soln = solveODE odes inits ts
                          return soln

tPlotEnzwithJac = do env <- tEnv "models/testEnzyme.cpi"
                     let pi = tProc env "Pi"
                         mts = processMTS env pi
                         pi' = wholeProc env pi mts
                         dpdt = dPdt' env mts pi'
                         odes = xdot env dpdt
                         jacob = jac env dpdt
                         inits = initials env pi' dpdt
                         ts = timePoints 250 (0,25)
                         soln = solveODE' odes jacob inits ts
                         ss = speciesIn env dpdt
                     plotTimeSeries ts soln ss
                            
tPlotEnzwithoutJac = do env <- tEnv "models/testEnzyme.cpi"
                        let pi = tProc env "Pi"
                            mts = processMTS env pi
                            pi' = wholeProc env pi mts
                            dpdt = dPdt' env mts pi'
                            odes = xdot env dpdt
                            inits = initials env pi' dpdt
                            ts = timePoints 250 (0,25)
                            soln = solveODE env pi' dpdt (250,(0,25)) ts
                            ss = speciesIn env dpdt
                        plotTimeSeries ts soln ss
-}