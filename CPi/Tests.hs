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

module CPi.Tests where

import CPi.Lib
import CPi.Parser
import CPi.Semantics
import CPi.ODE
import CPi.Logic
import CPi.Plot
import CPi.Matlab

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
{-
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
-}
-- Load an Env for testing:
tEnv x = do f <- readFile x
            case parseFile f of
              Left err -> return []
              Right x -> return x

tProc env x = maybe tEmptyProc id (lookupProcName env x)

tEmptyProc = Process [] (AffNet [])

-- tSpec x = case (parse pSpecies "" x) of
--             Left err -> error $ show err
--             Right x -> return x

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
              return ((\(Right x) -> x) (parseFile file))

tEnzPi = Process [(Def "S" ["s"],1.0),(Def "E" ["e"],0.1),(Def "P" [],0.0)] (AffNet [Aff (("e","s"),1.0)])

tEnzPi' = Process [(Def "S" ["s"],1.0),(Def "E" ["e"],0.5),(Def "P" [],0.0),((New (AffNet [Aff (("a","t"),1.0),Aff (("a","u"),0.5)]) (Par [Sum [(Comm "a" [] [],Def "E" ["e"])],Sum [(Comm "t" [] [],Def "P" []),(Comm "u" [] [],Def "S" ["s"])]])),0.0)] (AffNet [Aff (("e","s"),1.0)])

{-
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
-}
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
tcSum1TauP = Sum [((Tau 0.5),tcP)]
--  tau@<0.5>.P() + tau@<0.5>.Q()
tcSum2TauPQ = Sum [((Tau 0.5),tcP),((Tau 0.6),tcQ)]
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
tcNet1 = AffNet [Aff (("s","s'"),1)]
tcNet2 = AffNet [Aff (("a","b"),1)]
tcNet3 = AffNet [Aff (("s","s'"),1),Aff (("x","y"),1)]
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
{-
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
-}
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
{-
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
-}

-------------------------
-- Model checker tests:
-------------------------
{-
tModelCheck src p f trc = do env <- tEnv src
                             let pi = tProc env p
                             return $ modelCheck env solveODE trc pi mcts f

tModelCheckDP src p f trc = do env <- tEnv src
                               let pi = tProc env p
                               return $ modelCheckDP env solveODE trc pi mcts f

tModelCheckHy src p f trc = do env <- tEnv src
                               let pi = tProc env p
                               return $ modelCheckHy env solveODE trc pi mcts f

tModelCheckHy2 src p f trc = do env <- tEnv src
                                let pi = tProc env p
                                return $ modelCheckHy2 env solveODE trc pi mcts f
-}
mcts = (1000,(0,100))
--
-- Some contrived formulae for benchmarking:
-- (Use on the testGT.cpi model)

-- F(S<0.1)
tF1 = Pos (0,10) (ValLT (Conc (Def "S" ["s"])) (R 0.1))

-- F{5}(S<0.4 AND (F{5} (S<0.1)))
tF1b = Pos (0,5) 
       (Conj 
        (ValLT (Conc (Def "S" ["s"])) (R 0.4)) 
        (Pos (0,5)
         (ValLT (Conc (Def "S" ["s"])) (R 0.1))))

tF1c = Pos (0,10) (Nec (0,5)
                  (ValLT (Conc (Def "S" ["s"])) (R 0.1)))

-- test gaurantee (introduce an inhibitor)
tF2 = Gtee "In" (Neg tF1)

-- G(E>0.001) enzyme never runs out
tF3 = Nec (0,25) (ValGT (Conc (Def "E" ["e"])) (R 0.001))
tFn3 = Neg tF3

-- still true with inhibitor:
tF4 = Gtee "In" tF3

-- test nested guarantee (re-solves for every time-point):
tF5 = Nec (0,25) (Gtee "In" tF3)

-- test nested TL
-- G(F(S<0.1))
tF6 = Nec (0,25) tF1
-- G(F(G(E>0.001)))
tF7 = Nec (0,25) $ Pos (0,25) tF3
-- G(F(G(Inhib|>(E>0.001))))
tF8 = Nec (0,25) $ Pos (0,25) tF4

-- should be faster in Hy than in DP:
tF9 = Pos (0,20) (ValGT (Conc (Def "P" [])) (R 0.5))

tF10 = Nec (0,25) tF9

-- for checking sim times:
phi = (ValGT (Conc (Def "A" [])) (R 0.5))
psi = (ValGT (Conc (Def "B" [])) (R 0.5))
tF11 = Pos (0,10) (Nec (0,20) phi)
tF12 = Nec (0,10) (Conj (Pos (0,10) phi) (Pos (0,20) psi))
tF13 = Gtee "I" (Pos (0,10) phi)


tFR1 = (Disj (Until (0.0,3.3935742971887564) T (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Until (0.0,3.4939759036144586) T (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Until (0.0,3.594377510040161) T (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Until (0.0,3.694779116465864) T (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Until (0.0,3.7951807228915664) T (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Until (0.0,3.8955823293172696) T (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Until (0.0,3.9959839357429727) T (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Until (0.0,4.096385542168675) T (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Until (0.0,4.196787148594378) T (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Until (0.0,4.2971887550200805) T (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Until (0.0,4.397590361445784) T (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Until (0.0,4.497991967871486) T (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Until (0.0,4.598393574297189) T (ValLT (Conc (Def "S" ["s"])) (R 0.1)))))))))))))))

tFR2 = (Disj (Rels (0.0,2.3895582329317264) F (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Rels (0.0,2.4899598393574305) F (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Rels (0.0,2.5903614457831328) F (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Rels (0.0,2.690763052208835) F (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Rels (0.0,2.791164658634539) F (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Rels (0.0,2.8915662650602414) F (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Rels (0.0,2.9919678714859437) F (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Rels (0.0,3.092369477911646) F (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Rels (0.0,3.19277108433735) F (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Disj (Rels (0.0,3.2931726907630523) F (ValLT (Conc (Def "S" ["s"])) (R 0.1))) (Rels (0.0,3.3935742971887546) F (ValLT (Conc (Def "S" ["s"])) (R 0.1)))))))))))))

tFR3 = (Conj (Disj (Rels (0.0,24.49799196787149) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Disj (Rels (0.0,24.59839357429719) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Disj (Rels (0.0,24.698795180722893) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Disj (Rels (0.0,24.799196787148595) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.49799196787149) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))))))) (Conj (Disj (Rels (0.0,24.59839357429719) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Disj (Rels (0.0,24.698795180722893) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Disj (Rels (0.0,24.799196787148595) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.59839357429719) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3)))))))) (Conj (Disj (Rels (0.0,24.698795180722893) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Disj (Rels (0.0,24.799196787148595) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.698795180722893) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))))) (Conj (Disj (Rels (0.0,24.799196787148595) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.799196787148595) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3)))))) (Conj (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.899598393574298) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))) (Rels (0.0,24.49799196787149) F (Until (0.0,25.0) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))))))))

tFR4 = (Conj (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,23.895582329317275) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))) (Conj (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,23.995983935742977) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))) (Conj (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.09638554216868) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))) (Conj (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.19678714859438) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))) (Conj (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.297188755020084) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))) (Conj (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.397590361445786) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))) (Conj (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.49799196787149) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))) (Conj (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.59839357429719) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))) (Conj (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.698795180722893) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))) (Conj (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.799196787148595) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))) (Conj (Disj (Rels (0.0,24.899598393574298) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))) (Until (0.0,24.899598393574298) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))) (Rels (0.0,23.895582329317275) F (Until (0.0,25.0) T (Rels (0.0,25.0) F (ValGT (Conc (Def "E" ["e"])) (R 1.0e-3))))))))))))))))

tf z@(Disj (Until (0.0,t1) a b) (Disj (Until (0.0,t2) c d) e))
    | a==c&&b==d
        = (Disj (Until (0,(max t1 t2)) a b) e)
    | otherwise
        = z


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