import CpiLib
import CpiTest
import CpiODE
import CpiSemantics
import CpiLogic

import System.Environment (getArgs)

-- Time points
--tps = (100,(0,25))

-- Basic
f1 = Pos (0,infty) (ValGT (Conc (Def "P" [])) (R 0.05))
f2 = Pos (0,infty) (ValLE (Conc (Def "S" ["s"])) (R 0.01))
f3 = Nec (0,infty) (ValGT (Conc (Def "E" ["e"])) (R 0.01))
f4 = Nec (0,infty) (ValGT (Conc (Def "E" ["e"])) (R 0.4))

-- 1-nested TL
f5 = Nec (0,infty) f1
f6 = Nec (0,infty) f1
f7 = Pos (0,infty) f1
f8 = Pos (0,infty) f1

-- 2-nested TL
f9 = Pos (0,infty) f5
f10 = Pos (0,infty) f6
f11 = Nec (0,infty) f7
f12 = Nec (0,infty) f8

-- 3-nested TL
f13 = Nec (0,infty) f9
f14 = Nec (0,infty) f10
f15 = Pos (0,infty) f11
f16 = Pos (0,infty) f12

-- Basic Gtee
f17 = Gtee "In" f1
f18 = Gtee "In" f2
f19 = Gtee "In" f3
f20 = Gtee "In" f4

-- 1-nested Gtee
f21 = Pos (0,infty) f17
f22 = Pos (0,infty) f18
f23 = Pos (0,infty) f19
f24 = Pos (0,infty) f20

f25 = Nec (0,infty) f17
f26 = Nec (0,infty) f18
f27 = Nec (0,infty) f19
f28 = Nec (0,infty) f20


main = do env <- tEnv "models/testGT.cpi"
          res <- getArgs
          let tps = (read(res!!0),(0,25))
          let pi = tProc env "Pi"
              mts = processMTS env pi
              pi' = wholeProc env pi mts
              dpdt = dPdt' env mts pi'
              odes = xdot env dpdt
              inits = initials env pi' dpdt
              ts = timePoints (read(res!!0)) (0,25)
              soln = solveODE env pi' dpdt tps
              ss = speciesIn env dpdt
              trace = timeSeries ts soln ss
          
          let r1 = {-# SCC "f17-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f17
              r2 = {-# SCC "f17-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f17
              r3 = {-# SCC "f17-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f17
              r4 = {-# SCC "f17-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f17
          print $ pretty f17
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f18-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f18
              r2 = {-# SCC "f18-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f18
              r3 = {-# SCC "f18-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f18
              r4 = {-# SCC "f18-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f18
          print $ pretty f18
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f19-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f19
              r2 = {-# SCC "f19-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f19
              r3 = {-# SCC "f19-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f19
              r4 = {-# SCC "f19-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f19
          print $ pretty f19
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f20-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f20
              r2 = {-# SCC "f20-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f20
              r3 = {-# SCC "f20-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f20
              r4 = {-# SCC "f20-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f20
          print $ pretty f20
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f21-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f21
              r2 = {-# SCC "f21-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f21
              r3 = {-# SCC "f21-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f21
              r4 = {-# SCC "f21-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f21
          print $ pretty f21
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f22-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f22
              r2 = {-# SCC "f22-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f22
              r3 = {-# SCC "f22-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f22
              r4 = {-# SCC "f22-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f22
          print $ pretty f22
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f23-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f23
              r2 = {-# SCC "f23-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f23
              r3 = {-# SCC "f23-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f23
              r4 = {-# SCC "f23-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f23
          print $ pretty f23
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f24-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f24
              r2 = {-# SCC "f24-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f24
              r3 = {-# SCC "f24-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f24
              r4 = {-# SCC "f24-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f24
          print $ pretty f24
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f25-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f25
              r2 = {-# SCC "f25-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f25
              r3 = {-# SCC "f25-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f25
              r4 = {-# SCC "f25-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f25
          print $ pretty f25
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f26-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f26
              r2 = {-# SCC "f26-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f26
              r3 = {-# SCC "f26-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f26
              r4 = {-# SCC "f26-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f26
          print $ pretty f26
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f27-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f27
              r2 = {-# SCC "f27-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f27
              r3 = {-# SCC "f27-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f27
              r4 = {-# SCC "f27-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f27
          print $ pretty f27
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f28-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f28
              r2 = {-# SCC "f28-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f28
              r3 = {-# SCC "f28-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f28
              r4 = {-# SCC "f28-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f28
          print $ pretty f28
          print r1
          print r2
          print r3
          print r4
          

