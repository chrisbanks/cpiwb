import CPi.Lib
import Tests
import CPi.ODE
import CPi.Semantics
import CPi.Logic

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

-- 4-nested TL
f14b = Pos (0,infty) f14

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
          let r1 = {-# SCC "f1-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f1
              r2 = {-# SCC "f1-DynProg" #-} modelCheckDP
                   env solveODE (Just trace) pi tps f1
              r3 = {-# SCC "f1-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f1
              r4 = {-# SCC "f1-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f1
          print $ pretty f1
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f2-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f2
              r2 = {-# SCC "f2-DynProg" #-} modelCheckDP
                   env solveODE (Just trace) pi tps f2
              r3 = {-# SCC "f2-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f2
              r4 = {-# SCC "f2-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f2
          print $ pretty f2
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f3-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f3
              r2 = {-# SCC "f3-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f3
              r3 = {-# SCC "f3-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f3
              r4 = {-# SCC "f3-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f3
          print $ pretty f3
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f4-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f4
              r2 = {-# SCC "f4-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f4
              r3 = {-# SCC "f4-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f4
              r4 = {-# SCC "f4-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f4
          print $ pretty f4
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f5-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f5
              r2 = {-# SCC "f5-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f5
              r3 = {-# SCC "f5-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f5
              r4 = {-# SCC "f5-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f5
          print $ pretty f5
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f6-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f6
              r2 = {-# SCC "f6-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f6
              r3 = {-# SCC "f6-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f6
              r4 = {-# SCC "f6-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f6
          print $ pretty f6
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f7-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f7
              r2 = {-# SCC "f7-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f7
              r3 = {-# SCC "f7-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f7
              r4 = {-# SCC "f7-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f7
          print $ pretty f7
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f8-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f8
              r2 = {-# SCC "f8-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f8
              r3 = {-# SCC "f8-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f8
              r4 = {-# SCC "f8-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f8
          print $ pretty f8
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f9-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f9
              r2 = {-# SCC "f9-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f9
              r3 = {-# SCC "f9-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f9
              r4 = {-# SCC "f9-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f9
          print $ pretty f9
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f10-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f10
              r2 = {-# SCC "f10-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f10
              r3 = {-# SCC "f10-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f10
              r4 = {-# SCC "f10-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f10
          print $ pretty f10
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f11-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f11
              r2 = {-# SCC "f11-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f11
              r3 = {-# SCC "f11-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f11
              r4 = {-# SCC "f11-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f11
          print $ pretty f11
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f12-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f12
              r2 = {-# SCC "f12-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f12
              r3 = {-# SCC "f12-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f12
              r4 = {-# SCC "f12-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f12
          print $ pretty f12
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f13-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f13
              r2 = {-# SCC "f13-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f13
              r3 = {-# SCC "f13-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f13
              r4 = {-# SCC "f13-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f13
          print $ pretty f13
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f14-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f14
              r2 = {-# SCC "f14-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f14
              r3 = {-# SCC "f14-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f14
              r4 = {-# SCC "f14-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f14
          print $ pretty f14
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f15-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f15
              r2 = {-# SCC "f15-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f15
              r3 = {-# SCC "f15-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f15
              r4 = {-# SCC "f15-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f15
          print $ pretty f15
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f16-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f16
              r2 = {-# SCC "f16-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f16
              r3 = {-# SCC "f16-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f16
              r4 = {-# SCC "f16-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f16
          print $ pretty f16
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f14b-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f14b
              r2 = {-# SCC "f14b-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f14b
              r3 = {-# SCC "f14b-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f14b
              r4 = {-# SCC "f14b-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f14b
          print $ pretty f14b
          print r1
          print r2
          print r3
          print r4