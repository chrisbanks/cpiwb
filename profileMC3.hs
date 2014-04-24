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

-- nested Gtee and nested TL
f29 = Nec (0,infty) f21
f30 = Nec (0,infty) f22
f31 = Nec (0,infty) f23
f32 = Nec (0,infty) f24
f33 = Pos (0,infty) f25
f34 = Pos (0,infty) f26
f35 = Pos (0,infty) f27
f36 = Pos (0,infty) f28

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
          
          let r1 = {-# SCC "f29-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f29
              r2 = {-# SCC "f29-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f29
              r3 = {-# SCC "f29-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f29
              r4 = {-# SCC "f29-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f29
          print $ pretty f29
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f30-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f30
              r2 = {-# SCC "f30-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f30
              r3 = {-# SCC "f30-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f30
              r4 = {-# SCC "f30-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f30
          print $ pretty f30
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f31-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f31
              r2 = {-# SCC "f31-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f31
              r3 = {-# SCC "f31-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f31
              r4 = {-# SCC "f31-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f31
          print $ pretty f31
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f32-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f32
              r2 = {-# SCC "f32-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f32
              r3 = {-# SCC "f32-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f32
              r4 = {-# SCC "f32-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f32
          print $ pretty f32
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f33-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f33
              r2 = {-# SCC "f33-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f33
              r3 = {-# SCC "f33-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f33
              r4 = {-# SCC "f33-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f33
          print $ pretty f33
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f34-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f34
              r2 = {-# SCC "f34-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f34
              r3 = {-# SCC "f34-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f34
              r4 = {-# SCC "f34-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f34
          print $ pretty f34
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f35-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f35
              r2 = {-# SCC "f35-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f35
              r3 = {-# SCC "f35-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f35
              r4 = {-# SCC "f35-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f35
          print $ pretty f35
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f36-Naive" #-} modelCheck 
                   env solveODE (Just trace) pi tps f36
              r2 = {-# SCC "f36-DynProg" #-} modelCheckDP 
                   env solveODE (Just trace) pi tps f36
              r3 = {-# SCC "f36-Hybrid" #-} modelCheckHy 
                   env solveODE (Just trace) pi tps f36
              r4 = {-# SCC "f36-Hybrid2" #-} modelCheckHy2
                   env solveODE (Just trace) pi tps f36
          print $ pretty f36
          print r1
          print r2
          print r3
          print r4
          