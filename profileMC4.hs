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

-- 2 nested Gtees
f37 = Gtee "Q'" f21
f38 = Gtee "Q'" f22
f39 = Gtee "Q'" f23
f40 = Gtee "Q'" f24
f41 = Gtee "Q'" f25
f42 = Gtee "Q'" f26
f43 = Gtee "Q'" f27
f44 = Gtee "Q'" f28

-- sandwich nested Gtees
f45 = Pos (0,infty) f37
f46 = Pos (0,infty) f38
f47 = Pos (0,infty) f39
f48 = Pos (0,infty) f40
f49 = Pos (0,infty) f41
f50 = Pos (0,infty) f42
f51 = Pos (0,infty) f43
f52 = Pos (0,infty) f44

f53 = Nec (0,infty) f37
f54 = Nec (0,infty) f38
f55 = Nec (0,infty) f39
f56 = Nec (0,infty) f40
f57 = Nec (0,infty) f41
f58 = Nec (0,infty) f42
f59 = Nec (0,infty) f43
f60 = Nec (0,infty) f44


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
          
          let r1 = {-# SCC "f4-Naive" #-} modelCheck 
                   env solveODE (Nothing) pi tps f4
              r2 = {-# SCC "f4-DynProg" #-} modelCheckDP 
                   env solveODE (Nothing) pi tps f4
              r3 = {-# SCC "f4-Hybrid" #-} modelCheckHy 
                   env solveODE (Nothing) pi tps f4
              r4 = {-# SCC "f4-Hybrid2" #-} modelCheckHy2
                   env solveODE (Nothing) pi tps f4
          print $ pretty f4
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f20-Naive" #-} modelCheck 
                   env solveODE (Nothing) pi tps f20
              r2 = {-# SCC "f20-DynProg" #-} modelCheckDP 
                   env solveODE (Nothing) pi tps f20
              r3 = {-# SCC "f20-Hybrid" #-} modelCheckHy 
                   env solveODE (Nothing) pi tps f20
              r4 = {-# SCC "f20-Hybrid2" #-} modelCheckHy2
                   env solveODE (Nothing) pi tps f20
          print $ pretty f20
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f24-Naive" #-} modelCheck 
                   env solveODE (Nothing) pi tps f24
              r2 = {-# SCC "f24-DynProg" #-} modelCheckDP 
                   env solveODE (Nothing) pi tps f24
              r3 = {-# SCC "f24-Hybrid" #-} modelCheckHy 
                   env solveODE (Nothing) pi tps f24
              r4 = {-# SCC "f24-Hybrid2" #-} modelCheckHy2
                   env solveODE (Nothing) pi tps f24
          print $ pretty f24
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f40-Naive" #-} modelCheck 
                   env solveODE (Nothing) pi tps f40
              r2 = {-# SCC "f40-DynProg" #-} modelCheckDP 
                   env solveODE (Nothing) pi tps f40
              r3 = {-# SCC "f40-Hybrid" #-} modelCheckHy 
                   env solveODE (Nothing) pi tps f40
              r4 = {-# SCC "f40-Hybrid2" #-} modelCheckHy2
                   env solveODE (Nothing) pi tps f40
          print $ pretty f40
          print r1
          print r2
          print r3
          print r4
          let r1 = {-# SCC "f48-Naive" #-} modelCheck 
                   env solveODE (Nothing) pi tps f48
              r2 = {-# SCC "f48-DynProg" #-} modelCheckDP 
                   env solveODE (Nothing) pi tps f48
              r3 = {-# SCC "f48-Hybrid" #-} modelCheckHy 
                   env solveODE (Nothing) pi tps f48
              r4 = {-# SCC "f48-Hybrid2" #-} modelCheckHy2
                   env solveODE (Nothing) pi tps f48
          print $ pretty f48
          print r1
          print r2
          print r3
          print r4
