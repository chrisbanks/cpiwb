import CPi.Lib
import Tests
import CPi.ODE
import CPi.Matlab
import CPi.Semantics
import CPi.Logic
import CPi.Signals

import System.Environment (getArgs)

-- Time points
--tps = (100,(0,25))

-- Basic
f1 = Pos (0,25) (ValGT (Conc (Def "P" [])) (R 0.05))
f2 = Pos (0,25) (ValLE (Conc (Def "S" ["s"])) (R 0.01))
f3 = Nec (0,25) (ValGT (Conc (Def "E" ["e"])) (R 0.01))
f4 = Nec (0,25) (ValGT (Conc (Def "E" ["e"])) (R 0.4))

-- 1-nested TL
f5 = Nec (0,25) f1
f6 = Nec (0,25) f2
f7 = Pos (0,25) f3
f8 = Pos (0,25) f4

-- 2-nested TL
f9 = Pos (0,25) f5
f10 = Pos (0,25) f6
f11 = Nec (0,25) f7
f12 = Nec (0,25) f8

-- 3-nested TL
f13 = Nec (0,25) f9
f14 = Nec (0,25) f10
f15 = Pos (0,25) f11
f16 = Pos (0,25) f12

-- 4-nested TL
f14b = Pos (0,25) f14

-- Basic Gtee
f17 = Gtee "In" f1
f18 = Gtee "In" f2
f19 = Gtee "In" f3
f20 = Gtee "In" f4

-- 1-nested Gtee
f21 = Pos (0,25) f17
f22 = Pos (0,25) f18
f23 = Pos (0,25) f19
f24 = Pos (0,25) f20

f25 = Nec (0,25) f17
f26 = Nec (0,25) f18
f27 = Nec (0,25) f19
f28 = Nec (0,25) f20

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
          let trace = solve env solveODEoctave tps pi
          let r1 = {-# SCC "f1-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f1
              r2 = {-# SCC "f1-Trace" #-} modelCheck
                   env solveODE (Just trace) pi tps f1
          print $ pretty f1
          print r1
          print r2
          let r1 = {-# SCC "f2-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f2
              r2 = {-# SCC "f2-Trace" #-} modelCheck
                   env solveODE (Just trace) pi tps f2
          print $ pretty f2
          print r1
          print r2
          let r1 = {-# SCC "f3-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f3
              r2 = {-# SCC "f3-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f3
          print $ pretty f3
          print r1
          print r2
          let r1 = {-# SCC "f4-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f4
              r2 = {-# SCC "f4-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f4
          print $ pretty f4
          print r1
          print r2
          let r1 = {-# SCC "f5-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f5
              r2 = {-# SCC "f5-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f5
          print $ pretty f5
          print r1
          print r2
          let r1 = {-# SCC "f6-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f6
              r2 = {-# SCC "f6-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f6
          print $ pretty f6
          print r1
          print r2
          let r1 = {-# SCC "f7-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f7
              r2 = {-# SCC "f7-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f7
          print $ pretty f7
          print r1
          print r2
          let r1 = {-# SCC "f8-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f8
              r2 = {-# SCC "f8-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f8
          print $ pretty f8
          print r1
          print r2
          let r1 = {-# SCC "f9-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f9
              r2 = {-# SCC "f9-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f9
          print $ pretty f9
          print r1
          print r2
          let r1 = {-# SCC "f10-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f10
              r2 = {-# SCC "f10-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f10
          print $ pretty f10
          print r1
          print r2
          let r1 = {-# SCC "f11-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f11
              r2 = {-# SCC "f11-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f11
          print $ pretty f11
          print r1
          print r2
          let r1 = {-# SCC "f12-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f12
              r2 = {-# SCC "f12-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f12
          print $ pretty f12
          print r1
          print r2
          let r1 = {-# SCC "f13-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f13
              r2 = {-# SCC "f13-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f13
          print $ pretty f13
          print r1
          print r2
          let r1 = {-# SCC "f14-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f14
              r2 = {-# SCC "f14-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f14
          print $ pretty f14
          print r1
          print r2
          let r1 = {-# SCC "f15-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f15
              r2 = {-# SCC "f15-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f15
          print $ pretty f15
          print r1
          print r2
          let r1 = {-# SCC "f16-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f16
              r2 = {-# SCC "f16-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f16
          print $ pretty f16
          print r1
          print r2
          let r1 = {-# SCC "f14b-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f14b
              r2 = {-# SCC "f14b-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f14b
          print $ pretty f14b
          print r1
          print r2
