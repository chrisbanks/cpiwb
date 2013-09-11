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
f6 = Nec (0,25) f1
f7 = Pos (0,25) f1
f8 = Pos (0,25) f1

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
f29 = Nec (0,25) f21
f30 = Nec (0,25) f22
f31 = Nec (0,25) f23
f32 = Nec (0,25) f24
f33 = Pos (0,25) f25
f34 = Pos (0,25) f26
f35 = Pos (0,25) f27
f36 = Pos (0,25) f28

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
f45 = Pos (0,25) f37
f46 = Pos (0,25) f38
f47 = Pos (0,25) f39
f48 = Pos (0,25) f40
f49 = Pos (0,25) f41
f50 = Pos (0,25) f42
f51 = Pos (0,25) f43
f52 = Pos (0,25) f44

f53 = Nec (0,25) f37
f54 = Nec (0,25) f38
f55 = Nec (0,25) f39
f56 = Nec (0,25) f40
f57 = Nec (0,25) f41
f58 = Nec (0,25) f42
f59 = Nec (0,25) f43
f60 = Nec (0,25) f44



main = do env <- tEnv "models/testGT.cpi"
          res <- getArgs
          let tps = (read(res!!0),(0,25))
          let pi = tProc env "Pi"
          let trace = solve env solveODEoctave tps pi
          let r1 = {-# SCC "f21-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f21
              r2 = {-# SCC "f21-Trace" #-} modelCheck
                   env solveODE (Just trace) pi tps f21
          print $ pretty f21
          print r1
          print r2
          let r1 = {-# SCC "f22-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f22
              r2 = {-# SCC "f22-Trace" #-} modelCheck
                   env solveODE (Just trace) pi tps f22
          print $ pretty f22
          print r1
          print r2
          let r1 = {-# SCC "f23-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f23
              r2 = {-# SCC "f23-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f23
          print $ pretty f23
          print r1
          print r2
          let r1 = {-# SCC "f24-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f24
              r2 = {-# SCC "f24-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f24
          print $ pretty f24
          print r1
          print r2
          let r1 = {-# SCC "f25-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f25
              r2 = {-# SCC "f25-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f25
          print $ pretty f25
          print r1
          print r2
          let r1 = {-# SCC "f26-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f26
              r2 = {-# SCC "f26-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f26
          print $ pretty f26
          print r1
          print r2
          let r1 = {-# SCC "f27-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f27
              r2 = {-# SCC "f27-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f27
          print $ pretty f27
          print r1
          print r2
          let r1 = {-# SCC "f28-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f28
              r2 = {-# SCC "f28-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f28
          print $ pretty f28
          print r1
          print r2
          let r1 = {-# SCC "f29-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f29
              r2 = {-# SCC "f29-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f29
          print $ pretty f29
          print r1
          print r2
          let r1 = {-# SCC "f30-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f30
              r2 = {-# SCC "f30-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f30
          print $ pretty f30
          print r1
          print r2
          let r1 = {-# SCC "f31-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f31
              r2 = {-# SCC "f31-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f31
          print $ pretty f31
          print r1
          print r2
          let r1 = {-# SCC "f32-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f32
              r2 = {-# SCC "f32-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f32
          print $ pretty f32
          print r1
          print r2
          let r1 = {-# SCC "f33-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f33
              r2 = {-# SCC "f33-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f33
          print $ pretty f33
          print r1
          print r2
          let r1 = {-# SCC "f34-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f34
              r2 = {-# SCC "f34-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f34
          print $ pretty f34
          print r1
          print r2
          let r1 = {-# SCC "f35-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f35
              r2 = {-# SCC "f35-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f35
          print $ pretty f35
          print r1
          print r2
          let r1 = {-# SCC "f36-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f36
              r2 = {-# SCC "f36-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f36
          print $ pretty f36
          print r1
          print r2
