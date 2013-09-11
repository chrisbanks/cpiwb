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
          let r1 = {-# SCC "f37-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f37
              r2 = {-# SCC "f37-Trace" #-} modelCheck
                   env solveODE (Just trace) pi tps f37
          print $ pretty f37
          print r1
          print r2
          let r1 = {-# SCC "f38-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f38
              r2 = {-# SCC "f38-Trace" #-} modelCheck
                   env solveODE (Just trace) pi tps f38
          print $ pretty f38
          print r1
          print r2
          let r1 = {-# SCC "f39-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f39
              r2 = {-# SCC "f39-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f39
          print $ pretty f39
          print r1
          print r2
          let r1 = {-# SCC "f40-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f40
              r2 = {-# SCC "f40-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f40
          print $ pretty f40
          print r1
          print r2
          let r1 = {-# SCC "f41-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f41
              r2 = {-# SCC "f41-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f41
          print $ pretty f41
          print r1
          print r2
          let r1 = {-# SCC "f42-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f42
              r2 = {-# SCC "f42-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f42
          print $ pretty f42
          print r1
          print r2
          let r1 = {-# SCC "f43-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f43
              r2 = {-# SCC "f43-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f43
          print $ pretty f43
          print r1
          print r2
          let r1 = {-# SCC "f44-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f44
              r2 = {-# SCC "f44-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f44
          print $ pretty f44
          print r1
          print r2
          let r1 = {-# SCC "f45-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f45
              r2 = {-# SCC "f45-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f45
          print $ pretty f45
          print r1
          print r2
          let r1 = {-# SCC "f46-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f46
              r2 = {-# SCC "f46-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f46
          print $ pretty f46
          print r1
          print r2
          let r1 = {-# SCC "f47-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f47
              r2 = {-# SCC "f47-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f47
          print $ pretty f47
          print r1
          print r2
          let r1 = {-# SCC "f48-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f48
              r2 = {-# SCC "f48-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f48
          print $ pretty f48
          print r1
          print r2
          let r1 = {-# SCC "f49-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f49
              r2 = {-# SCC "f49-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f49
          print $ pretty f49
          print r1
          print r2
          let r1 = {-# SCC "f50-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f50
              r2 = {-# SCC "f50-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f50
          print $ pretty f50
          print r1
          print r2
          let r1 = {-# SCC "f51-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f51
              r2 = {-# SCC "f51-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f51
          print $ pretty f51
          print r1
          print r2
          let r1 = {-# SCC "f52-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f52
              r2 = {-# SCC "f52-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f52
          print $ pretty f52
          print r1
          print r2
          let r1 = {-# SCC "f53-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f53
              r2 = {-# SCC "f53-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f53
          print $ pretty f53
          print r1
          print r2
          let r1 = {-# SCC "f54-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f54
              r2 = {-# SCC "f54-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f54
          print $ pretty f54
          print r1
          print r2
          let r1 = {-# SCC "f55-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f55
              r2 = {-# SCC "f55-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f55
          print $ pretty f55
          print r1
          print r2
          let r1 = {-# SCC "f56-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f56
              r2 = {-# SCC "f56-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f56
          print $ pretty f56
          print r1
          print r2
          let r1 = {-# SCC "f57-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f57
              r2 = {-# SCC "f57-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f57
          print $ pretty f57
          print r1
          print r2
          let r1 = {-# SCC "f58-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f58
              r2 = {-# SCC "f58-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f58
          print $ pretty f58
          print r1
          print r2
          let r1 = {-# SCC "f59-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f59
              r2 = {-# SCC "f59-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f59
          print $ pretty f59
          print r1
          print r2
          let r1 = {-# SCC "f60-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f60
              r2 = {-# SCC "f60-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f60
          print $ pretty f60
          print r1
          print r2
          