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
          let r1 = {-# SCC "f3-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f3
              r2 = {-# SCC "f3-Trace" #-} modelCheck
                   env solveODE (Just trace) pi tps f3
          print $ pretty f3
          print r1
          print r2
          let r1 = {-# SCC "f19-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f19
              r2 = {-# SCC "f19-Trace" #-} modelCheck
                   env solveODE (Just trace) pi tps f19
          print $ pretty f19
          print r1
          print r2
          let r1 = {-# SCC "f27-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f27
              r2 = {-# SCC "f27-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f27
          print $ pretty f27
          print r1
          print r2
          let r1 = {-# SCC "f43-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f43
              r2 = {-# SCC "f43-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f43
          print $ pretty f43
          print r1
          print r2
          let r1 = {-# SCC "f59-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps f59
              r2 = {-# SCC "f59-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps f59
          print $ pretty f59
          print r1
          print r2
          