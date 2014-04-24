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

propPhi = (ValGT (Conc (Def "P" [])) (R 0.05))
propPsi = (ValLE (Conc (Def "S" ["s"])) (R 0.01))
propChi = (ValGT (Conc (Def "E" ["e"])) (R 0.01))
propOmega = (ValGT (Conc (Def "E" ["e"])) (R 0.4))


main = do env <- tEnv "models/testGT.cpi"
          res <- getArgs
          let tps = (read(res!!0),(0,25))
          let pi = tProc env "Pi"
          let trace = solve env solveODEoctave tps pi
          let r1 = {-# SCC "propPhi-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps propPhi
              r2 = {-# SCC "propPhi-Trace" #-} modelCheck
                   env solveODE (Just trace) pi tps propPhi
          print $ pretty propPhi
          print r1
          print r2
          let r1 = {-# SCC "propPsi-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps propPsi
              r2 = {-# SCC "propPsi-Trace" #-} modelCheck
                   env solveODE (Just trace) pi tps propPsi
          print $ pretty propPsi
          print r1
          print r2
          let r1 = {-# SCC "propChi-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps propChi
              r2 = {-# SCC "propChi-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps propChi
          print $ pretty propChi
          print r1
          print r2
          let r1 = {-# SCC "propOmega-Signal" #-} modelCheckSig 
                   env solveODE (Just trace) pi tps propOmega
              r2 = {-# SCC "propOmega-Trace" #-} modelCheck 
                   env solveODE (Just trace) pi tps propOmega
          print $ pretty propOmega
          print r1
          print r2
         