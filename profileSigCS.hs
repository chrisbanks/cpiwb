import CPi.Lib
import Tests
import CPi.ODE
import CPi.Matlab
import CPi.Semantics
import CPi.Logic
import CPi.Signals

import System.Environment (getArgs)


osc1 s p t  
    = Nec (0,t) (Pos (0,p) 
                 (Conj 
                  (ValGT (Deriv s) (R 0)) 
                  (Pos (0,p) (ValLT (Deriv s) (R 0)))))
osc2 m' s s' p1 p2 t tr e
    = Pos (p1,p2) (Gtee m' 
                   (Pos (0,tr) (Nec (0,t) 
                                (Conj 
                                 (ValLT (Minus (Conc s) (Conc s')) (R e)) 
                                 (ValGT (Minus (Conc s) (Conc s')) (R (-e)))
                                ))))

propPTOOsc1 s = osc1 s 24000 80000 
propPTOOsc2 s s' m' = osc2 "Pi'" s s' 20000 24000 80000 10000 10

propForallInhibOsc1 s
    = Nec (0,25000) (Gtee "Inhib" (Pos (0,30000) (propPTOOsc1 s)))
propForallInhibOsc2 s s' m'
    = Nec (0,25000) (Gtee "Inhib" (Pos (0,30000) (propPTOOsc2 s s' m')))

propForallCompOsc1 s t
    = Nec (0,25000) (Gtee "Pi2" (Pos (0,30000) (Conj (propPTOOsc1 s) (propPTOOsc1 t))))
propForallCompOsc2 s s' t t' m'
    = Nec (0,25000) (Gtee "Pi2" (Pos (0,30000) (Conj (propPTOOsc2 s s' m') (propPTOOsc2 t t' m'))))


main = do env1 <- tEnv "models/oscTest.cpi"
          res <- getArgs
          let tps = (read(res!!0),(0,80000))
          let pi1 = tProc env1 "Pi"
          let s00 = (\(Just x)->x) (lookupSpecName env1 "S00")
          let s'00 = (\(Just x)->x) (lookupSpecName env1 "S'00")
          let t00 = (\(Just x)->x) (lookupSpecName env1 "T00")
          let t'00 = (\(Just x)->x) (lookupSpecName env1 "T'00")
          let trace1 = solve env1 solveODEoctave tps pi1
          -- Osc -----------------------
          let r1 = {-# SCC "PTO-Osc1-Signal" #-} modelCheckSig 
                   env1 solveODEoctave (Just trace1) pi1 tps (propPTOOsc1 s00)
              r2 = {-# SCC "PTO-Osc1-Trace" #-} modelCheck
                   env1 solveODEoctave (Just trace1) pi1 tps (propPTOOsc1 s00)
          print $ pretty (propPTOOsc1 s00)
          print r1
          print r2
          {-let r1 = {-# SCC "PTO-Osc2-Signal" #-} modelCheckSig 
                   env1 solveODEoctave (Just trace1) pi1 tps (propPTOOsc2 s00 s'00 "Pi'")
              r2 = {-# SCC "PTO-Osc2-Trace" #-} modelCheck
                   env1 solveODEoctave (Just trace1) pi1 tps (propPTOOsc2 s00 s'00 "Pi'")
          print $ pretty (propPTOOsc2 s00 s'00 "Pi'")
          print r1
          print r2-}
          -- Inhib -----------------------------
          let r1 = {-# SCC "Inhib1-Signal" #-} modelCheckSig 
                   env1 solveODEoctave (Just trace1) pi1 tps (propForallInhibOsc1 s00)
              r2 = {-# SCC "Inhib1-Trace" #-} modelCheck
                   env1 solveODEoctave (Just trace1) pi1 tps (propForallInhibOsc1 s00)
          print $ pretty (propForallInhibOsc1 s00)
          print r1
          print r2
          {-let r1 = {-# SCC "Inhib2-Signal" #-} modelCheckSig 
                   env1 solveODEoctave (Just trace1) pi1 tps (propForallInhibOsc2 s00 s'00 "Pi'")
              r2 = {-# SCC "Inhib2-Trace" #-} modelCheck
                   env1 solveODEoctave (Just trace1) pi1 tps (propForallInhibOsc2 s00 s'00 "Pi'")
          print $ pretty (propForallInhibOsc2 s00 s'00 "Pi'")
          print r1
          print r2-}
          -- Comp ------------------------------
          let r1 = {-# SCC "Comp1-Signal" #-} modelCheckSig 
                   env1 solveODEoctave (Just trace1) pi1 tps (propForallCompOsc1 s00 t00)
              r2 = {-# SCC "Comp1-Trace" #-} modelCheck
                   env1 solveODEoctave (Just trace1) pi1 tps (propForallCompOsc1 s00 t00)
          print $ pretty (propForallCompOsc1 s00 t00)
          print r1
          print r2
          {-let r1 = {-# SCC "Comp2-Signal" #-} modelCheckSig 
                   env1 solveODEoctave (Just trace1) pi1 tps (propForallCompOsc2 s00 s'00 t00 t'00 "Pi12'")
              r2 = {-# SCC "Comp2-Trace" #-} modelCheck
                   env1 solveODEoctave (Just trace1) pi1 tps (propForallCompOsc2 s00 s'00 t00 t'00 "Pi12'")
          print $ pretty (propForallCompOsc2 s00 s'00 t00 t'00 "Pi12'")
          print r1
          print r2-}