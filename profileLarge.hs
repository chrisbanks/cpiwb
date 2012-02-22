import CpiTest
import CpiLogic
import CpiLib

main = do env <- tEnv "models/mapk.cpi"
          let pi = tProc env "MAPK"
          print $ modelCheck env pi ((0,25),250) prop

prop = Nec (ValLT (Conc (Def "Ras" ["ras"])) (R 3.0))
