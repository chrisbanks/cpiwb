import CpiTest
import CpiLogic

main = do env <- tEnv "models/testGT.cpi"
          let pi = tProc env "Pi"
          print $ modelCheck env pi ((0,25),250) tF3