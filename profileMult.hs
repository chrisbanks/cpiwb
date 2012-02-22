import CpiTest
import CpiLogic
import CpiLib

main = do putStrLn "Doing profileSmall..."
          profileSmall
          putStrLn "...done.\nDoing profileLarge..."
          profileLarge
          putStrLn "...done."

profileLarge = do env <- tEnv "models/mapk.cpi"
                  let pi = tProc env "MAPK"
                  print $ modelCheck env pi ((0,25),250) prop

prop = Nec (ValLT (Conc (Def "Ras" ["ras"])) (R 3.0))

profileSmall = do env <- tEnv "models/testGT.cpi"
                  let pi = tProc env "Pi"
                  print $ modelCheck env pi ((0,25),250) tF3
