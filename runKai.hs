import CpiLib
import CpiParser
import CpiSemantics
import CpiODE(timePoints,speciesIn,timeSeries)
import CpiLogic
import CpiPlot
import CpiTest(tEnv,tProc)
import CpiMatlab

import qualified TestKai as K

import Text.ParserCombinators.Parsec
import System.IO
import Data.List as L
import qualified Data.Map as Map

import qualified Numeric.GSL as GSL
import qualified Numeric.LinearAlgebra as LA
import qualified Graphics.Plot as Plot

import Control.Concurrent(forkIO)
import System.TimeIt

-- time points:
tps = (720,(0,72))
ts = timePoints (fst tps) (snd tps)

----------------
-- species:
----------------

findSpec str = maybe Nil id (lookupSpecName K.env str)

cc0 = findSpec "CC0"
cc1 = findSpec "CC1"
cc2 = findSpec "CC2"
cc3 = findSpec "CC3"
cc4 = findSpec "CC4"
cc5 = findSpec "CC5"
cc6 = findSpec "CC6"

c0 = findSpec "C0"
c1 = findSpec "C1"
c2 = findSpec "C2"
c3 = findSpec "C3"
c4 = findSpec "C4"
c5 = findSpec "C5"
c6 = findSpec "C6"

kaib = findSpec "B"
kaia = findSpec "A"

----------------------
-- formulae:
----------------------

-- F(([C6]+[CC6])>0)
-- Eventually we get some fully phosphorylated KaiC hexamers:
fullPhos = Pos (ValGT (Plus (Conc c6) (Conc cc6)) (R 0.0))

-- Phosphorylation level:
--     (([C0]+[CC0])*0)+...+(([C6]+[CC6])*6)
--     -------------------------------------
--        ([C0]+[CC0])+...+([C6]+[CC6])    
phosL = Quot  
        (Plus (Times (Plus (Conc c0) (Conc cc0)) (R 0)) 
         (Plus (Times (Plus (Conc c1) (Conc cc1)) (R 1)) 
          (Plus (Times (Plus (Conc c2) (Conc cc2)) (R 2)) 
           (Plus (Times (Plus (Conc c3) (Conc cc3)) (R 3)) 
            (Plus (Times (Plus (Conc c4) (Conc cc4)) (R 4)) 
             (Plus (Times (Plus (Conc c5) (Conc cc5)) (R 5)) 
              (Times (Plus (Conc c6) (Conc cc6)) (R 6))))))))
        (Plus (Plus (Conc c0) (Conc cc0)) 
         (Plus (Plus (Conc c1) (Conc cc1))
          (Plus (Plus (Conc c2) (Conc cc2))
           (Plus (Plus (Conc c3) (Conc cc3))
            (Plus (Plus (Conc c4) (Conc cc4))
             (Plus (Plus (Conc c5) (Conc cc5))
              (Plus (Conc c6) (Conc cc6))))))))

-- F(phosL >= x)
-- Phosphorylation level reaches x (in {0..6}).
reaches x = Pos (ValGE phosL (R x))

-- F(G(phosL >= x)) 
-- Phos. level reaches x and remains >=x indefinitely
remains x = Pos (Nec (ValGE phosL (R x)))

{-- Oscillation around phos level x?
-- G(((phosL==x)==>F(phosL/=x))AND((phosL/=x)==>F(phosL==k)))
osc x = Nec (Conj (Impl (ValEq phosL (R x)) (Pos (ValNEq phosL (R x)))) 
                      (Impl (ValNEq phosL (R x)) (Pos (ValEq phosL (R x)))))
-}

-- Oscillation around phos level x
-- F(¬(G(phosL < x)OR(G(phosL > y))))
oscB x = Pos (Neg (Disj (Nec (ValLT phosL (R x))) (Nec (ValGT phosL (R x)))))

-- Oscillation of a species concentration around conc x:
-- (formula as for oscB)
oscS spec x = Pos (Neg (Disj (Nec (ValLT (Conc spec) (R x))) (Nec (ValGT (Conc spec) (R x)))))

-- [0.56]A |> oscB(3)
-- Introducing some A gives oscillation?
gtee1 = Gtee pA (oscB 3)
    where
      pA = Process [(kaia,"0.56")] (AffNet [])

-- ¬([0.5]Inhib(in):{a-in@2e5} |> oscB(3))
-- Introducing Inhib kills osc.
--  Inib binds KaiB
ginhib = Neg (Gtee pIn (oscB 3))
pIn = Process [(Def "Inhib" ["in"],"0.5")] (AffNet [Aff (("in","a"),"2e5")])
-- Inhib(in) = {x-u@5.0} in<x>.u.Inhib(in)
inhib = SpeciesDef "Inhib" ["in"] 
        (New (AffNet [Aff (("x","u"),"5.0")]) 
         (Sum [(Comm "in" ["x"] [],Sum [(Comm "u" [] [],Def "Inhib" ["in"])])]))

-- G(¬([0.5]Inhib(in):{a-in@2e5} |> oscB(3)))
-- nested gtee: Whenever we introduce Inhib it kills the osc.?
ginhibG = Nec (Neg (Gtee pIn (oscB 3)))

--------------
-- computation
--------------

main = do
  -- get time series: 
  putStrLn "Solving KaiABC model..."
  let soln = solveODEoctave K.env K.kai K.dpdt tps
  timeIt $ putStrLn ("...done: " ++ show (LA.cols soln) ++ " species.")
  let ss = speciesIn K.env K.dpdt
      ss' = speciesInProc K.kai
      trace = timeSeries ts soln ss
  
  {-- plot time series
  putStrLn "Plotting..."
  plotTimeSeriesFiltered ts soln ss ss'-}

  -- We get some fully phos KaiC?
  putStrLn ("Checking :" ++ pretty fullPhos)
  let fp = modelCheck K.env solveODEoctave (Just trace) K.kai tps (fullPhos)
  timeIt $ putStrLn ("...done: " ++ show fp)

  -- Phosphorylation level:
  putStrLn ("Checking: F(phosL>=6)")
  let r6 =  modelCheck K.env solveODEoctave (Just trace) K.kai tps (reaches 6)
  timeIt $ putStrLn ("...done: " ++ show r6)
  putStrLn ("Checking: F(phosL>=5)")
  let r5 =  modelCheck K.env solveODEoctave (Just trace) K.kai tps (reaches 5)
  timeIt $ putStrLn ("...done: " ++ show r5)

  -- Phos level reaches and remains above x
  putStrLn ("Checking: F(G(PhosL>=5))")
  let rem5 =  modelCheck K.env solveODEoctave (Just trace) K.kai tps (remains 5)
  timeIt $ putStrLn ("...done: " ++ show rem5)

  {-- Phos level oscillates around 3:
  putStrLn ("Checking: Osc(3): ") -- ++ pretty (osc 3)) {- not pretty! -}
  let osc3 = modelCheck K.env solveODEoctave (Just trace) K.kai tps (osc 3)
  timeIt $ putStrLn ("...done: " ++ show osc3)-}

  -- Phos level oscillates between around 3:
  putStrLn ("Checking: OscB(3): ")
  let oscB3 = modelCheck K.env solveODEoctave (Just trace) K.kai tps (oscB 3)
  timeIt $ putStrLn ("...done: " ++ show oscB3)

  -- Conc of KaiB oscillates around 1M
  putStrLn ("Checking: OscS([B],1): ")
  let oscSB = modelCheck K.env solveODEoctave (Just trace) K.kai tps (oscS kaib 1)
  timeIt $ putStrLn ("...done: " ++ show oscSB)

  -- Conc of KaiB oscillates around 1M
  putStrLn ("Checking: OscS([A],0.1): ")
  let oscSA = modelCheck K.env solveODEoctave (Just trace) K.kai tps (oscS kaia 0.1)
  timeIt $ putStrLn ("...done: " ++ show oscSA)

  {-- Introducing some KaiA causes oscillation?
  putStrLn ("Checking: [0.56]A |> OscB(3): ")
  let gt1 = modelCheck K.env solveODEoctave (Just trace) K.kai tps (gtee1)
  timeIt $ putStrLn ("...done: " ++ show gt1)-}

  -- Introducing some Inhib kills the osc.?
  putStrLn ("Checking: ¬([0.5]Inhib |> OscB(3)): ")
  let gin = modelCheck (inhib : K.env) solveODEoctave (Just trace) K.kai tps (ginhib)
  timeIt $ putStrLn ("...done: " ++ show gin)

  {-- Introducing some Inhib at any time kills the osc.?
    -- Takes way too long to check right now....
  putStrLn ("Checking: G(¬([0.5]Inhib |> OscB(3))): ")
  let ginG = modelCheck (inhib : K.env) solveODEoctave (Just trace) K.kai tps (ginhibG)
  timeIt $ putStrLn ("...done: " ++ show ginG)-}