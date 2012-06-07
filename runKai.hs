import CpiLib
import CpiParser
import CpiSemantics
import CpiODE
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

-- formulae:
f1 = undefined

main = do
  putStrLn "Solving KaiABC model..."
  soln <- timeIt $ solveODEoctave K.env K.kai K.dpdt tps
  putStrLn ("...done: " ++ show (LA.cols soln) ++ " species.")
  let ss = speciesIn K.env K.dpdt
      ss' = speciesInProc K.kai
  putStrLn "Plotting..."
  plotTimeSeriesFiltered ts soln ss ss'
  putStrLn "...done."
  
