-- (C) Copyright Chris Banks 2011-2012

-- This file is part of The Continuous Pi-calculus Workbench (CPiWB). 

--     CPiWB is free software: you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation, either version 3 of the License, or
--     (at your option) any later version.

--     CPiWB is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.

--     You should have received a copy of the GNU General Public License
--     along with CPiWB.  If not, see <http://www.gnu.org/licenses/>.

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module TestKai where

import CpiLib
import CpiParser
import CpiSemantics
import CpiODE
import CpiLogic
import CpiPlot
import CpiTest(tEnv,tProc)
import CpiMatlab

import Text.ParserCombinators.Parsec
import System.IO
import Data.List as L
import qualified Data.Map as Map

import qualified Numeric.GSL as GSL
import qualified Numeric.LinearAlgebra as LA
import qualified Graphics.Plot as Plot

import System.IO.Unsafe


--------------------------------------------------------
-- set up some bindings for playing with KaiABC in GHCi
--------------------------------------------------------

env = unsafePerformIO $ tEnv "models/kaiABC.2.cpi"
kai = tProc env "Kai"
mts = processMTS env kai
kai' = wholeProc env kai mts
dpdt = dPdt' env mts kai'
odes = xdot env dpdt
jacob = jac env dpdt
ivs = initials env kai' dpdt
ts = timePoints 720 (0,72)
soln = solveODE odes ivs ts
soln' = solveODE' odes jacob ivs ts
soln'' = solveODE'' odes ivs ts

psize (Process scs net) = length scs
tsize (MTS ts) = length ts
dsize p = Map.size p

out :: (Pretty a)=>a->IO ()
out = putStrLn . pretty
outs :: (Pretty a)=>[a]->IO ()
outs = putStrLn . prettys
outD = putStrLn . prettyODE env

listPrimes (MTS trs) = L.nub $ map transSrc trs