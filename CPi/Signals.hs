-- (C) Copyright Chris Banks 2011-2013

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

module CPi.Signals 
    (modelCheckSig
    )where

import CPi.Lib 
import qualified CPi.ODE as ODE
import CPi.Semantics
import CPi.Logic

import Data.Map (Map)
import qualified Data.Map as Map

-------------------------------------
-- Signal Monitoring for LBC and CPi
-------------------------------------

-- Signal for a formula is a covering list of intervals with truth value.
type Signal = [((Double,Double),Bool)]
type SignalSet = Map Formula Signal

-- | Model checking using the Signal Monitoring technique
modelCheckSig :: Env                   -- ^ Environment
              -> ODE.Solver            -- ^ ODE solver function
              -> (Maybe Trace)         -- ^ Pre-computed time series (or Nothing)
              -> Process               -- ^ Process to execute
              -> (Int,(Double,Double)) -- ^ Time points: (points,(t0,tn))
              -> Formula               -- ^ Formula to check
              -> Bool
modelCheckSig env solver trace p tps f 
    | (trace == Nothing)
        = initSat $ mt $ basicSigs (solve env solver tps p) f
    | otherwise
        = initSat $ mt $ basicSigs ((\(Just x)->x) trace) f
    where
      mt :: SignalSet -> SignalSet
      mt = undefined --TODO:#######################
      
      initSat :: SignalSet -> Bool 
      initSat = undefined --TODO:###################

-- Produce the set of basic signals for the atomic propositions of a formula.
basicSigs :: Trace -> Formula -> SignalSet
basicSigs trace f = basicSigs' trace (aps f)
    where
      basicSigs' trace [] = Map.empty
      basicSigs' trace (f:fs) = Map.insert f (sig trace f) (basicSigs' trace fs)

-- Produce the basic signal for an AP
sig :: Trace -> Formula -> Signal
sig trace f = undefined --TODO:###############
