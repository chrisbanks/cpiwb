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
    (modelCheckSig,
     Signal,
     SignalSet
    )where

import CPi.Lib 
import qualified CPi.ODE as ODE
import CPi.Semantics
import CPi.Logic

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Exception as X

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
basicSigs tr f = basicSigs' tr (aps f)
    where
      basicSigs' tr [] = Map.empty
      basicSigs' tr (f:fs) = Map.insert f (sig tr f) (basicSigs' tr fs)

-- Produce the basic signal for an AP
sig :: Trace -> Formula -> Signal
sig tr T = [(traceInterval tr,True)]
sig tr F = [(traceInterval tr,False)]
sig tr (ValGT v1 v2) = minCover $ sigTrav tr (\t-> getVal t v1 > getVal t v2)
sig tr (ValGE v1 v2) = minCover $ sigTrav tr (\t-> getVal t v1 >= getVal t v2)
sig tr (ValLT v1 v2) = minCover $ sigTrav tr (\t-> getVal t v1 < getVal t v2)
sig tr (ValLE v1 v2) = minCover $ sigTrav tr (\t-> getVal t v1 <= getVal t v2)
sig tr (ValEq v1 v2) = minCover $ sigTrav tr (\t-> getVal t v1 == getVal t v2)
sig tr (ValNEq v1 v2) = minCover $ sigTrav tr (\t-> getVal t v1 /= getVal t v2)
sig _ _ = X.throw $ CpiException "CPi.Signals.sig only takes APs"

sigTrav :: Trace -> (Trace -> Bool) -> Signal 
sigTrav [] _ = []
sigTrav tr f = ((traceStart tr, traceStart(traceNext tr)), (f tr)) 
               : sigTrav (traceNext tr) f

-- Produces the minimal covering of a signal
minCover :: Signal -> Signal
minCover [] = []
minCover [i] = [i]
minCover (i1:i2:is)
    | (snd i1 == snd i2) 
        = minCover $ ((fst(fst i1), snd(fst i2)), snd i1) : is
    | otherwise 
        = i1 : minCover (i2:is)