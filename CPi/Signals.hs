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

-- | Signal for a formula is a covering list of intervals, each with truth value.
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
        = initSat $ combine f $ basicSigs (solve env solver tps p) f
    | otherwise
        = initSat $ combine f $ basicSigs ((\(Just x)->x) trace) f
    where
      combine :: Formula -> SignalSet -> SignalSet
      combine = undefined --TODO:###########################
      
      initSat :: SignalSet -> Bool 
      initSat ss = maybe False (\x->snd(head(x))) (Map.lookup f ss)

-- Produce the set of basic signals for the atomic propositions of a formula.
basicSigs :: Trace -> Formula -> SignalSet
basicSigs tr f = basicSigs' tr (aps f)
    where
      basicSigs' tr [] = Map.empty
      basicSigs' tr (f:fs) = Map.insert f (sig tr f) (basicSigs' tr fs)

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

-- The minimal covering of a signal
minCover :: Signal -> Signal
minCover [] = []
minCover [i] = [i]
minCover (i1:i2:is)
    | (snd i1 == snd i2) 
        = minCover $ ((fst(fst i1), snd(fst i2)), snd i1) : is
    | otherwise 
        = i1 : minCover (i2:is)

-- The negation of a signal
negSig :: Signal -> Signal
negSig [] = []
negSig ((i,v):sig) = (i,not v) : negSig sig

-- The minimal common (uniform) covering of signals:
-- mccl gives the left signal w.r.t. the right
-- FIXME: assumes signals have same cover
mccl :: Signal -> Signal -> Signal
mccl (((l1,u1),v1):sig1) (((l2,u2),v2):sig2)
    | u1 < u2
        = ((l1,u1),v1) : mccl sig1 (((l2,u2),v2):sig2)
    | u1 > u2
        = ((l1,u2),v1) : mccl (((u2,u1),v1):sig1) sig2
    | otherwise
        = ((l1,u1),v1) : mccl sig1 sig2
mccl [] (s:sig) = X.throw $ CpiException 
                  "CPi.Signals.mccl requires signals of same cover"
mccl (s:sig) [] = X.throw $ CpiException
                  "CPi.Signals.mccl requires signals of same cover"
mccl [] [] = []
-- and mccr gives the right signal w.r.t. the left
-- FIXME: assumes signals have same cover
mccr :: Signal -> Signal -> Signal
mccr (((l1,u1),v1):sig1) (((l2,u2),v2):sig2)
    | u1 < u2
        = ((l2,u1),v2) : mccr sig1 (((u1,u2),v2):sig2)
    | u1 > u2
        = ((l2,u2),v2) : mccr (((u2,u1),v1):sig1) sig2
    | otherwise
        = ((l2,u2),v2) : mccr sig1 sig2
mccr [] (s:sig) = X.throw $ CpiException
                  "CPi.Signals.mccr requires signals of same cover"
mccr (s:sig) [] = X.throw $ CpiException
                  "CPi.Signals.mccr requires signals of same cover"
mccr [] [] = []

-- Test if two signals have uniform covering
uniform :: Signal -> Signal -> Bool
uniform ((i,_):s) ((j,_):t)
    | i==j      = uniform s t
    | otherwise = False
uniform ((i,_):s) [] = False
uniform [] ((i,_):s) = False
uniform [] [] = True

-- The conjunction of two signals
conjSig :: Signal -> Signal -> Signal
conjSig sig1 sig2 = minCover $ conjSig' (mccl sig1 sig2) (mccr sig1 sig2)
    where
      conjSig' ((i1,v1):s1) ((i2,v2):s2)
          | i1==i2
              = (i1,(v1 && v2)) : conjSig' s1 s2
          | otherwise
              = X.throw $ CpiException
                "CPi.Signals.conjSig: signals are not uniform!"
      conjSig' [] (s:sig) = X.throw $ CpiException
                            "CPi.Signals.conjSig: signals are not uniform!"
      conjSig' (s:sig) [] = X.throw $ CpiException
                            "CPi.Signals.conjSig: signals are not uniform!"
      conjSig' [] [] = []

-- The disjunction of two signals
disjSig :: Signal -> Signal -> Signal
disjSig sig1 sig2 = minCover $ disjSig' (mccl sig1 sig2) (mccr sig1 sig2)
    where
      disjSig' ((i1,v1):s1) ((i2,v2):s2)
          | i1==i2
              = (i1,(v1 || v2)) : disjSig' s1 s2
          | otherwise
              = X.throw $ CpiException
                "CPi.Signals.disjSig: signals are not uniform!"
      disjSig' [] (s:sig) = X.throw $ CpiException
                            "CPi.Signals.disjSig: signals are not uniform!"
      disjSig' (s:sig) [] = X.throw $ CpiException
                            "CPi.Signals.disjSig: signals are not uniform!"
      disjSig' [] [] = []



---------------------------
-- Tests
---------------------------

test1 = not (uniform testSig1 testSig2)
test2 = uniform (mccl testSig1 testSig2) (mccr testSig1 testSig2)
test3 = conjSig testSig1 testSig2 == testConj12
test4 = disjSig testSig1 testSig2 == testDisj12
test5 = conjSig testSig3 testSig4 == testConj34
test6 = disjSig testSig3 testSig4 == testDisj34

-------------
-- Test data
-------------

testSig1 = [((0,1),False),
            ((1,3),True),
            ((3,6),False),
            ((6,8),True)] :: Signal
testSig2 = [((0,2),False),
            ((2,7),True),
            ((7,8),False)] :: Signal
testConj12 = [((0,2),False),
              ((2,3),True),
              ((3,6),False),
              ((6,7),True),
              ((7,8),False)] :: Signal
testDisj12 = [((0,1),False),
              ((1,8),True)] :: Signal
testSig3 = [((0,2),True),
            ((2,3),False),
            ((3,5),True),
            ((5,7),False)] :: Signal
testSig4 = [((0,1),True),
            ((1,2),False),
            ((2,4),True),
            ((4,5),False),
            ((5,7),True)] :: Signal
testConj34 = [((0,1),True),
              ((1,3),False),
              ((3,4),True),
              ((4,7),False)] :: Signal 
testDisj34 = [((0,7),True)] :: Signal