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
-- import CPi.Semantics
import CPi.Logic

import Data.Map (Map)
-- import qualified Data.Map as Map
import qualified Control.Exception as X
-- import qualified Control.Monad.Trans.State as S


-------------------------------------
-- Signal Monitoring for LBC and CPi
-------------------------------------

-- | Signal for a formula is a covering list of intervals, each with truth value.
type Signal = ((Double,Double),[(Double,Double)])
type SignalSet = Map Formula Signal

-- | Model checking using the Signal Monitoring technique
modelCheckSig :: Env                   -- ^ Environment
              -> ODE.Solver            -- ^ ODE solver function
              -> (Maybe Trace)         -- ^ Pre-computed time series (or Nothing)
              -> Process               -- ^ Process to execute
              -> (Int,(Double,Double)) -- ^ Time points: (points,(t0,tn))
              -> Formula               -- ^ Formula to check
              -> Bool
modelCheckSig env solver (Just trace) p (res,tms) f
    = initSat $ minCover $ sig env p solver trace f
modelCheckSig env solver Nothing p (res,tms) f
    = initSat $ minCover $ sig env p solver (solve env solver (res,tms) p) f
--TODO:  optimise: memoize sigs, overusing minCover?

-- is the formula true at the start of the signal?
initSat :: Signal -> Bool
initSat (_,[]) = False
initSat (c,(i:is)) = (fst c) == (fst i)

-- calculate the signal for an LBC formula
sig :: Env -> Process -> ODE.Solver -> Trace -> Formula -> Signal
sig _ _ _ tr T = (traceInterval tr, [traceInterval tr])
sig _ _ _ tr F = (traceInterval tr, [])
sig _ _ _ tr (ValGT v1 v2) 
    = minCover $ sigTrav tr (\t-> getVal t v1 > getVal t v2)
sig _ _ _ tr (ValGE v1 v2) 
    = minCover $ sigTrav tr (\t-> getVal t v1 >= getVal t v2)
sig _ _ _ tr (ValLT v1 v2) 
    = minCover $ sigTrav tr (\t-> getVal t v1 < getVal t v2)
sig _ _ _ tr (ValLE v1 v2) 
    = minCover $ sigTrav tr (\t-> getVal t v1 <= getVal t v2)
sig _ _ _ tr (ValEq v1 v2) 
    = minCover $ sigTrav tr (\t-> getVal t v1 == getVal t v2)
sig _ _ _ tr (ValNEq v1 v2) 
    = minCover $ sigTrav tr (\t-> getVal t v1 /= getVal t v2)
sig env p solver tr (Conj f1 f2) 
    = conjSig (sig env p solver tr f1) (sig env p solver tr f2)
sig env p solver tr (Disj f1 f2) 
    = disjSig (sig env p solver tr f1) (sig env p solver tr f2)
sig env p solver tr (Impl f1 f2) 
    = sig env p solver tr (Disj f2 (Neg f1))
sig env p solver tr (Neg f) 
    = negSig (sig env p solver tr f)
sig env p solver tr (Until i f1 f2) 
    = untilSig i (sig env p solver tr f1) (sig env p solver tr f2)
sig env p solver tr (Rels i f1 f2) 
    = sig env p solver tr (Neg (Until i (Neg f1) (Neg f2)))
sig env p solver tr (Nec i f) 
    = sig env p solver tr (Neg (Pos i (Neg f)))
sig env p solver tr (Pos i f) 
    = shiftSig i (sig env p solver tr f)
sig env p solver tr (Gtee q f) 
    = gteeSig env p solver tr (maybe (bad q) id (lookupProcName env q)) f
      where bad q = X.throw $ CpiException $
                    "Process \""++q++"\" not in the Environment."

-- Traverse the trace and produce a signal for the given
-- boolean function
sigTrav :: Trace -> (Trace -> Bool) -> Signal
sigTrav tr f = (traceInterval tr, sigTrav' tr)
    where
      sigTrav' [] = []
      sigTrav' [tr]
          | f [tr] 
              = (traceStart [tr], traceStart [tr]) : []
          | otherwise
              = []
      sigTrav' tr 
          | f tr 
              = (traceStart tr, traceStart(traceNext tr)) 
                : sigTrav' (traceNext tr)
          | otherwise
              = sigTrav' (traceNext tr)

-- The negation of a signal
negSig :: Signal -> Signal
negSig (c,is) = (c, ns c is)
    where
      ns t []
          | fst t < snd t
              = (fst t, snd t):[]
          | otherwise
              = []
      ns t (i:is)
          | fst i > fst t
              = (fst t, fst i) : ns (fst i, snd t) is
          | otherwise
              = ns (snd i, snd t) is

-- The minimal common (uniform) coverings of two signals
mcc :: (Signal,Signal) -> (Signal,Signal)
mcc (((c0,cn),is), ((d0,dn),js))
    -- make sure range is the same
    | c0 < d0 = (((c0,cn),is), ((c0,dn),js))
    | c0 > d0 = (((d0,cn),is), ((d0,dn),js))
    | cn < dn = (((c0,dn),is), ((d0,dn),js))
    | cn > dn = (((c0,cn),is), ((d0,cn),js))
    -- make sure there are no overlaps
    | otherwise = (((c0,cn), mccr js is), ((d0,dn), mccr is js))
    where
      mccr (i:is) (j:js)
          | fst i < fst j && snd i <= fst j
              = mccr is (j:js)
          | fst i < fst j && snd i > fst j && snd i > snd j
              = j : mccr (i:is) js
          | fst i < fst j && snd i > fst j && snd i <= snd j
              = (fst j, snd i) : mccr is ((snd i, snd j):js)
          | fst i == fst j && snd i >= snd j
              = j : mccr (i:is) js
          | fst i == fst j && snd i < snd j
              = (fst j, snd i) : mccr is ((snd i, snd j):js)
          | fst i > fst j && fst i >= snd j
              = j : mccr (i:is) js
          | fst i > fst j && fst i < snd j
              = (fst j, fst i) : mccr (i:is) ((fst i, snd j):js)
          | otherwise = undefined --to satisfy the case checker
      mccr [] js = js
      mccr _ [] = []

-- The conjunction of two signals
conjSig :: Signal -> Signal -> Signal
conjSig s1 s2 
    | c == d = minCover (c, conjSig' is js)
    | otherwise = undefined
    where
      ((c,is),(d,js)) = mcc (s1,s2)
      conjSig' [] _ = []
      conjSig' _ [] = []
      conjSig' (i:is) (j:js) 
          | j < i  = conjSig' (i:is) js
          | j > i  = conjSig' is (j:js)
          | otherwise = i : conjSig' is js

-- The disjunction of two signals
disjSig :: Signal -> Signal -> Signal
disjSig s1 s2 
    | c == d = minCover (c, disjSig' is js)
    | otherwise = undefined
    where
      ((c,is),(d,js)) = mcc (s1,s2)
      disjSig' [] js = js
      disjSig' is [] = is
      disjSig' (i:is) (j:js) 
          | j < i  = j : disjSig' (i:is) js
          | j > i  = i : disjSig' is (j:js)
          | otherwise = i : disjSig' is js

-- the Minkowski difference of two intervals
minkDiff :: (Double,Double) -> (Double,Double) -> (Double,Double)
minkDiff (m,n) (a,b) = (m-b,n-a)

-- Shift a signal by [a,b] -- the Minkowski difference of each 
-- positive interval, intersecting the covering range.
shiftSig :: (Double,Double) -> Signal -> Signal
shiftSig int ((c0,cn),is) = minCover $ ((c0,cn), shift' int is)
    where
      shift' _ [] = []
      shift' (a,b) ((m,n):is)
          | a>b || a<0 || b<0
              = X.throw $ CpiException
                "CPi.Signals.shiftSig only takes positive intervals"
          | otherwise
              = (\(x,y)->(max x c0, min y cn)) (minkDiff (m,n) (a,b)) 
                : shift' (a,b) is

-- decompose a signal into unitary signals
decomposeSig :: Signal -> [Signal]
decomposeSig (_,[]) = []
decomposeSig (c,i:is) = (c,[i]) : decomposeSig (c,is) 

-- compose a signal of given cover from a list of unitary signals
composeSig :: [Signal] -> Signal
composeSig sigs = foldr disjSig (cover (head sigs),[]) sigs

-- temporal until of two signal
untilSig :: (Double,Double) -> Signal -> Signal -> Signal
untilSig (a,b) p q 
    = composeSig $
      map (\x->conjSig x (shiftSig (a,b) (conjSig x q))) (decomposeSig p)

-- produce the signal for a guarantee
-- NOTE: here we calculate the signal at every time point
--      of the original trace. Can we do better?
gteeSig :: Env 
        -> Process
        -> ODE.Solver 
        -> Trace
        -> Process 
        -> Formula 
        -> Signal
gteeSig env p solver tr q f 
    = minCover $ 
      (traceInterval tr, gteeSig' env p solver (traceInterval tr) q f tr)
    where
      gteeSig' _ _ _ _ _ _ [] = []
      gteeSig' env p solver i q f trs
          | modelCheckSig env solver Nothing
            (compose (constructP p trs) q)
            (traceLength tr,(0,simTime f)) f
              = (traceStart trs, end)
                : gteeSig' env p solver i q f (traceNext trs)
          | otherwise
              = gteeSig' env p solver i q f (traceNext trs)
          where
            end = if traceNext trs == [] 
                  then traceStart trs 
                  else traceStart (traceNext trs)               

-- The minimal covering of a signal
minCover :: Signal -> Signal
minCover (c,is) = (c, minCover' is)
    where
      minCover' [] = []
      minCover' [i] = [i]
      minCover' (i1:i2:is)
          | (snd i1 >= fst i2) 
              = minCover' $ (fst i1, snd i2) : is
          | otherwise 
              = i1 : minCover' (i2:is)

-- Test if two signals have uniform covering
uniform :: Signal -> Signal -> Bool
uniform (c,is) (d,js) = c == d && uniform' is js
    where
      uniform' [] [] = True
      uniform' [] (j:js) = True
      uniform' (i:is) [] = True
      uniform' (i:is) (j:js) 
          | fst i > fst j
              = fst i >= snd j && uniform' (i:is) js
          | fst i < fst j
              = fst j >= snd i && uniform' is (j:js)
          | otherwise
              = snd i == snd j && uniform' is js 

-- Give the covering range of the signal
cover :: Signal -> (Double,Double)
cover (c,_) = c

---------------------------
-- Tests
---------------------------

--all tests
tests = [test1,test2,test3,test4,test5,test6,
         test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,
         test21,test22,test23,test24,test25,test26,test27]


-- min common cover
test1 = not (uniform tSig1 tSig2)
test2 = uncurry uniform $ mcc (tSig1,tSig2)
-- conjunction/disjunction
test3 = conjSig tSig1 tSig2 == tConj12
test4 = disjSig tSig1 tSig2 == tDisj12
test5 = conjSig tSig3 tSig4 == tConj34
test6 = disjSig tSig3 tSig4 == tDisj34

-- shift
test11 = shiftSig (0,1) tSig1 == tSig1shift01
test12 = shiftSig (0,2) tSig1 == tSig1shift02
test13 = shiftSig (0,3) tSig1 == tSig1shift03
test14 = shiftSig (1,2) tSig1 == tSig1shift12

-- until
test15 = decomposeSig tSigP == [tSigP1,tSigP2]
test16 = conjSig tSigP1 tSigQ == tConjP1Q
test17 = conjSig tSigP2 tSigQ == tConjP2Q
test18 = shiftSig (1,2) tConjP1Q == tShift12ConjP1Q 
test19 = shiftSig (1,2) tConjP2Q == tShift12ConjP2Q 
test20 = conjSig tShift12ConjP1Q tSigP1 == tConjR1P1
test21 = conjSig tShift12ConjP2Q tSigP2 == tConjR2P2
test22 = disjSig tConjR1P1 tConjR2P2 == tPUntilQ
test23 = composeSig [tConjR1P1, tConjR2P2] == tPUntilQ
test24 = untilSig (1,2) tSigP tSigQ == tPUntilQ

-- relative bounds
test25 = shiftSig (0,2) tSigL == tShift2L
test26 = conjSig tSigH tShift2L == tHConjFL
test27 = shiftSig (0,5) tHConjFL == tShift5HFL

-- more until tests
test28 = decomposeSig tSigS11i == [tSigS11i1,tSigS11i2,tSigS11i3]
test29 = conjSig tSigS11i1 tSigS11d == tConjid
test30 = conjSig tSigS11i2 tSigS11d == tConjid
test31 = conjSig tSigS11i3 tSigS11d == tConjid
test32 = shiftSig (0,6) tConjid == tConjid

-------------
-- Test data
-------------

tSig1 = ((0,8), [(1,3),(6,8)]) :: Signal
tSig2 = ((0,8), [(2,7)]) :: Signal
tConj12 = ((0,8), [(2,3),(6,7)]) :: Signal
tDisj12 = ((0,8), [(1,8)]) :: Signal
tSig3 = ((0,8), [(0,2),(3,5)]) :: Signal
tSig4 = ((0,8), [(0,1),(2,4),(5,7)]) :: Signal
tConj34 = ((0,8), [(0,1),(3,4)]) :: Signal 
tDisj34 = ((0,8), [(0,7)]) :: Signal
tSig1shift01 = ((0,8), [(0,3),(5,8)]) ::Signal
tSig1shift02 = ((0,8), [(0,3),(4,8)]) ::Signal
tSig1shift03 = ((0,8), [(0,8)]) ::Signal
tSig1shift12 = ((0,8), [(0,2),(4,7)]) :: Signal

tSigP = ((0,8), [(1,3),(4,7)]) :: Signal
tSigQ = ((0,8), [(4,5),(6,8)]) :: Signal
tSigP1 = ((0,8), [(1,3)]) :: Signal
tSigP2 = ((0,8), [(4,7)]) :: Signal
tConjP1Q = ((0,8), []) :: Signal
tConjP2Q = ((0,8), [(4,5),(6,7)]) :: Signal
tShift12ConjP1Q = tConjP1Q -- = R1
tShift12ConjP2Q = ((0,8), [(2,6)]) :: Signal -- = R2
tConjR1P1 = tConjP1Q
tConjR2P2 = ((0,8), [(4,6)]) :: Signal
tPUntilQ = tConjR2P2

tSigH = ((0,10), [(4,5)]) :: Signal
tSigL = ((0,10), [(0,4),(5,10)]) ::Signal
tShift2L = ((0,10), [(0,10)]) :: Signal -- = FL
tHConjFL = ((0,10), [(4,5)]) :: Signal -- = HFL
tShift5HFL = ((0, 10), [(0,5)]) :: Signal

tSigS11i = ((0,6), [(0,1),(2,3),(4,5)]) :: Signal
tSigS11d = ((0,6), [(1,2),(3,4),(5,6)]) :: Signal
tSigS11i1 = ((0,6), [(0,1)]) :: Signal
tSigS11i2 = ((0,6), [(2,3)]) :: Signal
tSigS11i3 = ((0,6), [(4,5)]) :: Signal
tConjid = ((0,6),[]) :: Signal

