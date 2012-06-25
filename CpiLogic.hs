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

module CpiLogic where

import CpiLib 
import CpiODE (timeSeries,timePoints,dPdt',speciesIn,Solver)
import CpiSemantics

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Exception as X

-------------------------
-- Data Structures:
-------------------------

data Formula = T                      -- True
             | F                      -- False
             | ValGT Val Val          -- x > y
             | ValGE Val Val          -- x >= y
             | ValLT Val Val          -- x < y
             | ValLE Val Val          -- x <= y
             | ValEq Val Val          -- x == y
             | ValNEq Val Val         -- x /= y
             | Conj Formula Formula   -- a AND b
             | Disj Formula Formula   -- a OR b
             | Impl Formula Formula   -- a IMPLIES b
             | Neg Formula            -- NOT a
             | Until (Double,Double) Formula Formula -- a U[t0,tn] b
             | Nec (Double,Double) Formula           -- G[t0,tn] a
             | Pos (Double,Double) Formula           -- F[t0,tn] a
             | Gtee Process Formula   -- Q |> a
               deriving (Show, Eq)

data Val = R Double   -- Real
         | Conc Species  -- Concentration of Species
         | Deriv Species -- Derivative of concentration
         | Plus Val Val  -- x+y
         | Minus Val Val -- x-y
         | Times Val Val -- x*y
         | Quot Val Val  -- x/y
           deriving (Show, Eq)


-------------------------
-- Model Checking:
-------------------------

-- This is an implementation of the trace-based model checker for LTL(R)+Guarantee

-- A trace is a time series from the ODE solver:
type Trace = [(Double, Map Species Double)]

-- The recursive model checking function
modelCheck :: Env                   -- Environment
           -> Solver                -- ODE solver function
           -> (Maybe Trace)         -- Pre-computed time series (or Nothing)
           -> Process               -- Process to execute
           -> (Int,(Double,Double)) -- Time points: (points,(t0,tn))
           -> Formula               -- Formula to check
           -> Bool
modelCheck env solver trace p tps f 
    | (trace == Nothing)
        = modelCheck' (solve env solver tps p) f
    | otherwise
        = modelCheck' ((\(Just x)->x) trace) f
    where
      modelCheck' [] _ = False
      modelCheck' _ T = True
      modelCheck' _ F = False
      modelCheck' ts (ValGT x y) = (getVal ts x) > (getVal ts y)
      modelCheck' ts (ValLT x y) = (getVal ts x) < (getVal ts y)
      modelCheck' ts (ValGE x y) = (getVal ts x) >= (getVal ts y)
      modelCheck' ts (ValLE x y) = (getVal ts x) <= (getVal ts y)
      modelCheck' ts (ValEq x y) = (getVal ts x) == (getVal ts y)
      modelCheck' ts (ValNEq x y) = (getVal ts x) /= (getVal ts y)
      modelCheck' ts (Conj x y) = modelCheck' ts x && modelCheck' ts y
      modelCheck' ts (Disj x y) = modelCheck' ts x || modelCheck' ts y
      modelCheck' ts (Impl x y) = not(modelCheck' ts x) || modelCheck' ts y
      modelCheck' ts (Neg x) = not $ modelCheck' ts x
      modelCheck' (t:ts) (Until (t0,tn) x y) 
          | (fst t) < t0
              = modelCheck' ts (Until (t0,tn) x y)
          | otherwise
              = ((fst t) <= tn) && (modelCheck' (t:ts) y ||
                                    (modelCheck' (t:ts) x && 
                                     modelCheck' ts (Until (t0,tn) x y)))
      modelCheck' ts (Pos (t0,tn) x) 
          = modelCheck' ts (Until (t0,tn) T x)
      modelCheck' ts (Nec (t0,tn) x) 
          = modelCheck' ts (Neg (Pos (t0,tn) (Neg x)))
      modelCheck' ts (Gtee p' x) = modelCheck' (solve env solver tps (compose p' p)) x

-- The dynamic programming model checking function
modelCheckDP :: Env                   -- Environment
             -> Solver                -- ODE solver function
             -> (Maybe Trace)         -- Pre-computed time series (or Nothing)
             -> Process               -- Process to execute
             -> (Int,(Double,Double)) -- Time points: (points,(t0,tn))
             -> Formula               -- Formula to check
             -> Bool
modelCheckDP env solver trace p tps f 
    | (trace == Nothing)
        = modelCheckDP' (reverse(solve env solver tps p)) (fPostOrd f') f'
    | otherwise
        = modelCheckDP' (reverse((\(Just x)->x) trace)) (fPostOrd f') f'
    where
      f' = rewriteU f
      modelCheckDP' ts sfs f = infst f (reverse(satTs ts sfs []))
      infst f [] = False
      infst f ((_,fs):_) = elem f fs
      satTs [] _ pts = pts
      satTs (t:ts) fs pts = satTs ts fs ((t,(satSubs t [] pts fs)):pts)
      satSubs _ tls _ [] = tls
      satSubs t tls pts ((F):fs) = satSubs t tls pts fs
      satSubs t tls pts ((T):fs) = satSubs t (T:tls) pts fs
      satSubs t tls pts (v@(ValGT x y):fs)
          | (getVal [t] x) > (getVal [t] y)
              = satSubs t (v:tls) pts fs
          | otherwise
              = satSubs t tls pts fs
      satSubs t tls pts (v@(ValGE x y):fs)
          | (getVal [t] x) >= (getVal [t] y)
              = satSubs t (v:tls) pts fs
          | otherwise
              = satSubs t tls pts fs
      satSubs t tls pts (v@(ValLT x y):fs)
          | (getVal [t] x) < (getVal [t] y)
              = satSubs t (v:tls) pts fs
          | otherwise
              = satSubs t tls pts fs
      satSubs t tls pts (v@(ValLE x y):fs)
          | (getVal [t] x) <= (getVal [t] y)
              = satSubs t (v:tls) pts fs
          | otherwise
              = satSubs t tls pts fs
      satSubs t tls pts (v@(ValEq x y):fs)
          | (getVal [t] x) == (getVal [t] y)
              = satSubs t (v:tls) pts fs
          | otherwise
              = satSubs t tls pts fs
      satSubs t tls pts (v@(ValNEq x y):fs)
          | (getVal [t] x) /= (getVal [t] y)
              = satSubs t (v:tls) pts fs
          | otherwise
              = satSubs t tls pts fs
      satSubs t tls pts (f@(Conj a b):fs)
          | (a `elem` tls) && (b `elem` tls)
              = satSubs t (f:tls) pts fs
          | otherwise
              = satSubs t tls pts fs
      satSubs t tls pts (f@(Disj a b):fs)
          | (a `elem` tls) || (b `elem` tls)
              = satSubs t (f:tls) pts fs
          | otherwise
              = satSubs t tls pts fs
      satSubs t tls pts (f@(Impl a b):fs)
          | not(a `elem` tls) || (b `elem` tls)
              = satSubs t (f:tls) pts fs
          | otherwise
              = satSubs t tls pts fs
      satSubs t tls pts (f@(Neg a):fs)
          | not(a `elem` tls)
              = satSubs t (f:tls) pts fs
          | otherwise
              = satSubs t tls pts fs
      satSubs t tls (pt:pts) (f@(Until (t0,tn) a b):fs)
          | ((fst t)<=tn) && (b `elem` tls)
              = satSubs t (f:tls) (pt:pts) fs
          | ((fst t)<=tn) && (a `elem` tls) && (b `elem` (snd pt))
              = satSubs t (f:tls) (pt:pts) fs
          | ((fst t)<t0) && (f `elem` (snd pt))
              = satSubs t (f:tls) (pt:pts) fs
          | otherwise
              = satSubs t tls (pt:pts) fs
      satSubs t tls [] (f@(Until (t0,tn) a b):fs)
          | ((fst t)<=tn) && (b `elem` tls)
              = satSubs t (f:tls) [] fs
          | otherwise
              = satSubs t tls [] fs
      satSubs t tls pts (f@(Gtee q a):fs)
          | (modelCheckDP env solver Nothing (compose p q) tps a)
              = satSubs t (f:tls) pts fs
          | otherwise
              = satSubs t tls pts fs
      satSubs _ _ _ ((Nec _ _):_)
          = X.throw $ CpiException "DP model checker only checks Until (not F or G)"
      satSubs _ _ _ ((Pos _ _):_)
          = X.throw $ CpiException "DP model checker only checks Until (not F or G)"

-- The hybrid model checking function
modelCheckHy :: Env                   -- Environment
             -> Solver                -- ODE solver function
             -> (Maybe Trace)         -- Pre-computed time series (or Nothing)
             -> Process               -- Process to execute
             -> (Int,(Double,Double)) -- Time points: (points,(t0,tn))
             -> Formula               -- Formula to check
             -> Bool
modelCheckHy env solver trace p tps f 
    = undefined



-- Get a value from the trace:
getVal :: Trace -> Val -> Double
getVal _ (R d) = d
getVal (t:ts) (Conc s) = maybe 0.0 id (Map.lookup s (snd t))
getVal (t:ts) (Deriv s) = X.throw $ CpiException "Concentration derivatives not implemeted yet."
getVal ts (Plus x y) = getVal ts x + getVal ts y
getVal ts (Minus x y) = getVal ts x - getVal ts y
getVal ts (Times x y) = getVal ts x * getVal ts y
getVal ts (Quot x y) = getVal ts x / getVal ts y
getVal [] _ = 0.0

-- Take a process and get a trace from the solver:
solve :: Env -> Solver -> (Int,(Double,Double)) -> Process -> Trace
solve env solver (r,(t0,tn)) p = timeSeries ts soln ss
    where
      ts = timePoints r (t0,tn)
      mts = processMTS env p
      p' = wholeProc env p mts
      dpdt = dPdt' env mts p'
      soln = solver env p dpdt (r,(t0,tn))
      ss = speciesIn env dpdt

-- the post-order flattening of a formula
fPostOrd :: Formula -> [Formula]
fPostOrd f@(Conj a b) = (fPostOrd a)++(fPostOrd b)++[f]
fPostOrd f@(Disj a b) = (fPostOrd a)++(fPostOrd b)++[f]
fPostOrd f@(Impl a b) = (fPostOrd a)++(fPostOrd b)++[f]
fPostOrd f@(Neg a) = (fPostOrd a)++[f]
fPostOrd f@(Until _ a b) = (fPostOrd a)++(fPostOrd b)++[f]
fPostOrd f@(Nec _ a) = (fPostOrd a)++[f]
fPostOrd f@(Pos _ a) = (fPostOrd a)++[f]
fPostOrd x = [x]

-- rewrite a temporal formula in terms of Until (i.e. rewrite F and G)
rewriteU :: Formula -> Formula
rewriteU (Nec i f) = Neg (Until i T (Neg (rewriteU f)))
rewriteU (Pos i f) = Until i T (rewriteU f)
rewriteU (Neg f) = Neg (rewriteU f)
rewriteU (Gtee p f) = Gtee p (rewriteU f)
rewriteU (Until i f1 f2) = Until i (rewriteU f1) (rewriteU f2)
rewriteU (Impl f1 f2) = Impl (rewriteU f1) (rewriteU f2)
rewriteU (Disj f1 f2) = Disj (rewriteU f1) (rewriteU f2)
rewriteU (Conj f1 f2) = Conj (rewriteU f1) (rewriteU f2)
rewriteU x = x


-------------------------
-- Pretty printing:
-------------------------

instance Pretty Formula where
    pretty T = "true"
    pretty F = "false"
    pretty (ValGT x y) = pretty x ++ ">" ++ pretty y
    pretty (ValGE x y) = pretty x ++ ">=" ++ pretty y
    pretty (ValLT x y) = pretty x ++ "<" ++ pretty y
    pretty (ValLE x y) = pretty x ++ "<=" ++ pretty y
    pretty (ValEq x y) = pretty x ++ "==" ++ pretty y
    pretty (ValNEq x y) = pretty x ++ "/=" ++ pretty y
    pretty z@(Conj x y) = parens x z ++ " && " ++ parens y z
    pretty z@(Disj x y) = parens x z ++ " || " ++ parens y z
    pretty z@(Impl x y) = parens x z ++ " ==> " ++ parens y z
    pretty z@(Until (t0,tn) x y)
        | (t0 == 0) && (tn == infty)
            = parens x z ++ " U " ++ parens y z
        | (t0 == 0)
            = parens x z ++ " U[" ++ show tn ++ "]" ++ parens y z
        | otherwise 
            = parens x z ++ " U[" ++ show t0 ++ "," ++ show tn ++ "]" ++ parens y z
    pretty z@(Gtee pi y) = pretty pi ++ " |> " ++ parens y z
    pretty z@(Neg x) = "¬" ++ parens x z
    pretty z@(Nec (t0,tn) x) 
        | (t0 == 0) && (tn == infty)
            = "G" ++ parens x z
        | (t0 == 0)
            = "G[" ++ show tn ++ "]" ++ parens x z
        | otherwise
            = "G[" ++ show t0 ++ "," ++ show tn ++ "]" ++ parens x z
    pretty z@(Pos (t0,tn) x) 
        | (t0 == 0) && (tn == infty)
            = "F" ++ parens x z
        | (t0 == 0)
            = "F[" ++ show tn ++ "]" ++ parens x z
        | otherwise
            = "F[" ++ show t0 ++ "," ++ show tn ++ "]" ++ parens x z
parens x c
    | ((prio x)<=(prio c))
        = pretty x
    | otherwise
        = "(" ++ pretty x ++ ")"
    where
      prio T = 10
      prio F = 10
      prio (Neg _) = 10
      prio (ValGT _ _) = 30
      prio (ValGE _ _) = 30
      prio (ValLT _ _) = 30
      prio (ValLE _ _) = 30
      prio (ValEq _ _) = 30
      prio (ValNEq _ _) = 30
      prio (Nec _ _) = 10
      prio (Pos _ _) = 10
      prio (Until _ _ _) = 40
      prio (Conj _ _) = 50
      prio (Disj _ _) = 50
      prio (Impl _ _) = 55
      prio (Gtee _ _) = 60

instance Pretty Val where
    pretty (R d) = show d
    pretty (Conc s) = "[" ++ pretty s ++ "]"
    pretty (Deriv s) = "[" ++ pretty s ++ "]'"
    pretty (Plus x y) = pretty x ++ "+" ++ pretty y
    pretty (Minus x y) = pretty x ++ "-" ++ pretty y
    pretty (Times x y) = pretty x ++ "*" ++ pretty y
    pretty (Quot x y) = pretty x ++ "/" ++ pretty y