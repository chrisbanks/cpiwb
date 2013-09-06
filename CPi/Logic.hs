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

module CPi.Logic 
    (Formula(..),
     Val(..),
     Trace,
     modelCheck,
     modelCheckFR,
     simTime,
     nnf,
     aps,
     reconcileSpecs,
     solve,
     getVal,
     traceNext,
     traceInterval,
     traceLength,
     traceStart,
     fPostOrd,
     constructP
    )where

import CPi.Lib 
import qualified CPi.ODE as ODE
import CPi.Semantics

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Exception as X
import qualified Control.Monad.Trans.State.Strict as S
import qualified Debug.Trace as DBG
import qualified Data.List as L

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
             | Rels (Double,Double) Formula Formula -- a R[t0,tn] b
             | Nec (Double,Double) Formula           -- G[t0,tn] a
             | Pos (Double,Double) Formula           -- F[t0,tn] a
             | Gtee String Formula   -- Q |> a
               deriving (Show, Eq, Ord)

data Val = R Double   -- Real
         | Conc Species  -- Concentration of Species
         | Deriv Species -- Derivative of concentration
         | Plus Val Val  -- x+y
         | Minus Val Val -- x-y
         | Times Val Val -- x*y
         | Quot Val Val  -- x/y
           deriving (Show, Eq, Ord)


-------------------------
-- Model Checking:
-------------------------

-- This is an implementation of the trace-based model checker for LTL(R)+Guarantee

-- A trace is a time series from the ODE solver:
--   State = (Time,   Concentrations,     Derivatives)
type State = (Double, Map Species Double, Map Species Double)
--   Trace = Sequence of states
type Trace = [State]

-- | The default model checker
modelCheck :: Env                   -- ^ Environment
           -> ODE.Solver            -- ^ ODE solver function
           -> (Maybe Trace)         -- ^ Pre-computed time series (or Nothing)
           -> Process               -- ^ Process to execute
           -> (Int,(Double,Double)) -- ^ Time points: (points,(t0,tn))
           -> Formula               -- ^ Formula to check
           -> Bool
modelCheck = modelCheckHy -- use the hybrid checker.
--modelCheck = modelCheckFR -- use formula rewriting checker.

-- The recursive model checking function
modelCheckRec :: Env                   -- Environment
              -> ODE.Solver                -- ODE solver function
              -> (Maybe Trace)         -- Pre-computed time series (or Nothing)
              -> Process               -- Process to execute
              -> (Int,(Double,Double)) -- Time points: (points,(t0,tn))
              -> Formula               -- Formula to check
              -> Bool
modelCheckRec env solver trace p tps f 
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
      modelCheck' (t@(t',_,_):ts) (Until (t0,tn) x y) 
          | t' < t0
              = modelCheck' ts (Until (t0,tn) x y)
          | otherwise
              = (t' <= tn) && (modelCheck' (t:ts) y ||
                                    (modelCheck' (t:ts) x && 
                                     modelCheck' ts (Until (t0,tn) x y)))
      modelCheck' ts (Rels i x y)
          = modelCheck' ts (Neg (Until i (Neg x) (Neg y)))
      modelCheck' ts (Pos (t0,tn) x) 
          = modelCheck' ts (Until (t0,tn) T x)
      modelCheck' ts (Nec (t0,tn) x) 
          = modelCheck' ts (Neg (Pos (t0,tn) (Neg x)))
      modelCheck' ts (Gtee p' x) 
          = modelCheck' (solve env solver tps (compose (constructP p ts) 
                                               (maybe err id (lookupProcName env p'))
                                              )) x
            where
              err = X.throw $ CpiException $ p' ++ " not a defined process."


-- DP model checker using explicit state
modelCheckDP :: Env                   -- Environment
             -> ODE.Solver             -- ODE solver function
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
      modelCheckDP' ts sfs f = f `elem` S.evalState (satTs ts sfs) []
      satTs :: [State] -> [Formula]
            -> S.State [Formula] [Formula]
      satTs [] _ = S.get >>= return
      satTs (t:ts) fs = do prev <- S.get
                           S.put $ satSubs t [] prev fs
                           satTs ts fs
      satSubs :: State -> [Formula] -> [Formula] -> [Formula] 
              -> [Formula]
      -- TODO: erk! this needs cleaning up!
      satSubs _ lbls _ [] = lbls
      satSubs t lbls prev ((F):fs) = satSubs t lbls prev fs
      satSubs t lbls prev ((T):fs) = satSubs t (T:lbls) prev fs
      satSubs t lbls prev (v@(ValGT x y):fs)
          | (getVal [t] x) > (getVal [t] y)
              = satSubs t (v:lbls) prev fs
          | otherwise
              = satSubs t lbls prev fs
      satSubs t lbls prev (v@(ValGE x y):fs)
          | (getVal [t] x) >= (getVal [t] y)
              = satSubs t (v:lbls) prev fs
          | otherwise
              = satSubs t lbls prev fs
      satSubs t lbls prev (v@(ValLT x y):fs)
          | (getVal [t] x) < (getVal [t] y)
              = satSubs t (v:lbls) prev fs
          | otherwise
              = satSubs t lbls prev fs
      satSubs t lbls prev (v@(ValLE x y):fs)
          | (getVal [t] x) <= (getVal [t] y)
              = satSubs t (v:lbls) prev fs
          | otherwise
              = satSubs t lbls prev fs
      satSubs t lbls prev (v@(ValEq x y):fs)
          | (getVal [t] x) == (getVal [t] y)
              = satSubs t (v:lbls) prev fs
          | otherwise
              = satSubs t lbls prev fs
      satSubs t lbls prev (v@(ValNEq x y):fs)
          | (getVal [t] x) /= (getVal [t] y)
              = satSubs t (v:lbls) prev fs
          | otherwise
              = satSubs t lbls prev fs
      satSubs t lbls prev (f@(Conj a b):fs)
          | (a `elem` lbls) && (b `elem` lbls)
              = satSubs t (f:lbls) prev fs
          | otherwise
              = satSubs t lbls prev fs
      satSubs t lbls prev (f@(Disj a b):fs)
          | (a `elem` lbls) || (b `elem` lbls)
              = satSubs t (f:lbls) prev fs
          | otherwise
              = satSubs t lbls prev fs
      satSubs t lbls prev (f@(Impl a b):fs)
          | not(a `elem` lbls) || (b `elem` lbls)
              = satSubs t (f:lbls) prev fs
          | otherwise
              = satSubs t lbls prev fs
      satSubs t lbls prev (f@(Neg a):fs)
          | not(a `elem` lbls)
              = satSubs t (f:lbls) prev fs
          | otherwise
              = satSubs t lbls prev fs
      satSubs t@(t',_,_) lbls [] (f@(Until (t0,tn) a b):fs)
          | (t'<=tn) && (b `elem` lbls)
              = satSubs t (f:lbls) [] fs
          | otherwise
              = satSubs t lbls [] fs
      satSubs t@(t',_,_) lbls prev (f@(Until (t0,tn) a b):fs)
          | (t'<t0) && (f `elem` prev)
              = satSubs t (f:lbls) prev fs
          | (t'<=tn) && (b `elem` lbls)
              = satSubs t (f:lbls) prev fs
          | (t'<=tn) && (a `elem` lbls) && (f `elem` prev)
              = satSubs t (f:lbls) prev fs
          | otherwise
              = satSubs t lbls prev fs
      satSubs t lbls prev (f@(Gtee q a):fs)
          | (modelCheckDP env solver Nothing (compose (constructP p [t]) 
                                              (maybe err id (lookupProcName env q))) 
             tps a)
              = satSubs t (f:lbls) prev fs
          | otherwise
              = satSubs t lbls prev fs
          where
            err = X.throw $ 
                  CpiException $ q ++ " not a defined process."
      satSubs _ _ _ _
          = X.throw $ 
            CpiException $ "DP model checker only checks "
                             ++ "Until (not F, G, or R)"
-- The hybrid model checking function
modelCheckHy :: Env                   -- Environment
             -> ODE.Solver                -- ODE solver function
             -> (Maybe Trace)         -- Pre-computed time series (or Nothing)
             -> Process               -- Process to execute
             -> (Int,(Double,Double)) -- Time points: (points,(t0,tn))
             -> Formula               -- Formula to check
             -> Bool
modelCheckHy env solver trace p tps f
    | trace == Nothing
        = maybe err id $ Map.lookup f' $ 
          hyEval (reverse (fPostOrd f')) (solve env solver tps p)
    | otherwise
        = maybe err id $ Map.lookup f' $ 
          hyEval (reverse (fPostOrd f')) ((\(Just x)->x) trace)
    where
      f' = rewriteU f
      err = X.throw $ CpiException "CPi.Logic.modelCheckHy: Formula not in map."
      hyEval :: [Formula] -> Trace -> Map Formula Bool
      hyEval _ [] = Map.empty
      hyEval fs (t:ts) = eval fs t (hyEval fs ts)
          where
            eval :: [Formula] -> State -> Map Formula Bool -> Map Formula Bool
            eval [] _ _ = Map.empty
            eval (f:fs) t@(t',_,_) next = Map.insert f (eval' f) subs
                where
                  subs = eval fs t next
                  eval' T = True
                  eval' F = False
                  eval' (ValGT x y) = (getVal [t] x) > (getVal [t] y)
                  eval' (ValLT x y) = (getVal [t] x) < (getVal [t] y)
                  eval' (ValGE x y) = (getVal [t] x) >= (getVal [t] y)
                  eval' (ValLE x y) = (getVal [t] x) <= (getVal [t] y)
                  eval' (ValEq x y) = (getVal [t] x) == (getVal [t] y)
                  eval' (ValNEq x y) = (getVal [t] x) /= (getVal [t] y)
                  eval' (Conj x y) = maybe False id (Map.lookup x subs)
                                     && maybe False id (Map.lookup y subs)
                  eval' (Disj x y) = maybe False id (Map.lookup x subs)
                                     || maybe False id (Map.lookup y subs)
                  eval' (Impl x y) = not (maybe False id (Map.lookup x subs))
                                     || maybe False id (Map.lookup y subs)
                  eval' (Neg x) = not (maybe False id (Map.lookup x subs))
                  eval' u@(Until (t0,tn) x y)
                      | t' < t0
                          = maybe False id (Map.lookup u next)
                      | t' <= tn
                          = maybe False id (Map.lookup y subs)
                            || (maybe False id (Map.lookup x subs)
                                && maybe False id (Map.lookup u next))
                      | otherwise
                          = False
                  eval' (Gtee q x)
                      = modelCheckHy env solver Nothing 
                        (compose 
                         (maybe err id (lookupProcName env q)) 
                         (constructP p [t])) tps x
                            where
                              err = X.throw $ 
                                    CpiException $ q ++" not a defined process."
                  eval' _ = X.throw $ 
                            CpiException $ "Hybrid model checker only checks "
                                             ++"Until (not F, G, or R)"

-- The lazy circular hybrid model checking function
modelCheckHy2 :: Env                   -- Environment
              -> ODE.Solver                -- ODE solver function
              -> (Maybe Trace)         -- Pre-computed time series (or Nothing)
              -> Process               -- Process to execute
              -> (Int,(Double,Double)) -- Time points: (points,(t0,tn))
              -> Formula               -- Formula to check
              -> Bool
modelCheckHy2 env solver trace p tps f
    | trace == Nothing
        = maybe err id $ Map.lookup f' $ 
          hyEval (fPostOrd f') (solve env solver tps p)
    | otherwise
        = maybe err id $ Map.lookup f' $ 
          hyEval (fPostOrd f') ((\(Just x)->x) trace)
    where
      f' = rewriteU f
      err = X.throw $ CpiException "CPi.Logic.modelCheckHy: Formula not in map."
      hyEval :: [Formula] -> Trace -> Map Formula Bool
      hyEval _ [] = Map.empty
      hyEval fs (t:ts) = eval fs t (hyEval fs ts)
          where
            eval :: [Formula] -> State -> Map Formula Bool -> Map Formula Bool
            eval fs t@(t',_,_) next = now
                where
                  now = eval'' fs
                  eval'' (f:fs) = Map.insert f (eval' f) (eval'' fs)
                  eval'' [] = Map.empty
                  eval' T = True
                  eval' F = False
                  eval' (ValGT x y) = (getVal [t] x) > (getVal [t] y)
                  eval' (ValLT x y) = (getVal [t] x) < (getVal [t] y)
                  eval' (ValGE x y) = (getVal [t] x) >= (getVal [t] y)
                  eval' (ValLE x y) = (getVal [t] x) <= (getVal [t] y)
                  eval' (ValEq x y) = (getVal [t] x) == (getVal [t] y)
                  eval' (ValNEq x y) = (getVal [t] x) /= (getVal [t] y)
                  eval' (Conj x y) = maybe False id (Map.lookup x now)
                                     && maybe False id (Map.lookup y now)
                  eval' (Disj x y) = maybe False id (Map.lookup x now)
                                     || maybe False id (Map.lookup y now)
                  eval' (Impl x y) = not (maybe False id (Map.lookup x now))
                                     || maybe False id (Map.lookup y now)
                  eval' (Neg x) = not (maybe False id (Map.lookup x now))
                  eval' u@(Until (t0,tn) x y)
                      | t' < t0
                          = maybe False id (Map.lookup u next)
                      | t' <= tn
                          = maybe False id (Map.lookup y now)
                            || (maybe False id (Map.lookup x now)
                                && maybe False id (Map.lookup u next))
                      | otherwise
                          = False
                  eval' (Gtee q x)
                      = modelCheckHy env solver Nothing 
                        (compose 
                         (maybe err id (lookupProcName env q)) 
                         (constructP p [t])) tps x
                            where
                              err = X.throw $ 
                                    CpiException $ q ++ " not a defined process."
                  eval' _ 
                      = X.throw $ 
                        CpiException $ "Hybrid model checker only checks "
                                         ++"Until (not F, G, or R)"

-- | The formula rewriting algorithm:
modelCheckFR :: Env                   -- ^ Environment
             -> ODE.Solver            -- ^ ODE solver function
             -> (Maybe Trace)         -- ^ Pre-computed time series (or Nothing)
             -> Process               -- ^ Process to execute
             -> (Int,(Double,Double)) -- ^ Time points: (points,(t0,tn))
             -> Formula               -- ^ Formula to check
             -> Bool
modelCheckFR env solver trace p tps f
    | trace == Nothing
        = check (nnf f) (solve env solver tps p)
    | otherwise
        = check (nnf f) ((\(Just x)->x) trace)
          where
            -- recurse over the trace
            -- check if we've rewritten to T or F.
            check :: Formula -> Trace -> Bool
            check f [] = False
            check f (t@(t',_,_):ts)
                = DBG.trace
                  ("CHECK ("++(pretty f)++") @ "++(show(t')))
                  $ case eta(beta(gamma t (step(t:ts)) f)) of
                      T -> True
                      F -> False
                      otherwise -> check (eta(beta(gamma t (step(t:ts)) f))) ts
            step ((t,_,_):(t',_,_):ts) = t'-t
            step _ = infty
            -- Reduce connectives:
            beta :: Formula -> Formula
            beta (Conj a b)
                | beta a == F || beta b == F
                    = F
                | beta b == T
                    = beta a
                | beta a == T
                    = beta b
                | otherwise
                    = Conj (beta a) (beta b)
            beta (Disj a b)
                | beta a == T || beta b == T
                    = T
                | beta b == F
                    = beta a
                | beta a == F
                    = beta b
                | otherwise
                    = Disj (beta a) (beta b)
            beta x = x
            -- Reduce redundant temporal formulae:
            eta z@(Disj (Until (0.0,t1) a b) (Disj (Until (0.0,t2) c d) e))
                | a==c&&b==d
                    = (Disj (Until (0,(max t1 t2)) a b) (eta e))
                | otherwise = z
            eta z@(Conj (Until (0.0,t1) a b) (Conj (Until (0.0,t2) c d) e))
                | a==c&&b==d
                    = (Conj (Until (0,(min t1 t2)) a b) (eta e))
                | otherwise = z
            eta z@(Disj (Rels (0.0,t1) a b) (Disj (Rels (0.0,t2) c d) e))
                | a==c&&b==d
                    = (Disj (Rels (0,(max t1 t2)) a b) (eta e))
                | otherwise = z
            eta z@(Conj (Rels (0.0,t1) a b) (Conj (Rels (0.0,t2) c d) e))
                | a==c&&b==d
                    = (Conj (Rels (0,(min t1 t2)) a b) (eta e))
                | otherwise = z
            eta (Conj a b) = Conj (eta a) (eta b)
            eta (Disj a b) = Disj (eta a) (eta b)
            eta x = x
            -- Rewrite:
            gamma :: State -> Double -> Formula -> Formula
            gamma t v F = F
            gamma t v T = T
            gamma t v (ValGT v1 v2) | (getVal [t] v1) > (getVal [t] v2)
                                        = T | otherwise = F
            gamma t v (ValGE v1 v2) | (getVal [t] v1) >= (getVal [t] v2)
                                        = T | otherwise = F
            gamma t v (ValLT v1 v2) | (getVal [t] v1) < (getVal [t] v2)
                                        = T | otherwise = F
            gamma t v (ValLE v1 v2) | (getVal [t] v1) <= (getVal [t] v2)
                                        = T | otherwise = F
            gamma t v (ValEq v1 v2) | (getVal [t] v1) == (getVal [t] v2)
                                        = T | otherwise = F
            gamma t v (ValNEq v1 v2) | (getVal [t] v1) /= (getVal [t] v2)
                                         = T | otherwise = F
            gamma t v (Neg (ValGT v1 v2)) | (getVal [t] v1) > (getVal [t] v2)
                                              = F | otherwise = T
            gamma t v (Neg (ValGE v1 v2)) | (getVal [t] v1) >= (getVal [t] v2)
                                              = F | otherwise = T
            gamma t v (Neg (ValLT v1 v2)) | (getVal [t] v1) < (getVal [t] v2)
                                              = F | otherwise = T
            gamma t v (Neg (ValLE v1 v2)) | (getVal [t] v1) <= (getVal [t] v2)
                                              = F | otherwise = T
            gamma t v (Neg (ValEq v1 v2)) | (getVal [t] v1) == (getVal [t] v2)
                                              = F | otherwise = T
            gamma t v (Neg (ValNEq v1 v2)) | (getVal [t] v1) /= (getVal [t] v2)
                                               = F | otherwise = T
            gamma t v (Conj a b) = Conj (gamma t v a) (gamma t v b)
            gamma t v (Disj a b) = Disj (gamma t v a) (gamma t v b)
            gamma t v u@(Until (t0,tn) a b)
                | t0 > 0 && v <= tn
                    = Conj 
                       (gamma t v a) 
                       (Until ((max (t0-v) 0),tn-v) a b)
                | t0 == 0 && v <= tn
                    = Disj 
                      (gamma t v b) 
                      (Conj 
                        (gamma t v a) 
                        (Until (0,tn-v) a b))
                | t0 == 0 && v > tn
                    = gamma t v b
                | t0 > 0 && v > tn
                    = F
            gamma t v r@(Rels (t0,tn) a b)
                | t0 > 0 && v <= tn 
                    = Conj 
                      (gamma t v b) 
                      (Rels ((max (t0-v) 0),tn-v) a b)
                | t0 == 0 && v <= tn
                    = Conj 
                      (gamma t v b) 
                      (Disj 
                       (gamma t v a) 
                       (Rels (0,tn-v) a b))
                | v > tn
                    = gamma t v b
            gamma t v (Gtee q f)
                | checkContext = T
                | otherwise = F
                where
                  checkContext = modelCheckFR env solver Nothing 
                                 (compose 
                                  (maybe err id (lookupProcName env q)) 
                                  (constructP p [t])) tps f
                                     where
                                       err = X.throw $ 
                                             CpiException $ 
                                             q++" not a defined process."
            gamma _ _ _ = X.throw $ CpiException $
                          "Rewriting model cheker not defined for formula: "
                          ++(pretty f)++"\n"
                          ++"Must be in NNF."


-- | Get a value from the trace.
getVal :: Trace -> Val -> Double
getVal _ (R d) = d
getVal ((_,cs,_):ts) (Conc s) = maybe 0.0 id (Map.lookup s (cs))
getVal ((_,_,ds):ts) (Deriv s) = maybe 0.0 id (Map.lookup s (ds))
getVal ts (Plus x y) = getVal ts x + getVal ts y
getVal ts (Minus x y) = getVal ts x - getVal ts y
getVal ts (Times x y) = getVal ts x * getVal ts y
getVal ts (Quot x y) = getVal ts x / getVal ts y
getVal [] _ = 0.0

-- | Trace iterator: Return the trace from the next time point.
traceNext :: Trace -> Trace
traceNext [] = []
traceNext (t:tr) = tr

-- | Time interval the trace covers
traceInterval :: Trace -> (Double,Double)
traceInterval [] = undefined
traceInterval [(t,_,_)] = (t,t)
traceInterval ((t,_,_):tr) = (t, traceStart ((last tr):[]))

-- | Initial time of a trace
traceStart :: Trace -> Double
traceStart [] = undefined
traceStart ((t,_,_):_) = t

-- | Number of time-points in the trace
traceLength :: Trace -> Int
traceLength [] = 0
traceLength (_:tr) = 1 + traceLength tr

-- Take a process and get a trace from the solver:
solve :: Env -> ODE.Solver -> (Int,(Double,Double)) -> Process -> Trace
solve env solver (r,(t0,tn)) p 
    | (t0,tn) == (0,0)
        = [(0, 
             Map.fromList(zip ss (ODE.initials env p' dpdt)),
             Map.fromList(zip ss (repeat 0)))]
    | otherwise 
        = ODE.timeSeries ts soln deriv ss
    where
      ts = ODE.timePoints (r,(t0,tn))
      mts = processMTS env p
      p' = wholeProc env p mts
      dpdt = ODE.dPdt env mts p'
      soln = solver env p mts dpdt (r,(t0,tn))
      deriv = ODE.derivs env dpdt (r,(t0,tn)) soln
      ss = ODE.speciesIn env dpdt 

-- Construct a process from the initial time-point of a trace:
constructP :: Process -> Trace -> Process
constructP (Process scs net) ((_,map,_):_) = Process (Map.toList map) net 
constructP _ [] = Process [] (AffNet [])

-- the post-order flattening of a formula
fPostOrd :: Formula -> [Formula]
fPostOrd f@(Conj a b) = (fPostOrd a)++(fPostOrd b)++[f]
fPostOrd f@(Disj a b) = (fPostOrd a)++(fPostOrd b)++[f]
fPostOrd f@(Impl a b) = (fPostOrd a)++(fPostOrd b)++[f]
fPostOrd f@(Neg a) = (fPostOrd a)++[f]
fPostOrd f@(Until _ a b) = (fPostOrd a)++(fPostOrd b)++[f]
fPostOrd f@(Rels _ a b) = (fPostOrd a)++(fPostOrd b)++[f]
fPostOrd f@(Nec _ a) = (fPostOrd a)++[f]
fPostOrd f@(Pos _ a) = (fPostOrd a)++[f]
fPostOrd x = [x]

-- rewrite a temporal formula in terms of U (i.e. rewrite F, G, R)
rewriteU :: Formula -> Formula
rewriteU (Nec i f) = Neg (Until i T (Neg (rewriteU f)))
rewriteU (Pos i f) = Until i T (rewriteU f)
rewriteU (Neg f) = Neg (rewriteU f)
rewriteU (Gtee p f) = Gtee p (rewriteU f)
rewriteU (Until i f1 f2) = Until i (rewriteU f1) (rewriteU f2)
rewriteU (Rels i f1 f2) = Neg (Until i (Neg (rewriteU f1)) (Neg (rewriteU f2)))
rewriteU (Impl f1 f2) = Impl (rewriteU f1) (rewriteU f2)
rewriteU (Disj f1 f2) = Disj (rewriteU f1) (rewriteU f2)
rewriteU (Conj f1 f2) = Conj (rewriteU f1) (rewriteU f2)
rewriteU x = x

-- | rewrite a formula in Negation Normal Form
nnf :: Formula -> Formula
nnf (Conj a b) = Conj (nnf a) (nnf b)
nnf (Disj a b) = Disj (nnf a) (nnf b)
nnf (Neg (Conj a b)) = Disj (nnf (Neg a)) (nnf (Neg b))
nnf (Neg (Disj a b)) = Conj (nnf (Neg a)) (nnf (Neg b))
nnf (Pos i a) = Until i T (nnf a)
nnf (Nec i a) = Rels i F (nnf a)
nnf (Until i a b) = Until i (nnf a) (nnf b)
nnf (Rels i a b) = Rels i (nnf a) (nnf b)
nnf (Neg (Pos i a)) = Rels i F (nnf (Neg a))
nnf (Neg (Nec i a)) = Until i T (nnf (Neg a))
nnf (Neg (Until i a b)) = Rels i (nnf (Neg a)) (nnf (Neg b))
nnf (Neg (Rels i a b)) = Until i (nnf (Neg a)) (nnf (Neg b))
nnf (Gtee p a) = Gtee p (nnf a)
nnf (Neg (Gtee p a)) = Gtee p (nnf (Neg a))
nnf (Neg (Neg x)) = nnf x
nnf x = x

-- | Atomic propositions of a formula
-- | NOTE: treats context modality as non-atomic and any sub-formula
-- |      of a context modality is ignored (as it is in a different context).
aps :: Formula -> [Formula]
aps f = L.nub $ aps' f
    where
      aps' (Conj a b) = aps' a ++ aps' b
      aps' (Disj a b) = aps' a ++ aps' b
      aps' (Impl a b) = aps' a ++ aps' b
      aps' (Neg a) = aps' a
      aps' (Until _ a b) = aps' a ++ aps' b
      aps' (Rels _ a b) = aps' a ++ aps' b
      aps' (Nec _ a) = aps' a
      aps' (Pos _ a) = aps' a
      aps' (Gtee _ a) = []
      aps' x = [x]

-- | get simulation time required to verify the formula:
simTime :: Formula -> Double
simTime (Neg a) = simTime a
simTime (Conj a b) = max (simTime a) (simTime b)
simTime (Disj a b) = max (simTime a) (simTime b)
simTime (Impl a b) = max (simTime a) (simTime b)
simTime (Nec (x,y) a) = y + (simTime a)
simTime (Pos (x,y) a) = y + (simTime a)
simTime (Until (x,y) a b) = y + max (simTime a) (simTime b)
simTime (Rels (x,y) a b) = y + max (simTime a) (simTime b)
simTime _ = 0

-- | Take parsed (possibly incomplete) Speciess in forumla
-- | and reconcile with Species from the Environment.
reconcileSpecs :: Env -> Formula -> Formula
reconcileSpecs env T = T
reconcileSpecs env F = F
reconcileSpecs env (ValGT v1 v2) 
    = ValGT (reconcileVal env v1) (reconcileVal env v2)
reconcileSpecs env (ValGE v1 v2) 
    = ValGE (reconcileVal env v1) (reconcileVal env v2)
reconcileSpecs env (ValLT v1 v2) 
    = ValLT (reconcileVal env v1) (reconcileVal env v2)
reconcileSpecs env (ValLE v1 v2) 
    = ValLE (reconcileVal env v1) (reconcileVal env v2)
reconcileSpecs env (ValEq v1 v2) 
    = ValEq (reconcileVal env v1) (reconcileVal env v2)
reconcileSpecs env (ValNEq v1 v2) 
    = ValNEq (reconcileVal env v1) (reconcileVal env v2)
reconcileSpecs env (Conj f1 f2)
    = Conj (reconcileSpecs env f1) (reconcileSpecs env f2)
reconcileSpecs env (Disj f1 f2)
    = Disj (reconcileSpecs env f1) (reconcileSpecs env f2)
reconcileSpecs env (Impl f1 f2)
    = Impl (reconcileSpecs env f1) (reconcileSpecs env f2)
reconcileSpecs env (Neg f) 
    = Neg (reconcileSpecs env f)
reconcileSpecs env (Until i f1 f2) 
    = Until i (reconcileSpecs env f1) (reconcileSpecs env f2)
reconcileSpecs env (Rels i f1 f2) 
    = Rels i (reconcileSpecs env f1) (reconcileSpecs env f2)
reconcileSpecs env (Nec i f) 
    = Nec i (reconcileSpecs env f)
reconcileSpecs env (Pos i f) 
    = Pos i (reconcileSpecs env f)
reconcileSpecs env (Gtee p f)
    = Gtee p (reconcileSpecs env f)

reconcileVal :: Env -> Val -> Val
reconcileVal env (R d) = R d
reconcileVal env (Conc s) = Conc (reconcileSpec env s)
reconcileVal env (Deriv s) = Deriv (reconcileSpec env s)
reconcileVal env (Plus v1 v2) = Plus (reconcileVal env v1) (reconcileVal env v2)
reconcileVal env (Minus v1 v2) = Minus (reconcileVal env v1) (reconcileVal env v2)
reconcileVal env (Times v1 v2) = Times (reconcileVal env v1) (reconcileVal env v2)
reconcileVal env (Quot v1 v2) = Quot (reconcileVal env v1) (reconcileVal env v2)

reconcileSpec env Nil = Nil
reconcileSpec env (Sum pxs) = Sum (reconcilePxs env pxs)
reconcileSpec env (Par ss) = Par (map (reconcileSpec env) ss)
reconcileSpec env (New n s) = New n (reconcileSpec env s)
reconcileSpec env (Def s ns) = maybe (Def s ns) id (lookupSpecName env s)

reconcilePxs env [] = []
reconcilePxs env ((p,s):pxs) = (p,(reconcileSpec env s)):(reconcilePxs env pxs)

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
    pretty z@(Conj x y) = parens x z ++ " & " ++ parens y z
    pretty z@(Disj x y) = parens x z ++ " | " ++ parens y z
    pretty z@(Impl x y) = parens x z ++ " ==> " ++ parens y z
    pretty z@(Until (t0,tn) x y)
        | (t0 == 0) && (tn == infty)
            = parens x z ++ " U " ++ parens y z
        | (t0 == 0)
            = parens x z ++ " U{" ++ show tn ++ "} " ++ parens y z
        | otherwise 
            = parens x z ++ " U{" ++ show t0 ++ "," ++ show tn ++ "} " ++ parens y z
    pretty z@(Rels (t0,tn) x y)
        | (t0 == 0) && (tn == infty)
            = parens x z ++ " R " ++ parens y z
        | (t0 == 0)
            = parens x z ++ " R{" ++ show tn ++ "} " ++ parens y z
        | otherwise 
            = parens x z ++ " R{" ++ show t0 ++ "," ++ show tn ++ "} " ++ parens y z
    pretty z@(Gtee p y) = p ++ " |> " ++ parens y z
    pretty z@(Neg x) = "!" ++ parens x z
    pretty z@(Nec (t0,tn) x) 
        | (t0 == 0) && (tn == infty)
            = "G" ++ parens x z
        | (t0 == 0)
            = "G{" ++ show tn ++ "}" ++ parens x z
        | otherwise
            = "G{" ++ show t0 ++ "," ++ show tn ++ "}" ++ parens x z
    pretty z@(Pos (t0,tn) x) 
        | (t0 == 0) && (tn == infty)
            = "F" ++ parens x z
        | (t0 == 0)
            = "F{" ++ show tn ++ "}" ++ parens x z
        | otherwise
            = "F{" ++ show t0 ++ "," ++ show tn ++ "}" ++ parens x z
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
      prio (Rels _ _ _) = 40
      prio (Conj _ _) = 50
      prio (Disj _ _) = 52
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