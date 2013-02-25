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

module CPi.Logic 
    (Formula(..),
     Val(..),
     modelCheck
    )where

import CPi.Lib 
import qualified CPi.ODE as ODE
import CPi.Semantics

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Exception as X
import qualified Control.Monad.Trans.State.Strict as S
--import qualified Debug.Trace as DBG

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
type State = (Double, Map Species Double)
type Trace = [State]

-- The default model checker
modelCheck :: Env                   -- Environment
           -> ODE.Solver            -- ODE solver function
           -> (Maybe Trace)         -- Pre-computed time series (or Nothing)
           -> Process               -- Process to execute
           -> (Int,(Double,Double)) -- Time points: (points,(t0,tn))
           -> Formula               -- Formula to check
           -> Bool
modelCheck = modelCheckHy -- use the hybrid checker.

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
      modelCheck' ts (Gtee p' x) 
          = modelCheck' (solve env solver tps (compose (constructP p ts) 
                                               (maybe err id (lookupProcName env p'))
                                              )) x
            where
              err = X.throw $ CpiException $ p' ++ " not a defined process."


-- DP model checker using explicit state
modelCheckDP :: Env                   -- Environment
             -> ODE.Solver                -- ODE solver function
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
      satSubs t lbls [] (f@(Until (t0,tn) a b):fs)
          | ((fst t)<=tn) && (b `elem` lbls)
              = satSubs t (f:lbls) [] fs
          | otherwise
              = satSubs t lbls [] fs
      satSubs t lbls prev (f@(Until (t0,tn) a b):fs)
          | ((fst t)<t0) && (f `elem` prev)
              = satSubs t (f:lbls) prev fs
          | ((fst t)<=tn) && (b `elem` lbls)
              = satSubs t (f:lbls) prev fs
          | ((fst t)<=tn) && (a `elem` lbls) && (f `elem` prev)
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
            err = X.throw $ CpiException $ q ++ " not a defined process."
      satSubs _ _ _ ((Nec _ _):_)
          = X.throw $ CpiException "DP model checker only checks Until (not F or G)"
      satSubs _ _ _ ((Pos _ _):_)
          = X.throw $ CpiException "DP model checker only checks Until (not F or G)"

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
            eval (f:fs) t next = Map.insert f (eval' f) subs
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
                      | (fst t) < t0
                          = maybe False id (Map.lookup u next)
                      | (fst t) <= tn
                          = maybe False id (Map.lookup y subs)
                            || (maybe False id (Map.lookup x subs)
                                && maybe False id (Map.lookup u next))
                      | otherwise
                          = False
                  eval' (Gtee q x)
                      = modelCheckHy env solver Nothing 
                        (compose (maybe err id (lookupProcName env q)) (constructP p [t])) tps x
                            where
                              err = X.throw $ CpiException $ q ++ " not a defined process."
                  eval' (Pos tn x) 
                      = X.throw $ 
                        CpiException "Hybrid model checker only checks Until (not F or G)"
                  eval' (Nec tn x)
                      = X.throw $ 
                        CpiException "Hybrid model checker only checks Until (not F or G)"

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
            eval fs t next = now
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
                      | (fst t) < t0
                          = maybe False id (Map.lookup u next)
                      | (fst t) <= tn
                          = maybe False id (Map.lookup y now)
                            || (maybe False id (Map.lookup x now)
                                && maybe False id (Map.lookup u next))
                      | otherwise
                          = False
                  eval' (Gtee q x)
                      = modelCheckHy env solver Nothing 
                        (compose (maybe err id (lookupProcName env q)) (constructP p [t])) tps x
                            where
                              err = X.throw $ CpiException $ q ++ " not a defined process."
                  eval' (Pos tn x) 
                      = X.throw $ 
                        CpiException "Hybrid model checker only checks Until (not F or G)"
                  eval' (Nec tn x)
                      = X.throw $ 
                        CpiException "Hybrid model checker only checks Until (not F or G)"
                  

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
solve :: Env -> ODE.Solver -> (Int,(Double,Double)) -> Process -> Trace
solve env solver (r,(t0,tn)) p = ODE.timeSeries ts soln ss
    where
      ts = ODE.timePoints (r,(t0,tn))
      mts = processMTS env p
      p' = wholeProc env p mts
      dpdt = ODE.dPdt env mts p'
      soln = solver env p mts dpdt (r,(t0,tn))
      ss = ODE.speciesIn env dpdt

-- Construct a process from the initial time-point of a trace:
constructP :: Process -> Trace -> Process
constructP (Process scs net) ((_,map):_) = Process (cons' scs map) net 
    where
      cons' [] _ = []
      cons' ((s,_):ss) map = (s,(maybe 0.0 id (Map.lookup s map))) : (cons' ss map)
constructP _ [] = Process [] (AffNet [])

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