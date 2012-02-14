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
import CpiODE (timeSeries,timePoints,solveODE,xdot,dPdt',speciesIn,initials)
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
             | Conj Formula Formula   -- a AND b
             | Disj Formula Formula   -- a OR b
             | Neg Formula            -- NOT a
             | Until Formula Formula  -- a U b
             | Nec Formula           -- G a
             | Pos Formula           -- F a
             | Gtee Process Formula   -- Pi |> a
               deriving Show

data Val = R Double   -- Real
         | Conc Species  -- Concentration of Species
         | Deriv Species -- Derivative of concentration
         | Plus Val Val  -- x+y
         | Minus Val Val -- x-y
         | Times Val Val -- x*y
         | Quot Val Val  -- x/y
           deriving Show


-------------------------
-- Model Checking:
-------------------------

-- This is an implementation of the trace-based model checker for LTL(R)+Guarantee

-- A trace is a time series from the ODE solver:
type Trace = [(Double, Map Species Double)]
type LinSpace = ((Double,Double),Int)

-- The model checking function
modelCheck :: Env -> Process -> LinSpace -> Formula -> Bool
modelCheck env p tps f = modelCheck' (solve env tps p) f
    where
      modelCheck' [] _ = False
      modelCheck' _ T = True
      modelCheck' _ F = False
      modelCheck' ts (ValGT x y) = (getVal ts x) > (getVal ts y)
      modelCheck' ts (ValLT x y) = (getVal ts x) < (getVal ts y)
      modelCheck' ts (ValGE x y) = (getVal ts x) >= (getVal ts y)
      modelCheck' ts (ValLE x y) = (getVal ts x) <= (getVal ts y)
      modelCheck' ts (Conj x y) = modelCheck' ts x && modelCheck' ts y
      modelCheck' ts (Disj x y) = modelCheck' ts x || modelCheck' ts y
      modelCheck' ts (Neg x) = not $ modelCheck' ts x
      modelCheck' (t:ts) (Until x y) = modelCheck' (t:ts) y ||
                                       (modelCheck' (t:ts) x && modelCheck' ts (Until x y))
      modelCheck' ts (Pos x) = modelCheck' ts (Until T x)
      modelCheck' ts (Nec x) = modelCheck' ts (Neg (Pos (Neg x)))
      modelCheck' ts (Gtee p' x) = modelCheck' (solve env tps (compose p' p)) x


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
solve :: Env -> LinSpace -> Process -> Trace
solve env ((b,e),r) p = timeSeries ts soln ss
    where
      ts = timePoints r (b,e)
      soln = solveODE odes inits ts
      ss = speciesIn env dpdt
      odes = xdot env dpdt
      inits = initials env p' dpdt
      dpdt = dPdt' env p'
      p' = wholeProc env p mts
      mts = processMTS env p



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
    pretty z@(Conj x y) = parens x z ++ " && " ++ parens y z
    pretty z@(Disj x y) = parens x z ++ " || " ++ parens y z
    pretty z@(Until x y) = parens x z ++ " U " ++ parens y z
    pretty z@(Gtee pi y) = pretty pi ++ " |> " ++ parens y z
    pretty z@(Neg x) = "¬" ++ parens x z
    pretty z@(Nec x) = "G" ++ parens x z
    pretty z@(Pos x) = "F" ++ parens x z

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
      prio (Nec _) = 10
      prio (Pos _) = 10
      prio (Until _ _) = 40
      prio (Conj _ _) = 50
      prio (Disj _ _) = 50
      prio (Gtee _ _) = 60

instance Pretty Val where
    pretty (R d) = show d
    pretty (Conc s) = "[" ++ pretty s ++ "]"
    pretty (Deriv s) = "[" ++ pretty s ++ "]'"
    pretty (Plus x y) = pretty x ++ "+" ++ pretty y
    pretty (Minus x y) = pretty x ++ "-" ++ pretty y
    pretty (Times x y) = pretty x ++ "*" ++ pretty y
    pretty (Quot x y) = pretty x ++ "/" ++ pretty y