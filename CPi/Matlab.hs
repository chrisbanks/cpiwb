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

module CPi.Matlab
    (solveODEoctave,
     matlabScript
    ) where

import CPi.Lib
import CPi.Semantics
import CPi.ODE

import qualified Data.List as L
import qualified Control.Exception as X
import qualified Data.Map as Map
import Data.Map (Map)
--import Data.Maybe

--import qualified Numeric.LinearAlgebra as LA

import System.IO.Unsafe
import qualified System.Process as OS


-------------------------------
-- MATLAB script output:
-------------------------------

matlabODE :: Env -> Process -> P' -> (Int,(Double,Double)) -> String
matlabODE env p dpdt (n,(t0,tn))
    = "init = [" ++ (L.concat (L.intersperse ";" (map show (initials env p p')))) ++ "];\n" ++
      "t = linspace(" ++ show t0 ++ "," ++ show tn ++ "," ++ show n ++ ");\n" ++
      "function xdot = f(x,t) \n\n" ++
      (L.concat (L.intersperse ";\n" 
                 ((map (\(k,v) -> 
                        (maybe "XXX" id (Map.lookup k (svxd)) ++ " = " ++
                         matlabExpr env (svx) v))) 
                  (Map.toList p')))) ++ ";\nendfunction;\n\n" ++ 
      "function jac = jj(x,t)\n\n" ++
      matlabJac env p' ++ ";\nendfunction;\n\n" ++
      "x = lsode({@f,@jj}, init, t);\n" ++
      "save (\"-ascii\", \"-\", \"x\");"
          where
            xdots = ["xdot("++ show i ++")" | i<-[1..]]
            svxd = speciesVars env xdots p'
            xs = ["x("++ show i ++")" | i<-[1..]]
            svx = speciesVars env xs p'
            p' = simpP' env dpdt

matlabExpr :: Env -> Map Species String -> Expr -> String
matlabExpr env vs (Var x) 
    = maybe (ex x) id (Map.lookup x vs)
      where 
        ex x = X.throw $ CpiException 
               ("Bug: failed var lookup in CPi.ODE.matlabExpr: " ++ show x)
matlabExpr env vs (Plus x y) = matlabExpr env vs x ++ " .+ " ++ matlabExpr env vs y
matlabExpr env vs (Times (Plus x y) (Plus x' y')) 
    = "(" ++ matlabExpr env vs (Plus x y) ++ ").*(" ++ matlabExpr env vs (Plus x' y') ++ ")"
matlabExpr env vs (Times (Plus x y) z) 
    = "(" ++ matlabExpr env vs (Plus x y) ++ ").*" ++ matlabExpr env vs z 
matlabExpr env vs (Times x (Plus y z)) 
    = matlabExpr env vs x ++ ".*(" ++ matlabExpr env vs (Plus y z) ++ ")"
matlabExpr env vs (Times x y) = matlabExpr env vs x ++ ".*" ++ matlabExpr env vs y
matlabExpr env vs (Num k) = show k

matlabJac :: Env -> P' -> String
matlabJac env p' = L.concat $ L.intersperse ";\n" $
                   map (\(a,b) -> a ++ " = " ++ b) $
                   zip rhss $
                   map (matlabExpr env (speciesVars env xs p')) $ 
                   map (simp env) $
                   map diff' $ cp (unzip (Map.toList (sp')))
    where
      cp (x,y) = [(a,b)|b<-y,a<-x]
      diff' (x,e) = diff x e
      xs = ["x("++ show i ++")" | i<-[1..]]
      sp' = simpP' env p'
      len = Map.size p'
      rhss = ["jac("++ show x ++ "," ++ show y ++ ")" | x<-[1..len], y<-[1..len]]

---------------------------------------
-- Using Octave to execute the scripts
---------------------------------------

-- | Solver which calculates the symbolic Jacobian, writes MATLAB code, and executes it with GNU Octave. (General purpose, deals with stiff systems, uses LSODE.)
solveODEoctave :: Solver
solveODEoctave env p mts p' ts@(n,(t0,tn)) 
    = let raw = unsafePerformIO 
                $ OS.readProcess 
                      "octave" ["-q"] (matlabODE env (wholeProc env p mts) p' ts)
      in (n><(Map.size p')) $ map s2d $ words raw


-- Return the MATLAB script for ODEs
matlabScript :: Env 
             -> Process 
             -> MTS 
             -> P' 
             -> (Int, (Double, Double)) 
             -> String
matlabScript env p mts p' ts = matlabODE env (wholeProc env p mts) p' ts
