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

module CpiODE where

import qualified Data.List as L
import qualified Control.Exception as X
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

import qualified Numeric.GSL as GSL
import qualified Numeric.LinearAlgebra as LA
import qualified Graphics.Plot as Plot

import CpiLib
import CpiSemantics

--------------------------------------------
-- Output the ODEs describing a CPi system
--------------------------------------------

-- Get the ODEs in hmatrix form.
xdot :: Env -> P' -> (Double -> [Double] -> [Double])
xdot env p = xdot'
    where
      -- the xdot function we need to return
      xdot' t xs = toODE p' vmap xs
      -- simplify the P'
      p' = Map.toList (simpP' env p)
      -- map species to indexed vars:
      defs2vars :: [(Species,Expr)] -> [Int] -> [(Species,Int)]
      defs2vars ((k,v):ks) (x:xs) = (k,x) : (defs2vars ks xs)
      defs2vars [] _ = []
      defs2vars _ [] = X.throw $ CpiException "CpiODE.xdot: run out of vars!"
      xvars :: [Int]
      xvars = [0..]
      vmap :: [(Species,Int)]
      vmap = defs2vars p' xvars
      -- get ODE expressions from P' expressions:
      toODE :: [(Species,Expr)] -> [(Species,Int)] -> [Double] -> [Double]
      toODE ps vmap xs = map (convert . snd) ps
          where
            convert (Var s') = xs!!(maybe (err s') id (lookup s' vmap))
            convert (Plus e e') = (convert e) + (convert e')
            convert (Times e e') = (convert e) * (convert e')
            convert (Num d) = d
            err s' = X.throw $ CpiException 
                     ("Bug: bad lookup ("++(pretty s')++") in CpiODE.xdot.toODE")

{- FIXME: xdot and jac share a lot of inlined code. Refactor!-}

-- Get the Jacobian in hmatrix form
jac :: Env -> P' -> (Double -> LA.Vector Double -> LA.Matrix Double)
jac env p = jac'
    where
      jac' t xs = toJac p' vmap (LA.toList xs)
      -- simplify the P'
      p' = Map.toList (simpP' env p)
      -- map species to indexed vars:
      defs2vars :: [(Species,Expr)] -> [Int] -> [(Species,Int)]
      defs2vars ((k,v):ks) (x:xs) = (k,x) : (defs2vars ks xs)
      defs2vars [] _ = []
      defs2vars _ [] = X.throw $ CpiException "CpiODE.xdot: run out of vars!"
      xvars :: [Int]
      xvars = [0..]
      vmap :: [(Species,Int)]
      vmap = defs2vars p' xvars
      -- get the jacobian matrix:
      toJac p vmap xs = (len><len) (map convert (map (simp env) (map diff' (cp (unzip p')))))
          where
            len = length p'
            cp (x,y) = [(a,b)|b<-y,a<-x]
            diff' (x,e) = diff x e
            convert (Var s') = xs!!(maybe (err s') id (lookup s' vmap))
            convert (Plus e e') = (convert e) + (convert e')
            convert (Times e e') = (convert e) * (convert e')
            convert (Num d) = d
            err s' = X.throw $ CpiException 
                     ("Bug: bad lookup ("++(pretty s')++") in CpiODE.xdot.toODE")

-- Bringing in some infix funs from Numeric.LinearAlgebra:
-- Matrix constructor:
x><y = x LA.>< y
-- Vector accessor:
x@>y = x LA.@> y

-- get the initial concentrations of the primes in process space:
initials :: Env -> Process -> P' -> [Double]
initials env proc p' = initials' (Map.toList (simpP' env p'))
    where
      initials' [] = []
      initials' ((s,_):ss) = (initconc proc s):(initials' ss)

-- Get the time points
timePoints :: Int -> (Double,Double) -> LA.Vector Double
timePoints r (o,l) = LA.linspace r (o,l)

-- solve the ODEs with hmatrix
solveODE :: (Double -> [Double] -> [Double]) 
         -> [Double] 
         -> LA.Vector Double 
         -> LA.Matrix Double
solveODE x init ts = GSL.odeSolve x init ts

-- solve ODEs using Jacobian with hmatrix
solveODE' :: (Double -> [Double] -> [Double]) 
          -> (Double -> LA.Vector Double -> LA.Matrix Double)
          -> [Double] 
          -> LA.Vector Double 
          -> LA.Matrix Double
solveODE' odes jac inits ts
    = GSL.odeSolveV GSL.BSimp hi epsAbs epsRel (l2v odes) (Just jac) (LA.fromList inits) ts
      where 
        hi = (ts@>1 - ts@>0)/100
        epsAbs = 1e-08
        epsRel = 1e-08
        l2v f = \t -> LA.fromList . f t . LA.toList

-- plot the time series with GNUPlot
-- NOTE: deprecated by the CpiPlot module.
plotODE :: LA.Matrix Double -> LA.Vector Double -> IO ()
plotODE x ts = Plot.mplot (ts : LA.toColumns x)

-- pretty print a Map of ODEs:
prettyODE env map = pp (Map.toList (simpP' env map))
    where
      pp [] = []
      pp ((x,y):z) = "d[" ++ pretty(nice x) ++ "]/dt ===> " ++ pretty y ++ "\n" ++ pp z
      nice x = maybe x id (revLookupDef env x)

---------------------------------------
-- Time series output of ODE solutions:
---------------------------------------

-- e.g. for sending to the model checker

-- A time series here is a list of time points with corresponding 
-- map from Species to concentration.
type TimeSeries = [(Double, Map Species Double)]

-- List of species in a process space (reduced to Defs if possible):
speciesIn :: Env -> P' -> [Species]
speciesIn env p = map 
                  (\s->maybe s id (revLookupDef env s)) 
                  (map fst (Map.toList (simpP' env p)))

-- Get the raw time series:
timeSeries :: LA.Vector Double -> LA.Matrix Double -> [Species]-> TimeSeries
timeSeries v m ss 
    = timeSeries' (LA.toList v) (map LA.toList (LA.toColumns (LA.trans m))) ss
      where
        timeSeries' [] _ _ = []
        timeSeries' _ [] _ = []
        timeSeries' _ _ [] = []
        timeSeries' (t:ts) (cs:css) ss 
            = (t, Map.fromList (zip ss cs)) : timeSeries' ts css ss




--------------------------------------------
-- Symbolic semantics
--------------------------------------------

-- Here we build the symbolic counterpart to the CPi immediate behaviour
-- substituting a variable for the real values.

-- We map to an expression, rather than a real value:
data Expr = Var Species
          | Plus Expr Expr
          | Times Expr Expr
          | Num Double
            deriving (Show,Eq,Ord)

instance Pretty Expr where
    pretty (Var x) = "[" ++ (pretty x) ++ "]"
    pretty (Plus x y) = pretty x ++ " + " ++ pretty y
    pretty (Times (Plus x y) (Plus x' y')) 
        = "(" ++ pretty (Plus x y) ++ ") * (" ++ pretty (Plus x' y') ++ ")"
    pretty (Times (Plus x y) z) 
        = "(" ++ pretty (Plus x y) ++ ") * " ++ pretty z 
    pretty (Times x (Plus y z)) 
        = pretty x ++ " * (" ++ pretty (Plus y z) ++ ")"
    pretty (Times x y) = pretty x ++ " * " ++ pretty y
    pretty (Num k) = show k

-- Symbolic process space P and potential space D:
type P' = Map Species Expr 
type D' = Map (Species,Name,Concretion) Expr

-- pretty print our symbolic immediate behaviour:
prettyP' x = concat $ map (\(k,v)->((pretty k)++" |-> "++(pretty v)++"\n")) (Map.toList x)

-- Zero vectors:
p0' :: P'
p0' = Map.empty
d0' :: D'
d0' = Map.empty

-- vector constructors:
pVec' :: Env -> Species -> Expr -> P'
pVec' env s x 
    = foldr (\s' -> \m -> Map.insertWith Plus s' x m) p0' (primes env s)
dVec' :: (Species,Name,Concretion) -> Expr -> D'
dVec' t x = Map.singleton t x

-- Vector addition in P' and D':
pplus' :: P' -> P' -> P'
pplus' x y = Map.unionWith (Plus) x y
dplus' :: D' -> D' -> D'
dplus' x y = Map.unionWith (Plus) x y

-- Scalar multiplication in P and D:
ptimes' :: P' -> Double -> P'
ptimes' p v = Map.map (Times (Num v)) p
dtimes' :: D' -> Double -> D'
dtimes' d v = Map.map (Times (Num v)) d

-- Vector subtraction in P and D:
pminus' :: P' -> P' -> P'
pminus' x y = x `pplus'` (y `ptimes'` (-1))
dminus' :: D' -> D' -> D'
dminus' x y = x `dplus'` (y `dtimes'` (-1))

partial' :: Env -> Process -> D'
partial' _ (Process [] _) = d0'
partial' env proc@(Process ps _) = foldr dplus' d0' (map partial'' ps)
    where
      partial'' (s,_) = foldr dplus' d0' 
                        (map (\tr->dVec' (triple tr) (Var s)) (pots s))
      pots x = potentials (trans env (MTS []) x)
      -- mts = processMTS env proc
      triple (TransSC s n c) = (s,n,c)
      triple _ = X.throw $ CpiException ("Bug: CpiODE.partial'.triple passed something other than a TransSC")
      only x y
          | nf x == nf (transSrc y) = True
          | otherwise = False

tensor' :: Env -> AffNet -> D' -> D' -> P'
tensor' env net ds1 ds2 = foldr pplus' p0' (map expr ds)
    where
      expr (x,y,p) = pVec' env p (expr' x y) `pminus'`
                     pVec' env (tri1(fst(x))) (expr' x y) `pminus'`
                     pVec' env (tri1(fst(y))) (expr' x y)
      ds = [(x,y,p)
            |x<-Map.toList ds1, y<-Map.toList ds2,
             p<-[maybe Nil id (pseudoapp (tri3(fst(x))) (tri3(fst(y))))],
                p/=Nil
           ]
      -- above: x,y are (Spec,Name,Conc),Concentration);
      -- a is Rate; p is Species result of pseudoapplication
      expr' ((s,n,c),e) ((s',n',c'),e') 
          = Times (Num (s2d (maybe "0.0" id (aff net (n,n'))))) (Times e e')

-- The symbolic immediate behaviour (the set of ODEs):
{- FIXME: this requires the process with all species (inc. generated primes)
          change this so we take the pre-calculated MTS and get our primes
          from this. and avoids recalculating the MTS.
   Can apply this principle for partial too. We also then need a function to
   get the initial concentratiosn from the process. -}
dPdt' :: Env -> Process -> P'
dPdt' _ (Process [] _) = p0'
dPdt' env p@(Process [(s,c)] net)
    = foldr pplus' p0' (map f taus)
      `pplus'` Map.map (\x->(Times (Num 0.5) x))
             (tensor' env net (partial' env p) (partial' env p))
      where
        taus = [x|x<-openMTS(trans env (MTS []) s), tau x]
        tau (TransT _ _ _) = True
        tau _ = False
        f (TransT src r dst)
            = pVec' env dst (Times (Num k) (Var s)) `pplus'`
              pVec' env s (Times (Num(-1*k)) (Var s))
                  where k = s2d $ rate r
                        rate (TTau r) = r
        f _ = X.throw $ CpiException ("Bug: CpiODE.dPdt'.f passed something other than a TransT")
dPdt' env (Process (p:ps) net)
    = (tensor' env net partH partT) `pplus'` (dPdt' env procH)  `pplus'` (dPdt' env procT) 
      where
        partH = partial' env procH
        partT = partial' env procT
        procH = Process [p] net
        procT = Process ps net

-- Symbolic differentiation of an Expr
diff :: Species -> Expr -> Expr
diff _ (Num _) = Num 0.0
diff x (Var s)
    | s==x
        = Num 1.0
    | otherwise
        = Num 0.0
diff x (Plus a b)
    = Plus (diff x a) (diff x b)
diff x (Times a b)
    = Plus (Times (diff x a) b) (Times a (diff x b))

-- Simple simplification of an expression
simp :: Env -> Expr -> Expr
simp env x
    | x==x' = x'
    | otherwise = simp env x'
    where
      x' = simp' x
      simp' (Num n) = Num n
      simp' (Var s) = Var (simpS env s)
      simp' (Plus (Num 0.0) b) = simp env b
      simp' (Plus a (Num 0.0)) = simp env a
      simp' (Plus (Num a) (Num b)) = Num (a+b)
      simp' (Plus a b) = Plus (simp env a) (simp env b)
      simp' (Times (Num 0.0) b) = Num 0.0
      simp' (Times a (Num 0.0)) = Num 0.0
      simp' (Times (Num 1.0) b) = simp env b
      simp' (Times a (Num 1.0)) = simp env a
      simp' (Times (Num (-1.0)) (Num b)) = Num (-b)
      simp' (Times (Num a) (Num (-1.0))) = Num (-a)
      simp' (Times (Num a) (Num b)) = Num (a*b)
      simp' (Times a b) = Times (simp env a) (simp env b)

-- Simplify dP/dt
simpP' :: Env -> P' -> P'
simpP' env p = Map.mapKeys (simpS env) (Map.map (simp env) p)

-- Simplify species
simpS :: Env -> Species -> Species
simpS env s = maybe s id (revLookupDef env s)
