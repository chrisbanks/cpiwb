-- (C) Copyright Chris Banks 2011

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

--xdot :: Env -> P' -> (Double -> [Double] -> [Double])
xdot env p = vmap
    where
      -- replace species with their Defs (for human reading):
      key2Def :: Env -> [(Species,Expr)] -> [(Species,Expr)]
      key2Def env ((k,v):ks) 
          = (maybe k id (revLookupDef env k),v) : (key2Def env ks)
      key2Def _ [] = []
      p' :: [(Species,Expr)]
      p' = key2Def env (Map.toList p)
      -- map species to vars:
      -- TODO: cache the map of vars->defs (in Env)?
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
            convert (Scale d e) = d * (convert e)
            convert (Times e e') = (convert e) * (convert e')
            err s' = X.throw $ CpiException 
                     ("Bug: bad lookup ("++(pretty s')++") in CpiODE.xdot.toODE")
      -- the xdot function we need to return
      xdot' t xs = toODE p' vmap xs

--------------------------------------------
-- Symbolic semantics
--------------------------------------------

-- Here we build the symbolic counterpart to the CPi immediate behaviour
-- substituting a variable for the real values.

-- We map to an expression, rather than a real value:
data Expr = Var Species
          | Plus Expr Expr
          | Scale Double Expr
          | Times Expr Expr
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
    pretty (Scale k (Plus x y)) 
        = (show k) ++ " * (" ++ pretty (Plus x y) ++ ")"
    pretty (Scale k x) = (show k) ++ " * " ++ pretty x
    -- show (Const k) = k
    -- TODO: Used Marek's print fun for now, needs sorting out?

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
ptimes' p v = Map.map (Scale v) p
dtimes' :: D' -> Double -> D'
dtimes' d v = Map.map (Scale v) d

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
          = Scale (s2d (maybe "0.0" id (aff net (n,n')))) (Times e e')

dPdt' :: Env -> Process -> P'
dPdt' _ (Process [] _) = p0'
dPdt' env p@(Process [(s,c)] net)
    = foldr pplus' p0' (map f taus)
      `pplus'` Map.map (\x->(Scale 0.5 x)) 
             (tensor' env net (partial' env p) (partial' env p))
      where
        taus = [x|x<-openMTS(trans env (MTS []) s), tau x]
        tau (TransT _ _ _) = True
        tau _ = False
        f (TransT src r dst)
            = pVec' env dst (Scale k (Var s)) `pplus'` 
              pVec' env s (Scale (-1*k) (Var s))
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
