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

import CpiLib
import CpiSemantics

--------------------------------------------
-- Output the ODEs describing a CPi system
--------------------------------------------


--------------------------------------------
-- Symbolic semantics
--------------------------------------------

-- Here we build the symbolic counterpart to the CPi immediate behaviour
-- substituting a variable for the real values.

-- We map to an expression, rather than a real value:
data Expr = Var Species
          | Plus Expr Expr
          | Scale Double Expr
            deriving (Show)

-- Symbolic process space P and potential space D:
type P' = Map Species Expr 
type D' = Map (Species,Name,Concretion) Expr

-- Zero vectors:
p0' :: P'
p0' = Map.empty
d0' :: D'
d0' = Map.empty

-- Construct vectors:
pVec' :: Species -> Expr -> P'
pVec' s x = Map.singleton s x
dVec' :: (Species,Name,Concretion) -> Expr -> D'
dVec' t x = Map.singleton t x

-- Vector addition in P and D:
pplus' :: P' -> P' -> P'
pplus' x y = Map.unionWith Plus x y
dplus' :: D' -> D' -> D'
dplus' x y = Map.unionWith Plus x y

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

-- Interaction potential
partial' :: Env -> Process -> D'
partial' env (Process [] _) = Map.empty
partial' env proc@(Process ps _)
    = foldr dplus' d0' (map partial'' ps)
      where
        partial'' (s,c) = foldr dplus' d0' 
                         (map (\tr-> 
                               dVec' (triple tr)
                               (Scale ((fromInteger(cardT tr mts))*
                                       (fromInteger(cardP env (transSrc tr) s))
                                      ) (Var s))
                              ) pots)
        pots = potentials mts
        mts = processMTS env proc
        triple (TransSC s n c) = (s,n,c)
        triple _ = X.throw $ CpiException ("Bug: CpiODE.partial'.triple passed something other than a TransSC")

-- ####################################
-- TODO: Continue adapting from here:
{-
-- Species embedding
embed :: Env -> Species -> P
embed env Nil = Map.empty
embed env d@(Def _ _) = maybe ex (\s->embed env s) (lookupDef env d)
    where
      ex = X.throw $ CpiException
           ("Error: Tried to embed unknown definition "++(pretty d)++".")
    -- NOTE: if the Def is in S# maybe we want to embed the Def itself
    --       rather than its expression? (for UI reasons)
embed env (Par ss) = foldr pplus p0 (map (embed env) ss)
embed env s = p1(s)

-- Interaction tensor
tensor :: Env -> AffNet -> D -> D -> P
tensor env net ds1 ds2 = foldr pplus p0 (map f ds)
    where
      ds = [(x,y,a,p)
            |x<-Map.toList ds1, y<-Map.toList ds2,
             a<-[maybe 0.0 s2d (aff net ((tri2(fst(x))),(tri2(fst(y)))))],
                a/=0.0,
             p<-[maybe Nil id (pseudoapp (tri3(fst(x))) (tri3(fst(y))))],
                p/=Nil
           ]
      f (((s,n,c),v),((s',n',c'),v'),a,p)
          = ((((embed env p) `pminus` (p1 s)) `pminus` (p1 s')) 
            `ptimes` a) `ptimes` (v*v')
      
-- Immediate behaviour
dPdt :: Env -> Process -> P
dPdt _ (Process [] _) = p0
dPdt env p@(Process [(s,c)] net)
    = (foldr pplus p0 (map tauexpr taus)) -- TODO: plus potentials expression!
      where
        tauexpr (TransT src (TTau r) dst)
            = (((embed env src) `pminus` (embed env dst)) 
               `ptimes` (s2d r)) `ptimes` (s2d c)
        tauexpr _ = X.throw $ CpiException
                    ("Bug: CpiSemantics.dPdt.tauexpr passed something other than a TransT")
        taus = [x|x<-openMTS(processMTS env p), tau x]
        tau (TransT _ _ _) = True
        tau _ = False
dPdt env p@(Process ps net)
    = undefined -- TODO:sum

-}