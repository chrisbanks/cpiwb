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

module CpiSemantics where

import qualified Data.List as L
import qualified Control.Exception as X

import CpiLib

--------------------
-- Species semantics
--------------------

-- Semantic data structures:
data MTS = MTS [Trans]
           deriving (Show)

data Trans = Trans1 (Species,Name,Concretion)  -- A ----a-----> (x;y)B
           | Trans2 (Species,TTau,Species)     -- A ---t@k----> B
           | Trans3 (Species,TTauAff,Species)  -- A -t<a,b>@k-> B
             deriving (Show)

data Concretion = ConcBase Species OutNames InNames
                | ConcPar Concretion [Species]
                | ConcNew Concretion AffNet
                  deriving (Show)

data TTau = TTau Rate
            deriving (Show)
data TTauAff = TTauAff (Name,Name) Rate
               deriving (Show)
-- TODO: pretty print Multi-Transition System
--       (instantiate pretty)

-- Get the Multi-Transition System for a Process:
getMTS :: Process -> MTS
getMTS (Process ss net) = undefined -- TODO:

-- Add the transitions for a species to the MTS
trans :: [Definition] -> MTS -> Species -> MTS
trans env mts s = ifnotnil (lookupTrans mts s) (\x -> mts) (trans' env mts s)
    where
      trans' :: [Definition] -> MTS -> Species -> MTS
      -- Nil
      trans' env mts Nil = mts
      -- Def
      trans' env mts (Def _ _)
          = maybe ex (trans env mts) (lookupDef env s)
            where ex = X.throw (CpiException
                       ("Species "++(show s)++" not in the Environment."))
      -- Sum
      trans' _ mts (Sum []) = mts
      -- Sum(Tau + ...)
      trans' env mts (Sum (((Tau r),dst):pss))
          = MTS ((Trans2 (s,(TTau r),dst)):
                 ((\(MTS x) -> x)(trans env mts (Sum pss))))
      -- Sum(Comm + ...)
      trans' env mts (Sum (((Comm n o i),dst):pss))
          = undefined
      -- TODO: finish this.


lookupTrans :: MTS -> Species -> [Trans]
lookupTrans (MTS []) _ =  []
lookupTrans (MTS (tran:trans)) s
    | s == (transSrc tran)   = tran:(lookupTrans (MTS trans) s) 
    | otherwise              = lookupTrans (MTS trans) s

-- The source Species of a transition:
transSrc :: Trans -> Species
transSrc (Trans1 (s,_,_)) = s
transSrc (Trans2 (s,_,_)) = s
transSrc (Trans3 (s,_,_)) = s

-- Pseudo-application of concretions:
pseudoapp :: Concretion -> Concretion -> Species
pseudoapp (ConcBase s1 a x) (ConcBase s2 b y) 
    = Par [(sub (zip x b) s1),(sub (zip y a) s2)]
pseudoapp c1 (ConcPar c2 s2)
    = Par $ (pseudoapp c1 c2):s2
pseudoapp c1 (ConcNew c2 net)
    = New net (pseudoapp c1 c2)
pseudoapp (ConcPar c1 ss) c2
    = Par $ (pseudoapp c1 c2):ss
pseudoapp (ConcNew c1 net) c2
    = New net (pseudoapp c1 c2)


--------------------
-- Process semantics
--------------------