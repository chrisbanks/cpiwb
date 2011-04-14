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

import CpiLib

--------------------
-- Species semantics
--------------------

-- Semantic data structures:
data MTS = MTS [Trans]

data Trans = Trans1 (Species,Name,Concretion)     -- A ----a-----> (x;y)B
           | Trans2 (Species,ConcTau,Species)     -- A ---t@k----> B
           | Trans3 (Species,ConcTauAff,Species)  -- A -t<a,b>@k-> B

data Concretion = ConcBase Species OutNames InNames
                | ConcPar Concretion [Species]
                | ConcNew Concretion AffNet

data ConcTau = ConcTau Rate
data ConcTauAff = ConcTauAff (Name,Name) Rate

-- Get the Multi-Transition System for a Process:
getMTS :: Process -> MTS
getMTS (Process ss net) = undefined -- TODO:

-- Add the transitions for a species to the MTS
trans :: MTS -> Species -> MTS
trans mts s = ifnotnil (lookupTrans mts s) (\x -> mts) (trans' mts s)
    where
      trans' mts Nil = mts
      trans' mts (Def n ns) = undefined -- TODO:

-- FIXME: !!!!!!!!!!!!!!!! BEGIN
lookupTrans :: MTS -> Species -> [Trans]
lookupTrans (MTS []) _ =  []
lookupTrans (MTS ((x,y,z):xyzs)) s
    | s == x    = (x,y,z):(lookupTrans (MTS xyzs) s) 
    | otherwise = lookupTrans xyzs s
-- FIXME: !!!!!!!!!!!!!!!! END

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

-- Substitution of names in a Species:
-- sub s (ns,ns') = find Names ns in Species s and replace with New Names ns'
sub :: [(Name,Name)] -> Species -> Species
sub _ Nil = Nil
sub subs (Def l ns) = (Def l (nameSub ns subs))
sub subs (Sum ps) = (Sum (map prefixSub ps))
    where prefixSub :: PrefixSpecies -> PrefixSpecies
          prefixSub ((Tau r),s) = ((Tau r),(sub subs s))
          prefixSub ((Comm n o i),s)
              = ((Comm (maybe n id (L.lookup n subs)) (nameSub o subs) i),
                 (sub subs' s))
                     where subs' = [x | x <- subs, not (elem (fst x) i)]
sub subs (Par ss) = (Par (map (sub subs) ss))
sub subs (New net s) = (New net (sub subs' s))
    where subs' = [x | x <- subs, not (elem (fst x) (sites net))]

-- Substitution on name vectors
nameSub :: [Name] -> [(Name,Name)] -> [Name]
nameSub [] _ = []
nameSub (n:ns) ss = (maybe n id (L.lookup n ss)):(nameSub ns ss)


--------------------
-- Process semantics
--------------------