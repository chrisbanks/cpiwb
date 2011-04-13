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
getMTS (Process ss net) = undefined -- TODO: but first the foundations:

-- Pseudo-application of concretions:
pseudoapp :: Concretion -> Concretion -> Species
pseudoapp (ConcBase s1 a x) (ConcBase s2 b y) 
    = Par [(sub s1 (b,x)),(sub s2 (a,y))]
pseudoapp c1 (ConcPar c2 s2)
    = Par $ (pseudoapp c1 c2):s2
pseudoapp c1 (ConcNew c2 net)
    = New net (pseudoapp c1 c2)
pseudoapp (ConcPar c1 ss) c2
    = Par $ (pseudoapp c1 c2):ss
pseudoapp (ConcNew c1 net) c2
    = New net (pseudoapp c1 c2)

-- Substitution of names in a Species:
sub :: Species -> ([Name],[Name]) -> Species
sub s ((n:ns),(n':ns')) = undefined -- TODO: define substitution


--------------------
-- Process semantics
--------------------