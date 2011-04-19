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

data Trans = TransSC Species Name Concretion  -- A ----a-----> (x;y)B
           | TransT Species TTau Species      -- A ---t@k----> B
           | TransTA Species TTauAff Species  -- A -t<a,b>@k-> B
             deriving (Show)

data Concretion = ConcBase Species OutNames InNames
                | ConcPar Concretion [Species]
                | ConcNew AffNet Concretion
                  deriving (Show)

data TTau = TTau Rate
            deriving (Show)
data TTauAff = TTauAff (Name,Name)
               deriving (Show)

-- TODO: pretty print Multi-Transition System
--       (instantiate pretty)
-- Pretty printing:
instance Pretty MTS where
    pretty (MTS (t:ts)) = ((pretty t)++"\n")++(pretty (MTS ts))
    pretty (MTS []) = ""

instance Pretty Trans where 
    pretty (TransSC s n c) 
        = (pretty s)++" ---"++n++"--> "++(pretty c)
    pretty (TransT s t s')
        = prettyTauTrans s t s'
    pretty (TransTA s t s')
        = prettyTauTrans s t s'

instance Pretty Concretion where
    pretty (ConcBase s o i) 
        = "("++(prettyNames o)++";"++(prettyNames i)++")"++(pretty s)
    pretty (ConcPar c ss)
        = (pretty c)++" | "++concat(L.intersperse " | " (map pretty ss))
          -- TODO: parens?
    pretty (ConcNew net c)
        = (pretty net)++" "++(pretty c)

instance Pretty TTau where
    pretty (TTau r) = "tau@<"++r++">"
instance Pretty TTauAff where
    pretty (TTauAff (n1,n2)) = "tau@<"++n1++","++n2++">"

prettyTauTrans s t s' = (pretty s)++" ---"++(pretty t)++"--> "++(pretty s')

-- Get the Multi-Transition System for a Process:
getMTS :: Process -> MTS
getMTS (Process ss net) = undefined -- TODO:

-- Add the transitions for a species to the MTS
trans :: [Definition] -> MTS -> Species -> MTS
trans env mts s = trans' env mts s
    where
      trans' env mts s' 
          = ifnotnil (lookupTrans mts s') (\x -> mts) (trans'' env mts s')
          where
            trans'' :: [Definition] -> MTS -> Species -> MTS
            -- Nil
            trans'' env mts Nil = mts
            -- Def
            trans'' env mts (Def _ _)
                = maybe ex (trans' env mts) (lookupDef env s)
                where ex = X.throw (CpiException
                                    ("Species "++(pretty s)++" not in the Environment."))
            -- Sum
            trans'' _ mts (Sum []) = mts
            -- Sum(Tau + ...)
            trans'' env mts (Sum (((Tau r),dst):pss))
                = MTS ((TransT s (TTau r) dst):
                       (openMTS(trans' env mts (Sum pss))))
            -- Sum(Comm + ...)
            trans'' env mts (Sum (((Comm n o i),dst):pss))
                = MTS ((TransSC s n (ConcBase dst o i)):
                       (openMTS(trans' env mts (Sum pss))))
            -- Par
            trans'' _ mts (Par []) = mts
            trans'' env mts (Par (c:cs))
                = MTS ((openMTS(trans' env mts c))++
                       (openMTS(trans' env mts (Par cs))))
            -- New
            trans'' env mts (New net c)
                = MTS ((restrict(openMTS(trans' env (MTS []) c)))
                       ++(openMTS mts))
                where 
                  restrict [] = []
                  restrict ((TransSC _ n dst):trs) 
                      | n `elem` (sites net) 
                          = restrict trs
                      | otherwise
                          = (TransSC s n (ConcNew net dst)):(restrict trs)
                  restrict ((TransT _ t dst):trs)
                          = (TransT s t (New net dst)):(restrict trs)
                  restrict ((TransTA _ t@(TTauAff (n,n')) dst):trs)
                      | (r /= Nothing)
                          = (TransT s (TTau ((\(Just x)->x)r)) (New net dst))
                            :(restrict trs)
                      | otherwise
                          = (TransTA s t (New net dst)):(restrict trs)
                      where r = (aff net (n,n'))

-- Get the transition list of an MTS:
openMTS = \(MTS x) -> x

-- Lookup the transitions for a species in an MTS.
lookupTrans :: MTS -> Species -> [Trans]
lookupTrans (MTS []) _ =  []
lookupTrans (MTS (tran:trans)) s
    | s == (transSrc tran)   = tran:(lookupTrans (MTS trans) s) 
    | otherwise              = lookupTrans (MTS trans) s

-- The source Species of a transition:
transSrc :: Trans -> Species
transSrc (TransSC s _ _) = s
transSrc (TransT s _ _) = s
transSrc (TransTA s _ _) = s

-- Pseudo-application of concretions:
pseudoapp :: Concretion -> Concretion -> Species
pseudoapp (ConcBase s1 a x) (ConcBase s2 b y) 
    = Par [(sub (zip x b) s1),(sub (zip y a) s2)]
pseudoapp c1 (ConcPar c2 s2)
    = Par $ (pseudoapp c1 c2):s2
pseudoapp c1 (ConcNew net c2)
    = New net (pseudoapp c1 c2)
pseudoapp (ConcPar c1 ss) c2
    = Par $ (pseudoapp c1 c2):ss
pseudoapp (ConcNew net c1) c2
    = New net (pseudoapp c1 c2)


--------------------
-- Process semantics
--------------------