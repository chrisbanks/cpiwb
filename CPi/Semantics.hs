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

{-# OPTIONS_GHC -XPatternGuards #-}

module CPi.Semantics 
    (-- * Important functions:
     processMTS,
     -- * Datatypes:
     Concretion(..),
     MTS(..),
     Trans(..),
     TTau(..),
     TTauAff(..),
     -- * Functions:
     lookupTrans,
     pseudoapp,
     potentials,
     initconc,
     primes,
     wholeProc
    )where

import qualified Data.List as L
import qualified Control.Exception as X
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

import CPi.Lib

--------------------
-- Species semantics
--------------------

-- Semantic data structures:
data MTS = MTS [Trans]
           deriving (Show)

data Trans = TransSC Species Name Concretion  -- A ----a-----> (x;y)B
           | TransT Species TTau Species      -- A ---t@k----> B
           | TransTA Species TTauAff Species  -- A -t<a,b>@k-> B
             deriving (Show,Eq,Ord)

data Concretion = ConcBase Species OutNames InNames
                | ConcPar Concretion [Species]
                | ConcNew AffNet Concretion
                  deriving (Show,Eq,Ord)

data TTau = TTau Rate
            deriving (Show,Eq,Ord)
data TTauAff = TTauAff (Name,Name)
               deriving (Show,Eq,Ord)

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
    
prettyTauTrans s t s' = (pretty s)++" ---"++(pretty t)++"--> "++(pretty s')

instance Pretty Concretion where
    pretty (ConcBase s o i) 
        = "("++(prettyNames o)++";"++(prettyNames i)++")"++(pretty s)
    pretty (ConcPar c ss)
        = (pretty c)++" | "++concat(L.intersperse " | " (map pretty ss))
          -- TODO: parens?
    pretty (ConcNew net c)
        = (pretty net)++" "++(pretty c)

instance Pretty TTau where
    pretty (TTau r) = "tau@<"++(show r)++">"
instance Pretty TTauAff where
    pretty (TTauAff (n1,n2)) = "tau@<"++n1++","++n2++">"

-- Free/bound names of concretions:
fnc :: Concretion -> [Name]
fnc (ConcBase s o i) = o \/ ((fn s) \\ i)
fnc (ConcPar c ss) = (fnc c) \/ (fn (Par ss))
fnc (ConcNew n c) = (fnc c) \\ (sites n)
bnc :: Concretion -> [Name]
bnc (ConcBase s o i) = (bn s) \/ ((fn s) /\ i)
bnc (ConcPar c ss) = (bnc c) \/ (bn (Par ss))
bnc (ConcNew n c) = (bnc c) \/ ((fnc c) /\ (sites n))

-- Fresh-for test for restricted concretions
(#<) :: AffNet -> Concretion -> Bool
net#<c = ((sites net) /\ (fnc c)) == []

-- Renaming function for concretions:
concRename :: (Name,Name) -> Concretion -> Concretion
concRename r (ConcBase s ons ins)
    = ConcBase (rename r s) (vecRename r ons) (vecRename r ins)
concRename r (ConcPar c ss)
    = ConcPar (concRename r c) (map (rename r) ss)
concRename r (ConcNew net c)
    = ConcNew (netRename r net) (concRename r c)

-- Alpha-conversion of concretions
concAconv :: (Name,Name) -> Concretion -> Concretion
concAconv (old,new) c
    | (not(old `elem` (fnc c))) && (not(new `elem` (fnc c)))
        = concRename (old,new) c
    | (new `elem` (fnc c))
        = X.throw $ CpiException $ 
          "CPi.Semantics.concAconv: " 
          ++"Tried to alpha-convert to an existing free name."
    | otherwise
        = X.throw $ CpiException 
          "CPi.Semantics.concAconv: Tried to alpha-convert a non-bound name."

-- Normal form for concretions
-- NOTE: see note on normal form in CPi.Lib
instance Nf Concretion where
    nf s
        | result==s = result
        | otherwise = nf result
        where
          result = nf' s
          -- (b;y)(A|B)=A|(b;y)B  when y#A
          nf' (ConcBase (Par ss) o i) = liftfps (o,i) ss [] []
              where
                liftfps (o,i) [] [] ins
                    = ConcBase (nf (Par ins)) o i
                liftfps (o,i) [] outs ins
                    = ConcPar (ConcBase (nf (Par ins)) o i) (map nf outs)
                liftfps (o,i) (s:ss) outs ins
                    | (i/\(fn s))==[]     
                        = liftfps (o,i) ss (s:outs) ins
                    | otherwise 
                        = liftfps (o,i) ss outs (s:ins)
          -- (b;y)A=(b;y)B  when A=B
          nf' (ConcBase s o i) = ConcBase (nf s) o i
          -- Commu. and assoc. of ConcPar and F|0 = F
          nf' (ConcPar c []) = nf c
          nf' (ConcPar c ss) 
              = ConcPar (nf c) (L.sort (dropNils (flatten (map nf ss))))
                where
                  dropNils = filter (\x->x/=Nil)
                  flatten [] = []
                  flatten (x:xs) = (f x)++(flatten xs)
                      where
                        f (Par ss) = ss 
                        f s = [s]
          nf' (ConcNew net@(AffNet ns) (ConcNew net'@(AffNet ns') c))
              | net##net' && not(net#<c || net'#<c)
                  = nf (ConcNew (net `netUnion` net') c)
              | net#<c
                  = nf (ConcNew net' c)
              | net'#<c
                  = nf (ConcNew net c)
          nf' (ConcNew net@(AffNet ns) cp@(ConcPar c ss))
              | net#<c       = ConcPar (nf c) [(nf(New net (Par ss)))]
              | net#(Par ss) = ConcPar (nf (ConcNew net c)) [(nf (Par ss))]
              | otherwise    = ConcNew (AffNet (L.sort ns)) (nf cp)
          nf' (ConcNew net@(AffNet ns) c)
              | net#<c    = nf c
              | otherwise = ConcNew (AffNet (L.sort ns)) (nf c)

-- | Get the Multi-Transition System for a Process.
processMTS :: Env -> Process -> MTS
processMTS env (Process scs net) = buildMTS env net (MTS []) (map fst scs)

buildMTS :: Env -> AffNet -> MTS -> [Species] -> MTS
buildMTS env net mts ss = ifnotnil
                          (newAppls env net precompx)
                          (buildMTS env net precompx)
                          precompx
    where
      precompx = derivMTS env (transs env mts ss) 

-- Given initial species transtions, calculate all transition derivatives:
derivMTS :: Env -> MTS -> MTS
derivMTS env mts = ifnotnil (newPrimes env mts) (\x->(derivMTS env (transs env mts x))) mts

-- Find any pseudoapplications in an MTS and calculate their transitions:
complexMTS :: Env -> AffNet -> MTS -> MTS
complexMTS env net mts = derivMTS env (transs env mts (appls net mts))

-- Takes an MTS and returns the all prime species on the RHS of a transition 
-- which don't appear on the LHS of some transition.
newPrimes :: Env -> MTS -> [Species]
newPrimes env mts = newPrimes' (openMTS mts)
    where
      newPrimes' trs = [s | s<-(L.nub (concatMap (primes env) (mapMaybe transDest trs))),
                            not(inMTS mts s),
                            revLookupDef env s == Nothing]

-- Find any pseudoapplications in an MTS whose resultant species is a new prime
newAppls :: Env -> AffNet -> MTS -> [Species]
newAppls env net mts = newAppls' net (openMTS mts)
    where
      newAppls' net trs = [s | s<-(L.nub(appls net mts)), not(inMTS mts s)]

-- Is the species in the MTS?
inMTS :: MTS -> Species -> Bool
inMTS mts s = lookupTrans mts s /= []

-- Cardinality of an MTS:
mtsCard :: MTS -> Int
mtsCard x = length $ openMTS x

-- Add the immediate transitions for a species to the MTS:
trans :: Env -> MTS -> Species -> MTS
trans env mts s = trans' env mts s
    where
      trans' env mts s' 
          = ifnotnil (lookupTrans mts s') (\x -> mts) (trans'' env mts s')
          where
            trans'' :: Env -> MTS -> Species -> MTS
            -- Nil
            trans'' env mts Nil = mts
            -- Def
            trans'' env mts (Def _ _)
                = maybe ex (trans' env mts) (lookupDef env s')
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
                = MTS ((TransSC s n (nf(ConcBase dst o i))):
                       (openMTS(trans' env mts (Sum pss))))
            -- Par
            trans'' _ mts (Par []) = mts
            trans'' env mts (Par (ss)) 
                = MTS (transPar ss [] []) ->++ mts
                  where 
                    transPar (x:xs) alphas taus
                        = transPar xs (alphas'++alphas) ((taus' s alphas' alphas)++taus)
                          where 
                            alphas' = openMTS(trans env (MTS []) x)
                            taus' src (tr:trs) trs'
                                | (TransSC s n c) <- tr
                                    = (taus'' tr trs')++(taus' src trs trs')
                                | otherwise
                                    = taus' src trs trs'
                                where 
                                  taus'' (TransSC src n c) ((TransSC src' n' c'):trs')
                                      | Just dst <- pseudoapp c c'
                                          = (TransTA s (TTauAff (n,n'))
                                             (Par (remove src (replace src' dst ss))))
                                            :(taus'' tr trs')
                                      | otherwise
                                          = taus'' tr trs'
                                  taus'' tr (_:trs') = taus'' tr trs'
                                  taus'' _ [] = []
                            taus' _ [] _ = []
                    transPar [] alphas taus
                        = ((evs alphas)++taus) 
                          where 
                            evs ((TransT src tau dst):ts)
                                = (TransT s tau (nf(Par (replace src dst ss)))):(evs ts)
                            evs ((TransSC src n dst):ts) 
                                = (TransSC s n (nf(ConcPar dst (remove src ss)))):(evs ts)
                            evs (_:ts) = evs ts
                            evs [] = []
            -- New
            trans'' env mts (New net c)
                = MTS ((restrict(openMTS(trans' env (MTS []) c)))
                       ++(openMTS mts))
                where 
                  restrict [] = []
                  -- restrict (new x...) x.A ----x---> A
                  restrict ((TransSC _ n dst):trs) 
                      | n `elem` (sites net) 
                          = restrict trs
                      | otherwise
                          = (TransSC s n (nf(ConcNew net dst))):(restrict trs)
                  -- allow taus
                  restrict ((TransT _ t dst):trs)
                          = (TransT s t (nf(New net dst))):(restrict trs)
                  restrict ((TransTA _ t@(TTauAff (n,n')) dst):trs)
                      -- allow tau<n,m> 
                      | (r /= Nothing)
                          = (TransT s (TTau ((\(Just x)->x)r)) 
                                        (nf(New net dst)))
                            :(restrict trs)
                      -- restrict tau<n,m> where n or m not in net
                      | (r == Nothing) && (not(null((sites net) /\ [n,n'])))
                          = restrict trs
                      -- allow tau<n,m> otherwise.
                      | otherwise
                          = (TransTA s t (nf(New net dst))):(restrict trs)
                      where r = (aff net (n,n'))


-- Concatenate MTSs
(->++) :: MTS -> MTS -> MTS
x ->++ y = MTS ((openMTS x)++(openMTS y))

-- Add multiple species to the MTS:
transs :: Env -> MTS -> [Species] -> MTS
transs _ mts [] = mts
transs defs mts (spec:specs)
    = (transs defs mts' specs)
      where mts' = (trans defs mts spec)

-- Get the transition list of an MTS:
openMTS = \(MTS x) -> x

-- | Lookup the transitions (in an MTS) from a species.
lookupTrans :: MTS -> Species -> [Trans]
lookupTrans (MTS []) _ =  []
lookupTrans (MTS (tran:trans)) s
    | (nf s) == (nf(transSrc tran))   = tran:(lookupTrans (MTS trans) s) 
    | otherwise                       = lookupTrans (MTS trans) s

-- The source Species of a transition:
transSrc :: Trans -> Species
transSrc (TransSC s _ _) = s
transSrc (TransT s _ _) = s
transSrc (TransTA s _ _) = s

-- The destination species of a transition
--  is Nothing if RHS is a concretion
transDest :: Trans -> Maybe Species
transDest (TransSC _ _ _) = Nothing
transDest (TransT _ _ s) = Just s
transDest (TransTA _ _ s) = Just s


-- | Pseudo-application of concretions. Takes takes two concretions
--   and if they're compatable then gives the species they combine to form.
pseudoapp :: Concretion -> Concretion -> Maybe Species
pseudoapp (ConcBase s1 a x) (ConcBase s2 b y)
    | (length a == length y)&&(length x == length b)
        = Just $ nf(Par [(sub (zip x b) s1),(sub (zip y a) s2)])
    | otherwise
        = Nothing
pseudoapp c1 (ConcPar c2 s2)
    = maybe Nothing (Just.(\x->Par (x:s2))) (pseudoapp c1 c2)
pseudoapp c1 (ConcNew net c2)
    | net#<c1
        = maybe Nothing (Just.(\x->New net x)) (pseudoapp c1 c2)
    | otherwise
        = pseudoapp c1 (makeFresh (ConcNew net c2))
pseudoapp (ConcPar c1 ss) c2
    = maybe Nothing (Just.(\x->Par (x:ss))) (pseudoapp c1 c2)
pseudoapp (ConcNew net c1) c2
    | net#<c2
        = maybe Nothing (Just.(\x->New net x)) (pseudoapp c1 c2)
    | otherwise
        = pseudoapp (makeFresh (ConcNew net c1)) c2

-- give a restricted concretion fresh names for its AffNet
makeFresh :: Concretion -> Concretion
makeFresh c@(ConcNew net c') = freshen (sites net) c
    where
      freshen [] c = c
      freshen (n:ns) c = freshen ns (concAconv (n,(concRenaming n c)) c)
makeFresh x = x

-- a fresh renaming of a name in a concretion
concRenaming :: Name -> Concretion -> Name
concRenaming n c = renaming' (renames n) c
    where
      renaming' (n:ns) c
          | not(n `elem` (fnc c)) = n
          | otherwise             = renaming' ns c
      renaming' [] _
          = X.throw $ CpiException
            "CPi.Semantics.concRenaming: Renaming stream has been exhausted."
      -- a stream of possible renamings for a given name
      renames x = [x++p | p <- iterate (++"'") "'"]

-- get the resultants (complexes) of pseudoapplications in an MTS
appls :: AffNet -> MTS -> [Species]
appls net (MTS []) = []
appls net (MTS (tr:trs)) = appls' $ concs (tr:trs)
    where concs :: [Trans] -> [(Concretion,Name)]
          concs [] = []
          concs ((TransSC s n c):trs)
              = (c,n):(concs trs)
          concs (_:trs)
              = concs trs
          appls' :: [(Concretion,Name)] -> [Species]
          appls' cns = [maybe Nil id (pseudoapp c c') 
                        | (c,n) <- cns, 
                          (c',n') <- cns, 
                          aff net (n,n') /= Nothing ]


-- List the distinct prime species in an MTS:
allPrimes :: Env -> MTS -> [Species]
allPrimes env (MTS ts) 
    = nice . L.nub . concat . map (primes env) $ map transSrc ts
      where
        nice [] = []
        nice (s:ss) = maybe s id (revLookupDef env s) : nice ss

-- | Initial concentration of a species in a process.
initconc :: Process -> Species -> Double
initconc (Process scs _) s = initconc' scs s
    where
      initconc' ((s',c):scs) s
          | s == s'   = c
          | otherwise = initconc' scs s
      initconc' [] _ = 0

-- | Gives the complete syntactic process, including all potentially generated primes.
wholeProc :: Env -> Process -> MTS -> Process
wholeProc env p@(Process scs net) mts = Process scs' net
    where
      scs' = map (\s->(s,initconc p s)) (allPrimes env mts)

--------------------
-- Process semantics
 --------------------

-- | Prime components of a species, i.e. species that are not compositions.
primes :: Env -> Species -> [Species]
primes env spec = primes' env (nf spec)
    where
      primes' env Nil = []
      primes' env s@(Def _ _) = maybe ex (primes env) (lookupDef env s)
          where ex = X.throw (CpiException
                              ("Species "++(pretty s)++" not in the Environment."))
      primes' env s@(Sum _) = [s]
      primes' env s@(New _ _) = [s]
      primes' env (Par []) = []
      primes' env (Par ss) = concatMap (primes env) ss

-- Support of a process - prime species in a process:
-- NOTE: do we want to check conc.>0 ??
supp :: Env -> Process -> [Species]
supp env (Process [] _) = []
supp env (Process [(s,c)] _) = primes env s    
supp env (Process ((s,c):ps) aff) = (primes env s)++(supp env (Process ps aff))

-- | Gives the Class 1 (Species-->Concretion) transitions of an MTS.
potentials :: MTS -> [Trans]
potentials (MTS []) = []
potentials (MTS (t:ts))
    | (TransSC _ _ _) <- t
        = t:(potentials (MTS (ts)))
    | otherwise
        = potentials (MTS (ts))

-- cardinality of a transition in an MTS
cardT :: Trans -> MTS -> Integer
cardT t (MTS ts) = card t ts

-- cardinality of a species (s) in the prime decomposition of a species (s')
cardP :: Env -> Species -> Species -> Integer
cardP env d@(Def _ _) s' = case lookupDef env d of
                             Just s -> card (nf s) (primes env s')
                             Nothing -> X.throw (CpiException 
                                                 ("Species "++(pretty d)++" not in the Environment."))
cardP env s s' = card (nf s) (primes env s')

-----------------------------------------
-- Process space P and potential space D:
type P = Map Species Double
type D = Map (Species,Name,Concretion) Double

prettyP x = concat $ map (\(k,v)->((pretty k)++" |-> "++(show v)++"\n")) (Map.toList x)

-- Zero vectors:
p0 :: P
p0 = Map.empty
d0 :: D
d0 = Map.empty

-- Basis vectors:
p1 :: Species -> P
p1 x = Map.singleton x 1
d1 :: (Species,Name,Concretion) -> D
d1 x = Map.singleton x 1

-- Scaled basis vectors:
pVec :: Species -> Double -> P
pVec s x = Map.singleton s x
dVec :: (Species,Name,Concretion) -> Double -> D
dVec t x = Map.singleton t x

-- Vector addition in P and D:
pplus :: P -> P -> P
pplus x y = Map.unionWith (+) x y
dplus :: D -> D -> D
dplus x y = Map.unionWith (+) x y

-- Scalar multiplication in P and D:
ptimes :: P -> Double -> P
ptimes p v = Map.map (v *) p
dtimes :: D -> Double -> D
dtimes d v = Map.map (v *) d

-- Vector subtraction in P and D:
pminus :: P -> P -> P
pminus x y = x `pplus` (y `ptimes` (-1))
dminus :: D -> D -> D
dminus x y = x `dplus` (y `dtimes` (-1))

-- Interaction potential
partial :: Env -> Process -> D
partial env (Process [] _) = Map.empty
partial env proc@(Process ps _)
    = foldr dplus d0 (map partial' ps)
      where
        partial' (s,c) = foldr dplus d0 
                         (map (\tr-> 
                               dVec (triple tr)
                               (c * (fromInteger(cardP env (transSrc tr) s))))
                          pots)
        pots = potentials mts
        mts = processMTS env proc
        triple (TransSC s n c) = (s,n,c)
        triple _ = X.throw $ CpiException ("Bug: CPi.Semantics.partial.triple passed something other than a TransSC")

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
             a<-[maybe 0.0 id (aff net ((tri2(fst(x))),(tri2(fst(y)))))],
                a/=0.0,
             p<-[maybe Nil id (pseudoapp (tri3(fst(x))) (tri3(fst(y))))],
                p/=Nil
           ]
      -- x,y are (Spec,Name,Conc),Concentration);
      -- a is Rate; p is Species result of pseudoapplication
      f (((s,n,c),v),((s',n',c'),v'),a,p)
          = ((((embed env p) `pminus` (p1 s)) `pminus` (p1 s')) 
            `ptimes` a) `ptimes` (v*v')
      
-- Immediate behaviour
dPdt :: Env -> Process -> P
dPdt _ (Process [] _) = p0
dPdt env p@(Process [(s,c)] net)
    = (foldr pplus p0 (map tauexpr taus)) 
      `pplus` ((tensor env net part1 part1) `ptimes` 0.5)
      where
        tauexpr (TransT src (TTau r) dst)
            = (((embed env src) `pminus` (embed env dst)) 
               `ptimes` r) `ptimes` c
        tauexpr _ = X.throw $ CpiException
                    ("Bug: CPi.Semantics.dPdt.tauexpr passed something other than a TransT")
        taus = [x|x<-openMTS(processMTS env p), tau x]
        tau (TransT _ _ _) = True
        tau _ = False
        part1 = partial env (Process [(s,c)] net)
dPdt env (Process (p:ps) net)
    = (tensor env net partT partH) `pplus` (dPdt env procT) `pplus` (dPdt env procH)
      where
        partH = partial env procH
        partT = partial env procT
        procH = Process [p] net
        procT = Process ps net

{-
---------------------------
Here lies an attempted implementation of process semantics 
in a nice, lazy way...
---------------------------

type P = Species -> Double
type D = Trans -> Double

-- the interaction potential
partial :: Env -> MTS -> Process -> D
partial env mts (Process ss net) = partial' env mts ss
    where
      partial' env mts ss
          = \t@(TransSC s n c) -> (expr t ss)
      expr _ [] = 0
      expr t@(TransSC s n c) ((spec,conc):ss) =
          ((s2d conc) * (fromInteger(cardT t mts)) * (fromInteger(cardP env s spec))) + (expr t ss)
      -- NOTE: (\x.f(x))+(\x.f(x)) == \x.f(x)+f(x)
      expr t _ = X.throw (CPi.Exception
                          ("This is a bug! Partial behavior depends only on Class 1 trans (SC). Incorrectly given: "++(pretty t)))

-- the species embedding
embed :: Env -> Species -> P
embed env s = embed' $ primes env s
    where
      embed' ss = (\a->(if (expr a ss) then 1 else 0))
      expr _ [] = False
      expr a (x:xs) = (a==x)||(expr a xs)

-- the interaction tensor
tensor :: AffNet -> D -> D -> P
tensor = undefined -- TODO:
-}
