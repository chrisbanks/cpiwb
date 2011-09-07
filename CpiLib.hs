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

{-# OPTIONS_GHC -XDeriveDataTypeable -XTypeSynonymInstances #-}

module CpiLib where

import qualified Data.List as L
import qualified Control.Exception as X
import qualified Data.Typeable as T

data CpiException = CpiException String
                    deriving (Show,T.Typeable)
instance X.Exception CpiException

----------------------
--Data structures:
-----------------------

type Name = String
type OutNames = [Name]
type InNames = [Name]
type Rate = String
type Conc = String
type PrefixSpecies = (Prefix,Species)

data Prefix = Comm Name OutNames InNames
            | Tau Rate
              deriving (Eq, Ord, Show)

data Aff = Aff ((Name,Name),Rate)
         deriving(Eq, Ord, Show)

data AffNet = AffNet [Aff]
         deriving(Eq, Ord, Show)

data Species = Nil
             | Def String [Name]
             | Sum [PrefixSpecies]
             | Par [Species]
             | New AffNet Species
               deriving (Ord, Show)

data Process = Process [(Species,Conc)] AffNet 
               deriving (Eq, Ord, Show)

data Definition = SpeciesDef Name [Name] Species
                | ProcessDef Name Process
                  deriving (Eq,Show)

-- Referential equality of Species (e.g. for lookup).
instance Eq Species where
    Nil == Nil                 =  True
    (Def s ns) == (Def s' ns') =  (s==s')&&(ns==ns')
    (Sum ss) == (Sum ss')      =  ss==ss'
    (Par ss) == (Par ss')      =  ss==ss'
    (New n s) == (New n' s')   =  (n==n')&&(s==s')
    _ == _                     =  False

----------------------
-- Pretty printing:
----------------------
-- Instances of Pretty class are pretty-printable
-- CPi components.
class (Show a) => Pretty a where
    pretty :: a -> String
    pretty x = show x

instance Pretty Process where
    pretty (Process x@((s,c):scs) n)
        | null x = ""
        | length x == 1
            = "["++c++"] "++(pretty s)
        | otherwise
            = (pretty (Process [(s,c)] n))++" || "++(pretty (Process scs n))
    pretty (Process [] _) = "<Empty process>"

instance Pretty Species where
    pretty Nil = "0"
    pretty (Def i ns) = i++"("++(prettyNames ns)++")"
    pretty x'@(Sum x@((p,s):pss)) 
        | (length x == 1) 
            = (pretty p)++(prettyPs s x')
        | otherwise 
            = (pretty $ Sum [(p,s)])++" + "++(pretty $ Sum pss)
    pretty (Sum []) = "<Empty Sum>"
    pretty x'@(Par x@(s:ss))
        | (null x) 
            = ""
        | (length x == 1) 
            = pretty s
        | otherwise 
            = (prettyPs s x')++" | "++(prettyPs (Par ss) x')
    pretty (Par []) = "<Empty Par>"
    pretty (New n s) = (pretty n)++" "++(pretty s)

instance Pretty Prefix where
    pretty (Tau r) = "tau<"++r++">."
    pretty (Comm n [] []) = n++"."
    pretty (Comm n [] is) = n++"("++(prettyNames is)++")."
    pretty (Comm n os []) = n++"<"++(prettyNames os)++">."
    pretty (Comm n os is) = n++"("++(prettyNames os)++";"
                            ++(prettyNames is)++")."

instance Pretty Aff where
    pretty (Aff ((n1,n2),r)) = "("++n1++"-"++n2++"@"++r++")"

instance Pretty AffNet where
    pretty an = "(new "++(prettyNames(sites an))++")"

instance Pretty Definition where
    pretty (SpeciesDef n fns s) 
        = n++"("++(prettyNames fns)++") = "++(pretty s)
    pretty (ProcessDef  n p) 
        = n++" = "++(pretty p)

-- Ordering for Definitions:
instance Ord Definition where
    -- Any SpeciesDef < any ProcessDef
    compare (SpeciesDef _ _ _) (ProcessDef _ _)    = LT
    compare (ProcessDef _ _)   (SpeciesDef _ _ _)  = GT
    -- Both ProcessDef and SpeciesDef sort by name.
    compare (SpeciesDef n _ _) (SpeciesDef n' _ _) = compare n n'
    compare (ProcessDef n _)   (ProcessDef n' _)   = compare n n'

prettyNames :: [Name] -> String
prettyNames ns = concat(L.intersperse "," (L.sort ns))

-- Parenthesisation
prettyPs :: Species -> Species -> String
prettyPs x x' 
    | ((prio x)<=(prio x')) 
        = (pretty x)
    | otherwise 
        =  "("++(pretty x)++")"
    where prio Nil = 10
          prio (Def _ _) = 10
          prio (Sum ss)
              | (length ss == 1) = 10
              | otherwise = 20
          prio (Par _) = 30
          prio (New _ _) = 40

----------------------
--Functions:
----------------------

--AffNet funs:
sites :: AffNet -> [Name]
sites net = sites' net []
    where
      sites' :: AffNet -> [Name] -> [Name]
      sites' (AffNet ((Aff ((s1,s2),_)):affs)) r 
          = sites' (AffNet affs) (s1:s2:r)
      sites' (AffNet []) r = L.nub r

aff :: AffNet -> (Name,Name) -> Maybe Rate
aff (AffNet affs) (n,n') 
    | (a /= Nothing) = a
    | otherwise = (aff' (n',n) affs)
    where a = (aff' (n,n') affs)
          aff' _ []          =  Nothing
          aff' k ((Aff ((x,y),r)):affs')
              | k == (x,y) =  Just r
              | otherwise  =  aff' k affs'

netUnion :: AffNet -> AffNet -> AffNet
netUnion (AffNet n) (AffNet n') = AffNet (n \/ n')

--Free/Bound names:
fn :: Species -> [Name]
fn Nil = []
fn (Def l ns) = ns
fn (Par []) = []
fn (Par (x:xs)) = (fn x) \/ (fn (Par xs))
fn (New n s) = (fn s) \\ (sites n)
fn (Sum []) = []
fn (Sum (((Tau r),s):xs)) = fn s
fn (Sum (((Comm n o i),s):xs)) = [n] \/ o \/ ((fn s) \\ i) \/ (fn (Sum xs))

-- Definition lookup:
lookupDef :: [Definition] -> Species -> Maybe Species
lookupDef [] (Def _ _) = Nothing
lookupDef ((SpeciesDef i ps s):env) def@(Def x ns)
    | i == x    = Just (sub (zip ps ns) s)
    | otherwise = lookupDef env def
lookupDef ((ProcessDef _ _):env) def = lookupDef env def
lookupDef _ _ = X.throw $ CpiException 
                "Unexpected pattern: CpiLib.lookupDef expects a Def!"

-- Process lookup by name:
lookupProcName :: [Definition] -> String -> Maybe Process
lookupProcName [] _ = Nothing
lookupProcName ((SpeciesDef _ _ _):env) str = lookupProcName env str
lookupProcName ((ProcessDef name proc):env) str
    | (str == name) = Just proc
    | (otherwise)   = lookupProcName env str

-- Substitution of names in a Species:
-- sub (ns,ns') s = find free Names ns in Species s 
--                  and replace with New Names ns'
-- FIXME: can allow the substitution of names already bound e.g.:
-- !!!!!  A() = New(a,u,t) e<u,t>.a.E(e)
-- !!!!!   sub will allow A{e\a}, but here 'a' is bound!
-- !!!!!  Need to check the name really is free before sub'ing!
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

-- Fresh-for tests for restrictions
(#) :: AffNet -> Species -> Bool
net#s = ((sites net)/\(fn s)) == []

(##) :: AffNet -> AffNet -> Bool
net##net' = ((sites net)/\(sites net')) == []

------------------
-- Normal form
------------------
-- The "normal form" here is reduction by structural congruence
-- (not including alpha equiv.) and alphanumeric ordering of term/name lists
-- This allows us to define a smaller equivalence based on the referential
-- equality (see above).
class Nf a where
    nf :: a -> a

-- normal form for species
instance Nf Species where
    nf Nil = Nil
    nf (Def s ns) = Def s (L.sort ns)
    nf (Sum []) = Nil
    nf (Sum pfs) = Sum (L.sort (map nf pfs))
    nf (Par []) = Nil
    nf (Par [s]) = nf s
    nf (Par ss) = Par (L.sort (dropNils (flatten (map nf ss))))
        where
          dropNils = filter (\x->x/=Nil)
          flatten [] = []
          flatten (x:xs) = (f x)++(flatten xs)
              where
                f (Par ss) = ss 
                f s = [s]
    nf (New net (New net' s))
        | net##net' = undefined -- FIXME: finish struct.cong. of New...
    nf (New net@(AffNet ns) s)
        | net#s     = nf s
        | otherwise = New (AffNet (L.sort ns)) (nf s)
    
instance Nf PrefixSpecies where
    nf (p,s) = (p,(nf s))

-- TODO: normal form for concretions

-- TODO: normal form for processes

---------------------
-- Utility functions:
---------------------

--Set operations:
(\/) :: (Eq a) => [a] -> [a] -> [a]
(\/) = L.union

(/\) :: (Eq a) => [a] -> [a] -> [a]
(/\) = L.intersect

(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) = (L.\\)

--Real<->String
d2s :: Double -> String
d2s x = show x

s2d :: String -> Double
s2d x = read x :: Double

-- If not nil:
ifnotnil :: [a] -> ([a] -> b) -> b -> b
ifnotnil [] f b = b
ifnotnil xs f b = f xs

-- Pretty print a list of pretty printable expressions:
prettys :: (Pretty a) => [a] -> String
prettys x = concat $ map (\z->(pretty z)++"\n") x