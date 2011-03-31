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

module CpiLib where

import qualified Data.List as L

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
               deriving (Eq, Ord, Show)

data Process = Process [(Species,Conc)] AffNet 
               deriving (Eq, Ord, Show)

data Definition = SpeciesDef Name [Name] Species
                | ProcessDef Name Process
                  deriving (Eq,Show)



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

instance Pretty Species where
    pretty Nil = "0"
    pretty (Def i ns) = i++"("++(prettyNames ns)++")"
    pretty x'@(Sum x@((p,s):pss)) 
        | (null x) = ""
        | (length x == 1) 
            = (pretty p)++(prettyPs s x')
        | otherwise 
            = (pretty $ Sum [(p,s)])++" + "++(pretty $ Sum pss)
    pretty x'@(Par x@(s:ss))
        | (null x) 
            = ""
        | (length x == 1) 
            = pretty s
        | otherwise 
            = (prettyPs s x')++" | "++(prettyPs (Par ss) x')
    pretty (New n s) = (pretty n)++" "++(pretty s)

instance Pretty Prefix where
    pretty (Tau r) = "tau<"++r++">"
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



---------------------
--Utility functions:
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