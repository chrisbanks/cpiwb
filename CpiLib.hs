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

data Prefix = Comm Name OutNames InNames
            | Tau Rate
              deriving (Eq, Ord)

data Aff = Aff ((Name,Name),Rate)
         deriving(Eq,Ord)

type AffNet = [Aff]

data Species = Nil
             | Def String [Name]
             | Sum [(Prefix, Species)]
             | Par [Species]
             | New AffNet Species
               deriving (Eq, Ord)

data Process = Process [(Species,Conc)] AffNet 
               deriving (Eq, Ord, Show)
--TODO: pretty print process

type SpeciesDef = (Name, [Name], Species)
type ProcessDef = (Name, Process)

----------------------
-- Pretty printing:
----------------------
-- by instantiation of show for each type
-- or a function for each synonym type

instance Show Species where
    show Nil = "0"
    show (Def i ns) = i++"("++(showNames ns)++")"
--    show (Sum ((p,s):pss)) = show' ((p,s):pss)
--        where show' ((p,s):pss) r = 
-- FIXME: non-exhaustive!!!!!!!!!!

instance Show Prefix where
    show (Tau r) = "tau<"++r++">"
    show (Comm n [] []) = n++"."
    show (Comm n [] is) = n++"("++(showNames is)++")."
    show (Comm n os []) = n++"<"++(showNames os)++">."
    show (Comm n os is) = n++"("++(showNames os)++";"++(showNames is)++")."

showNames :: [Name] -> String
showNames ns = concat(L.intersperse "," (L.sort ns))

instance Show Aff where
    show (Aff ((n1,n2),r)) = "("++n1++"-"++n2++"@"++r++")"

showAffNet :: AffNet -> String
showAffNet an = "(new "++(showNames(sites an))++")"

-- TODO: showSpeciesDef :: SpeciesDef -> String
-- TODO: showProcessDef :: ProcessDef -> String

-- TODO: show with correct parentheses:
-- showParens (x,x') = if ((prio x)>=(prio x'))
--                     then (show x)
--                     else "("++(show x)++")"
--     where prio Nil = 90
--           prio (Def _ _) = 90
--           prio (Sum ss) = --FIXME: finish this (cases for Sum, etc.)

----------------------
--Functions:
----------------------

--AffNet funs:
sites :: AffNet -> [Name]
sites net = sites' net []
    where
      sites' :: AffNet -> [Name] -> [Name]
      sites' ((Aff ((s1,s2),_)):affs) r = sites' affs (s1:s2:r)
      sites' [] r = L.nub r

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