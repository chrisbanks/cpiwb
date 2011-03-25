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
              deriving (Eq, Ord, Show)

data Aff = Aff ((Name,Name),Rate)
         deriving(Eq, Ord, Show)

type AffNet = [Aff]

data Species = Nil
             | Def String [Name]
             | Sum [PrefixSpecies]
             | Par [Species]
             | New AffNet Species
               deriving (Eq, Ord, Show)

type PrefixSpecies = (Prefix,Species)

data Process = Process [(Species,Conc)] AffNet 
               deriving (Eq, Ord, Show)

type SpeciesDef = (Name, [Name], Species)
type ProcessDef = (Name, Process)

----------------------
-- Pretty printing:
----------------------
-- by instantiation of "pretty" for each type
-- or a function for each synonym type

class (Show a) => Expr a where
    pretty :: a -> String
    pretty x = show x

instance Expr Process where
    pretty (Process x@((s,c):scs) n)
        | null x = ""
        | length x == 1
            = "["++c++"] "++(pretty s)
        | otherwise
            = (pretty (Process [(s,c)] n))++" || "++(pretty (Process scs n))

instance Expr Species where
    pretty Nil = "0"
    pretty (Def i ns) = i++"("++(prettyNames ns)++")"
    pretty x'@(Sum x@((p,s):pss)) 
        | (null x) = ""
        | (length x == 1) 
            = (pretty p)++(prettyPs s x')
        | otherwise 
            = (pretty $ Sum [(p,s)])++" + "++(pretty $ Sum pss)
    pretty (Par x@(s:ss))
        | (null x) 
            = ""
        | (length x == 1) 
            = pretty s
        | otherwise 
            = (pretty s)++" | "++(pretty(Par ss))
    pretty (New n s) = (prettyAffNet n)++" "++(pretty s)

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

instance Expr Prefix where
    pretty (Tau r) = "tau<"++r++">"
    pretty (Comm n [] []) = n++"."
    pretty (Comm n [] is) = n++"("++(prettyNames is)++")."
    pretty (Comm n os []) = n++"<"++(prettyNames os)++">."
    pretty (Comm n os is) = n++"("++(prettyNames os)++";"
                            ++(prettyNames is)++")."

prettyNames :: [Name] -> String
prettyNames ns = concat(L.intersperse "," (L.sort ns))

instance Expr Aff where
    pretty (Aff ((n1,n2),r)) = "("++n1++"-"++n2++"@"++r++")"

prettyAffNet :: AffNet -> String
prettyAffNet an = "(new "++(prettyNames(sites an))++")"

prettySpeciesDef :: SpeciesDef -> String
prettySpeciesDef (n,fns,s) = n++"("++(prettyNames fns)++") = "++(pretty s)

prettyProcessDef :: ProcessDef -> String
prettyProcessDef (n,p) = n++" = "++(pretty p)


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