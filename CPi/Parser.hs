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

module CPi.Parser
    (parseFile,
     parseDefn,
     parseFormula
    ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Control.Exception as X

import CPi.Lib
import qualified CPi.Logic as LBC


---------------------
-- Definition Parser:
---------------------

-- | Parses a single CPi definiton (Species or Process).
parseDefn :: String -> Either ParseError Definition
parseDefn x = parse pDefinition "" x

-- | Parses a string of CPi definitions (i.e. as read from a file).
parseFile :: String -> Either ParseError [Definition]
parseFile x = parse pDefinitionLines "" x

----------------
-- Logic Parser:
----------------

-- | Parses an LBC formula.
parseFormula :: String -> Either ParseError LBC.Formula
parseFormula x = parse pFormula "" x

-----------------
-- Lexer
-----------------

lexer = P.makeTokenParser cpiDef

cpiDef = emptyDef {
           commentLine = "--",
           reservedNames = 
               ["species","network","process","tau", "new", "0"],
           reservedOpNames =
               ["=","|","+","||","-","@",".",":"] }

parens = P.parens lexer     -- ()
braces = P.braces lexer     -- {}
brackets = P.brackets lexer -- []
angles = P.angles lexer     -- <>
symbol = P.symbol lexer
comma = P.comma lexer
semi = P.semi lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
rOp = P.reservedOp lexer

double = try (P.float lexer) 
         <|> do i<- P.integer lexer;
                return (fromInteger i)

ws = P.whiteSpace lexer
commaSep = P.commaSep lexer

------------------
-- Parser
------------------

-- TODO: Use <?> combinator to generate human readable err msgs.

-- Process/Species definition
pDefinition :: Parser Definition
pDefinition = (do reserved "process";
                  i <- identifier;
                  rOp "=";
                  p <- pProcess;
                  return (ProcessDef i p) )
              <|>
              (do reserved "species";
                  i <- identifier;
                  ns <- pFreeNames;
                  rOp "=";
                  s <- pSpecies;
                  return (SpeciesDef i ns s) )

pDefinitionLines :: Parser [Definition]
pDefinitionLines = many1(do ws;
                            d <- pDefinition;
                            semi;
                            return d)

-- Process expression
pProcess :: Parser Process
pProcess = do pcs <- sepBy pProcessComponent (rOp "||");
              rOp ":";
              aff <- pAffNet;
              return (Process pcs aff)

pProcessComponent :: Parser (Species,Conc)
pProcessComponent = do c <- brackets(double);
                       s <- pSpecies;
                       return (s,c)

-- Species expression
pSpecies :: Parser Species
pSpecies = pPar

-- Parallel expression
pPar :: Parser Species
pPar = chainr1 pSum pParOp

pParOp :: Parser (Species -> Species -> Species)
pParOp = do rOp "|";
            return parOp

parOp :: Species -> Species -> Species
parOp (Par ss) (Par ss') = (Par (ss++ss'))
parOp s (Par ss) = (Par (s:ss))
parOp (Par ss) s = (Par (s:ss))
parOp s1 s2 = (Par [s1,s2])

-- Sum expression
pSum :: Parser Species
pSum = chainl1 pPre pSumOp

pSumOp :: Parser (Species -> Species -> Species)
pSumOp = do rOp "+";
            return sumOp

sumOp :: Species -> Species -> Species
sumOp (Sum ss) (Sum ss') = (Sum (ss++ss'))
sumOp _ _ = X.throw $ X.PatternMatchFail 
            "Unexpected pattern: Sum must be prefix guarded!"
-- FIXME: This won't happen for correct syntax (prefix guarded Sum)
--        Either change the parser so we don't allow it (hard?), 
--         or handle this properly up in the IO monad.


-- Prefix expression
pPre :: Parser Species
pPre = try (do p <- pPrefix;
               rOp ".";
               s <- pPre;
               return (Sum [(p,s)]) )
       <|> pSimple

-- Simple species terms
pSimple :: Parser Species
pSimple = pNil
          <|> pDef
          <|> try pNew
          <|> (parens pSpecies)

-- Nil (0):
pNil :: Parser Species
pNil = do reserved "0";
          return Nil

-- Species constant:
pDef :: Parser Species
pDef = do i <- identifier;
          ns <- pFreeNames;
          return (Def i ns)

pFreeNames :: Parser [Name]
pFreeNames = parens pNameList

pNameList = commaSep identifier

-- New network:
pNew :: Parser Species
pNew = do net <- pAffNet;
          s <- pSpecies;
          return (New net s)

-- Affinity network:
pAffNet :: Parser AffNet
pAffNet = do as <- braces(commaSep pAff);
             return (AffNet as)

pAff :: Parser Aff
pAff = do s1 <- identifier;
          rOp "-";
          s2 <- identifier;
          rOp "@";
          r <- double;
          return (Aff ((s1,s2),r))

-- Prefix:
pPrefix :: Parser Prefix
pPrefix = pTau <|> pComm

pTau :: Parser Prefix
pTau = do reserved "tau";
          r <- angles double;
          return (Tau r)

pComm :: Parser Prefix
pComm = try (do n <- identifier;
                (os,is) <- pCommParam;
                return (Comm n os is) )
        <|> (do n <- identifier;
                return (Comm n [] []) )

pCommParam = try pCommIO <|> pCommI <|> pCommO
pCommI = do is <- parens pNameList;
            return ([],is) 
pCommO = do os <- angles pNameList;
            return (os,[])
pCommIO = parens (do os <- pNameList;
                     semi;
                     is <- pNameList;
                     return (os,is) )


----------------------
-- Logic Lexer:
----------------------

lexerF = P.makeTokenParser defLBC

defLBC = emptyDef {
           reservedNames = 
               ["true","false"],
           reservedOpNames =
               ["+","-","*","/",
                ">","<",">=","<=",
                "&","|","==>","!","¬",
                "F","G","U",
                "|>"] }

fparens = P.parens lexerF     -- ()
fbrackets = P.brackets lexerF -- []
fbraces = P.braces lexerF     -- {}
fidentifier = P.identifier lexerF -- Species
freserved = P.reserved lexerF
fOp = P.reservedOp lexerF
fdouble = try (P.float lexerF) 
          <|> do i<- P.integer lexerF;
                 return (fromInteger i)
fws = P.whiteSpace lexerF
fcomma = P.comma lexerF


----------------------
-- Logic Parser:
----------------------

pFormula = buildExpressionParser fOps pFAtom
fOps = [[unop "!" LBC.Neg,
         itl "F" LBC.Pos,
         itl "G" LBC.Nec,
         btl "F" LBC.Pos,
         btl "G" LBC.Nec,
         unop "F" (LBC.Pos (0,infty)),
         unop "G" (LBC.Nec (0,infty))],
        [iu "U" LBC.Until,
         bu "U" LBC.Until,
         biop "U" (LBC.Until (0,infty))],
        [biop "&" LBC.Conj], 
        [biop "|" LBC.Disj],
        [biop "==>" LBC.Impl],
        [gtee]]
    where
      biop s f = Infix (try (fOp s >> return f)) AssocLeft
      unop s f = Prefix (try (fOp s >> return f))
      btl s f = Prefix (try (pFbtl s f))
      itl s f = Prefix (try (pFitl s f))
      bu s f = Infix (try (pFbtl s f)) AssocLeft
      iu s f = Infix (try (pFitl s f)) AssocLeft
      gtee = Prefix (try pFGtee)

pFbtl s f = do fOp s
               t <- fbraces fdouble
               return $ f (0,t)
pFitl s f = do fOp s
               i <- fbraces (do t1 <- fdouble
                                fcomma
                                t2 <- fdouble
                                return (t1,t2))
               return $ f i
pFGtee = do s <- fidentifier 
            fOp "|>"
            return $ LBC.Gtee s

pFAtom = try pFValBool <|> pFTrue <|> pFFalse <|> parens pFormula

pFValBool = do v1 <- pFValExpr
               op <- relation
               v2 <- pFValExpr
               return $ op v1 v2
    where
      relation = try (fOp ">=" >> return LBC.ValGE) <|>
                 try (fOp "<=" >> return LBC.ValLE) <|>
                 (fOp ">" >> return LBC.ValGT) <|>
                 (fOp "<" >> return LBC.ValLT)

pFValExpr = buildExpressionParser valOps pFVal
valOps = [[op "*" LBC.Times, op "/" LBC.Quot],
         [op "+" LBC.Plus, op "-" LBC.Minus]]          
    where
      op s f = Infix (fOp s >> return f) AssocLeft

pFVal = parens pFValExpr <|> pFReal <|> try pFDeriv <|> pFConc

pFReal = do r <- fdouble
            return (LBC.R r)
pFConc = do id <- brackets fidentifier
            return (LBC.Conc (Def id []))
pFDeriv = do id <- brackets fidentifier
             symbol "'"
             return (LBC.Deriv (Def id []))

pFTrue = freserved "true" >> return LBC.T
pFFalse = freserved "false" >> return LBC.F
