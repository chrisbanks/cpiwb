module CpiTest where

import CpiLib
import CpiParser

import Text.ParserCombinators.Parsec
import IO

-- Parser Test harnesses:
testParse :: (Expr a) => Parser a -> String -> IO ()
testParse p input = case (parse p "" input) of
                      Left err -> do putStr "parse error at";
                                     print err
                      Right x -> print (pretty x)

testParse' :: (Expr a) => Parser a -> String -> IO ()
testParse' p input = case (parse p "" input) of
                      Left err -> do putStr "parse error at";
                                     print err
                      Right x -> print x

-- Test file input:

-- getRight (Right x) = x

-- testFile x = do file <- openFile x ReadMode;
--                 contents <- hGetContents file;
--                 result <- parse pDefinitionLines contents;
--                 result' <- getRight result;
--                 print result'
                