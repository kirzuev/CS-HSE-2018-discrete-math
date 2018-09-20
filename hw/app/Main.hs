{-# LANGUAGE OverloadedStrings #-}
module Main where

import Formula
import Parser
import CNF
import Resolution

import Data.Text (pack)
import Data.Attoparsec.Text

errorMsg :: String
errorMsg = "*** Parsing error, wrong expression ***"

main :: IO ()
main = do
  str          <- pack <$> getLine
  formula      <- case parseOnly parseExpr str of
    Left _  -> putStrLn errorMsg >> return (Constant False)
    Right f -> return f
  disjunctions <- return $ toDisjunctionsList formula
  cnf          <- return $ toCNF disjunctions
  putStrLn $ "CNF: " ++ showCNF cnf
  putStrLn $ "Satisfiable: " ++ if isSatisfiable disjunctions
    then "yes"
    else "no"
