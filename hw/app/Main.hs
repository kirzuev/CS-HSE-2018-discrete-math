{-# LANGUAGE OverloadedStrings #-}
module Main where

import Formula
import Parser
import CNF

import Data.Text (pack)
import Data.Attoparsec.Text

errorMsg :: String
errorMsg = "*** Parsing error, wrong expression ***"

main :: IO ()
main = do
  str     <- pack <$> getLine
  formula <- case parseOnly parseExpr str of
    Left _  -> putStrLn errorMsg >> return (Constant False)
    Right f -> return f
  cnf <- return $ toCNF formula
  putStrLn $ showCNF cnf
