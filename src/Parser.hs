{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Formula

import Control.Applicative
import Data.Attoparsec.Text

parseExpr :: Parser Formula
parseExpr = parseFormula <* endOfInput

parseFormula :: Parser Formula
parseFormula = parseEq <|> parseImpl <|> parseOr <|> parseAnd
  <|> parseNot <|> (char '(' *> parseFormula <* char ')')
  <|> parseConstant <|> parseVariable

parseEq :: Parser Formula
parseEq = do
  left  <- parseImpl <|> parseOr <|> parseAnd <|> parseNot
    <|> (char '(' *> parseFormula <* char ')')
    <|> parseConstant <|> parseVariable
  many space
  string "<->"
  many space
  right <- parseFormula
  return $ Equivalence left right

parseImpl :: Parser Formula
parseImpl = do
  left  <- parseOr <|> parseAnd <|> parseNot
    <|> (char '(' *> parseFormula <* char ')')
    <|> parseConstant <|> parseVariable
  many space
  string "->"
  many space
  right <- parseImpl <|> parseOr <|> parseAnd <|> parseNot
    <|> (char '(' *> parseFormula <* char ')')
    <|> parseConstant <|> parseVariable
  return $ Implication left right

parseOr :: Parser Formula
parseOr = do
  left  <- parseAnd <|> parseNot
    <|> (char '(' *> parseFormula <* char ')')
    <|> parseConstant <|> parseVariable
  many space
  string "\\/"
  many space
  right <- parseOr <|> parseAnd <|> parseNot
    <|> (char '(' *> parseFormula <* char ')')
    <|> parseConstant <|> parseVariable
  return $ Or left right

parseAnd :: Parser Formula
parseAnd = do
  left  <- parseNot
    <|> (char '(' *> parseFormula <* char ')')
    <|> parseConstant <|> parseVariable
  many space
  string "/\\"
  many space
  right <- parseAnd <|> parseNot
    <|> (char '(' *> parseFormula <* char ')')
    <|> parseConstant <|> parseVariable
  return $ And left right

parseNot :: Parser Formula
parseNot = do
  char '~'
  param <- (char '(' *> parseFormula <* char ')')
    <|> parseVariable <|> parseConstant
  return $ Not param

parseVariable :: Parser Formula
parseVariable = Variable <$> many1 letter

parseConstant :: Parser Formula
parseConstant = (char '1' >> (return $ Constant True))
  <|> (char '0' >> (return $ Constant False))
