module CNF where

import Formula

import Data.List (nub, foldl1)

showCNF :: [Formula] -> String
showCNF []     = ""
showCNF [f]    = show f
showCNF (f:fs) = show f ++ " /\\ " ++ showCNF fs

toCNF :: Formula -> [Formula]
toCNF formula = mkDisjunction <$> fst <$> table
  where
    table = filter (\(_,val) -> not val) $ mkTruthTable formula

mkDisjunction :: [(String, Bool)] -> Formula
mkDisjunction set = foldl1 Or $ varOnDisjuction <$> set

varOnDisjuction :: (String, Bool) -> Formula
varOnDisjuction (x, val) = if val
  then Not (Variable x)
  else Variable x

getSetOfVars :: Formula -> [String]
getSetOfVars = nub . getListOfVars

getListOfVars :: Formula -> [String]
getListOfVars formula = case formula of
  Variable s        -> [s]
  Constant _        -> []
  Not expr          -> getListOfVars expr
  And e1 e2         -> getListOfVars e1 ++ getListOfVars e2
  Or e1 e2          -> getListOfVars e1 ++ getListOfVars e2
  Implication e1 e2 -> getListOfVars e1 ++ getListOfVars e2
  Equivalence e1 e2 -> getListOfVars e1 ++ getListOfVars e2

mkTruthTable :: Formula -> [([(String, Bool)], Bool)]
mkTruthTable formula = evalFormula formula <$> tvars
  where
    tvars = mkTruthTableVars $ getSetOfVars formula

mkTruthTableVars :: [String] -> [[(String, Bool)]]
mkTruthTableVars [] = []
mkTruthTableVars [x] = [[(x, False)], [(x, True)]]
mkTruthTableVars (x:xs) = ((\l -> (x, False) : l) <$> table)
  ++ ((\l -> (x, True) : l) <$> table)
  where
    table = mkTruthTableVars xs

evalFormula :: Formula -> [(String, Bool)] -> ([(String, Bool)], Bool)
evalFormula formula tvars = case formula of
  Constant val      -> (tvars, val)
  Variable x        -> case lookup x tvars of
    Just val -> (tvars, val)
    Nothing  -> (tvars, False)
  Not expr          -> (tvars, not $ snd $ evalFormula expr tvars)
  And e1 e2         -> (tvars, snd (evalFormula e1 tvars)
    && snd (evalFormula e2 tvars))
  Or e1 e2          -> (tvars, snd (evalFormula e1 tvars)
    || snd (evalFormula e2 tvars))
  Implication e1 e2 -> (tvars, (not . snd) (evalFormula e1 tvars)
    || snd (evalFormula e2 tvars))
  Equivalence e1 e2 -> (tvars, snd (evalFormula e1 tvars)
    == snd (evalFormula e2 tvars))
