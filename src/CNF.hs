module CNF where

import Formula

import           Data.List (nub, foldl1)
import           Data.Monoid
import qualified Data.Set as S

showCNF :: Formula -> String
showCNF (And e1 e2) = showDisjunction e1 ++ " /\\ " ++ showDisjunction e2
showCNF (Or e1 e2) = showCNF e1 ++ " \\/ " ++ showCNF e2
showCNF e          = show e

showDisjunction :: Formula -> String
showDisjunction formula = case formula of
  Or e1 e2 -> '(' : showCNF e1 ++ " \\/ " ++ showCNF e2 ++ ")"
  _        -> showCNF formula

toCNF :: [Formula] -> Formula
toCNF = foldl1 And

toDisjunctionsList :: Formula -> [Formula]
toDisjunctionsList formula
  | table == []    = [Constant $ snd $ evalFormula formula []]
  | cnfTable == [] = [Constant True]
  | otherwise      = disjuctions
  where
    table       = mkTruthTable formula
    cnfTable    = fst <$> filter (\(_,val) -> not val) table
    disjuctions = mkDisjunction <$> cnfTable

mkDisjunction :: [(String, Bool)] -> Formula
mkDisjunction set = foldl1 Or $ varOnDisjuction <$> set

varOnDisjuction :: (String, Bool) -> Formula
varOnDisjuction (x, val) = if val
  then Not (Variable x)
  else Variable x

getSetOfVars :: Formula -> S.Set String
getSetOfVars formula = case formula of
  Variable s        -> S.singleton s
  Constant _        -> S.empty
  Not expr          -> getSetOfVars expr
  And e1 e2         -> getSetOfVars e1 <> getSetOfVars e2
  Or e1 e2          -> getSetOfVars e1 <> getSetOfVars e2
  Implication e1 e2 -> getSetOfVars e1 <> getSetOfVars e2
  Equivalence e1 e2 -> getSetOfVars e1 <> getSetOfVars e2

mkTruthTable :: Formula -> [([(String, Bool)], Bool)]
mkTruthTable formula = evalFormula formula <$> tvars
  where
    tvars = mkTruthTableVars $ S.toList $ getSetOfVars formula

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
