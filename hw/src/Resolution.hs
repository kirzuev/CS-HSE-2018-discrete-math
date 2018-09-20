module Resolution where

import Formula

import qualified Data.Set as S
import           Data.List (intersect, union, (\\), sortOn, nub)
import           Data.Monoid

type Disjunct = [(String, Bool)]

isSatisfiable :: [Formula] -> Bool
isSatisfiable []             = False
isSatisfiable [Constant val] = val
isSatisfiable disjunctions   = all (/= []) $
  checkStatisfiable $ toLiteralList <$> disjunctions

checkStatisfiable :: [Disjunct] -> [Disjunct]
checkStatisfiable []           = []
checkStatisfiable disjunctions = if newSet == oldSet || [] `S.member` newSet
  then S.toList newSet
  else checkStatisfiable $ S.toList newSet
  where
    oldSet = S.fromList disjunctions
    newSet = S.fromList $ mkResolutions disjunctions

mkResolutions :: [Disjunct] -> [Disjunct]
mkResolutions []     = []
mkResolutions (x:xs) = (x : concat (mkResolutions2 x <$> xs))
  ++ mkResolutions xs

mkResolutions2 :: Disjunct -> Disjunct -> [Disjunct]
mkResolutions2 d1 d2 = mkResolution d1 d2 <$> (commonNames \\ commonVars)
  where
    commonNames = (fst <$> d1) `intersect` (fst <$> d2)
    commonVars  = fst <$> (d1 `intersect` d2)

mkResolution :: Disjunct -> Disjunct -> String -> Disjunct
mkResolution d1 d2 var = nub $ truePairs $ sortOn fst $
  filter (\(x,_) -> x /= var) (d1 `union` d2)

truePairs :: Disjunct -> Disjunct
truePairs []  = []
truePairs [x] = [x]
truePairs (x:y:xs)
  | fst x == fst y && snd x /= snd y = ("", True) : truePairs xs
  | otherwise                        = x : truePairs (y:xs)

toLiteralList :: Formula -> [(String, Bool)]
toLiteralList formula = case formula of
  Or e1 e2         -> toLiteralList e1 <> toLiteralList e2
  Not (Variable x) -> [(x, False)]
  Variable x       -> [(x, True)]