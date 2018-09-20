module Formula where

data Formula = Variable String
             | Constant Bool
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Implication Formula Formula
             | Equivalence Formula Formula
             deriving (Eq, Ord)

instance Show Formula where
  show formula = case formula of
    Variable s      -> s
    Constant True   -> "1"
    Constant False  -> "0"
    Not a           -> "~" ++ show a
    And a b         -> "(" ++ show a ++ " /\\ " ++ show b ++ ")"
    Or a b          -> "(" ++ show a ++ " \\/ " ++ show b ++ ")"
    Implication a b -> "(" ++ show a ++ " -> " ++ show b ++ ")"
    Equivalence a b -> "(" ++ show a ++ " <-> " ++ show b ++ ")"
