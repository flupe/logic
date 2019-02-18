{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}


module Formula where

import Data.List (intercalate)

data Set a

data Typ a where
    TBool :: Typ Bool

data Term a where
    TTrue :: Term Bool
    TFalse :: Term Bool

    Var :: String -> Term a

    Not     :: Term Bool -> Term Bool
    Eq      :: Term Bool -> Term Bool -> Term Bool
    And     :: Term Bool -> Term Bool -> Term Bool
    Or      :: Term Bool -> Term Bool -> Term Bool
    Implies :: Term Bool -> Term Bool -> Term Bool

    Forall :: String -> Typ a -> Term Bool -> Term Bool
    Exists :: String -> Typ a -> Term Bool -> Term Bool


type Formula = Term Bool


instance Show (Term a) where
    show TTrue = "⊤"
    show TFalse = "⊥"

    show (Var x) = x

    show (Not x) = "¬" ++ show x
    show (Eq a b) = show a ++ " = " ++ show b
    show (And a b) = "(" ++ show a ++ " ∧ " ++ show b ++ ")"
    show (Or a b)  = "(" ++ show a ++ " ∨ " ++ show b ++ ")"
    show (Implies a b) = show a ++ " ⇒ " ++ show b

    show (Forall v tp t)   = "∀" ++ v ++ ". (" ++ show t ++ ")"
    show (Exists v tp t)   = "∃" ++ v ++ ".(" ++ show t ++ ")"
