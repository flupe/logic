{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}


module Formula where

import Data.List (intercalate)

data Set a

data Typ a where
    TBool :: Typ Bool

data Formula a where
    TTrue :: Formula Bool
    TFalse :: Formula Bool

    ILit :: Int -> Formula Int

    Var :: String -> Formula a

    Not     :: Formula Bool -> Formula Bool
    Eq      :: Formula Bool -> Formula Bool -> Formula Bool
    And     :: Formula Bool -> Formula Bool -> Formula Bool
    Or      :: Formula Bool -> Formula Bool -> Formula Bool
    Implies :: Formula Bool -> Formula Bool -> Formula Bool

    Forall :: String -> Typ a -> Formula Bool -> Formula Bool
    Exists :: String -> Typ a -> Formula Bool -> Formula Bool


type Predicate = Formula Bool


instance Show (Formula a) where
    show TTrue = "⊤"
    show TFalse = "⊥"

    show (ILit x) = show x
    show (Var x) = x

    show (Not x) = "¬" ++ show x
    show (Eq a b) = show a ++ " = " ++ show b
    show (And a b) = "(" ++ show a ++ " ∧ " ++ show b ++ ")"
    show (Or a b)  = "(" ++ show a ++ " ∨ " ++ show b ++ ")"
    show (Implies a b) = show a ++ " ⇒ " ++ show b

    show (Forall v tp t)   = "∀" ++ v ++ ". (" ++ show t ++ ")"
    show (Exists v tp t)   = "∃" ++ v ++ ".(" ++ show t ++ ")"
