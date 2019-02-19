{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}


module Formula where

import Data.List (intercalate)
import Data.SBV (SymVal)
import Data.Typeable

data Typ a where
    TBool :: Typ Bool
    TInt  :: Typ Integer

data Formula a where
    TTrue :: Formula Bool
    TFalse :: Formula Bool

    ILit :: Int -> Formula Integer

    Var :: SymVal a => String -> Formula a

    Not     :: Formula Bool -> Formula Bool
    Eq      :: Typeable a => Formula a -> Formula a -> Formula Bool
    And     :: Formula Bool -> Formula Bool -> Formula Bool
    Or      :: Formula Bool -> Formula Bool -> Formula Bool

    Forall :: SymVal a => String -> Typ a -> Formula Bool -> Formula Bool
    Exists :: SymVal a => String -> Typ a -> Formula Bool -> Formula Bool


type Predicate = Formula Bool


instance Show (Formula a) where
    show TTrue = "⊤"
    show TFalse = "⊥"

    show (ILit x) = show x
    show (Var x) = x

    show (Eq a b) = show a ++ " = " ++ show b
    show (Not (Eq a b)) = show a ++ " ≠ " ++ show b
    show (Not v@(Var _)) = "¬" ++ show v
    show (Not v@(Not _)) = "¬" ++ show v
    show (Not x) = "¬(" ++ show x ++ ")"
    show (And a b) = "(" ++ show a ++ " ∧ " ++ show b ++ ")"
    show (Or (Not a) b) = "(" ++ show a ++ " ⇒ " ++ show b ++ ")"
    show (Or a b)  = "(" ++ show a ++ " ∨ " ++ show b ++ ")"

    show (Forall v tp t)   = "∀" ++ v ++ ". (" ++ show t ++ ")"
    show (Exists v tp t)   = "∃" ++ v ++ ".(" ++ show t ++ ")"
