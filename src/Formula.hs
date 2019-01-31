{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}


module Formula where

import Data.List (intercalate)

data Set a

data Term a where
    BLit :: Bool -> Term Bool
    ILit :: Int -> Term Int
    Var :: String -> Term a

    UninterpretedFunc :: String -> Term (a -> b)
    App :: Term (a -> b) -> Term a -> Term b

    Not     :: Term Bool -> Term Bool
    And     :: Term Bool -> Term Bool -> Term Bool
    Or      :: Term Bool -> Term Bool -> Term Bool
    Implies :: Term Bool -> Term Bool -> Term Bool
    Eq      :: Term a    -> Term a    -> Term Bool
    Neq     :: Term a    -> Term a    -> Term Bool
    Leq     :: Term Int  -> Term Int  -> Term Bool
    Lt      :: Term Int  -> Term Int  -> Term Bool
    Plus    :: Term Int  -> Term Int  -> Term Int
    Minus   :: Term Int  -> Term Int  -> Term Int
    Mult    :: Term Int  -> Term Int  -> Term Int
    Div     :: Term Int  -> Term Int  -> Term Int

    Forall :: [String] -> Term Bool -> Term Bool
    Exists :: [String] -> Term Bool -> Term Bool

    -- how do we specify the type of the comprehension???
    -- depending on whether we bind on several variables, could be a product
    Comprehension :: [String] -> Term Bool -> Term (Set a)

    In :: Term a -> Term (Set a) -> Term Bool
    Card :: Term (Set a) -> Term Int
    Union :: Term (Set a) -> Term (Set a) -> Term (Set a)
    Intersection :: Term (Set a) -> Term (Set a) -> Term (Set a)
    Subset :: Term (Set a) -> Term (Set a) -> Term Bool


type Formula = Term Bool


instance Show (Term a) where
    show (BLit x) = show x
    show (ILit x) = show x
    show (Var x) = x
    show (UninterpretedFunc x) = x
    show (App f x) = show f ++ "(" ++ show x ++ ")"
    show (Not x) = "¬" ++ show x
    show (And a b) = "(" ++ show a ++ " ∧ " ++ show b ++ ")"
    show (Or a b)  = "(" ++ show a ++ " ∨ " ++ show b ++ ")"
    show (Implies a b) = show a ++ " ⇒ " ++ show b
    show (Eq a b)  = show a ++ " = " ++ show b
    show (Neq a b) = show a ++ " ≠ " ++ show b
    show (Leq a b) = show a ++ " ≤ " ++ show b
    show (Lt a b)  = show a ++ " < " ++ show b
    show (Plus a b)  = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Minus a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b)  = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Div a b)   = "(" ++ show a ++ " / " ++ show b ++ ")"
    show (Forall v t)   = "∀" ++ intercalate " " v ++ ".(" ++ show t ++ ")"
    show (Exists v t)   = "∃" ++ intercalate " " v ++ ".(" ++ show t ++ ")"
    show (Comprehension v t)   = "{" ++ intercalate " " v ++ " | " ++ show t ++ "}"
    show (In x t)   = show x ++ " in " ++ show t
    show (Card t)   = "|" ++ show t ++ "|"
    show (Union a b)   = show a ++ " u " ++ show b
    show (Intersection a b)   = show a ++ " n " ++ show b
    show (Subset a b)   = show a ++ " c " ++ show b


instance Num (Term Int) where
    ILit a + ILit b = ILit (a + b)
    a + b = Plus a b
    ILit a - ILit b = ILit (a - b)
    a - b = Minus a b
    ILit a * ILit b = ILit (a * b)
    a * b = Mult a b
    fromInteger = ILit . fromInteger


normalize :: Formula -> Formula
normalize (Implies a b) = Or (Not a) b
normalize (Neq a b)     = Not (Eq a b)
normalize x = x


simplifyBool :: Formula -> Formula
simplifyBool (Eq (BLit x) other) = if x then other else Not other
simplifyBool (Eq other (BLit x)) = if x then other else Not other
simplifyBool (And l@(BLit x) other) = if x then other else l
simplifyBool (And other r@(BLit x)) = if x then other else r
simplifyBool (Or l@(BLit x) other) = if x then l else other
simplifyBool (Or other r@(BLit x)) = if x then r else other
simplifyBool (Not (BLit x)) = BLit (not x)
simplifyBool x = x
