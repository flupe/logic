{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Formula where

import Prelude hiding ((<), (>), (==))
import Language.Haskell.TH
import Language.Haskell.TH.Quote


type a ⇝ b = Term (a → b)

infix  5 ≤, ≥, <, >
infixl 3 ∧
infixl 2 ∨
infixr 1 ==>

(≤) = Leq
(<) = Lt
(>) a b = a < b
(≥) a b = b ≤ a
(∧) = And
(∨) = Or
(===) = Eq
(==>) = Implies


data Var a = Var String


data Term a where
    BLit ∷ Bool → Term Bool
    ILit ∷ Int → Term Int
    TVar  ∷ Var a → Term a

    UninterpretedFunc ∷ String → Term (a → b)
    App ∷ Term (a → b) → Term a → Term b

    Not ∷ Term Bool → Term Bool
    And ∷ Term Bool → Term Bool → Term Bool
    Or  ∷ Term Bool → Term Bool → Term Bool
    Implies ∷ Term Bool → Term Bool → Term Bool
    Eq  ∷ Term a → Term a → Term Bool
    Neq ∷ Term a → Term a → Term Bool
    Leq ∷ Term Int → Term Int → Term Bool
    Lt  ∷ Term Int → Term Int → Term Bool

    Plus  ∷ Term Int → Term Int → Term Int
    Minus ∷ Term Int → Term Int → Term Int
    Mult  ∷ Term Int → Term Int → Term Int
    Div   ∷ Term Int → Term Int → Term Int


instance Show (Term a) where
    show (BLit x) = show x
    show (ILit x) = show x
    show (TVar (Var x)) = x

    show (UninterpretedFunc x) = x
    show (App f x) = show f ++ "(" ++ show x ++ ")"

    show (Not x) = "¬" ++ show x
    show (And a b) = "(" ++ show a ++ " ∧ " ++ show b ++ ")"
    show (Or a b) = "(" ++ show a ++ " ∨ " ++ show b ++ ")"
    show (Implies a b) = show a ++ " ⇒ " ++ show b
    show (Eq a b) = show a ++ " = " ++ show b
    show (Neq a b) = show a ++ " ≠ " ++ show b
    show (Leq a b) = show a ++ " ≤ " ++ show b
    show (Lt a b) = show a ++ " < " ++ show b
    show (Plus a b)  = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Minus a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b)  = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Div a b)   = "(" ++ show a ++ " / " ++ show b ++ ")"


instance Num (Term Int) where
    ILit a + ILit b = ILit (a + b)
    a + b = Plus a b
    ILit a - ILit b = ILit (a - b)
    a - b = Minus a b
    ILit a * ILit b = ILit (a * b)
    a * b = Mult a b
    fromInteger = ILit . fromInteger


{-|
  Transformer allowing the use of unbound identifiers,
  which are automatically transformed into free logic variables.
  It also permits the function application syntax for uninterpreted functions.
-}
formula ∷ Q Exp → Q Exp
formula f = f >>= varify
    where
        varify ∷ Exp → Q Exp
        varify (AppE (VarE name) right) = AppE (AppE (ConE 'App) (VarE name)) <$> varify right
        varify (UnboundVarE n) = return $ AppE (ConE 'Var) (LitE (StringL (show n)))
        varify (AppE a b) = AppE <$> varify a <*> varify b
        varify (InfixE a b c) = InfixE <$> mapM varify a <*> varify b <*> mapM varify c
        varify (ParensE e) = ParensE <$> varify e
        varify x = return x
