{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Formula.Utils where

import Formula
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Prelude hiding ((<), (>))
import Debug.Trace (trace)


infix  5 ≤, ≥, <, >
infixl 3 ∧
infixl 2 ∨
infixr 1 ⇒


(≤) = Leq
(<) = Lt
(>) a b = b < a
(≥) a b = b ≤ a
(/) = Div

(∧) = And
(∨) = Or
(≡) = Eq
(≢) = Neq
(⇒) = Implies


{-|
  Transformer allowing the use of unbound identifiers,
  which are automatically transformed into free logic variables.
  It also permits the function application syntax for uninterpreted functions.

  TODO: Add a context and use let bindings to ensure that typing is correct
        for bound variables (if we stick with GADTs)
-}
formula :: Q Exp -> Q Exp
formula f = f >>= varify
    where
        -- Overload binding constructors: Forall [i, j] → Forall ["i", "j"]
        --                                Forall i      → Forall ["i"]
        varify (AppE (ConE binding) right)
            | binding == 'Forall
            || binding == 'Exists
            || binding == 'Comprehension =
            return $ AppE (ConE binding) . ListE $
                case right of
                    ListE vars -> map bind vars
                    x -> [bind x]

        -- Overload function application: decided(i) → App(decided, i)
        varify (AppE (VarE name) right) =
            AppE (AppE (ConE 'App) (VarE name)) <$> varify right

        varify (UnboundVarE n) = return $
            (AppE (ConE 'Var)
                  (LitE (StringL (show n))))

        varify (AppE a b) = AppE <$> varify a <*> varify b
        varify (InfixE a b c) = InfixE <$> mapM varify a <*> varify b <*> mapM varify c
        varify (ParensE e) = ParensE <$> varify e
        varify x = return x


        bind (UnboundVarE x) = LitE (StringL $ show x)
        bind x@(LitE (StringL _)) = x
        bind (VarE _) = error "Cannot use bound variables in bindings."
        bind x = error "Can only use free variables in bindings."
