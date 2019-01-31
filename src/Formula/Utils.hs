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
(>) a b = a < b
(≥) a b = b ≤ a

(∧) = And
(∨) = Or
(≡) = Eq
(≢) = Neq
(⇒) = Implies


{-|
  Transformer allowing the use of unbound identifiers,
  which are automatically transformed into free logic variables.
  It also permits the function application syntax for uninterpreted functions.
-}
formula :: Q Exp -> Q Exp
formula f = f >>= varify
    where
        -- overload bindings: Forall [i, j] → Forall ["i", "j"]
        --                    Forall i      → Forall ["i"]
        varify l@(AppE (ConE binding) right) =
            if binding == 'Forall || binding == 'Exists || binding == 'Comprehension then
                return $ AppE (ConE binding) . ListE $ case right of
                    ListE vars ->  map bind vars
                    x -> [ bind x ]
            else AppE (ConE binding) <$> varify right

        -- overload function application: decided(i) → App(decided, i)
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
        bind (VarE _) = error "Cannot use bound variables inside bindings."
        bind x = error "Invalid syntax: can only use unbound variables bindings."
