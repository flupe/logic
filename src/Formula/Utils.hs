{-# LANGUAGE TemplateHaskellQuotes #-}

module Formula.Utils (f) where

import Data.List (union)
import Formula.Parser
import Language.Haskell.TH
import Language.Haskell.TH.Quote


{-|
Transform a @Formula@ into and @Exp@, while making sure that
variables whose name is in the given list stay symbolic and do not capture variables.

TODO: add variable syntax for quantifier variables names.
-}
toExp :: [String] -> Formula -> Exp
toExp vars f = case f of
    FTrue -> ConE 'FTrue
    FFalse -> ConE 'FFalse
    Var n | n `elem` vars -> AppE (ConE 'Var) (LitE (StringL n))
    Var n -> VarE (mkName n)
    And a b -> AppE (AppE (ConE 'And) (toExp vars a)) (toExp vars b)
    Or a b -> AppE (AppE (ConE 'Or) (toExp vars a)) (toExp vars b)
    Eq a b -> AppE (AppE (ConE 'Eq) (toExp vars a)) (toExp vars b)
    Implies a b -> AppE (AppE (ConE 'Implies) (toExp vars a)) (toExp vars b)
    Not f -> AppE (ConE 'Not) (toExp vars f)
    Forall v f ->
        AppE (AppE (ConE 'Forall) (LitE (StringL v))) (toExp (v:vars) f)
    Exists v f ->
        AppE (AppE (ConE 'Exists) (LitE (StringL v))) (toExp (v:vars) f)


patConfig :: ParserConfig Pat
patConfig = ParserConfig
    { fTrue = ConP 'FTrue []
    , fFalse = ConP 'FFalse []
    , fVar = \n -> VarP (mkName n)
    , fAnd = \a b -> ConP 'And [a, b]
    , fOr = \a b -> ConP 'Or [a, b]
    , fEq = \a b -> ConP 'Eq [a, b]
    , fImplies = \a b -> ConP 'Eq [a, b]
    , fNot = \a -> ConP 'Not [a]
    , fForall = \v f -> ConP 'Forall [VarP (mkName v), f]
    , fExists = \v f -> ConP 'Exists [VarP (mkName v), f]
    }

quote :: ParserConfig a -> String -> Q a
quote pc input =
    case parseFormula' pc input of
        Just exp -> return exp
        Nothing  -> fail ("error while parsing input \"" ++ input ++ "\"")

{-|
A @QuasiQuoter@ for logic formulas.

Can be used for expressions and patterns.

> let f = [f| ∀ a b. a ∧ b ⇒ b |]
-}

f :: QuasiQuoter
f = QuasiQuoter
    { quoteExp = fmap (toExp []) . quote defaultConfig
    , quotePat = quote patConfig
    }
