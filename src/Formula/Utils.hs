{-# LANGUAGE TemplateHaskellQuotes #-}

module Formula.Utils (f) where

import Data.List (union)
import Formula
import Formula.Parser
import Language.Haskell.TH
import Language.Haskell.TH.Quote


{-|
Transform a @Formula@ into and @Exp@, while making sure that
variables whose name is in the given list stay symbolic and do not capture variables.

TODO: add variable syntax for quantifier variables names.
-}
toExp :: [(String, Type)] -> UFormula -> Exp
toExp vars f = case f of
    UTrue -> ConE 'TTrue
    UFalse -> ConE 'TFalse

    UVar n -> case lookup n vars of
        Just tp -> SigE (AppE (ConE 'Var) (LitE (StringL n))) (AppT (ConT ''Term) tp)
        Nothing -> VarE (mkName n)

    UAnd a b -> AppE (AppE (ConE 'And) (toExp vars a)) (toExp vars b)
    UOr a b -> AppE (AppE (ConE 'Or) (toExp vars a)) (toExp vars b)
    UEq a b -> AppE (AppE (ConE 'Eq) (toExp vars a)) (toExp vars b)

    UImplies a b -> AppE (AppE (ConE 'Implies) (toExp vars a)) (toExp vars b)
    UNot f -> AppE (ConE 'Not) (toExp vars f)

    UForall v f ->
        AppE (AppE (AppE (ConE 'Forall) (LitE (StringL v)))
                   (ConE 'TBool))
             (toExp ((v, ConT ''Bool):vars) f)

    UExists v f ->
        AppE (AppE (AppE (ConE 'Exists) (LitE (StringL v)))
                   (ConE 'TBool))
             (toExp ((v, ConT ''Bool):vars) f)


patConfig :: ParserConfig Pat
patConfig = ParserConfig
    { fTrue = ConP 'TTrue []
    , fFalse = ConP 'TFalse []
    , fVar = \n -> VarP (mkName n)
    , fAnd = \a b -> ConP 'And [a, b]
    , fOr = \a b -> ConP 'Or [a, b]
    , fEq = \a b -> ConP 'Eq [a, b]
    , fImplies = \a b -> ConP 'Eq [a, b]
    , fNot = \a -> ConP 'Not [a]
    , fForall = \v f -> ConP 'Forall [VarP (mkName v), WildP, f]
    , fExists = \v f -> ConP 'Exists [VarP (mkName v), WildP, f]
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
