{-# LANGUAGE TemplateHaskellQuotes #-}

module Formula.Utils (f) where

import Formula
import Formula.Parser
import Language.Haskell.TH
import Language.Haskell.TH.Quote


{-|
Transform a @Formula@ into and @Exp@, while making sure that
variables whose name is in the given list stay symbolic and do not capture variables.
-}
toExp :: [(String, Type)] -> UFormula -> Exp
toExp vars f = case f of
    UTrue  -> ConE 'TTrue
    UFalse -> ConE 'TFalse

    UILit x -> AppE (ConE 'ILit)
                   (LitE (IntegerL $ toInteger x))

    UVar n -> case lookup n vars of
        Just tp -> SigE (AppE (ConE 'Var) (LitE (StringL n)))
                       (AppT (ConT ''Formula) tp)
        Nothing -> VarE (mkName n)

    UAnd a b -> binop 'And a b
    UOr a b  -> binop 'Or  a b
    UEq a b  -> binop 'Eq  a b

    UNot f -> AppE (ConE 'Not) (toExp vars f)

    UForall v f -> quantifier 'Forall v f
    UExists v f -> quantifier 'Exists v f

    where var ('$':n) = VarE (mkName n)
          var n       = LitE (StringL n)

          binop con a b =
              AppE (AppE (ConE con) (toExp vars a))
                   (toExp vars b)

          quantifier con v f = 
              AppE (AppE (AppE (ConE con) (var v)) (ConE 'TBool))
                   (toExp ((v, (ConT ''Bool)):vars) f)



patConfig :: ParserConfig Pat
patConfig = ParserConfig
    { fTrue = ConP 'TTrue []
    , fFalse = ConP 'TFalse []
    , fILit = \x -> ConP 'ILit [LitP (IntegerL $ toInteger x)]
    , fVar = \n -> VarP (mkName n)
    , fAnd = \a b -> ConP 'And [a, b]
    , fOr = \a b -> ConP 'Or [a, b]
    , fEq = \a b -> ConP 'Eq [a, b]
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
