{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Formula.Utils where

-- import Formula
import Formula.Parser
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Prelude hiding ((<), (>))
import Debug.Trace (trace)


expConfig :: ParserConfig Exp
expConfig = ParserConfig
    { fTrue = ConE 'FTrue
    , fFalse = ConE 'FFalse
    , fVar = \n -> AppE (ConE 'Var) (LitE (StringL n))
    , fAnd = \a b -> AppE (AppE (ConE 'And) a) b
    , fOr = \a b -> AppE (AppE (ConE 'Or) a) b
    , fEq = \a b -> AppE (AppE (ConE 'Eq) a) b
    , fImplies = \a b -> AppE (AppE (ConE 'Eq) a) b
    , fNot = \a -> AppE (ConE 'Not) a
    , fForall = \v f -> AppE (AppE (ConE 'Forall) (ListE $ map (LitE . StringL) v)) f
    , fExists = \v f -> AppE (AppE (ConE 'Forall) (ListE $ map (LitE . StringL) v)) f
    }

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
    , fForall = \v f -> ConP 'Forall [ListP $ map (LitP . StringL) v, f]
    , fExists = \v f -> ConP 'Exists [ListP $ map (LitP . StringL) v, f]
    }

f :: QuasiQuoter
f = QuasiQuoter
    { quoteExp = quote expConfig
    , quotePat = quote patConfig
    }

quote :: ParserConfig a -> String -> Q a
quote pc input =
    case parseFormula' pc input of
        Just exp -> return exp
        Nothing  -> fail ("error while parsing input \"" ++ input ++ "\"")
