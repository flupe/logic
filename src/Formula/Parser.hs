{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Formula.Parser where

import Data.List (uncons, stripPrefix)
import Data.String (IsString(..))
import Data.Functor
import Data.Bifunctor (first)
import Control.Applicative
import Control.Monad


newtype Parser a = Parser { parse :: String -> Maybe (a, String) }


data Formula
    = Var String
    | And Formula Formula
    | Or  Formula Formula
    | Eq  Formula Formula
    | Implies Formula Formula
    | Not Formula
    | Forall [String] Formula
    | Exists [String] Formula
    deriving (Eq, Show)


parseFormula :: String -> Maybe Formula
parseFormula s = fst <$> parse formulaParser s


instance Functor Parser where
    fmap f p = Parser (fmap (first f) . parse p)


instance Applicative Parser where
    pure x = Parser (Just . (x,))

    pa <*> pb = Parser parse'
        where parse' s0 = do
                  (f, s1) <- parse pa s0
                  (x, s2) <- parse pb s1
                  return (f x, s2)


instance Alternative Parser where
    empty = Parser (const Nothing)

    pa <|> pb = Parser (\s -> parse pa s <|> parse pb s)


instance IsString (Parser String) where
    fromString = keyword


item :: Parser Char
item = Parser uncons


keyword :: String -> Parser String
keyword s = Parser (fmap (s,) . stripPrefix s)


char :: Char -> Parser Char
char c = Parser parse'
    where
        parse' (x:xs) | x == c = Just (c, xs)
        parse' _ = Nothing



formulaParser :: Parser Formula
formulaParser = undefined
