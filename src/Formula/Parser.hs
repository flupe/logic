{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Formula.Parser where

import Prelude hiding (takeWhile)
import Data.List (uncons, stripPrefix, span)
import Data.String (IsString(..))
import Data.Char (isSpace, isAlpha)
import Data.Functor
import Data.Bifunctor (first)
import Control.Applicative
import Control.Monad


newtype Parser a = Parser { parse :: String -> Maybe (a, String) }


data Formula
    = FTrue | FFalse
    | Var String
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
    pure x    = Parser (Just . (x,))
    pa <*> pb = Parser parse'
        where parse' s0 = do
                  (f, s1) <- parse pa s0
                  (x, s2) <- parse pb s1
                  return (f x, s2)


instance Alternative Parser where
    empty     = Parser (const Nothing)
    pa <|> pb = Parser (\s -> parse pa s <|> parse pb s)


instance (a ~ String) => IsString (Parser a) where
    fromString = string


-- | Apply the actions in order until one of them succeeds.
choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

-- | Match a given string.
string :: String -> Parser String
string s = Parser (fmap (s,) . stripPrefix s)

-- | Match a char using the given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser \case
                (x:xs) | f x -> Just (x, xs)
                _            -> Nothing

-- | Consume input as long as the given predicate returns @True@.
takeWhile :: (Char -> Bool) -> Parser String
takeWhile f = Parser (Just . span f)

-- | Consume input as long as the given predicate returns @True@.
-- Will fail if the predicate returns @False@ on first char.
takeWhile1 :: (Char -> Bool) -> Parser String
takeWhile1 f = (:) <$> satisfy f <*> takeWhile f

-- | Match a whitespace character.
space :: Parser Char
space = satisfy isSpace

-- | @sepBy1 p s@ applies zero or more occurrences of @p@, separated by @s@.
-- Returns a list of the values returned by @p@.
sepBy :: Parser a -> Parser s -> Parser [a]
sepBy p s = sepBy1 p s <|> pure []

-- | @sepBy1 p s@ applies one or more occurrences of @p@, separated by @s@.
-- Returns a list of the values returned by @p@.
sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p s = scan
    where scan = (:) <$> p <*> ((s *> scan) <|> pure [])

-- | Match a given character.
char :: Char -> Parser Char
char c = satisfy (c==)

-- | Match any character.
anyChar :: Parser Char
anyChar = Parser uncons

-- | Skip zero or more occurences of @p@.
skipMany :: Parser a -> Parser ()
skipMany p = (p *> skipMany p) <|> pure ()

-- | Skip one or more occurences of @p@.
skipMany1 :: Parser a -> Parser ()
skipMany1 p = p *> skipMany p

-- | Match if all input has been consumed.
eoi :: Parser ()
eoi = Parser \case
          [] -> Just ((), [])
          _  -> Nothing

spaces :: Parser ()
spaces = skipMany space

ident :: Parser String
ident = takeWhile1 isAlpha

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = (p <**> op <*> (chainr1 p op)) <|> p

groundFormula :: Parser Formula
groundFormula = choice
    [ "(" *> spaces *> formula <* spaces <* ")"
    , FTrue  <$ ("⊤" <|> "true")
    , FFalse <$ ("⊥" <|> "false")
    , Var    <$> ident
    ]

andFormula :: Parser Formula
andFormula = chainr1 groundFormula (And <$ spaces <* "∧" <* spaces)

orFormula :: Parser Formula
orFormula = chainr1 andFormula (Or <$ spaces <* "∨" <* spaces)

implFormula :: Parser Formula
implFormula = chainr1 orFormula (Implies <$ spaces <* ("=>" <|> "==>" <|> "⇒") <* spaces)

quantifier :: ([String] -> Formula -> Formula) -> Parser a -> Parser Formula
quantifier c p =
    c <$ p <* spaces <*> sepBy1 ident space <* spaces <* "." <* spaces <*> formula

formula :: Parser Formula
formula = choice
    [ quantifier Forall ("∀" <|> "forall")
    , quantifier Exists ("∃" <|> "exists")
    , implFormula
    ]

formulaParser :: Parser Formula
formulaParser = spaces *> formula <* spaces <* eoi
