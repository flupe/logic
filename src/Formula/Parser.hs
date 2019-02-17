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
    | Forall String Formula
    | Exists String Formula
    deriving (Eq, Show)


parseFormula :: String -> Maybe Formula
parseFormula = parseFormula' defaultConfig

parseFormula' :: ParserConfig a -> String -> Maybe a
parseFormula' pc = fmap fst . parse (formulaParser pc)


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

-- | Succeeds if all input has been consumed.
eoi :: Parser ()
eoi = Parser \case
          [] -> Just ((), [])
          _  -> Nothing

-- | Skip zero or more space characters.
spaces :: Parser ()
spaces = skipMany space

-- | Match an occurence of @p@ surrounded by zero or more spaces.
trim :: Parser a -> Parser a
trim p = spaces *> p <* spaces

ident :: Parser String
ident = takeWhile1 isAlpha

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = (p <**> op <*> (chainr1 p op)) <|> p


data ParserConfig a = ParserConfig
    { fTrue    :: a
    , fFalse   :: a
    , fVar     :: String -> a
    , fAnd     :: a -> a -> a
    , fOr      :: a -> a -> a
    , fEq      :: a -> a -> a
    , fImplies :: a -> a -> a
    , fNot     :: a -> a
    , fForall  :: String -> a -> a
    , fExists  :: String -> a -> a
    }


defaultConfig :: ParserConfig Formula
defaultConfig = ParserConfig
    { fTrue    = FTrue
    , fFalse   = FFalse
    , fVar     = Var
    , fAnd     = And
    , fOr      = Or
    , fEq      = Eq
    , fImplies = Implies
    , fNot     = Not
    , fForall  = Forall
    , fExists  = Exists
    }


formulaParser :: ParserConfig a -> Parser a
formulaParser pc = trim formula <* eoi
    where
        formula = choice
            [ quantifier (fForall pc) ("∀" <|> ("forall" <* space))
            , quantifier (fExists pc) ("∃" <|> ("exists" <* space))
            , implFormula
            ]

        -- TODO(flupe): clean this
        quantifier c q = Parser \s -> do
            (_,    s) <- parse q s
            (vars, s) <- parse (trim (sepBy1 ident space)) s
            (f,    s) <- parse ("." *> spaces *> formula) s
            return (foldr c f vars, s)

        implFormula = chainr1 orFormula (fImplies pc <$ trim ("=>" <|> "==>" <|> "⇒"))
        orFormula   = chainr1 andFormula (fOr pc <$ trim "∨")
        andFormula  = chainr1 groundFormula (fAnd pc <$ trim "∧")

        groundFormula = choice
            [ "(" *> trim formula <* ")"
            , fTrue pc  <$ ("⊤" <|> "true")
            , fFalse pc <$ ("⊥" <|> "false")
            , fVar pc   <$> ident
            , fNot pc   <$ ("¬" <|> "not") <* spaces <*> groundFormula
            ]
