{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Solver where

import Control.Applicative
import Control.Monad (join)
import Data.Maybe
import Data.SBV
import Data.Typeable
import Data.Traversable (sequence)

import Formula


data Pack = forall a. Typeable a => Pack (SBV a) (Typ a)

convert :: Typeable a => [(String, Pack)] -> Formula a -> Symbolic (SBV a)
convert env TTrue = return sTrue
convert env TFalse = return sFalse
convert env (ILit x) = return $ fromInteger $ toInteger x
convert env (Not a) = sNot <$> convert env a
convert env (Eq a b) = (.==) <$> convert env a <*> convert env b
convert env (And a b) = (.&&) <$> convert env a <*> convert env b
convert env (Or a b) = (.||) <$> convert env a <*> convert env b
convert env (Var n) =
    case lookup n env of
        Just (Pack (v :: SBV b) _) -> case gcast v of
            Just x -> return x

convert env (Forall a (t :: Typ x) f) =
    forAll [a] (\(v :: SBV x) -> (convert ((a, Pack v t):env) f))

convert env (Exists a (t :: Typ x) f) =
    forSome [a] (\(v :: SBV x) -> (convert ((a, Pack v t):env) f))
