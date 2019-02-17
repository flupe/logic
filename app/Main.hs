{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Formula.Parser
import Formula.Utils

main :: IO ()
main =
    case [f| ∀ a b. a ∧ b ∨ c ⇒ b |] of
        [f| ∀ a b . y |] -> putStrLn "universal"
        [f| ∃ a . x |]   -> putStrLn "existential"
        [f| x |]         -> putStrLn "something else"
