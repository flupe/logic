{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Formula.Parser

main :: IO ()
main = putStrLn $ show (parse formulaParser "∀ a b. ∃ c. ⊤")
