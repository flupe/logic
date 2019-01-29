{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Formula

dat = UninterpretedFunc "data" ∷ Int ⇝ Int
dat' = UninterpretedFunc "data'" ∷ Int ⇝ Int

decided  = UninterpretedFunc "decided" ∷ Int ⇝ Bool
decided' = UninterpretedFunc "decided'" ∷ Int ⇝ Bool

agreement ∷ Term Int → Term Int → Term Bool
agreement i j =
    $(formula [| decided(i) ∧ decided(j) ==> dat(i) === dat(j) |])

integrity ∷ Term Int → Term Bool
integrity i =
    $(formula [| decided i ==> App decided' i ∧ App dat i === App dat' i |])

main :: IO ()
main = do
    putStrLn $ show $ agreement 2 3
    putStrLn $ show $ integrity 5
