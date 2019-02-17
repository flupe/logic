{-# LANGUAGE QuasiQuotes #-}

module Main where

import Formula.Parser
import Formula.Utils

simplify :: Formula -> Formula

simplify [f| ¬ ⊤ |]   = [f| ⊥ |]
simplify [f| ¬ ⊥ |]   = [f| ⊤ |]
simplify [f| ¬ ¬ x |] = [f| x |]

simplify [f| ¬ (a ∧ b) |] =
    let x = simplify [f| ¬ a |]
        y = simplify [f| ¬ b |]
    in [f| x ∨ y |]

simplify [f| ¬ (a ∨ b) |] =
    let x = simplify [f| ¬ a |]
        y = simplify [f| ¬ b |]
    in [f| x ∧ y |]

simplify [f| ¬ (∀ x . φ) |] =
    let ψ = simplify [f| ¬ φ |]
    in [f| ∃ x . ψ |]

simplify [f| ¬ (∃ x . φ) |] =
    let ψ = simplify [f| ¬ φ |]
    in [f| ∀ x . ψ |]

simplify x = x

main :: IO ()
main = do
    let c = [f| ¬ (∃ x . ∀ a b . a ∧ b ∨ ¬ x) |]
    print c
    print (simplify c)
