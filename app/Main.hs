{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Formula
import Formula.Parser
import Formula.Utils
import Solver
import Data.SBV

simplify :: Formula a -> Formula a

simplify [f| ¬ ⊤ |]   = [f| ⊥ |]
simplify [f| ¬ ⊥ |]   = [f| ⊤ |]
simplify [f| ¬ ¬ x |] = simplify x

simplify [f| ¬ (a ∧ b) |] =
    let x = simplify (Not a)
        y = simplify (Not b)
    in [f| x ∨ y |]

simplify [f| ¬ (a ∨ b) |] =
    let x = simplify (Not a)
        y = simplify (Not b)
    in [f| x ∧ y |]

simplify [f| ¬ (∀ x . φ) |] =
    let ψ = simplify (Not φ)
    in [f| ∃ $x . ψ |]

simplify [f| ¬ (∃ x . φ) |] =
    let ψ = simplify (Not φ)
    in [f| ∀ $x . ψ |]

simplify x = x

main :: IO ()
main = do
    let c = [f| ∀ x. ∀ y. x = y |]
    print c
    let pred = convert [] c
    prove pred >>= print

    let c = [f| ∀ x. ∃ y. x = y |]
    print c
    let pred = convert [] c
    prove pred >>= print
