{-# LANGUAGE TemplateHaskell #-}

module Main where

import Formula
import Formula.Utils
import Prelude hiding ((<), (>), (/))

type Pid = Int

ite :: Formula -> Formula -> Formula -> Formula
ite a b c = (Not a ∨ b) ∧ (a ∨ c)

ho = UninterpretedFunc "HO" :: Term (Pid -> Set Pid)

decided  = UninterpretedFunc "decided"  :: Term (Pid -> Bool)
decided' = UninterpretedFunc "decided'" :: Term (Pid -> Bool)

dat     = UninterpretedFunc "data"    :: Term (Pid -> Int)
dat0    = UninterpretedFunc "data0"   :: Term (Pid -> Int)
dat'    = UninterpretedFunc "data'"   :: Term (Pid -> Int)

agreement   = $(formula [| Forall [i, j] (decided(i) ∧ decided(j) ⇒ dat(i) ≡ dat(j)) |])
integrity   = $(formula [| Forall i (decided(i) ⇒ decided'(i) ∧ dat(i) ≡ dat'(i)) |])
termination = $(formula [| Forall i (decided(i)) |])
validity    = $(formula [| Forall i (Exists j (dat(i) ≡ dat0(j))) |])

invariantAgreement = $(formula [| Forall i (Not (decided(i))) ∨
                                  Exists v (Forall i (decided(i) ⇒ dat(i) ≡ v)) |])

topology = $(formula [| Forall i (Card (ho(i)) > n / 2) |])

main :: IO ()
main = putStrLn $ show topology
