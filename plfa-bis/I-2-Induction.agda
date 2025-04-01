module I-2-Induction where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; step-≡-∣; step-≡-⟩; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_; _^_)

-- Exercise (practice)
{- Give another example of a pair of operators that have an identity
   and are associative, commutative, and distribute over one another.
   -> (∩, ∅) & (∪, Ω) defined on subsets of Ω.
-}

{- Give an example of an operator that has an identity and is
   associative but is not commutative.
   -> String concatenation & the empty string.
-}
