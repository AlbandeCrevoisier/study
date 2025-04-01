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

_ : (3 + 4) + 5 ≡ 3 + (4 + 5)
_ = begin (3 + 4) + 5 ≡⟨⟩ 7 + 5
                      ≡⟨⟩ 12
                      ≡⟨⟩ 3 + 9
                      ≡⟨⟩ 3 + (4 + 5) ∎

+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc zero n p = begin (zero + n) + p ≡⟨⟩ n + p
                                        ≡⟨⟩ zero + (n + p) ∎
+-assoc (suc m) n p = begin
  (suc m + n) + p ≡⟨⟩ suc (m + n) + p
                  ≡⟨⟩ suc ((m + n) + p)
                  ≡⟨ cong suc (+-assoc m n p) ⟩ suc (m + (n + p))
                  ≡⟨⟩ suc m + (n + p) ∎

-- To prove commutativity, we first need two lemmas.
+-identityʳ : ∀ (m : ℕ) → m + zero ≡ m
+-identityʳ zero = begin zero + zero ≡⟨⟩ zero ∎
+-identityʳ (suc m) = begin suc m + zero ≡⟨⟩ suc (m + zero)
                                         ≡⟨ cong suc (+-identityʳ m) ⟩ suc m ∎

+-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc zero n = begin zero + suc n ≡⟨⟩ suc n
                                  ≡⟨⟩ suc (zero + n) ∎
+-suc (suc m) n = begin suc m + suc n ≡⟨⟩ suc (m + suc n)
                                      ≡⟨ cong suc (+-suc m n) ⟩ suc (suc (m + n))
                                      ≡⟨⟩ suc (suc m + n) ∎

+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero = begin m + zero ≡⟨ +-identityʳ m ⟩ m ≡⟨⟩ zero + m ∎
+-comm m (suc n) = begin m + suc n ≡⟨ +-suc m n ⟩ suc (m + n)
                                   ≡⟨ cong suc (+-comm m n) ⟩ suc (n + m)
                                   ≡⟨⟩ suc n + m ∎

-- Corollary
+-rearrange : ∀ (m n p q : ℕ) → (m + n) + (p + q) ≡ m + (n + p) + q
+-rearrange m n p q = begin
  (m + n) + (p + q) ≡⟨ sym (+-assoc (m + n) p q) ⟩ ((m + n) + p) + q
                    ≡⟨ cong (_+ q) (+-assoc m n p) ⟩ (m + (n + p)) + q ∎

{- Exercise (stretch)
   finite-+-assoc
   0:ℕ
   1:ℕ (0 + 0) + 0 = 0 + (0 + 0)
   2:ℕ (0 + 0) + 1 = 0 + (0 + 1)
       (0 + 1) + 0 = 0 + (1 + 0)
       (0 + 1) + 1 = 0 + (1 + 1)
       (1 + 0) + 0 = 1 + (0 + 0)
       (1 + 0) + 1 = 1 + (0 + 1)
       (1 + 1) + 0 = 1 + (1 + 0)
       (1 + 1) + 1 = 1 + (1 + 1)
  3:ℕ No.
-}

