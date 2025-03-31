module 1-1-Naturals where

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

seven : ℕ
seven = suc (suc (suc (suc (suc (suc (suc zero))))))

{-# BUILTIN NATURAL ℕ #-}
-- Allows the use of 0, 1, 2 ...

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; step-≡-∣; _∎)

_+_ : ℕ → ℕ → ℕ
zero + n = n
(suc m) + n = suc (m + n)

-- Explicit proof
_ : 2 + 3 ≡ 5
_ =
  begin
    2 + 3
    ≡⟨⟩ (suc (suc zero)) + (suc (suc (suc zero)))
    ≡⟨⟩ suc ((suc zero) + (suc (suc (suc zero))))
    ≡⟨⟩ suc (suc (zero + (suc (suc (suc zero)))))
    ≡⟨⟩ suc (suc (suc (suc (suc zero))))
    ≡⟨⟩ 5
  ∎

-- Proof using the standard library shorthands
_ : 2 + 3 ≡ 5
_ =
  begin
    2 + 3
    ≡⟨⟩ suc (1 + 3)
    ≡⟨⟩ suc (suc (0 + 3))
    ≡⟨⟩ suc (suc 3)
    ≡⟨⟩ 5
  ∎

-- Agda knows.
_ : 2 + 3 ≡ 5
_ = refl

-- Exercise
_ : 3 + 4 ≡ 7
_ = begin 3 + 4 ≡⟨⟩ suc (2 + 4)
                ≡⟨⟩ suc (suc (1 + 4))
                ≡⟨⟩ suc (suc (suc (0 + 4)))
                ≡⟨⟩ suc (suc (suc 4))
                ≡⟨⟩ 7 ∎
