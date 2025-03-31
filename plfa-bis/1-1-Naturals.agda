module 1-1-Naturals where

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

-- Exercise (practice)
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

-- Exercise (practice)
_ : 3 + 4 ≡ 7
_ = begin 3 + 4 ≡⟨⟩ suc (2 + 4)
                ≡⟨⟩ suc (suc (1 + 4))
                ≡⟨⟩ suc (suc (suc (0 + 4)))
                ≡⟨⟩ suc (suc (suc 4))
                ≡⟨⟩ 7 ∎

_*_ : ℕ → ℕ → ℕ
zero * n = zero
(suc m) * n = n + (m * n)

_ : 2 * 3 ≡ 6
_ = begin 2 * 3 ≡⟨⟩ 3 + (1 * 3)
                ≡⟨⟩ 3 + (3 + (0 * 3))
                ≡⟨⟩ 3 + (3 + 0)
                ≡⟨⟩ 6 ∎

-- Exercise (practice)
_ : 3 * 4 ≡ 12
_ = begin 3 * 4 ≡⟨⟩ 4 + (2 * 4)
                ≡⟨⟩ 4 + (4 + (1 * 4))
                ≡⟨⟩ 4 + (4 + (4 + (0 * 4)))
                ≡⟨⟩ 4 + (4 + (4 + 0))
                ≡⟨⟩ 12 ∎

-- Exercise (recommended)
_^_ : ℕ → ℕ → ℕ
m ^ 0 = 1
m ^ (suc n) = m * (m ^ n)

_ : 3 ^ 4 ≡ 81
_ = begin 3 ^ 4 ≡⟨⟩ 3 * (3 ^ 3)
                ≡⟨⟩ 3 * (3 * (3 ^ 2))
                ≡⟨⟩ 3 * (3 * (3 * (3 ^ 1)))
                ≡⟨⟩ 3 * (3 * (3 * (3 * (3 ^ 0))))
                ≡⟨⟩ 3 * (3 * (3 * (3 * 1))) ∎

_∸_ : ℕ → ℕ → ℕ
zero ∸ n = zero
(suc n) ∸ zero = (suc n)
(suc m) ∸ (suc n) = m ∸ n

-- Exercise (recommended)
_ : 5 ∸ 3 ≡ 2
_ = begin 5 ∸ 3 ≡⟨⟩ 4 ∸ 2
                ≡⟨⟩ 3 ∸ 1
                ≡⟨⟩ 2 ∸ 0
                ≡⟨⟩ 2 ∎
_ : 3 ∸ 5 ≡ 0
_ = begin 3 ∸ 5 ≡⟨⟩ 2 ∸ 4
                ≡⟨⟩ 1 ∸ 3
                ≡⟨⟩ 0 ∸ 2
                ≡⟨⟩ 0 ∎
