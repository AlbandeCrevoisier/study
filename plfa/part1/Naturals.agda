module plfa.part1.Naturals where

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

-- Exercise seven (practice)
-- 7 = suc (suc (suc (suc (suc (suc (suc (suc zero)))))))

{-# BUILTIN NATURAL ℕ #-}

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

_+_ : ℕ → ℕ → ℕ
zero + n = n
(suc m) + n = suc (m + n)

-- Exercise +-example (practice)
_ : 3 + 4 ≡ 7
_ =
  begin
    3 + 4 ≡⟨⟩ 1 + (2 + 4)
    ≡⟨⟩ 1 + (1 + (1 + 4))
    ≡⟨⟩ 1 + (1 + (1 + (0 + 4)))
    ≡⟨⟩ 7
  ∎

_*_ : ℕ → ℕ → ℕ
zero * _ = zero
(suc m) * n = n + (m * n)

-- Exercise *-example (practice)
_ : 3 * 4 ≡ 12
_ =
  begin
    3 * 4 ≡⟨⟩ 4 + (2 * 4)
    ≡⟨⟩ 4 + (4 + (1 * 4))
    ≡⟨⟩ 4 + (4 + (4 + (0 * 4)))
    ≡⟨⟩ 12
  ∎

-- Exercise _^_ (recommended)
_^_ : ℕ → ℕ → ℕ
_ ^ 0 = 1
m ^ (suc n) = m * (m ^ n)

_ : 3 ^ 4 ≡ 81
_ = refl

_∸_ : ℕ → ℕ → ℕ
m ∸ zero = m
zero ∸ suc m = zero
suc m ∸ suc n = m ∸ n

-- Exercise ∸-example₁ (recommended)
_ : 5 ∸ 3 ≡ 2
_ =
  begin
    5 ∸ 3 ≡⟨⟩ 4 ∸ 2 ≡⟨⟩ 3 ∸ 1 ≡⟨⟩ 2 ∸ 0 ≡⟨⟩ 2
  ∎

-- Exercise ∸-example₂ (recommended)
_ : 3 ∸ 5 ≡ 0
_ = begin 3 ∸ 5 ≡⟨⟩ 2 ∸ 4 ≡⟨⟩ 1 ∸ 3 ≡⟨⟩ 0 ∸ 2 ≡⟨⟩ 0 ∎
