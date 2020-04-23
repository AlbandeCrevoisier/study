module plfa.part1.naturals where

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

-- Exercise seven
-- 7 = suc (suc (suc (suc (suc (suc (suc (suc zero)))))))

{-# BUILTIN NATURAL ℕ #-}

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

_+_ : ℕ → ℕ → ℕ
zero + n = n
(suc m) + n = suc (m + n)

_ : 2 + 3 ≡ 5
_ =
  begin
    2 + 3
  ≡⟨⟩    -- is shorthand for
    (suc (suc zero)) + (suc (suc (suc zero)))
  ≡⟨⟩    -- inductive case
    suc ((suc zero) + (suc (suc (suc zero))))
  ≡⟨⟩    -- inductive case
    suc (suc (zero + (suc (suc (suc zero)))))
  ≡⟨⟩    -- base case
    suc (suc (suc (suc (suc zero))))
  ≡⟨⟩    -- is longhand for
    5
  ∎

_ : 2 + 3 ≡ 5
_ =
  begin
    2 + 3
  ≡⟨⟩
    suc (1 + 3)
  ≡⟨⟩
    suc (suc (0 + 3))
  ≡⟨⟩
    suc (suc 3)
  ≡⟨⟩
    5
  ∎

_ : 2 + 3 ≡ 5
_ = refl

-- Exercise +-example
_ : 3 + 4 ≡ 7
_ =
  begin
    3 + 4 ≡⟨⟩ 1 + (2 + 4)
    ≡⟨⟩ 1 + (1 + (1 + 4))
    ≡⟨⟩ 1 + (1 + (1 + (0 + 4)))
    ≡⟨⟩ 7
  ∎
