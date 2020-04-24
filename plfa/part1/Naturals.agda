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
suc m + n = suc (m + n)

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

infixl 6 _+_ _∸_
infixl 7 _*_

{-# BUILTIN NATPLUS _+_ #-}
{-# BUILTIN NATTIMES _*_ #-}
{-# BUILTIN NATMINUS _∸_ #-}

-- Exercise Bin (stretch)
data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

inc : Bin → Bin
inc ⟨⟩ = ⟨⟩ I
inc (b O) = b I
inc (b I) = (inc b) O

_ : inc ⟨⟩ ≡ ⟨⟩ I
_ = refl

_ : inc (⟨⟩ O) ≡ ⟨⟩ I
_ = refl

_ : inc (⟨⟩ I) ≡ ⟨⟩ I O
_ = refl

_ : inc (⟨⟩ I O) ≡ ⟨⟩ I I
_ = refl

_ : inc (⟨⟩ I I) ≡ ⟨⟩ I O O
_ = refl

to_ : ℕ → Bin
to zero = ⟨⟩ O
to suc m = inc (to m)

_ : to 3 ≡ ⟨⟩ I I
_ = refl

from_ : Bin → ℕ
from ⟨⟩ = zero
from (⟨⟩ O) = zero
from (b I) = 2 * from b + 1
from (b O) = 2 * from b

_ : from (to 3) ≡ 3
_ = refl

-- import Data.Nat using (ℕ; zero; suc; _+_; _*_; _^_; _∸_)
