module plfa.part1.Bin where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_)

data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

inc : Bin → Bin
inc ⟨⟩ = ⟨⟩ I
inc (b O) = b I
inc (b I) = (inc b) O

to_ : ℕ → Bin
to zero = ⟨⟩ O
to suc m = inc (to m)

from_ : Bin → ℕ
from ⟨⟩ = zero
from (b I) = 2 * from b + 1
from (b O) = 2 * from b
