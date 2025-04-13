module I-1-Appendix-Bins where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; step-≡-∣; step-≡-⟩; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_; _^_)

-- Exercise (stretch)
data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

inc : Bin → Bin
inc ⟨⟩    = ⟨⟩ I
inc (b O) = b I
inc (b I) = (inc b) O

_ : inc (⟨⟩ O O O O) ≡ ⟨⟩ O O O I
_ = refl

_ : inc (⟨⟩ O O O I) ≡ ⟨⟩ O O I O
_ = refl

_ : inc (⟨⟩ O O I O) ≡ ⟨⟩ O O I I
_ = refl

_ : inc (⟨⟩ O O I I) ≡ ⟨⟩ O I O O
_ = refl

_ : inc (⟨⟩ I O I I) ≡ ⟨⟩ I I O O
_ = refl

to : ℕ → Bin
to  zero   = ⟨⟩ O
to (suc n) = inc (to n)

from : Bin → ℕ
from  ⟨⟩   = zero
from (b O) = 2 * (from b)
from (b I) = 1 + 2 * (from b)

_ : 0 ≡ from (⟨⟩ O O O O)
_ = refl

_ : 1 ≡ from (⟨⟩ O O O I)
_ = refl

_ : 2 ≡ from (⟨⟩ O O I O )
_ = refl

_ : 3 ≡ from (⟨⟩ O O I I)
_ = refl

_ : 4 ≡ from (⟨⟩ O I O O)
_ = refl
