module I-3-Relations where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Nat.Properties using (+-comm; +-identityʳ; *-comm)


data _≤_ : ℕ → ℕ → Set where

  z≤n : ∀ {n : ℕ}
      --------
    → zero ≤ n

  s≤s : ∀ {m n : ℕ}
    → m ≤ n
      -------------
    → suc m ≤ suc n


{- z≤n -----
       0 ≤ 2
   s≤s -----
       1 ≤ 3
   s≤s -----
       2 ≤ 4
-}
_ : 2 ≤ 4
_ = s≤s (s≤s z≤n)

_ : 2 ≤ 4
_ = s≤s {1} {3} (s≤s {0} {2} (z≤n {2}))

_ : 2 ≤ 4
_ = s≤s {m = 1} {n = 3} (s≤s {m = 0} {n = 2} (z≤n {n = 2}))

_ : 2 ≤ 4
_ = s≤s {n = 3} (s≤s {n = 2} z≤n)

+-identityʳ′ : ∀ {m : ℕ} → m + zero ≡ m
+-identityʳ′ = +-identityʳ _

infix 4 _≤_

inv-s≤s : ∀ {m n : ℕ}
  → suc m ≤ suc n
    -------------
  → m ≤ n
inv-s≤s (s≤s m≤n) = m≤n

inv-z≤n : ∀ {m : ℕ}
  → m ≤ zero
    --------
  → m ≡ zero
inv-z≤n z≤n = refl

{- Exercise (practice)
   preorder & not partial: congruence modulo n, a ≡ b [n]
   partial order & not total: set inclusion ⊂
-}

≤-refl : ∀ {n : ℕ}
    -----
  → n ≤ n
≤-refl {zero}  = z≤n
≤-refl {suc n} = s≤s ≤-refl

≤-trans : ∀ {m n p : ℕ}
  → m ≤ n
  → n ≤ p
    -----
  → m ≤ p
≤-trans  z≤n       _        = z≤n
≤-trans (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans m≤n n≤p)

≤-antisym : ∀ {m n : ℕ}
  → m ≤ n
  → n ≤ m
    -----
  → m ≡ n
≤-antisym  z≤n       z≤n      = refl
≤-antisym (s≤s m≤n) (s≤s n≤m) = cong suc (≤-antisym m≤n n≤m)

-- Exercise (practice)
-- The cas ≤-antisym z≤n s≤s would mean that ∃ n : ℕ → suc n ≡ zero.


data Total (m n : ℕ) : Set where

  forward : m ≤ n
            ---------
          → Total m n

  flipped : n ≤ m
            ---------
          → Total m n


data Total′ : ℕ → ℕ → Set where

  forward′ : ∀ {m n : ℕ} → m ≤ n
                           ----------
                         → Total′ m n

  flipped′ : ∀ {m n : ℕ} → n ≤ m
                           ----------
                         → Total′ m n


≤-total : ∀ (m n : ℕ) → Total m n
≤-total  zero    n                       = forward z≤n
≤-total (suc m)  zero                    = flipped z≤n
≤-total (suc m) (suc n) with ≤-total m n
...                        | forward m≤n = forward (s≤s m≤n)
...                        | flipped n≤m = flipped (s≤s n≤m)

≤-total′ : ∀ (m n : ℕ) → Total m n
≤-total′  zero    n      = forward z≤n
≤-total′ (suc m)  zero   = flipped z≤n
≤-total′ (suc m) (suc n) = helper (≤-total′ m n)
  where
  helper : Total m n → Total (suc m) (suc n)
  helper (forward m≤n) = forward (s≤s m≤n)
  helper (flipped n≤m) = flipped (s≤s n≤m)

-- Return the flipped cas when m = n
≤-total″ : ∀ (m n : ℕ) → Total m n
≤-total″  m       zero                    = flipped z≤n
≤-total″  zero   (suc n)                  = forward z≤n
≤-total″ (suc m) (suc n) with ≤-total″ m n
...                         | forward m≤n = forward (s≤s m≤n)
...                         | flipped n≤m = flipped (s≤s n≤m)

-- Goal: m≤n & p≤q ⇒ m+n ≤ p+q, broken in three parts.
+-monoʳ-≤ : ∀ (m n p : ℕ)
  → m ≤ n
    -------------
  → p + m ≤ p + n
+-monoʳ-≤ m n  zero   m≤n = m≤n
+-monoʳ-≤ m n (suc p) m≤n = s≤s (+-monoʳ-≤ m n p m≤n)

+-monoˡ-≤ : ∀ (m n p : ℕ)
  → m ≤ n
    -------------
  → m + p ≤ n + p
+-monoˡ-≤ m n p m≤n rewrite +-comm m p | +-comm n p = +-monoʳ-≤ m n p m≤n

+-mono-≤ : ∀ (m n p q : ℕ)
  → m ≤ n
  → p ≤ q
    -------------
  → m + p ≤ n + q
+-mono-≤ m n p q m≤n p≤q = ≤-trans (+-monoˡ-≤ m n p m≤n) (+-monoʳ-≤ p q n p≤q)

-- Exercise (stretch)
*-mono-≤ : ∀ (m n p q : ℕ)
  → m ≤ n
  → p ≤ q
    -------------
  → m * p ≤ n * q
*-mono-≤  zero    n      p q  m≤n      p≤q = z≤n
*-mono-≤ (suc m) (suc n) p q (s≤s m≤n) p≤q =
  +-mono-≤ p q (m * p) (n * q) p≤q (*-mono-≤ m n p q m≤n p≤q)
