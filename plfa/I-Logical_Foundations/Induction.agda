module plfa.I-Logical_Foundations.Induction where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym; _≢_)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_; _^_)

open import plfa.I-Logical_Foundations.Bin using (Bin; ⟨⟩; _O; _I; inc; to; from)


-- Exercise operators (practice)
-- identity, associative, commutative, distribute: (max, max).
-- identity, associative, not commutative: ^.

+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc zero n p =
  begin
    (zero + n) + p
  ≡⟨⟩
    n + p
  ≡⟨⟩
    zero + (n + p)
  ∎
+-assoc (suc m) n p =
  begin
    (suc m + n) + p
  ≡⟨⟩
    suc (m + n) + p
  ≡⟨⟩
    suc ((m + n) + p)
  ≡⟨ cong suc (+-assoc m n p) ⟩
    suc (m + (n + p))
  ≡⟨⟩
    suc m + (n + p)
  ∎

+-identityʳ : ∀ (m : ℕ) → m + zero ≡ m
+-identityʳ zero =
  begin
      zero + zero
    ≡⟨⟩
      zero
    ∎
+-identityʳ (suc m) =
  begin
    suc m + zero
  ≡⟨⟩
    suc (m + zero)
  ≡⟨ cong suc (+-identityʳ m) ⟩
    suc m
  ∎

+-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc zero n =
  begin
    zero + suc n
  ≡⟨⟩
    suc n
  ≡⟨⟩
    suc (zero + n)
  ∎
+-suc (suc m) n =
  begin
    suc m + suc n
  ≡⟨⟩
    suc (m + suc n)
  ≡⟨ cong suc (+-suc m n) ⟩
    suc (suc (m + n))
  ≡⟨⟩
    suc (suc m + n)
   ∎

+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero =
  begin
    m + zero
  ≡⟨ +-identityʳ m ⟩
    m
  ≡⟨⟩
    zero + m
  ∎
+-comm m (suc n) =
  begin
    m + suc n
  ≡⟨ +-suc m n ⟩
    suc (m + n)
  ≡⟨ cong suc (+-comm m n) ⟩
    suc (n + m)
  ≡⟨⟩
    suc n + m
  ∎

+-rearrange : ∀ (m n p q : ℕ) → (m + n) + (p + q) ≡ m + (n + p) + q
+-rearrange m n p q =
  begin
    (m + n) + (p + q)
   ≡⟨ +-assoc m n (p + q) ⟩
     m + (n + (p + q))
   ≡⟨ cong (m +_) (sym (+-assoc n p q)) ⟩
     m + ((n + p) + q)
   ≡⟨ sym (+-assoc m (n + p) q) ⟩
     (m + (n + p)) + q
   ∎

-- Exercise finite-+-assoc (stretch)
-- base: (zero + n) + p ≡ zero + (n + p)
-- induction: (m + n) + p ≡ m + (n + p) ⇒ (suc m + n) + p ≡ suc m + (n + p)
-- 0:
-- 1: 0 + 0 ≡ 0
--    (0 + 0) + 0 ≡ 0 + (0 + 0)
-- 2: 1 + 0 ≡ 1
--    (0 + 0) + 1 ≡ 0 + (0 + 1)
-- 3: 0 + 2 ≡ 2, 1 + 1 ≡ 2
--    (0 + 0) + 2 ≡ 0 + (0 + 2)
--    (0 + 1) + 1 ≡ 0 + (1 + 1)
-- 4: 0 + 3 ≡ 3, 1 + 2 ≡ 3
--    (0 + 0) + 3 ≡ 0 + (0 + 3)
--    (0 + 1) + 2 ≡ 0 + (1 + 2)
--    (0 + 2) + 1 ≡ 0 + (2 + 1)

+-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc′ zero n p = refl
+-assoc′ (suc m) n p rewrite +-assoc′ m n p = refl

+-identity′ : ∀ (n : ℕ) → n + zero ≡ n
+-identity′ zero = refl
+-identity′ (suc n) rewrite +-identity′ n = refl

+-suc′ : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc′ zero n = refl
+-suc′ (suc m) n rewrite +-suc′ m n = refl

+-comm′ : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm′ m zero rewrite +-identity′ m = refl
+-comm′ m (suc n) rewrite +-suc′ m n | +-comm′ m n = refl

-- Exercise +-swap (recommended)
+-swap : ∀ (m n p : ℕ) → m + (n + p) ≡ n + (m + p)
+-swap m n p rewrite sym (+-assoc′ m n p) | +-comm′ m n | +-assoc′ n m p = refl

-- Exercise *-distrib-+ (recommended)
*-distrib-+ : ∀ (m n p : ℕ) → (m + n) * p ≡ m * p + n * p
*-distrib-+ zero zero p = refl
*-distrib-+ zero (suc n) p = refl
*-distrib-+ (suc m) n p rewrite *-distrib-+ m n p
                                | +-assoc′ p (m * p) (n * p) =  refl

-- Exercise *-assoc (recommended)
*-assoc : ∀ (m n p : ℕ) → (m * n) * p ≡ m * (n * p)
*-assoc zero n p = refl
*-assoc (suc m) n p rewrite *-distrib-+ n (m * n) p | *-assoc m n p = refl

-- Exercise *-comm (practice)
*-absorb : ∀ (m : ℕ) → m * zero ≡ zero
*-absorb zero = refl
*-absorb (suc m) rewrite *-absorb m = refl

*-id : ∀ (m : ℕ) → m * 1 ≡ m
*-id zero = refl
*-id (suc m) rewrite *-id m = refl

*-distrib-+ˡ : ∀ (m n p : ℕ) → m * (n + p) ≡ m * n + m * p
*-distrib-+ˡ zero n p = refl
*-distrib-+ˡ (suc m) n p rewrite *-distrib-+ˡ m n p
                                 | +-assoc′ n p (m * n + m * p)
                                 | cong (n +_) (+-swap p (m * n) (m * p))
                                 | sym (+-assoc′ n (m * n) (p + m * p)) = refl

*-comm : ∀ (m n : ℕ) → m * n ≡ n * m
*-comm zero n rewrite *-absorb n = refl
*-comm (suc m) n rewrite *-comm m n | *-distrib-+ˡ n 1 m | *-id n = refl

-- Exercise 0 ∸ n ≡ 0 (practice)
∸-zero : ∀ (n : ℕ) → zero ∸ n ≡ zero
∸-zero zero = refl
∸-zero (suc n) = refl

-- Exercise ∸-+-assoc (practice)
∸-+-assoc : ∀ (m n p : ℕ) → (m ∸ n) ∸ p ≡ m ∸ (n + p)
∸-+-assoc zero n p rewrite ∸-zero n | ∸-zero p | ∸-zero (n + p) = refl
∸-+-assoc (suc m) zero p = refl
∸-+-assoc (suc m) (suc n) p rewrite ∸-+-assoc m n p = refl

-- Exercise +*^ (stretch)
^-distribˡ-+-* : ∀ (m n p : ℕ) → m ^ (n + p) ≡ (m ^ n) * (m ^ p)
^-distribˡ-+-* m zero p rewrite +-identity′ (m ^ p) = refl
^-distribˡ-+-* m (suc n) p rewrite ^-distribˡ-+-* m n p
                                   | *-assoc m (m ^ n) (m ^ p) = refl

*-swap : ∀ (m n p : ℕ) → m * (n * p) ≡ n * (m * p)
*-swap m n p rewrite sym (*-assoc m n p) | *-comm m n | *-assoc n m p = refl

^-distribʳ-* : ∀ (m n p : ℕ) → (m * n) ^ p ≡ (m ^ p) * (n ^ p)
^-distribʳ-* m n zero = refl
^-distribʳ-* m n (suc p) rewrite ^-distribʳ-* m n p
                                 | *-assoc m n ((m ^ p) * (n ^ p))
                                 | cong (m *_) (*-swap n (m ^ p) (n ^ p))
                                 | sym (*-assoc m (m ^ p) (n * (n ^ p))) = refl

^-one : ∀ (m : ℕ) → 1 ^ m ≡ 1
^-one zero = refl
^-one (suc m) rewrite ^-one m = refl

^-*-assoc : ∀ (m n p : ℕ) → (m ^ n) ^ p ≡ m ^ (n * p)
^-*-assoc m zero p rewrite ^-one p = refl
^-*-assoc m (suc n) p rewrite ^-distribʳ-* m (m ^ n) p
                              | ^-*-assoc m n p
                              | sym (^-distribˡ-+-* m p (n * p)) = refl

-- Exercise Bin-laws (stretch)
from-inc : ∀ (b : Bin) → from (inc b) ≡ suc (from b)
from-inc ⟨⟩ = refl
from-inc (b O) rewrite from-inc b = refl
from-inc (b I) rewrite +-identityʳ (from (inc b))
                       | +-identityʳ (from b)
                       | from-inc b
                       | +-suc (from b) (from b) = refl

_ : to (from (⟨⟩)) ≡ ⟨⟩ O
_ = refl

from-to : ∀ (n : ℕ) → from (to n) ≡ n
from-to zero = refl
from-to (suc n) rewrite from-inc (to n) | from-to n = refl
