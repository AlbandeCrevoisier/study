module plfa.part1.Induction where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_)


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
+-swap zero n p = refl
+-swap (suc m) n p rewrite +-swap m n p | +-suc′ n (m + p) = refl
