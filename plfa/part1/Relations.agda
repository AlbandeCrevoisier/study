module plfa.part1.Relations where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Nat.Properties using (+-comm; *-comm)

data _≤_ : ℕ → ℕ → Set where

  z≤n : ∀ {n : ℕ}
      --------
    → zero ≤ n

  s≤s : ∀ {m n : ℕ}
    → m ≤ n
      -------------
    → suc m ≤ suc n

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

-- Exercise orderings (practice)
-- preorder that is not a partial order: ∃ path A → B.
-- partial order that is not a total order: divisibility.

≤-refl : ∀ {n : ℕ}
    -----
  → n ≤ n
≤-refl {zero} = z≤n
≤-refl {suc n} = s≤s ≤-refl

≤-trans : ∀ {m n p : ℕ}
  → m ≤ n
  → n ≤ p
    -----
  → m ≤ p
≤-trans z≤n       _          =  z≤n
≤-trans (s≤s m≤n) (s≤s n≤p)  =  s≤s (≤-trans m≤n n≤p)

≤-antisym : ∀ {m n : ℕ}
  → m ≤ n
  → n ≤ m
    -----
  → m ≡ n
≤-antisym z≤n       z≤n        =  refl
≤-antisym (s≤s m≤n) (s≤s n≤m)  =  cong suc (≤-antisym m≤n n≤m)

-- Exercise ≤-antisym-cases (practice)
-- If one argument is z≤n, then the other is n≤z, which implies n ≡ zero.

data Total (m n : ℕ) : Set where

  forward :
      m ≤ n
      ---------
    → Total m n

  flipped :
      n ≤ m
      ---------
    → Total m n

data Total′ : ℕ → ℕ → Set where

  forward′ : ∀ {m n : ℕ}
    → m ≤ n
      ----------
    → Total′ m n

  flipped′ : ∀ {m n : ℕ}
    → n ≤ m
      ----------
    → Total′ m n


≤-total : ∀ (m n : ℕ) → Total m n
≤-total zero    n                         =  forward z≤n
≤-total (suc m) zero                      =  flipped z≤n
≤-total (suc m) (suc n) with ≤-total m n
...                        | forward m≤n  =  forward (s≤s m≤n)
...                        | flipped n≤m  =  flipped (s≤s n≤m)

≤-total′ : ∀ (m n : ℕ) → Total m n
≤-total′ zero    n        =  forward z≤n
≤-total′ (suc m) zero     =  flipped z≤n
≤-total′ (suc m) (suc n)  =  helper (≤-total′ m n)
  where
  helper : Total m n → Total (suc m) (suc n)
  helper (forward m≤n)  =  forward (s≤s m≤n)
  helper (flipped n≤m)  =  flipped (s≤s n≤m)

≤-total″ : ∀ (m n : ℕ) → Total m n
≤-total″ m       zero                      =  flipped z≤n
≤-total″ zero    (suc n)                   =  forward z≤n
≤-total″ (suc m) (suc n) with ≤-total″ m n
...                        | forward m≤n   =  forward (s≤s m≤n)
...                        | flipped n≤m   =  flipped (s≤s n≤m)

+-monoʳ-≤ : ∀ (n p q : ℕ)
  → p ≤ q
    -------------
  → n + p ≤ n + q
+-monoʳ-≤ zero    p q p≤q  =  p≤q
+-monoʳ-≤ (suc n) p q p≤q  =  s≤s (+-monoʳ-≤ n p q p≤q)

+-monoˡ-≤ : ∀ (m n p : ℕ)
  → m ≤ n
    -------------
  → m + p ≤ n + p
+-monoˡ-≤ m n p m≤n rewrite +-comm m p | +-comm n p = +-monoʳ-≤ p m n m≤n

+-mono-≤ : ∀ (m n p q : ℕ)
  → m ≤ n
  → p ≤ q
    -------------
  → m + p ≤ n + q
+-mono-≤ m n p q m≤n p≤q  =  ≤-trans (+-monoˡ-≤ m n p m≤n) (+-monoʳ-≤ n p q p≤q)

-- Exercise *-mono-≤ (stretch)
*-mono-≤ʳ : ∀ (m n p : ℕ)
  → m ≤ n
    -----
  → p * m ≤ p * n
*-mono-≤ʳ m n zero m≤n = z≤n
*-mono-≤ʳ m n (suc p) m≤n = +-mono-≤ m n (p * m) (p * n) m≤n (*-mono-≤ʳ m n p m≤n)

*-mono-≤ˡ : ∀ (m n q : ℕ)
  → m ≤ n
    -----
  → m * q ≤ n * q
*-mono-≤ˡ m n q m≤n rewrite *-comm m q | *-comm n q = *-mono-≤ʳ m n q m≤n

*-mono-≤ : ∀ (m n p q : ℕ)
  → m ≤ n
  → p ≤ q
    -----
  → m * p ≤ n * q
*-mono-≤ m n p q m≤n p≤q = ≤-trans (*-mono-≤ˡ m n p m≤n) (*-mono-≤ʳ p q n p≤q)

infix 4 _<_

data _<_ : ℕ → ℕ → Set where

  z<s : ∀ {n : ℕ}
      ------------
    → zero < suc n

  s<s : ∀ {m n : ℕ}
    → m < n
      -------------
    → suc m < suc n

-- Exercise <-trans (recommended)
<-≤ : ∀ {m n : ℕ} → m < n → suc m ≤ n
<-≤ z<s = s≤s z≤n
<-≤ (s<s m<n) = s≤s (<-≤ m<n)

≤-< : ∀ {m n : ℕ} → m ≤ n → m < suc n
≤-< z≤n = z<s
≤-< (s≤s m≤n) = s<s (≤-< m≤n)

<-suc : ∀ {m n : ℕ}
  → m < n
    ---------
  → m < suc n
<-suc {zero} {n} m<n = z<s
<-suc (s<s m<n) = s<s (<-suc m<n)

inv-< : ∀ {m n : ℕ}
  → suc m < suc n
    -------------
  → m < n
inv-< (s<s m<n) = m<n

<-trans : ∀ {m n p : ℕ}
  → m < n
  → n < p
    -----
  → m < p
<-trans m<n n<p = inv-< (inv-< (<-suc (≤-< (≤-trans (s≤s (<-≤ m<n))
                                                    (<-≤ n<p)))))


-- Exercise trichotomy (practice)
data Trichotomy : ℕ → ℕ → Set where
  fwd : {m n : ℕ}
    → m < n
      -----
    → Trichotomy m n
  eq : {m n : ℕ}
    → m ≡ n
      -----
    → Trichotomy m n
  bwd : {m n : ℕ}
    → n < m
      -----
    → Trichotomy m n

<-trichotomy : ∀ (m n : ℕ) → Trichotomy m n
<-trichotomy zero zero = eq refl
<-trichotomy zero (suc n) = fwd z<s
<-trichotomy (suc m) zero = bwd z<s
<-trichotomy (suc m) (suc n) with <-trichotomy m n
...                               | fwd m<n = fwd (s<s m<n)
...                               | eq m≡n = eq (cong suc m≡n)
...                               | bwd n<m = bwd (s<s n<m)

-- Exercise +-mono-< (practice)
+-mono-<ʳ : ∀ (m n p : ℕ)
  → m < n
    -----
  → p + m < p + n
+-mono-<ʳ m n zero m<n = m<n
+-mono-<ʳ m n (suc p) m<n = s<s (+-mono-<ʳ m n p m<n)

+-mono-<ˡ : ∀ (m n q : ℕ)
  → m < n
    -----
  → m + q < n + q
+-mono-<ˡ m n q m<n rewrite +-comm m q | +-comm n q = +-mono-<ʳ m n q m<n
  
+-mono-< : ∀ (m n p q : ℕ)
  → m < n
  → p < q
    -----
  → m + p < n + q
+-mono-< m n p q m<n p<q = <-trans (+-mono-<ˡ m n p m<n) (+-mono-<ʳ p q n p<q)

-- Exercise ≤-iff-< (recommended)
-- Show that suc m ≤ n implies m < n, and conversely.
≤-iff-< : ∀ (m n : ℕ)
  → suc m ≤ n
    ---------
  → m < n
≤-iff-< m n sm≤n = inv-< (≤-< sm≤n)

-- <-iff-≤ ≡ <-≤.
