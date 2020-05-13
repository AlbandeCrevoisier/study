module plfa.part1.Equality where

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

infix 4 _≡_

sym : ∀ {A : Set} {x y : A}
  → x ≡ y
    -----
  → y ≡ x
sym refl = refl

trans : ∀ {A : Set} {x y z : A}
  → x ≡ y
  → y ≡ z
    -----
  → x ≡ z
trans refl refl = refl

cong : ∀ {A B : Set} (f : A → B) {x y : A}
  → x ≡ y
    ---------
  → f x ≡ f y
cong f refl = refl

cong₂ : ∀ {A B C : Set} (f : A → B → C) {u x : A} {v y : B}
  → u ≡ x
  → v ≡ y
    -------------
  → f u v ≡ f x y
cong₂ f refl refl = refl

cong-app : ∀ {A B : Set} {f g : A → B}
  → f ≡ g
    ---------------------
  → ∀ (x : A) → f x ≡ g x
cong-app refl = λ x → refl

subst : ∀ {A : Set} {x y : A} (P : A → Set)
  → x ≡ y
    ---------
  → P x → P y
subst P refl = λ px → px

module ≡-Reasoning {A : Set} where

  infix  1 begin_
  infixr 2 _≡⟨⟩_ _≡⟨_⟩_
  infix  3 _∎

  begin_ : ∀ {x y : A}
    → x ≡ y
      -----
    → x ≡ y
  begin x≡y  =  x≡y

  _≡⟨⟩_ : ∀ (x : A) {y : A}
    → x ≡ y
      -----
    → x ≡ y
  x ≡⟨⟩ x≡y  =  x≡y

  _≡⟨_⟩_ : ∀ (x : A) {y z : A}
    → x ≡ y
    → y ≡ z
      -----
    → x ≡ z
  x ≡⟨ x≡y ⟩ y≡z  =  trans x≡y y≡z

  _∎ : ∀ (x : A)
      -----
    → x ≡ x
  x ∎  =  refl

open ≡-Reasoning

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero    + n  =  n
(suc m) + n  =  suc (m + n)

postulate
  +-identity : ∀ (m : ℕ) → m + zero ≡ m
  +-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)

+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero =
  begin
    m + zero
  ≡⟨ +-identity m ⟩
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

-- Exercise ≤-Reasoning (stretch)
data _≤_ : ℕ → ℕ → Set where
  z≤n : ∀ {n : ℕ}
      --------
    → zero ≤ n
  s≤s : ∀ {m n : ℕ}
    → m ≤ n
      -------------
    → suc m ≤ suc n

≤-trans : ∀ {m n p : ℕ}
  → m ≤ n
  → n ≤ p
    -----
  → m ≤ p
≤-trans z≤n n≤p = z≤n
≤-trans (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans m≤n n≤p)

module ≤-reasoning where
  infix 1 nigeb_
  infixr 2 _≤⟨⟩_ _≤⟨_⟩_
  infix 3 _■

  nigeb_ : ∀ {m n : ℕ}
    → m ≤ n
      -----
    → m ≤ n
  nigeb_ m≤n = m≤n

  _≤⟨⟩_ : ∀ (m : ℕ) {n : ℕ}
    → m ≤ n
      -----
    → m ≤ n
  m ≤⟨⟩ m≤n = m≤n

  _≤⟨_⟩_ : ∀ (m : ℕ) {n p : ℕ}
    → m ≤ n
    → n ≤ p
      -----
    → m ≤ p
  m ≤⟨ m≤n ⟩ n≤p = ≤-trans m≤n n≤p

  _■ : ∀ (m : ℕ)
       -----
     → m ≤ m
  zero ■ = z≤n
  suc m ■ = s≤s (m ■)

open ≤-reasoning

≤-≡ : ∀ {m n : ℕ}
  → m ≡ n
    -----
  → m ≤ n
≤-≡ refl = _ ■

+-monoʳ-≤ : ∀ (m n p : ℕ)
  → m ≤ n
    -----------
  → (p + m) ≤ (p + n)
+-monoʳ-≤ m n zero m≤n = m≤n
+-monoʳ-≤ m n (suc p) m≤n = s≤s (+-monoʳ-≤ m n p m≤n)

+-monoˡ-≤ : ∀ (m n p : ℕ)
  → m ≤ n
    -------------
  → (m + p) ≤ (n + p)
+-monoˡ-≤ m n zero m≤n =
  nigeb
    m + zero
  ≤⟨ ≤-≡ (+-identity m) ⟩
    m
  ≤⟨ m≤n ⟩
    n
  ≤⟨ ≤-≡ (sym (+-identity n)) ⟩
    (n + zero)
  ■
+-monoˡ-≤ m n (suc p) m≤n =
  nigeb
    m + suc p
  ≤⟨ ≤-≡ (+-suc m p) ⟩
    suc (m + p)
  ≤⟨ s≤s (+-monoˡ-≤ m n p m≤n) ⟩
    suc (n + p)
  ≤⟨ ≤-≡ (sym (+-suc n p)) ⟩
    (n + suc p)
  ■

+-mono-≤ : (m n p q : ℕ)
  → m ≤ n
  → p ≤ q
    -------------
  → (m + p) ≤ (n + q)
+-mono-≤ m n p q m≤n p≤q =
  nigeb
    m + p
  ≤⟨ +-monoˡ-≤ m n p m≤n ⟩
    n + p
  ≤⟨ +-monoʳ-≤ p q n p≤q ⟩
    (n + q)
  ■
