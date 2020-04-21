module tuto where

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

_+_ : Nat -> Nat -> Nat
zero + m = m
suc m + n = suc (m + n)

_*_ : Nat -> Nat -> Nat
zero * _ = zero
suc m * n = (m * n) + n

infixl 60 _*_
infixl 40 _+_

infixr 40 _∷_
data List (A : Set) : Set where
  [] : List A
  _∷_ : A -> List A -> List A
