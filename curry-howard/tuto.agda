module tuto where

data Bool : Set where
  true : Bool
  false : Bool

not : Bool -> Bool
not true = false
not false = true

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

_+_ : Nat -> Nat -> Nat
zero + m = m
suc m + n = suc (m + n)

_*_ : Nat -> Nat -> Nat
zero * _ = zero
suc m * n = (m * n) + n

_or_ : Bool -> Bool -> Bool
false or x = x
true or _ = true

if_then_else_ : {A : Set} -> Bool -> A -> A -> A
if true then x else _ = x
if false then _ else x = x

infixl 60 _*_
infixl 40 _+_
infixr 20 _or_
infix 5 if_then_else_

infixr 40 _∷_
data List (A : Set) : Set where
  [] : List A
  _∷_ : A -> List A -> List A

identity : (A : Set) -> A -> A
identity A x = x

zero' : Nat
zero' = identity Nat zero

apply : (A : Set)(B : A -> Set) ->
        ((x : A) -> B x) -> (a : A) -> B a
apply A B f a = f a

identity₂ : (A : Set) -> A -> A
identity₂ = \A x -> x

identity₃ : (A : Set) -> A -> A
identity₃ = \(A : Set)(x : A) -> x

identity₄ : (A : Set) -> A -> A
identity₄ = \(A : Set) x -> x

id : {A : Set} -> A -> A
id x = x

true' : Bool
true' = id true

silly : {A : Set}{x : A} -> A
silly {_}{x} = x

false' : Bool
false' = silly {x = false}

one : Nat
one = identity _ (suc zero)

_∘_ : {A : Set}{B : A -> Set}{C : (x : A) -> B x -> Set}
      (f : {x : A}(y : B x) -> C x y)(g : (x : A) -> B x)
      (x : A) -> C x (g x)
(f ∘ g) x = f (g x)

plus-two = suc ∘ suc

map : {A B : Set} -> (A -> B) -> List A -> List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

_++_ : {A : Set} -> List A -> List A -> List A
[] ++ xs = xs
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)
