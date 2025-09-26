{-# OPTIONS --without-K --exact-split --safe --auto-inline #-}

module Main where

open import Universes public

variable
 𝓤 𝓥 𝓦 𝓣 : Universe

data 𝟙 : 𝓤₀ · where
 ⋆ : 𝟙

𝟙-induction : (A : 𝟙 → 𝓤 ·) → A ⋆ → (x : 𝟙) → A x
𝟙-induction A a ⋆ = a
