module plfa-bis.1-1-Naturals where

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

seven : ℕ
seven = suc (suc (suc (suc (suc (suc (suc zero))))))
