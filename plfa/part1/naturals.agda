module plfa.part1.naturals where

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

-- Exercise seven
-- 7 = suc (suc (suc (suc (suc (suc (suc (suc zero)))))))
