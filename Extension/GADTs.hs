{-# LANGUAGE GADTs #-}

module Extension.GADTs where

-- data Term a                 -- Extension.GADTs> :t Succ
--     = Lit a                 -- Succ :: Term a -> Term a
--     | Succ (Term a)         -- Extension.GADTs> :t Succ
--     | IsZero (Term a)       -- IsZero :: Term a -> Term a

-- eval :: Num a => Term a -> a
-- eval (Lit i) = i
-- eval (Succ t) = 1 + eval t
-- eval (IsZero i) = eval i == 0 -- Could not deduce (Eq a) arising from a use of ‘==’
                                 -- this will fail since eval :: Term a ->  a, instead
                                 -- of Term Int. We need type on Succ and IsZero
                                 -- constructor. 

-- We can solve this by GADTs extension

data Term a where
    Lit :: a -> Term a
    Succ :: Term Int -> Term Int
    IsZero :: Term Int -> Term Bool

eval :: Term a -> a
eval (Lit i) = i
eval (Succ t) = 1 + eval t
eval (IsZero i) = eval i == 0  -- OK, eval :: Term Int -> Bool

-- Extension.GADTs> :t IsZero
-- IsZero :: Term Int -> Term Bool

-- >>> eval (Lit 'a')
-- 'a'

-- >>> eval (IsZero (Lit 'a'))
-- Couldn't match type ‘Char’ with ‘Int’
-- Expected type: Term Int
--   Actual type: Term Char

-- optional way

