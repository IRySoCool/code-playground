{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Extension.KindSignatures where

-- Allow us to use * in type signature

data Vec :: * -> * -> * where
    Nil :: Vec n a
    Cons :: a -> Vec n a -> Vec n a

data Fix :: (* -> *) -> * where
    Fix :: f (Fix f) -> Fix f

data Toy a = Output a (Toy a) | Bell (Toy a) | Done

data ToyS a t = OutputS a t | BellS t | DoneS

run :: Toy a -> Fix (ToyS a)
run (Output a next) = Fix (OutputS a (run next))
run (Bell next) = Fix (BellS (run next))
run Done = Fix DoneS

sampleToy :: Toy String
sampleToy = Output "stpe1" (Bell (Output "step2" (Output "step3" Done)))