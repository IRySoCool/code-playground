{-# LANGUAGE GADTs #-}

module Math.Differentiation where

import qualified Data.Map.Strict as M

-- For simplicity, just consider the basic four arithmetic operations and power function.
-- It may be tedious since each elementary functions have their own differentiation rules...
infixl 4 ＋, －
infixl 5 ✕ , ÷

(＋) :: Num a => Expr a -> Expr a -> Expr a
a ＋ b = Add a b

(✕):: Num a => Expr a -> Expr a -> Expr a
a ✕ b = Mult a b

(－) :: Num a => Expr a -> Expr a -> Expr a
a － b = Add a (Const (-1) ✕ b)

(÷) ::Num a => Expr a -> Expr a -> Expr a
a ÷ b = Div a b

data Expr a where
    Symbol :: Num a => Char -> Expr a
    Const  :: Num a => a -> Expr a
    Add    :: Num a => Expr a -> Expr a -> Expr a
    Mult   :: Num a => Expr a -> Expr a -> Expr a
    Div    :: Num a => Expr a -> Expr a -> Expr a
    Pow    :: Num a => Expr a -> a -> Expr a

parenthesis :: Show a => a -> String
parenthesis a = "( " ++ show a ++ " )"

instance (Show a) => Show (Expr a) where
    show (Symbol c)     = show c
    show (Const n)      = show n
    show (Add a b)      = parenthesis a ++ " + " ++ parenthesis b
    show (Mult a b)     = parenthesis a ++ " * " ++ parenthesis b
    show (Div a b)      = parenthesis a ++ " / " ++ parenthesis b
    show (Pow expr pow) = parenthesis (parenthesis expr ++ " ^ " ++ show pow)

-- substituation
sub :: Num a => M.Map Char a -> Expr a -> Expr a
sub m s@(Symbol c)       = maybe s Const $ M.lookup c m
sub _ (Const v)          = Const v
sub m (Add expr1 expr2)  = Add (sub m expr1) (sub m expr2)
sub m (Mult expr1 expr2) = Mult (sub m expr1) (sub m expr2)
sub m (Div expr1 expr2)  = Div (sub m expr1) (sub m expr2)
sub m (Pow expr1 pow)    = Pow (sub m expr1) pow

-- differentiation
d_ :: Char -> Expr a -> Expr a
d_ c (Const v) = Const 0
d_ c s@(Symbol c') = if c == c' then Const 1 else Const 0
-- d (f + g) = d f + d g
d_ c (Add expr1 expr2) =  d_ c expr1 ＋ d_ c expr2
-- d (f * g) = f * (d g) + (d f) * g
d_ c (Mult expr1 expr2) = (d_ c expr1 ✕ expr2) ＋ (expr1 ✕ d_ c expr2)
-- d (f / g) = ((d f) * g - f * (d g) )/ g ^ 2
d_ c (Div expr1 expr2) = ((d_ c expr1 ✕ expr2) － (expr1 ✕ d_ c expr2)) ÷ (expr2 ✕ expr2)
-- d (f ^ n) = n * f ^ (n-1) * d f
d_ c (Pow expr pow) = Const pow ✕ Pow expr (pow - 1) ✕ d_ c expr

-- calculation
calc :: (Floating a) => Expr a -> Either String a
calc s@(Symbol c)       = Left $ c : " is still a symbol"
calc (Const v)          = Right v
calc (Add expr1 expr2)  = (+) <$> calc expr1 <*> calc expr2
calc (Mult expr1 expr2) = (*) <$> calc expr1 <*> calc expr2
calc (Div expr1 expr2)  = (/) <$> calc expr1 <*> calc expr2
calc (Pow expr pow)     = (** pow) <$> calc expr

gradient :: Num a => [Char] -> Expr a -> [Expr a]
gradient symbols f = fmap (`d_` f) symbols

divergence :: [Char] -> [Expr a] -> [Expr a]
divergence = zipWith d_

curl :: Num a => [Expr a] -> [Expr a]
curl [fx, fy, fz] = let
    px = d_ 'x'
    py = d_ 'y'
    pz = d_ 'z'
    in [py fz － pz fy, pz fx － px fz, px fy － py fx]
curl _ = error "curl is 3 dimensional only"

-- 3 dimensional basis
basis :: [Char]
basis = ['x'..'z']

evalVector :: Floating a => M.Map Char a -> [Expr a] -> [Either String a]
evalVector m = fmap (calc . sub m)

-- test case
-- may try to generate test cases through Quickcheck?
f1 :: Expr Double
f1 = Pow(Const 3 ✕ Symbol 'y' ＋ (Symbol 'x' ÷ Symbol 'z')) 2

f2 :: Expr Double
f2 = Pow(Symbol 'y' ＋ Symbol 'x' ＋ Symbol 'z') 2

f3 :: Expr Double
f3 = Pow(Symbol 'y' ✕ Symbol 'x' ✕ Symbol 'z') (-1)

-- it follows somewhat called d^2 = 0 in differential forms.
-- the intuitive meaning if it is basically taking the boundary 
-- of boundary, which obviously is an empty set.
m :: M.Map Char Double
m = M.fromList [('x', 1), ('y', 1), ('z', 1)]
f :: Expr Double -> [Either String Double]
f = evalVector m . curl . gradient basis

g :: [Expr Double] -> [Either String Double]
g = evalVector m . divergence basis . curl
-- >>> f <$> [f1, f2, f3]
-- [[Right 0.0,Right 0.0,Right 0.0],[Right 0.0,Right 0.0,Right 0.0],[Right 0.0,Right 0.0,Right 0.0]]
