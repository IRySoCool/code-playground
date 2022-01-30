module Main where

type J r x = (x -> r) -> x

type K r x = (x -> r) -> r

arginf :: [x] -> J Int x
arginf xs p = argsup xs (\x -> - p x)

argsup :: [x] -> J Int x
argsup [] p = undefined
argsup (x:xs) p = f xs x (p x)
    where
        f xs a 1 = a
        f [] a r = a
        f (x:xs) a (-1) = f xs x (p x)
        f xs a 0 = g xs
            where
                g [] = a
                g (x:xs) | p x == 1 = x
                         | otherwise = g xs

overline :: J r x -> K r x
overline e p = p (e p)

otimes :: J r x -> (x -> J r [x]) -> J r [x]
otimes e0 e1 p = a0 : a1
    where
        a0 = e0 (\x0 -> overline (e1 x0) (\x1 -> p (x0:x1)))
        a1 = e1 a0 (\x1 -> p (a0:x1))

bigotimes :: [[x] -> J r x] -> J r [x]
bigotimes []     = const []
bigotimes (e:es) = e [] `otimes` (\x-> bigotimes [\xs -> d(x:xs) | d <- es])

data Player = X | O
type R = Int
type Move = Int

type Board = ([Move], [Move])

someContained :: Ord x => [[x]] -> [x] -> Bool
someContained [] ys         = False
someContained xss []        = False
someContained (xs : xss) ys = contained xs ys || someContained xss ys
    where
        contained [] ys = True
        contained xs [] = False
        contained us@(x : xs) (y : ys)
            | x == y = contained xs ys
            | x >= y = contained us ys
            | otherwise = False

insert :: Ord x => x -> [x] -> [x]
insert x [] = [x]
insert x vs@(y : ys)
    | x == y = vs
    | x < y = x : vs
    | otherwise = y : insert x ys

delete :: Ord x => x -> [x] -> [x]
delete x [] = []
delete x vs@(y : ys)
    | x == y = ys
    | x < y = vs
    | otherwise = y : delete x ys

wins :: [Move] -> Bool
wins = someContained [[0,1,2],[3,4,5],[6,7,8],
                      [0,3,6],[1,4,7],[2,5,8],
                      [0,4,8],[2,4,6]]

value :: Board -> R
value (x,o) | wins x = 1
            | wins o = -1
            | otherwise = 0

outcome :: Player -> [Move] -> Board -> Board
outcome whoever [] board = board
outcome X (m:ms) (x,o) = if wins o then (x, o) else outcome O ms (insert m x, o)
outcome O (m:ms) (x,o) = if wins x then (x, o) else outcome X ms (x, insert m o)

epsilons :: [[Move] -> J R Move]
epsilons = take 9 all
    where
        all = epsilonX : epsilonO : all
        epsilonX h = argsup ([0..8] `setMinus` h)
        epsilonO h = arginf ([0..8] `setMinus` h)

setMinus :: Ord x => [x] -> [x] -> [x]
setMinus = foldl (flip delete)

p :: [Move] -> R
p ms = value (outcome X ms ([],[]))

optimalPlay :: [Move]
optimalPlay = bigotimes epsilons p

optimalOutcome :: R
optimalOutcome = p optimalPlay

optimalStrategy :: [Move] -> Move
optimalStrategy as = head(bigotimes epsilons' p')
    where
        epsilons' = drop (length as) epsilons
        p' xs = p(as ++ xs)

main :: IO ()
main = putStr ("An optimal play for Tic-Tac-Toe is "
        ++ show optimalPlay ++ "\nand the optimal outcome is "
        ++ show optimalOutcome ++ "\n")
