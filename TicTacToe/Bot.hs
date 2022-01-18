module TicTacToe.Bot where

import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Matrix
import           TicTacToe.Common
import           TicTacToe.Type

moves :: Mark -> Board -> [(Int, Int)]
moves m b = [ (i, j) | i <- [1..3], j <- [1..3], b ! (i, j) == Empty ]

scoring :: Mark -> Board -> [(Int, Int)] -> (Int, Int) -> Int
scoring m b s pos
    | b ! pos /= Empty = error "Grid is not empty"
    | win m b'         = 100
    | null s'          = 0
    | otherwise        = negate . maximum . fmap (scoring opposite b' s') $ s'
    where
        b' = setElem m pos b
        s' = delete pos s
        opposite = if m == X then O else X


botMove :: Mark -> Board -> (Int, Int)
botMove m board = maximumBy (compare `on` scoring m board s) s
    where
        s = moves m board
