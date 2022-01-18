module TicTacToe.Common where
import           Data.Matrix
import           TicTacToe.Type

win :: Mark -> Board -> Bool
win mark board =
    -- check from upper left to lower right
    all (== mark) (getDiag board)
    -- check from lower left to upper right
    || all (\d -> board ! (d, 4 - d) == mark) [1..3]
    -- check win in any row
    || any (all (== mark) . flip getRow board) [1..3]
    -- check win in any column
    || any (all (== mark) . flip getCol board) [1..3]

hasEmptyGrid :: Board -> Bool
hasEmptyGrid = elem Empty . toList
