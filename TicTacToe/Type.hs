module TicTacToe.Type where

import Data.Matrix

data Mark = X
          | O
          | Empty deriving (Eq)

instance Show Mark where
    show X     = "X"
    show O     = "O"
    show Empty = " "

type Board = Matrix Mark


data Result = Running | Draw | WinBy Mark deriving (Eq, Show)

data Game = Game
    { turn   :: Int
    , player :: Mark
    , board  :: Board
    , result :: Result
    }