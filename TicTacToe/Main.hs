{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module Main where

import           Control.Monad.State.Strict
import           Data.Matrix
import           Data.String.Interpolate
import           System.IO
import           System.Random
import qualified Text.Read                  as T
import           TicTacToe.Bot
import           TicTacToe.Common
import           TicTacToe.Type

printBoard :: MonadIO m => Matrix Mark -> m ()
printBoard b = liftIO $ do
    putStrLn "┌───┬───┬───┐"
    putStrLn [i|│ #{b ! (1, 1)} │ #{b ! (1, 2)} │ #{b ! (1, 3)} │|]
    putStrLn "├───┼───┼───┤"
    putStrLn [i|│ #{b ! (2, 1)} │ #{b ! (2, 2)} │ #{b ! (2, 3)} │|]
    putStrLn "├───┼───┼───┤"
    putStrLn [i|│ #{b ! (3, 1)} │ #{b ! (3, 2)} │ #{b ! (3, 3)} │|]
    putStrLn "└───┴───┴───┘"

printHr :: MonadIO m => Int -> m ()
printHr = liftIO . putStrLn . flip replicate '='

putMark :: MonadIO m => Mark -> Mark -> Board -> m Board
putMark player mark board =
    if player /= mark
        then do
            liftIO $ putStrLn "Computer move:" 
            pure $ setElem mark (botMove mark board) board
        else liftIO $ do
    let again str = putStrLn str >> putMark player mark board
    putStrLn [i|Where to put #{show mark} (1~3, 1~3)? |]
    line <- fmap T.readMaybe getLine
    case line of
        Nothing -> again "invalid input"
        Just ix@(i, j) -> case safeGet i j board of
            Nothing    -> again "out of boundary"
            Just Empty -> pure $ setElem mark ix board
            _          -> again "grid is not empty"

game :: (MonadState Game m, MonadIO m) => m ()
game = do
    Game turn player board result <- get
    when (result == Running) $ do
        board <- putMark player (if even turn then X else O) board

        let result = case () of
                _ | win X board               -> WinBy X
                  | win O board               -> WinBy O
                  | hasEmptyGrid board        -> Running
                  | otherwise                 -> Draw
        printBoard board
        printHr 100
        put (Game (if result == Running then turn + 1 else turn) player board result)
        game


main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    x <- randomIO
    let emptyBoard = fromList 3 3 $ replicate 9 Empty
        player     = if x then X else O
        gameState  = Game 1 player emptyBoard Running
    putStrLn [i| You are #{player} |]
    printBoard emptyBoard
    printHr 100
    ((), g) <- runStateT game (Game 1 player (fromList 3 3 $ replicate 9 Empty) Running)
    print $ result g
