module Main where

import           Criterion.Main
import qualified Data.HashSet   as S
import           Data.List
import           System.IO

main :: IO ()
main = defaultMain
    [ bench "openFile" $ whnfIO f1
    , bench "readFile" $ whnfIO f2
    ]

f1 :: IO ()
f1 = do
    rh <- openFile "./test.txt" ReadMode
    contents <- hGetContents rh
    let s = S.fromList $ words contents
    writeFile "./result.txt" . intercalate "\n" . S.toList $ s

f2 :: IO ()
f2 = do
    contents <- readFile "./test.txt"
    let s = S.fromList $ words contents
    writeFile "./result.txt" . intercalate "\n" . S.toList $ s
