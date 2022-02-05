{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashSet as S
import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    contents <- T.readFile "./test.txt"
    let s = S.fromList $ T.words contents
    T.writeFile "./result.txt" . T.intercalate "\n" . S.toList $ s
