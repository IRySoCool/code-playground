{-# LANGUAGE OverloadedStrings #-}

module Extract where

import qualified Data.ByteString         as BSS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.HashSet            as S
import           Data.List               (intercalate)
import qualified Data.Text               as TS
import qualified Data.Text.Encoding      as E
import qualified Data.Text.IO            as TS
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.Text.Lazy.IO       as TL

inS :: IO ()
inS = do
    contents <- readFile "./test.txt"
    let s = S.fromList $ words contents
    writeFile "./result.txt" . intercalate "\n" . S.toList $ s

inTS :: IO ()
inTS = do
    contents <- TS.readFile "./test.txt"
    let s = S.fromList $ TS.words contents
    TS.writeFile "./result.txt" . TS.intercalate "\n" . S.toList $ s

inTL :: IO ()
inTL = do
    contents <- TL.readFile "./test.txt"
    let s = S.fromList $ TL.words contents
    TL.writeFile "./result.txt" . TL.intercalate "\n" . S.toList $ s

inBSS :: IO ()
inBSS = do
    contents <- BSS.readFile "./test.txt"
    let s = S.fromList $ TS.words $ E.decodeUtf8 contents
    TS.writeFile "./result.txt" . TS.intercalate "\n" . S.toList $ s

inBSL :: IO ()
inBSL = do
    contents <- BSL.readFile "./test.txt"
    let s = S.fromList $ TL.words $ EL.decodeUtf8 contents
    TL.writeFile "./result.txt" . TL.intercalate "\n" . S.toList $ s
