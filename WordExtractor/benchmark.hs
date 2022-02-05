module Main where

import Criterion.Main
import Extract

main :: IO ()
main = defaultMain
    [ bench "String" $ nfIO inS
    , bench "Text Strict" $ nfIO inTS
    , bench "Text Lazy" $ nfIO inTL
    , bench "ByteString Strict" $ nfIO inBSS
    , bench "ByteString Lazy" $ nfIO inBSL
    ]