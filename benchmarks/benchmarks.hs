module Main where

import           BinaryTrie     (Trie)
import qualified BinaryTrie     as Trie

import           Data.IntMap    (IntMap)
import qualified Data.IntMap    as IntMap

import           Criterion.Main


main = defaultMain [
  env values $ \ values ->
    bgroup "IntMap: Creating" [
        bench "0..1000" $ nf IntMap.fromList values
    ],
  env values $ \ values ->
    bgroup "Binary Tries: Creating" [
        bench "0..1000" $ nf Trie.fromList values
    ]
  ]
  where values = return $ let xs = [0..1000] in zip xs xs
