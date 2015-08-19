{-# LANGUAGE DataKinds #-}
module Main where

import qualified BinaryTrie          as Binary
import           Trie                (Trie)
import qualified Trie                as Trie

import           Control.Applicative ((<$>), (<*>))

import           Data.Bits           (bit, xor)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap
import           Data.Vector         (Vector, (//))
import qualified Data.Vector         as Vector

import           Criterion.Main


main = defaultMain [
  env values $ \ values ->
    bgroup "Creating 0..100000" [
        bench "IntMap" $ nf IntMap.fromList values,
        bench "BinaryTrie" $ nf Binary.fromList values,
        bench "Trie 1" $ nf trie1 values,
        bench "Trie 2" $ nf trie2 values,
        bench "Trie 3" $ nf trie3 values,
        bench "Trie 4" $ nf trie4 values,
        bench "Trie 5" $ nf trie5 values,
        bench "Trie 6" $ nf trie6 values,
        bench "Trie 7" $ nf trie7 values,
        bench "Trie 8" $ nf trie8 values
    ]
    -- ,
    -- bgroup "branch" [
    --     bench "BinaryTrie" $ nf (Binary.branch bPrefix bControl bLeaf1) bLeaf2,
    --     bench "Trie 2" $ nf (Trie.branch prefix2 index2 :: Vector (Trie 2 String) -> Trie 2 String) children2
    -- ],
    -- bgroup "findIndex" [
    --     bench "BinaryTrie" $ nf Binary.highestBitSet (k1 `xor` k2),
    --     bench "Trie 2" $ nf trieTest 2,
    --     bench "Trie 3" $ nf trieTest 3,
    --     bench "Trie 4" $ nf trieTest 4,
    --     bench "Trie 5" $ nf trieTest 5,
    --     bench "Trie 6" $ nf trieTest 6,
    --     bench "Trie 7" $ nf trieTest 7,
    --     bench "Trie 8" $ nf trieTest 8
    -- ]-- ,
    -- bgroup "combine" [
    --     bench "BinaryTrie" $ nf (Binary.combine' k1 bLeaf1 k2) bLeaf2,
    --     bench "Trie 1" $ nf (Trie.combine' k1 t1Leaf1 k2) t1Leaf2,
    --     bench "Trie 2" $ nf (Trie.combine' k1 t2Leaf1 k2) t2Leaf2,
    --     bench "Trie 8" $ nf (Trie.combine' k1 t8Leaf1 k2) t8Leaf2
    -- ]
  ]
  where values :: IO [(Int, Int)]
        values = return $ let xs = [0..100000] in zip xs xs

        trie1 = Trie.fromList :: [(Int, Int)] -> Trie 1 Int
        trie2 = Trie.fromList :: [(Int, Int)] -> Trie 2 Int
        trie3 = Trie.fromList :: [(Int, Int)] -> Trie 3 Int
        trie4 = Trie.fromList :: [(Int, Int)] -> Trie 4 Int
        trie5 = Trie.fromList :: [(Int, Int)] -> Trie 5 Int
        trie6 = Trie.fromList :: [(Int, Int)] -> Trie 6 Int
        trie7 = Trie.fromList :: [(Int, Int)] -> Trie 7 Int
        trie8 = Trie.fromList :: [(Int, Int)] -> Trie 8 Int

        -- An arbitrary pair of keys that has to branch relatively
        -- high with some prefix.
        k1 = 0xDEAAEFC28
        k2 = 0xDEAFEFC28

        trieTest s = s + (Trie.countTrailingNonZeros (k1 `xor` k2) - 1)
          where round x = x `div` s * s

        bControl = Binary.highestBitSet (k1 `xor` k2)
        bPrefix = Binary.getPrefix k1 bControl

        index2 = 2 + ((Trie.countTrailingNonZeros (k1 `xor` k2)) `div` 2 * 2)
        children2 = Vector.fromList [t2Leaf1, t2Leaf2]
        prefix2 = Trie.getPrefix k1 index2

        bLeaf1 = Binary.Leaf k1 "abc"
        bLeaf2 = Binary.Leaf k2 "def"
        t1Leaf1 = Trie.Leaf k1 "abc" :: Trie 1 String
        t1Leaf2 = Trie.Leaf k2 "def" :: Trie 1 String
        t2Leaf1 = Trie.Leaf k1 "abc" :: Trie 2 String
        t2Leaf2 = Trie.Leaf k2 "def" :: Trie 2 String
        t8Leaf1 = Trie.Leaf k1 "abc" :: Trie 8 String
        t8Leaf2 = Trie.Leaf k2 "abc" :: Trie 8 String
