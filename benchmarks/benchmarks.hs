{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (replicateM)

import           Criterion.Main

import           Data.Bits           (bit, xor)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap
import           Data.Vector         (Vector)

import           System.Random       (randomIO)

import qualified Trie.Array          as Array
import qualified Trie.Binary         as Binary
import qualified Trie.Vector         as Vector

main :: IO ()
main = defaultMain [
  env inputs $ \ ~( values
                  , intmap
                  , vtrie1
                  , vtrie2
                  , vtrie3
                  , atrie1
                  , atrie2
                  , key
                  ) ->
      bgroup "Creating and Inserting" [
        -- bgroup "Creating 0..100000" [
        --     bench "IntMap"        $ nf IntMap.fromList values,
        --     -- bench "Binary Trie"   $ nf Binary.fromList values,
        --     bench "Vector Trie 1" $ nf (Vector.fromList @1) values,
        --     bench "Vector Trie 2" $ nf (Vector.fromList @2) values,
        --     bench "Array Trie 1"  $ nf (Array.fromList @1) values,
        --     bench "Array Trie 2"  $ nf (Array.fromList @2) values

        --     -- bench "Vector Trie 3" $ nf (Vector.fromList @3) values,
        --     -- bench "Vector Trie 4" $ nf (Vector.fromList @4) values,
        --     -- bench "Vector Trie 5" $ nf (Vector.fromList @5) values,
        --     -- bench "Vector Trie 6" $ nf (Vector.fromList @6) values,
        --     -- bench "Vector Trie 7" $ nf (Vector.fromList @7) values,
        --     -- bench "Vector Trie 8" $ nf (Vector.fromList @8) values
        -- ],
        bgroup "Inserting into 1000000 trie" [
            bench "IntMap" $ nf (IntMap.insert key 101) intmap,
            bench "Vector Trie 1" $ nf (Vector.insert key 101) vtrie1,
            bench "Vector Trie 2" $ nf (Vector.insert key 101) vtrie2,
            bench "Array Trie 1" $ nf (Array.insert key 101) atrie1,
            bench "Array Trie 2" $ nf (Array.insert key 101) atrie2
        ]
    ]
  ]
  where inputs = do
          xs <- replicateM 500000 randomIO
          key <- randomIO
          let pairs = xs `zip` xs
          return ( pairs
                 , IntMap.fromList pairs
                 , Vector.fromList @1 pairs
                 , Vector.fromList @2 pairs
                 , Vector.fromList @3 pairs
                 , Array.fromList @1 pairs
                 , Array.fromList @2 pairs
                 , key
                 )
        
        -- An arbitrary pair of keys that has to branch relatively
        -- high with some prefix.
        k1 = 0xDEAAEFC28
        k2 = 0xDEAFEFC28

        trieTest s = s + (Vector.countTrailingNonZeros (k1 `xor` k2) - 1)
          where round x = x `div` s * s

        bControl = Binary.highestBitSet (k1 `xor` k2)
        bPrefix = Binary.getPrefix k1 bControl

        index2 = 2 + ((Vector.countTrailingNonZeros (k1 `xor` k2)) `div` 2 * 2)
        children2 = [t2Leaf1, t2Leaf2] :: Vector (Vector.Trie 2 String)
        prefix2 = Vector.getPrefix k1 index2

        bLeaf1 = Binary.Leaf k1 "abc"
        bLeaf2 = Binary.Leaf k2 "def"
        t1Leaf1 = Vector.Leaf @1 k1 "abc"
        t1Leaf2 = Vector.Leaf @1 k2 "def"
        t2Leaf1 = Vector.Leaf @2 k1 "abc"
        t2Leaf2 = Vector.Leaf @2 k2 "def"
        t8Leaf1 = Vector.Leaf @8 k1 "abc"
        t8Leaf2 = Vector.Leaf @8 k2 "abc"
