{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PatternSynonyms     #-}
-- | A basic binary trie with fixed-size ints as keys.
module Trie where

import           Prelude   hiding (lookup)

import           Data.Bits

-- | A trie with a branching factor of 2 that inspects and @Int@
-- bit by bit.
data Trie a = Empty
            | Leaf Int a
              -- ^ Each leaf stores the whole key along with the value
              -- for that key.
            | Branch Int (Trie a) (Trie a) deriving (Show, Eq, Ord)
              -- ^ Each branch stores a control bit for the bit it
              -- branched on—an @Int@ with just that bit set.

width :: Int
width = finiteBitSize (0 :: Int)

                      -- TODO: Replace with efficient clz primop from 7.10!
countLeadingZeros :: Int -> Int
countLeadingZeros x = (width - 1) - go (width - 1)
  where go i | i < 0 = i
             | testBit x i = i
             | otherwise = go (i - 1)

-- | Calculates the branch control value for differentiating between
-- the two given keys.
firstDiff :: Int -> Int -> Int
firstDiff k k' = bit $ width - countLeadingZeros (k `xor` k') - 1

-- | A smart consructor for branches of the tree tha avoids creating
-- unnecessar nodes and puts children in the correct order.
branch :: Int -> Trie a -> Trie a -> Trie a
branch _ Empty Empty      = Empty
branch _ Empty (Leaf k v) = Leaf k v
branch _ (Leaf k v) Empty = Leaf k v
branch control l r        = Branch control l r

-- | We look a value up by branching based on bits. If we ever hit a
-- leaf node, we check whether our whole key matches. If we ever hit
-- an empty node, we know the key is not in the trie.
lookup :: Int -> Trie a -> Maybe a
lookup _ Empty              = Nothing
lookup k (Leaf k' v)        = [v | k == k']
lookup k (Branch level l r)
  | k .&. level == 0 = lookup k l
  | otherwise       = lookup k r
