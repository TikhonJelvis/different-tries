{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PatternSynonyms     #-}
-- | A PATRICIA binary trie with fixed-size ints as keys.
module Trie where

import           Prelude   hiding (lookup)

import           Data.Bits
import           Data.Monoid

-- | A PATRICIA trie that inspects and @Int@ bit by bit.
data Trie a = Empty
            | Leaf !Int a
              -- ^ Each leaf stores the whole key along with the value
              -- for that key.
            | Branch !Int !Int (Trie a) (Trie a) deriving (Show, Eq, Ord)
              -- ^ Each branch stores a prefix and a control bit for
              -- the bit it branched on—an @Int@ with just that bit
              -- set.

width :: Int
width = finiteBitSize (0 :: Int)

-- | Returns the key masked to every bit before (ie less significant)
-- than the control bit.
getPrefix :: Int -> Int -> Int
getPrefix key control = key .&. (control - 1)

-- | Returns the bit at which the two numbers first differ.
firstDiff :: Int -> Int -> Int
firstDiff a b = let x = a `xor` b in x .&. (-x)

-- | A smart consructor for branches of the tree tha avoids creating
-- unnecessar nodes and puts children in the correct order.
branch :: Int -> Int -> Trie a -> Trie a -> Trie a
branch _ _ Empty Empty      = Empty
branch _ _ Empty (Leaf k v) = Leaf k v
branch _ _ (Leaf k v) Empty = Leaf k v
branch prefix control l r   = Branch prefix control l r

-- | Branches on whether the bit of the key at the given control bit
-- is 0 (left) or 1 (right).
checkBit :: Int -> Int -> a -> a -> a
checkBit k control left right = if k .&. control == 0 then left else right

-- | We look a value up by branching based on bits. If we ever hit a
-- leaf node, we check whether our whole key matches. If we ever hit
-- an empty node, we know the key is not in the trie.
lookup :: Int -> Trie a -> Maybe a
lookup _ Empty              = Nothing
lookup k (Leaf k' v)        = [v | k == k']
lookup k (Branch prefix control l r)
  | getPrefix k control /= prefix = Nothing
  | otherwise                    = lookup k (checkBit k control l r)

-- | Combines two trees with two different prefixes.
combine :: Int -> Trie a -> Int -> Trie a -> Trie a
combine pl l pr r = checkBit pl control
                    (branch prefix control l r)
                    (branch prefix control r l)
  where control = firstDiff pl pr
        prefix  = getPrefix pl control

insert :: Monoid a => Int -> a -> Trie a -> Trie a
insert k v Empty        = Leaf k v
insert k v (Leaf k' v')
  | k == k'    = Leaf k (v <> v')
  | otherwise = combine k (Leaf k v) k' (Leaf k' v')
insert k v trie@(Branch prefix control l r)
  | getPrefix k control == prefix = checkBit k control
                                   (branch prefix control (insert k v l) r)
                                   (branch prefix control l (insert k v r))
  | otherwise                    = combine k (Leaf k v) prefix trie
