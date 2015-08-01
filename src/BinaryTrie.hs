{-# LANGUAGE MonadComprehensions #-}
-- | A big-endian binary PATRICIA trie with fixed-size ints as keys.
module BinaryTrie where

import           Prelude   hiding (lookup)

import           Data.Bits
import           Data.Monoid

import           Text.Printf

-- | A PATRICIA trie that inspects an @Int@ key bit by bit.
data Trie a = Empty
            | Leaf !Int a
              -- ^ Each leaf stores the whole key along with the value
              -- for that key.
            | Branch !Int !Int (Trie a) (Trie a) deriving (Eq, Ord)
              -- ^ Each branch stores a prefix and a control bit for
              -- the bit it branched on—an @Int@ with just that bit
              -- set.

instance Show a => Show (Trie a) where
  show Empty                       = "Empty"
  show (Leaf k v)                  = printf "Leaf %d %s" k (show v)
  show (Branch prefix control l r) = printf "(Branch %b %b %s %s)" prefix control (show l) (show r)

width :: Int
width = finiteBitSize (0 :: Int)

-- | Returns the key masked to every bit before (ie more significant)
-- than the control bit.
getPrefix :: Int -> Int -> Int
getPrefix key control = key .&. complement ((control `shiftL` 1) - 1)

-- | A smart consructor for branches of the tree tha avoids creating
-- unnecessary nodes and puts children in the correct order.
branch :: Int -> Int -> Trie a -> Trie a -> Trie a
branch _ _ Empty Empty      = Empty
branch _ _ Empty (Leaf k v) = Leaf k v
branch _ _ (Leaf k v) Empty = Leaf k v
branch prefix control l r   = Branch prefix control l r

                              -- TODO: Replace with efficient version with 7.10
countLeadingZeros :: Int -> Int
countLeadingZeros n = (width - 1) - go (width - 1)
  where go i | i < 0       = i
             | testBit n i = i
             | otherwise   = go (i - 1)

highestBitSet :: Int -> Int
highestBitSet n = bit $ width - countLeadingZeros n - 1

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
  where control = highestBitSet (pl `xor` pr)
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

fromList :: Monoid a => [(Int, a)] -> Trie a
fromList = foldr (\ (k, v) t -> insert k v t) Empty

keys :: Trie a -> [Int]
keys Empty = []
keys (Leaf k _) = [k]
keys (Branch _ _ l r) = keys l ++ keys r

merge :: Monoid a => Trie a -> Trie a -> Trie a
merge Empty t      = t
merge t Empty      = t
merge (Leaf k v) t = insert k v t
merge t (Leaf k v) = insert k v t
merge t₁@(Branch p₁ c₁ l₁ r₁) t₂@(Branch p₂ c₂ l₂ r₂)
  | p₁ == p₂ && c₁ == c₂ = branch p₁ c₁ (merge l₁ l₂) (merge r₁ r₂)
  | c₁ > c₂ && getPrefix p₂ c₁ == p₁ = checkBit p₂ c₁
                                     (branch p₁ c₁ (merge l₁ t₂) r₁)
                                     (branch p₁ c₁ l₁ (merge r₁ t₂))
  | c₂ > c₁ && getPrefix p₁ c₂ == p₂ = checkBit p₁ c₂
                                     (branch p₂ c₂ (merge t₁ l₂) r₂)
                                     (branch p₂ c₂ l₂ (merge t₁ r₂))
  | otherwise                      = combine p₁ t₁ p₂ t₂

delete :: Int -> Trie a -> Trie a
delete k Empty = Empty
delete k (Leaf k' v)
  | k == k'    = Empty
  | otherwise = Leaf k' v
delete k trie@(Branch prefix control l r)
  | getPrefix k control == prefix = checkBit k control
                                   (branch prefix control (delete k l) r)
                                   (branch prefix control l (delete k r))
  | otherwise                    = trie

intersect :: Monoid a => Trie a -> Trie a -> Trie a
intersect Empty _ = Empty
intersect _ Empty = Empty
intersect (Leaf k v) (Leaf k' v')
  | k == k'    = Leaf k (v <> v')
  | otherwise = Empty
intersect leaf@(Leaf k v) (Branch prefix control l r)
  | getPrefix k control == prefix = checkBit k control (intersect leaf l) (intersect leaf r)
  | otherwise                    = Empty
intersect (Branch prefix control l r) leaf@(Leaf k v)
  | getPrefix k control == prefix = checkBit k control (intersect l leaf) (intersect r leaf)
  | otherwise                    = Empty
intersect left@(Branch p₁ c₁ l₁ r₁) right@(Branch p₂ c₂ l₂ r₂)
  | c₁ == c₂ && p₁ == p₂              = branch p₁ c₁ (intersect l₁ l₂) (intersect r₁ r₂)
  | c₁ > c₂ && getPrefix p₂ c₁ == p₁ = checkBit p₂ c₁ (intersect l₁ right) (intersect r₁ right)
  | c₁ < c₂ && getPrefix p₁ c₂ == p₂ = checkBit p₁ c₂ (intersect left l₂) (intersect right r₂)
  | otherwise                      = Empty

-- Utility functions
b :: Int -> IO ()
b i = printf "%b\n" i
