{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MonadComprehensions        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- | A module for PATRICIA tries with configurable spans.
module Trie.Array where

import           Prelude         hiding (lookup, span)

import           Control.DeepSeq (NFData (..), deepseq)

import           Data.Array      (Array, (!), (//))
import qualified Data.Array      as Array
import           Data.Bits
import           Data.List       (foldl')
import           Data.Monoid     (Monoid, (<>))
import           Data.Proxy      (Proxy (..))
import           Data.Word       (Word64)

import           GHC.TypeLits

import           Text.Printf     (printf)

-- | A PATRICIA trie that inspects an @Int@ key 's' bits at a time.
data Trie (s :: Nat) a
  = Empty
  | Leaf {-# UNPACK #-} !Int a
    -- ^ Each leaf stores the whole key along with
    -- the value for that key.
  | Branch {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !(Array Int (Trie s a))
    -- ^ Each branch stores \(2^s\) children along with a prefix and
    -- the index of the leading bit branched on.
  deriving (Eq, Ord)

instance Show a => Show (Trie s a) where
  show Empty                          = "Empty"
  show (Leaf k v)                     = printf "Leaf %d %s" k (show v)
  show (Branch prefix index children) = printf "Branch 0b%b %d %s" prefix index (show children)

instance NFData (Trie s a) where
  rnf Empty                   = ()
  rnf (Leaf !k !v)            = ()
  rnf (Branch !_ !_ children) = rnf children `seq` ()

isEmpty :: Trie s a -> Bool
isEmpty Empty = True
isEmpty _     = False

isLeaf :: Trie s a -> Bool
isLeaf Leaf{} = True
isLeaf _      = False

-- | Get the span of a tree from its type. (The given argument is
-- discarded!)
span :: forall s a. KnownNat s => Trie s a -> Int
span _ = fromInteger $ natVal $ Proxy @s
{-# INLINE span #-}

width :: Int
width = finiteBitSize (0 :: Int)

         -- TODO: There's probably a better way to do this…
-- | Masks out everything less significant and including the bit at
-- the given index.
--
-- Example:
-- @
-- getPrefix 3 0b111111111 6 = 0b111000000
-- @
getPrefix :: Int -> Int -> Int
getPrefix key index = key .&. ((- 1) `shiftL` index)

-- | Get the chunk of the key for the given bit index, shifted all the
-- way right.
getChunk :: Int -> Int -> Int -> Int
getChunk span key index = (key `shiftR` (index - span)) .&. (bit span - 1)

-- | A "smart constructor" that consolidates paths that either end or
-- don't branch (ie have exactly one child leaf).
branch :: Int -> Int -> Array Int (Trie s a) -> Trie s a
branch prefix index children = case go first (Just Empty) of
  Just x  -> x
  Nothing -> Branch prefix index children
  where (first, last) = Array.bounds children

        go i x | i > last               = x
        go i (Just Empty)               = go (i + 1) $ Just $ children ! i
        go i x | isEmpty (children ! i) = go (i + 1) x
        go _ _                          = Nothing
{-# INLINE branch #-}

countTrailingNonZeros :: Int -> Int
countTrailingNonZeros n = finiteBitSize n - countLeadingZeros n
{-# INLINE countTrailingNonZeros #-}

-- | Gets the index on which a tree of span s branches on the given
-- key. The key has to be non-zero!
getIndex :: Int -> Int -> Int
getIndex s x = let n = countTrailingNonZeros x - 1 in n + s - (n `rem` s)
{-# INLINE getIndex #-}

lookup :: KnownNat s => Int -> Trie s a -> Maybe a
lookup _ Empty       = Nothing
lookup k (Leaf k' v) = [v | k == k']
lookup k t@(Branch prefix index children)
  | getPrefix k index /= prefix = Nothing
  | otherwise                   = lookup k (children ! chunk)
  where chunk = getChunk (span t) k index

-- | A helper function that combines two trees with two *different*,
-- *non-overlapping* prefixes. Make sure the prefixes don't overlap
-- before using this function!
combine :: KnownNat s => Int -> Trie s a -> Int -> Trie s a -> Trie s a
combine p₁ t₁ p₂ t₂ = branch prefix index newChildren
  where newChildren = Array.listArray (0, bound) $ go <$> [0..bound]
          where go x | x == chunk₁ = t₁
                     | x == chunk₂ = t₂
                     | otherwise   = Empty

        chunk₁ = getChunk s p₁ index
        chunk₂ = getChunk s p₂ index
        
        index = getIndex s $ p₁ `xor` p₂
        prefix = getPrefix p₁ index
        s = span t₁
        bound = 2 ^ s - 1
{-# INLINE combine #-}

-- -- | A faster version of combine that does not collapse lone Empty and
-- -- Leaf nodes.
-- combine' :: KnownNat s => Int -> Trie s a -> Int -> Trie s a -> Trie s a
-- combine' p₁ t₁ p₂ t₂ = Branch prefix index newChildren
--   where newChildren = Vector.unsafeUpd empties [(getChunk s p₁ index, t₁),
--                                                 (getChunk s p₂ index, t₂)]
--         empties = Vector.replicate (2^s) Empty
--         index = getIndex s $ p₁ `xor` p₂
--         prefix = getPrefix p₁ index
--         s = span t₁

insertWith :: forall s a. KnownNat s => (a -> a -> a) -> Int -> a -> Trie s a -> Trie s a
insertWith (⊗) !k v Empty = Leaf k v
insertWith (⊗) !k v t@(Leaf k' v')
  | k == k'   = Leaf k (v ⊗ v')
  | otherwise = combine k (Leaf k v) k' (Leaf k' v')
insertWith (⊗) !k v trie@(Branch prefix index children)
  | getPrefix k index == prefix = branch prefix index newChildren
  | otherwise                   = combine k (Leaf k v) prefix trie
   where newChildren =
           children // [(getChunk s k index, insertWith (⊗) k v old)]
         old = children ! getChunk s k index
         s = span trie

insert :: forall s a. KnownNat s => Int -> a -> Trie s a -> Trie s a
insert = insertWith const

fromList :: KnownNat s => [(Int, a)] -> Trie s a
fromList = foldr (\ (k, v) t -> insert k v t) Empty

toList :: KnownNat s => Trie s a -> [(Int, a)]
toList Empty                 = []
toList (Leaf k v)            = [(k, v)]
toList (Branch _ _ children) = Array.elems children >>= toList

keys :: KnownNat s => Trie s a -> [Int]
keys = map fst . toList

values :: KnownNat s => Trie s a -> [a]
values = map snd . toList

--                          -- TODO: Figure out ordering in insert case
-- mergeWith :: KnownNat s => (a -> a -> a) -> Trie s a -> Trie s a -> Trie s a
-- mergeWith _ Empty t      = t
-- mergeWith _ t Empty      = t
-- mergeWith f (Leaf k v) t = insertWith f k v t
-- mergeWith f t (Leaf k v) = insertWith f k v t
--   -- t_1 instead of t₁ to deal with GHC parsing bug with visible type
--   -- applications
-- mergeWith f t_1@(Branch p₁ i₁ cs₁) t_2@(Branch p₂ i₂ cs₂)
--   -- prefixes exactly the same:
--   | i₁ == i₂ && p₁ == p₂ = branch p₁ i₁ $ Vector.zipWith (mergeWith f) cs₁ cs₂
--   -- branching on t_1:
--   | i₁ > i₂ && getPrefix p₂ i₁ == p₁ = branch p₁ i₁ $
--       modify cs₁ (getChunk s p₂ i₁) (mergeWith (flip f) t_2)
--   -- branching on t_2:
--   | i₂ > i₁ && getPrefix p₁ i₂ == p₂ = branch p₂ i₂ $
--       modify cs₂ (getChunk s p₁ i₂) (mergeWith f t_1)
--   -- prefixes don't overlap:
--   | otherwise = combine p₁ t_1 p₂ t_2
--   where s = span t_1

-- merge :: KnownNat s => Trie s a -> Trie s a -> Trie s a
-- merge = mergeWith const

-- -- Utility functions
-- b :: Int -> IO ()
-- b i = printf "%b\n" i

