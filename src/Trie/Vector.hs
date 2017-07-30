{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MonadComprehensions        #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- | A module for PATRICIA tries with configurable spans.
module Trie.Vector where

import           Prelude         hiding (lookup, span)

import           Control.DeepSeq (NFData (..), deepseq)

import           Data.Bits
import           Data.List       (foldl')
import           Data.Monoid     (Monoid, (<>))
import           Data.Proxy      (Proxy (..))
import           Data.Vector     (Vector, (!))
import qualified Data.Vector     as Vector
import           Data.Word       (Word64)

import           GHC.Stack       (HasCallStack)
import           GHC.TypeLits

import           Text.Printf     (printf)

-- | A PATRICIA trie that inspects an @Int@ key 's' bits at a time.
data Trie (s :: Nat) a = Empty
                      | Leaf !Int a
                        -- ^ Each leaf stores the whole key along with
                        -- the value for that key.
                      | Branch !Int !Int !(Vector (Trie s a))
                        -- ^ Each branch stores \(2^s\) children along
                        -- with a prefix and the index of the leading
                        -- bit branched on.
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

-- | Get the span of a tree from its type. (The given argument is discarded!)
span :: forall s a. KnownNat s => Trie s a -> Int
span _ = fromInteger $ natVal (Proxy :: Proxy s)
{-# INLINE span #-}

width :: Int
width = finiteBitSize (0 :: Int)

         -- TODO: There's probably a better way to do this…
-- | Masks out everything less significant and including the bit at the given index.
--
-- Example:
-- @
-- getPrefix 3 0b111111111 6 = 0b111000000
-- @
getPrefix :: Int -> Int -> Int
getPrefix key index = key .&. ((- 1) `shiftL` index)

-- | Get the chunk of the key for the given bit index, shifted all the
-- way right.
--
-- For example, with a span of 8, the key @011000111100@ is broken up
-- into @01 10 00 11 11 00@, indexed from the least-significant chunk
-- starting with 0.
--
-- Keep in mind that the total number of bits in a key depends on your
-- platform—probably 32 or 64.)
getChunk :: Int -- ^ span of tree (# of bits to read at a time)
         -> Int -- ^ key
         -> Int -- ^ which chunk of the key to get
         -> Int
getChunk span (fromIntegral -> key) index =
  (fromIntegral @Word $ key `shiftR` (index * span)) .&. (bit span - 1)
-- note that we convert the Int key to a Word to get a logical
-- (unsigned) right shift.

-- | A "smart constructor" that consolidates paths that either end or
-- don't branch (ie have exactly one child leaf).
branch :: Int -> Int -> Vector (Trie s a) -> Trie s a
branch prefix index children = case Vector.foldl' go (Just Empty) children of
  Just x  -> x
  Nothing -> Branch prefix index children
  where go (Just Empty) x = Just x
        go x Empty        = x
        go _ _            = Nothing
{-# INLINE branch #-}

countTrailingNonZeros :: Int -> Int
countTrailingNonZeros n = finiteBitSize n - countLeadingZeros n

-- | Gets the index on which a tree of span s branches on the given key.
-- The key has to be positive!
getIndex :: Int -> Int -> Int
getIndex s x = let n = countTrailingNonZeros x - 1 in n + s - (n `rem` s)

lookup :: (HasCallStack, KnownNat s) => Int -> Trie s a -> Maybe a
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
  where newChildren = Vector.generate s go
          where go x | x == getChunk s p₁ index = t₁
                     | x == getChunk s p₂ index = t₂
                     | otherwise               = Empty
        index = getIndex s $ p₁ `xor` p₂
        prefix = getPrefix p₁ index
        s = span t₁

-- -- | A faster version of combine that does not collapse lone Empty and
-- -- Leaf nodes.
-- combine' :: (HasCallStack, KnownNat s)
--          => Int -> Trie s a -> Int -> Trie s a -> Trie s a
-- combine' p₁ t₁ p₂ t₂ = Branch prefix index newChildren
--   where newChildren = Vector.update empties [ (getChunk s p₁ index, t₁)
--                                             , (getChunk s p₂ index, t₂) ]
--         empties = Vector.replicate (2^s) Empty
--         index = getIndex s $ p₁ `xor` p₂
--         prefix = getPrefix p₁ index
--         s = span t₁

insertWith :: forall s a. (HasCallStack, KnownNat s)
           => (a -> a -> a) -> Int -> a -> Trie s a -> Trie s a
insertWith (⊗) !k v Empty = Leaf k v
insertWith (⊗) !k v t@(Leaf k' v')
  | k == k' = Leaf k (v ⊗ v')
  | otherwise = combine k (Leaf k v) k' (Leaf k' v')
insertWith (⊗) !k v trie@(Branch prefix index children)
  | getPrefix k index == prefix = branch prefix index newChildren
  | otherwise                   = combine k (Leaf k v) prefix trie
   where newChildren =
           modify children (getChunk s k index) (insertWith (⊗) k v)
         s = span trie

insert :: forall s a. KnownNat s => Int -> a -> Trie s a -> Trie s a
insert = insertWith const

fromList :: KnownNat s => [(Int, a)] -> Trie s a
fromList = foldr (\ (k, v) t -> insert k v t) Empty

      -- TODO: figure out how to do this properly?
modify :: HasCallStack => Vector a -> Int -> (a -> a) -> Vector a
modify v i f = Vector.update v [(i, f $ v ! i)]

toList :: KnownNat s => Trie s a -> [(Int, a)]
toList Empty                 = []
toList (Leaf k v)            = [(k, v)]
toList (Branch _ _ children) = Vector.toList children >>= toList

keys :: KnownNat s => Trie s a -> [Int]
keys = map fst . toList

values :: KnownNat s => Trie s a -> [a]
values = map snd . toList

                         -- TODO: Figure out ordering in insert case
mergeWith :: KnownNat s => (a -> a -> a) -> Trie s a -> Trie s a -> Trie s a
mergeWith _ Empty t      = t
mergeWith _ t Empty      = t
mergeWith f (Leaf k v) t = insertWith f k v t
mergeWith f t (Leaf k v) = insertWith f k v t
  -- t_1 instead of t₁ to deal with GHC parsing bug with visible type
  -- applications
mergeWith f t_1@(Branch p₁ i₁ cs₁) t_2@(Branch p₂ i₂ cs₂)
  -- prefixes exactly the same:
  | i₁ == i₂ && p₁ == p₂ = branch p₁ i₁ $ Vector.zipWith (mergeWith f) cs₁ cs₂
  -- branching on t_1:
  | i₁ > i₂ && getPrefix p₂ i₁ == p₁ = branch p₁ i₁ $
      modify cs₁ (getChunk s p₂ i₁) (mergeWith (flip f) t_2)
  -- branching on t_2:
  | i₂ > i₁ && getPrefix p₁ i₂ == p₂ = branch p₂ i₂ $
      modify cs₂ (getChunk s p₁ i₂) (mergeWith f t_1)
  -- prefixes don't overlap:
  | otherwise = combine p₁ t_1 p₂ t_2
  where s = span t_1

merge :: KnownNat s => Trie s a -> Trie s a -> Trie s a
merge = mergeWith const

-- Utility functions
b :: Int -> IO ()
b i = printf "%b\n" i
