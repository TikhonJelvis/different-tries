{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A module for PATRICIA tries with configurable spans.
module Trie where

import           Prelude      hiding (lookup, span)

import           Data.Bits
import           Data.List    (foldl')
import           Data.Monoid  (Monoid, (<>))
import           Data.Proxy   (Proxy (..))
import           Data.Vector  (Vector, (!), (//))
import qualified Data.Vector  as Vector
import           Data.Word    (Word64)

import           GHC.TypeLits

import           Text.Printf (printf)

-- | A PATRICIA trie that inspects an @Int@ key 's' bits at a time.
data Trie (s :: Nat) a = Empty
                      | Leaf !Int a
                        -- ^ Each leaf stores the whole key along with
                        -- the value for that key.
                      | Branch !Int !Int !(Vector (Trie s a))
                        -- ^ Each branch stores \(2^s\) children along
                        -- with a prefix and the level in the trie.
                      deriving (Eq, Ord)

instance Show a => Show (Trie s a) where
  show Empty                          = "Empty"
  show (Leaf k v)                     = printf "Leaf %d %s" k (show v)
  show (Branch prefix level children) = printf "Branch 0b%b %d %s" prefix level (show children)

isEmpty :: Trie s a -> Bool
isEmpty Empty = True
isEmpty _     = False

isLeaf :: Trie s a -> Bool
isLeaf Leaf{} = True
isLeaf _      = False

-- | Get the span of a tree from its type. (The given argument is discarded!)
span :: forall s a. KnownNat s => Trie s a -> Int
span _ = fromInteger $ natVal (Proxy :: Proxy s)

width :: Int
width = finiteBitSize (0 :: Int)

         -- TODO: There's probably a better way to do this…
-- | Masks out everything after and including the section under the control masked
-- for the given level.
--
-- Example:
-- @
-- getPrefix 3 0b111111111 0b111000 = 0b111000000
-- @
getPrefix :: Int -> Int -> Int -> Int
getPrefix span key level = key .&. ((-1) `shiftL` (span * level'))
  where level' = (width `div` span) - level - 1

-- | A "smart constructor" that consolidates paths that either end or
-- don't branch (ie have exactly one child leaf).
branch :: Int -> Int -> Vector (Trie s a) -> Trie s a
branch prefix level children
  | Vector.all isEmpty children = Empty
  | [leaf] <- oneLeaf            = leaf
  | otherwise                   = Branch prefix level children
  where oneLeaf = [leaf |
                   Vector.length filtered == 1,
                   let leaf = Vector.unsafeHead filtered,
                   isLeaf leaf]
        filtered = Vector.filter (not . isEmpty) children

empties :: forall s a. KnownNat s => Vector (Trie s a)
empties = Vector.replicate (2 ^ span (undefined :: Trie s a)) Empty

                               -- TODO: Replace with efficient version with 7.10
countLeadingZeros :: Int -> Int
countLeadingZeros n = (width - 1) - go (width - 1)
  where go i | i < 0       = i
             | testBit n i = i
             | otherwise   = go (i - 1)

highestBitSet :: Int -> Int
highestBitSet n = bit $ width - countLeadingZeros n - 1

mask :: Int -> Int -> Int
mask span level = (bit span - 1) `shiftL` (level' * span)
  where level' = width `div` span - level

getIndex :: Int -> Int -> Int -> Int
getIndex span key level = (mask span level .&. key) `shiftR` (level' * span)
  where level' = width `div` span - level

lookup :: KnownNat s => Int -> Trie s a -> Maybe a
lookup _ Empty       = Nothing
lookup k (Leaf k' v) = [v | k == k']
lookup k t@(Branch prefix level children)
  | getPrefix (span t) k level /= prefix = Nothing
  | otherwise                           = lookup k (children ! index)
  where index = getIndex (span t) k level

insert :: forall s a. (KnownNat s, Monoid a) => Int -> a -> Trie s a -> Trie s a
insert k v Empty        = Leaf k v
insert k v t@(Leaf k' v')
  | k == k' = Leaf k (v <> v')
  | otherwise = branch prefix level newChildren
  where level = countLeadingZeros (k `xor` k') `div` s + 1
        newChildren = empties // [(getIndex s k level, Leaf k v),
                                  (getIndex s k' level, Leaf k' v')]
        prefix = getPrefix s k level
        s = span t
insert k v t@(Branch prefix level children)
  | getPrefix s k level == prefix = branch prefix level newChildren
   where newChildren = modify children (getIndex s k level) (insert k v)
         s = span t

      -- TODO: figure out how to do this properly?
modify :: Vector a -> Int -> (a -> a) -> Vector a
modify v i f = v // [(i, f $ v ! i)]
