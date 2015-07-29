{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A module for PATRICIA tries with configurable spans.
module Trie where

import           Data.Bits
import           Data.Proxy   (Proxy (..))
import           Data.Vector  (Vector, (!))
import qualified Data.Vector  as Vector

import           GHC.TypeLits

-- | A PATRICIA trie that inspects an @Int@ key 's' bits at a time.
data Trie (s :: Nat) a = Empty
                      | Leaf !Int a
                        -- ^ Each leaf stores the whole key along with
                        -- the value for that key.
                      | Breanch !Int !Int !(Vector (Trie s a))
                        -- ^ Each branch stores \(2^s\) children along
                        -- with a prefix and control mask which
                        -- specifies the portion of the key that was
                        -- branched on.
                      deriving (Show, Eq, Ord)

isLeaf :: Trie s a -> Bool
isLeaf Leaf{} = True
isLeaf _      = False

-- | Get the span of a tree from its type. (The given argument is discarded!)
span :: forall s a. KnownNat s => Trie s a -> Int
span _ = fromInteger $ natVal (Proxy :: Proxy s)

width :: Int
width = finiteBitSize (0 :: Int)

         -- TODO: There's probably a better way to do this…
-- | Masks out everything after and including the section under the control masked.
--
--   Example: 
--   @
--   getPrefix 3 0b111111111 0b111000 = 0b111000000
--   @
getPrefix :: Int -> Int -> Int -> Int
getPrefix span key control = (key .|. (level - 1)) .&. complement level
  where level = control .&. (control `shiftR` span - 1)
