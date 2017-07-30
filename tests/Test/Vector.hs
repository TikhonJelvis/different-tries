{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Vector where

import           Trie.Vector                  (Trie)
import qualified Trie.Vector as Vector

import           Control.Applicative   ((<$>))

import qualified Data.List             as List

import           Data.Bits

import           Test.QuickCheck
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

instance Arbitrary (Trie 2 Int) where
  arbitrary = Vector.fromList <$> arbitrary

tests :: [TestTree]
tests = [
  QC.testProperty "countLeadingZeros" prop_countLeadingZeros
  ]

prop_countLeadingZeros x = Vector.countTrailingNonZeros x == Vector.width - countLeadingZeros' x
  where countLeadingZeros' n = (Vector.width - 1) - go (Vector.width - 1)
          where go i | i < 0       = i
                     | testBit n i = i
                     | otherwise   = go (i - 1)

