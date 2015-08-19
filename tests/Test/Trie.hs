{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Trie where

import           Trie                  (Trie)
import qualified Trie

import           Control.Applicative   ((<$>))

import qualified Data.List             as List

import           Data.Bits

import           Test.QuickCheck
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

instance Arbitrary (Trie 2 Int) where
  arbitrary = Trie.fromList <$> arbitrary

tests :: [TestTree]
tests = [
  QC.testProperty "countLeadingZeros" prop_countLeadingZeros
  ]

prop_countLeadingZeros x = Trie.countTrailingNonZeros x == Trie.width - countLeadingZeros' x
  where countLeadingZeros' n = (Trie.width - 1) - go (Trie.width - 1)
          where go i | i < 0       = i
                     | testBit n i = i
                     | otherwise   = go (i - 1)

