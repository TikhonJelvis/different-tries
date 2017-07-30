{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Binary where

import           Trie.Binary           (Trie)
import qualified Trie.Binary           as BinaryTrie

import           Control.Applicative   ((<$>))

import qualified Data.List             as List

import           Test.QuickCheck
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

instance Arbitrary (Trie Int) where
  arbitrary = BinaryTrie.fromList <$> arbitrary

tests :: [TestTree]
tests = [
  QC.testProperty "to-from list" prop_to_from_list,
  QC.testProperty "insert-lookup" prop_insert_lookup,
  QC.testProperty "insert-insert" prop_insert_insert,
  QC.testProperty "merge" prop_merge
  ]

  -- TODO: Figure out what to do with negative keys!
prop_to_from_list ks' vs = BinaryTrie.toList trie == zip ks vs
  where ks   = List.sort $ List.nub $ List.filter (>= 0) ks'
        trie = BinaryTrie.fromList (zip ks vs) :: Trie Int

prop_insert_lookup k v (trie :: Trie Int) = BinaryTrie.lookup k (BinaryTrie.insert k v trie) == Just v

prop_insert_insert k v v' (trie :: Trie Int) =
  BinaryTrie.lookup k trie' == Just v'
  where trie' = BinaryTrie.insert k v' $ BinaryTrie.insert k v trie

prop_merge :: Trie Int -> Trie Int -> Bool
prop_merge t_1 t_2 = all check keys
  where keys = BinaryTrie.keys t_1 ++ BinaryTrie.keys t_2
        check k = BinaryTrie.lookup k merged == BinaryTrie.lookup k t_1 ||
                  BinaryTrie.lookup k merged == BinaryTrie.lookup k t_2
        merged = BinaryTrie.merge t_1 t_2
