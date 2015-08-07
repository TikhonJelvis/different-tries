module Test.BinaryTrie where

import           BinaryTrie      (Trie)
import qualified BinaryTrie

import qualified Data.List       as List

import           Test.QuickCheck

prop_to_from_list ks' vs = BinaryTrie.toList trie == zip ks vs
  where ks   = List.sort $ List.nub $ List.filter (>= 0) ks'
        trie = BinaryTrie.fromList (zip ks vs) :: Trie Int
