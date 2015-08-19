module Main where

import qualified Test.BinaryTrie as BinaryTrie
import qualified Test.Trie       as Trie

import           Test.Tasty

main = defaultMain tests

tests :: TestTree
tests = testGroup "Test" [binaryTrie, trie]

binaryTrie :: TestTree
binaryTrie = testGroup "BinaryTrie" BinaryTrie.tests

trie :: TestTree
trie = testGroup "Trie" Trie.tests
