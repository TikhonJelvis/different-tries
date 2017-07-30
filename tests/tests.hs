module Main where

import qualified Test.Binary as Binary
import qualified Test.Vector as Vector

import           Test.Tasty

main = defaultMain tests

tests :: TestTree
tests = testGroup "Test" [binaryTrie]

binaryTrie :: TestTree
binaryTrie = testGroup "Binary Trie" Binary.tests

vector :: TestTree
vector = testGroup "Trie" Vector.tests
