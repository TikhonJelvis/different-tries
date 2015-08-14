module Main where

import qualified Test.BinaryTrie as BinaryTrie

import Test.Tasty

main = defaultMain tests

tests :: TestTree
tests = testGroup "Test" [binaryTrie]

binaryTrie :: TestTree
binaryTrie = testGroup "BinaryTrie" BinaryTrie.tests

