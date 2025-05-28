module Invariants where

import Trie
import Data.Maybe (isJust)

-- Trie invariants
toTrieInverseIsFromTrie :: (Ord b, Eq a) => Trie b [a] -> Bool
toTrieInverseIsFromTrie trie = trie == toTrie (fromTrie trie)

fromTrieInverseIsToTrie :: (Ord b, Semigroup l, Eq l) => [([b], l)] -> Bool
fromTrieInverseIsToTrie t = fromTrie (toTrie t) == t

prop_TrieRespectsAdditionOfSingleElem :: Ord b => b -> [b] -> Trie b [a] -> Bool
prop_TrieRespectsAdditionOfSingleElem tip sequ trie = isJust $ Trie.elem (tip:sequ) (add (tip:sequ) [] trie)

prop_TrieRespectsAdditionOfSingleSimpleChar :: SimpleChar -> [SimpleChar] -> Trie SimpleChar [Int] -> Bool
prop_TrieRespectsAdditionOfSingleSimpleChar = prop_TrieRespectsAdditionOfSingleElem
