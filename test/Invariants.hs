module Invariants where

import Trie
import Data.Maybe (isJust)
import Data.List (nubBy)

-- Trie invariants
toTrieInverseIsFromTrie :: (Ord b, Eq a) => Trie b [a] -> Bool
toTrieInverseIsFromTrie trie = trie == toTrie (fromTrie trie)

fromTrieInverseIsToTrie :: (Ord b, Semigroup l, Eq l) => [([b], l)] -> Bool
fromTrieInverseIsToTrie t = let t' = nubBy (\x y -> fst x == fst y) $ filter (not . null . fst) t in fromTrie (toTrie t') == t'

prop_TrieRespectsAdditionOfSingleElem :: Ord b => b -> [b] -> Trie b [a] -> Bool
prop_TrieRespectsAdditionOfSingleElem tip sequ trie = let
    sequ' = tip:sequ in isJust $ Trie.elem sequ' (add sequ' [] trie)

prop_TrieRespectsAdditionOfSingleSimpleChar :: SimpleChar -> [SimpleChar] -> Trie SimpleChar [Int] -> Bool
prop_TrieRespectsAdditionOfSingleSimpleChar = prop_TrieRespectsAdditionOfSingleElem
