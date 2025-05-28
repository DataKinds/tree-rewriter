module Invariants where

import Trie
import Data.Maybe (isJust)
import Data.List (nubBy, sort)
import Data.Function (on)

-- Trie invariant predicates
toTrieInverseIsFromTrie :: (Ord b, Eq a) => Trie b [a] -> Bool
toTrieInverseIsFromTrie trie = trie == toTrie (fromTrie trie)

fromTrieInverseIsToTrie :: (Ord b, Semigroup l, Ord l) => [([b], l)] -> Bool
fromTrieInverseIsToTrie t = let 
    t' = filter (not . null . fst) t -- Don't try adding empty sequences to the Trie
    t'' = nubBy (\x y -> fst x == fst y) t' -- Don't try adding identical sequences to the Trie
    in ((==) `on` sort) (fromTrie . toTrie $ t'') t''

type TrieWithElemPred b leaf = b -> [b] -> Trie b leaf -> Bool
addOnTrieSupportsElemCheck :: Ord b => TrieWithElemPred b [leaf]
addOnTrieSupportsElemCheck tip sequ trie = let
    sequ' = tip:sequ in isJust $ Trie.elem sequ' (add sequ' [] trie)