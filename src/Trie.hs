{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
module Trie where
import Data.HashMap.Strict (HashMap, unionWith)
import Data.Hashable
import Core
import Definitions (MatchCondition)
import qualified Data.Map as M
import Data.Monoid (Alt(..))
import Control.Applicative ((<|>))


data TrieNode branch leaf where
    TrieNode :: Ord branch => branch -> M.Map branch (TrieNode branch leaf) -> Maybe leaf -> TrieNode branch leaf
deriving instance (Show branch, Show leaf) => Show (TrieNode branch leaf)

data Trie branch leaf where
    TrieRoot :: Ord branch => M.Map branch (TrieNode branch leaf) -> Trie branch leaf
deriving instance (Show branch, Show leaf) => Show (Trie branch leaf)


preferJust :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
preferJust f (Just x) (Just y) = Just $ x `f` y
preferJust _ a b = a <|> b

-- | Combine two TrieNodes using a given function to combine the leaves.
-- TODO: this should be possible combineWith :: (b -> c -> d) -> TrieNode a b -> TrieNode a c -> TrieNode a d
combineWith :: (b -> b -> b) -> TrieNode a b -> TrieNode a b -> TrieNode a b
combineWith leafOp (TrieNode b1 rest1 leaf1) (TrieNode b2 rest2 leaf2)
    | b1 == b2 = TrieNode b1
        (M.unionWith (combineWith leafOp) rest1 rest2)
        (preferJust leafOp leaf1 leaf2)
    | otherwise = error "Impossible!" -- TODO: this _could_ return a TrieRoot if I Could Figure Out How To Type It

instance (Semigroup l) => Semigroup (TrieNode b l) where
    (<>) :: TrieNode b l -> TrieNode b l -> TrieNode b l
    (<>) = combineWith (<>)

instance  (Semigroup l) => Semigroup (Trie b l) where
    (<>) :: Trie b l -> Trie b l -> Trie b l
    (TrieRoot rest1) <> (TrieRoot rest2) = TrieRoot $ M.unionWith (<>) rest1 rest2

instance (Ord b, Semigroup l) => Monoid (Trie b l) where
    mempty = TrieRoot mempty

-- | Trie construction operators. Example usage:
-- (root $ 'h' <:< 'e' <:< 'l' <:< 'l' <:< 'o' <: []) <> (root $ 'h' <:< 'e' <:< 'c' <:< 'k' <: []) <> (root $ 'h' <: [])
-- ('h' <:< 'e' <:< 'l' <:< 'l' <:< 'o' <: []) <> ('h' <:< 'e' <:< 'c' <:< 'k' <: [])
-- ('h' <:< 'e'  <: []) <> ('h' <:< 'i' <: [])
-- ('h'  <: []) <> ('h' <: [])
infixr 8 <:<, <:

-- | Wrap a TrieNode layer in a parent TrieNode 
(<:<) :: Ord b => b -> TrieNode b l -> TrieNode b l
fresh <:< trie@(TrieNode b _ _) = TrieNode fresh (M.fromList [(b, trie)]) Nothing

-- | Construct the leaf of a Trie
(<:) :: Ord b => b -> l -> TrieNode b l
fresh <: leaf = TrieNode fresh mempty (Just leaf)

-- | Construct the root of a Trie
root :: TrieNode b l -> Trie b l
root trie@(TrieNode b _ _) = TrieRoot $ M.fromList [(b, trie)]

-- Trie usage functions

-- | Add a sequence into a Trie
add :: (Ord b, Semigroup l) => [b] -> l -> Trie b l -> Trie b l
add [] _ trie = trie
add sequ leaf trie = root (go (reverse sequ)) <> trie
    where go (tip:rest) = foldl (flip (<:<)) (tip <: leaf) rest

-- | Initialize a Trie
init :: (Ord b, Semigroup l) => [b] -> l -> Trie b l
init sequ leaf = add sequ leaf mempty

-- | Check if a sequence is in a Trie
elem :: [b] -> Trie b l -> Maybe l
elem [] _ = Nothing
elem (tip:rest) (TrieRoot children) = (children M.!? tip) >>= go (tip:rest)
    where 
        go :: (Eq b) => [b] -> TrieNode b l -> Maybe l
        go [] _ = Nothing
        go (tip:[]) (TrieNode b _ leaf)
            | tip == b = leaf
            | otherwise = Nothing
        go (tip:next:rest) (TrieNode b children _)
            | tip == b = (children M.!? next) >>= go (next:rest)
            | otherwise = Nothing