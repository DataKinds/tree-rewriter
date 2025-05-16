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

data Matcher a where
    Hole :: Matcher a
    Nest :: Matcher a
    Unnest :: Matcher a
    Exactly :: Eq a => a -> Matcher a
deriving instance Eq a => Eq (Matcher a)
instance Hashable a => Hashable (Matcher a) where
    hashWithSalt s Hole = s `hashWithSalt` (0::Int)
    hashWithSalt s (Exactly a) = s `hashWithSalt` a

data TreeTrie hay tip =  
      Choice [TreeTrie hay tip] 
    | Sentinel tip 
    | Match (Matcher hay) (TreeTrie hay tip)

insertMatchCond :: MatchCondition -> tip -> TreeTrie hay tip -> TreeTrie hay tip
insertMatchCond = undefined

-- instance Hashable hay => Monoid (Trie hay tip) where
--     mempty = Nest mempty 

-- instance Hashable hay => Semigroup (Trie hay tip) where
--     (Sentinel x) <> (Sentinel y) = error "Nonsense combo"
--     (Nest xs) <> (Nest ys) = Nest $ unionWith (<>) xs ys
--     (Sentinel x) <> (Nest ys) = Nest $ unionWith (<>) xs ys
--     (Nest xs) <> (Nest ys) = Nest $ unionWith (<>) xs ys


data Trie branch leaf where
    TrieRoot :: Ord branch => M.Map branch (Trie branch leaf) -> Trie branch leaf
    TrieNode :: Ord branch => branch -> Either leaf (M.Map branch (Trie branch leaf)) -> Trie branch leaf
deriving instance (Show branch, Show leaf) => Show (Trie branch leaf)

instance Semigroup (Trie a b) where
    (<>) :: Trie a b -> Trie a b -> Trie a b
    t1@(TrieRoot ts1) <> t2@(TrieRoot ts2) = undefined

instance Ord a => Monoid (Trie a b) where
    mempty = TrieRoot mempty

-- | Concatenate a node onto this layer of the trie. 
(<:>) :: Trie b l -> b -> Trie b l
t@(TrieRoot ts) <:> fresh = case M.lookup fresh ts of
    Just _ -> t
    Nothing -> TrieRoot $ M.insert fresh (TrieNode fresh (Right mempty)) ts
t@(TrieNode b (Right ts)) <:> fresh = case M.lookup fresh ts of
    Just _ -> t
    Nothing -> TrieNode b . Right $ M.insert fresh (TrieNode fresh (Right mempty)) ts
TrieNode b (Left leaf) <:> fresh = TrieNode b . Right $ M.fromList [(fresh, TrieNode fresh (Left leaf))]


-- -- | Add an entry into a trie.
-- add :: [branch] -> leaf -> Trie branch leaf -> Trie branch leaf
-- add xs l t = t <> (foldr (TrieNode x ))
--     where
