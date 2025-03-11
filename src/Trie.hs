{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Trie where
import Data.HashMap.Strict (HashMap, unionWith)
import Data.Hashable

data Matcher a where
    Hole :: Matcher a
    Exactly :: a -> Matcher a         
deriving instance Eq a => Eq (Matcher a)
instance Hashable a => Hashable (Matcher a) where
    hashWithSalt s Hole = s `hashWithSalt` (0::Int)
    hashWithSalt s (Exactly a) = s `hashWithSalt` a

data Trie hay tip where 
    Sentinel :: tip -> Trie hay tip
    Nest :: HashMap (Matcher hay) (Trie hay tip) -> Trie hay tip

instance Hashable hay => Monoid (Trie hay tip) where
    mempty = Nest mempty 

instance Hashable hay => Semigroup (Trie hay tip) where
    (Sentinel x) <> (Sentinel y) = error "Nonsense combo"
    (Nest xs) <> (Nest ys) = Nest $ unionWith (<>) xs ys
    (Sentinel x) <> (Nest ys) = Nest $ unionWith (<>) xs ys
    (Nest xs) <> (Nest ys) = Nest $ unionWith (<>) xs ys


