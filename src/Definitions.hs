
-- This module implements the datatypes and functions required to
-- manage Rosin rule definitions.
module Definitions where
import Core (Tree, RValue)
import qualified Multiset as MS 

-- is this definition single use or will it apply forever?
data UseCount = UseOnce | UseMany deriving (Show, Eq)

instance Semigroup UseCount where
    UseMany <> _ = UseMany
    UseOnce <> b = b

instance Monoid UseCount where
    mempty = UseOnce -- identity of <>

-- What's required for this definition to match?
data MatchCondition = TreePattern (Tree RValue) | MultisetPattern (MS.Multiset (Tree RValue)) deriving (Eq, Show)

-- What happens when a definition matches?
data MatchEffect = TreeReplacement [Tree RValue] | MultisetPush (MS.Multiset (Tree RValue)) deriving (Eq, Show)

-- Rosin rule definition type
data MatchRule = MatchRule {
    useCount :: UseCount,
    matchCondition :: [MatchCondition],
    matchEffect :: [MatchEffect]
} deriving (Eq, Show)

instance Semigroup MatchRule where
    (MatchRule uc mcs mes) <> (MatchRule uc' mcs' mes') = MatchRule (uc <> uc') (mcs <> mcs') (mes <> mes')

instance Monoid MatchRule where
    mempty = MatchRule mempty [] []

