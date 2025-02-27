
-- This module implements the datatypes and functions required to
-- manage Rosin rule definitions.
module Definitions where
import Core (Tree, RValue, sexprprint)
import Data.List (find)
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
-- instance Show MatchCondition where
--     show (TreePattern t) = sexprprint t
--     show (MultisetPattern t) = show t

isTreePattern :: MatchCondition -> Bool
isTreePattern (TreePattern _) = True
isTreePattern _ = False

isSetPattern :: MatchCondition -> Bool
isSetPattern (MultisetPattern _) = True
isSetPattern _ = False


-- What happens when a definition matches?
data MatchEffect = TreeReplacement [Tree RValue] | MultisetPush (MS.Multiset (Tree RValue)) deriving (Eq, Show)
-- instance Show MatchEffect where
--     show (TreeReplacement ts) = unwords $ sexprprint <$> ts
--     show (MultisetPush ts) = show ts

isTreeReplacement :: MatchEffect -> Bool
isTreeReplacement (TreeReplacement _) = True
isTreeReplacement _ = False

isMultisetPush :: MatchEffect -> Bool
isMultisetPush (MultisetPush _) = True
isMultisetPush _ = False


-- Rosin rule definition type
data EatenDef = EatenDef {
    useCount :: UseCount,
    matchCondition :: [MatchCondition],
    matchEffect :: [MatchEffect]
} deriving (Eq, Show)

instance Semigroup EatenDef where
    (EatenDef uc mcs mes) <> (EatenDef uc' mcs' mes') = EatenDef (uc <> uc') (mcs <> mcs') (mes <> mes')

instance Monoid EatenDef where
    mempty = EatenDef mempty [] []

-- instance Show EatenDef where
--     show (EatenDef useCount matchConds matchEffs) = case (touchesSet, touchesTree) of 
--         (True, False) -> unwords [setPats, setOp, setEffs]
--         (False, True) -> unwords [treePats, treeOp, treeEffs]
--         (True, True) -> unwords [setPats, setOp, setEffs, "&", treePats, treeOp, treeEffs]
--         _ -> ""
--         where
--             setOp = if useCount == UseOnce then "|" else "|>"
--             treeOp = if useCount == UseOnce then "~" else "~>"
--             setPats = unwords . map show $ filter isSetPattern matchConds
--             treePats = maybe "" show $ find isTreePattern matchConds
--             setEffs = unwords . map show $ filter isMultisetPush matchEffs
--             treeEffs = maybe "" show $ find isTreeReplacement matchEffs
--             touchesSet = setPats /= ""
--             touchesTree = treePats /= ""


