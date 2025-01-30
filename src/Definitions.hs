
-- This module implements the datatypes and functions required to
-- manage Rosin rule definitions.
module Definitions where
import Core (Tree, RValue, sexprprint)
import Data.List (find)
import qualified Multiset as MS 

-- is this definition single use or will it apply forever?
data UseCount = UseOnce | UseMany deriving (Show, Eq)

-- What's required for this definition to match?
data MatchCondition = TreePattern (Tree RValue) | MultisetPattern (Tree RValue)
instance Show MatchCondition where
    show (TreePattern t) = sexprprint t
    show (MultisetPattern t) = sexprprint t

isTreePattern :: MatchCondition -> Bool
isTreePattern (TreePattern _) = True
isTreePattern _ = False

isSetPattern :: MatchCondition -> Bool
isSetPattern (MultisetPattern _) = True
isSetPattern _ = False


-- What happens when a definition matches?
data MatchEffect = TreeReplacement [Tree RValue] | MultisetPush (MS.Multiset (Tree RValue))
instance Show MatchEffect where
    show (TreeReplacement ts) = unwords $ sexprprint <$> ts
    show (MultisetPush ts) = show ts

-- What types of definition are there?
-- data EatenDef = TreeDef UseCount (Tree RValue) [Tree RValue]
--               | MultisetDef UseCount MultisetAction
--               | ComboDef UseCount MultisetAction (Tree RValue) [Tree RValue] deriving (Eq)
data EatenDef = EatenDef UseCount [MatchCondition] [MatchEffect]

defUseCount :: EatenDef -> UseCount
defUseCount (EatenDef uc _ _) = uc

defMatchCondition :: EatenDef -> [MatchCondition]
defMatchCondition (EatenDef _ mcs _) = mcs

instance Show EatenDef where
    show (EatenDef useCount matchConds matchEffs) = case (touchesSet, touchesTree) of 
        (True, False) -> unwords [setPats, setOp]
        (False, True) -> unwords [treePats, treeOp]
        (True, True) -> unwords [treePats, treeOp, "&", setPats, setOp]
        _ -> ""
        where
            setOp = if useCount == UseOnce then "|" else "|>"
            treeOp = if useCount == UseOnce then "~" else "~>"
            setPats = unwords . map show $ filter isSetPattern matchConds
            treePats = maybe "" show $ find isTreePattern matchConds
            touchesSet = setPats /= ""
            touchesTree = treePats /= ""


