{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module Zipper where
import Core
import Data.Maybe (fromMaybe, catMaybes)

-- up, down, left, right, next
-- the only moves one needs to run
-- zipper remembers

-- General combinators that are useful for zippers --

-- | Combinator for going as far as you can in one direction of the zipper
-- See https://hackage.haskell.org/package/zippers-0.3.2/docs/src/Control.Zipper.Internal.html#farthest
most :: (a -> Maybe a) -> a -> a
most f = go 
    where go a = maybe a go (f a)

-- | Combinator for trying to go in a direction
-- See https://hackage.haskell.org/package/zippers-0.3.2/docs/src/Control.Zipper.Internal.html#tug
tug :: (a -> Maybe a) -> a -> a
tug f x = fromMaybe x (f x)

-- | Combinator for going in a direction until `end` returns something.
keepTryingUntil :: (a -> Maybe a) -> (a -> Maybe a) -> a -> a
keepTryingUntil f end = go
    where go x = case end x of
            Just y -> y
            Nothing -> maybe x go (f x)


-- Zipper types --

-- | Typeclass for Zipper navigation primitives, i.e. everything that doesn't rely on the actual content in the Zipper
class MovesLikeZipper a where
    up :: a -> Maybe a
    -- | Refocuses the zipper on its first child, if it exists
    firstChild :: a -> Maybe a
    left :: a -> Maybe a
    right :: a -> Maybe a
    -- | Drop the focused tree and give back a zipper looking to the left, above, or up and to the left of the previous focus
    -- Goes the opposite direction of `nextDfs` such that `nextDfs . dropFocus` should focus the same as `nextDfs` alone
    -- TODO: That's quickcheckable, ain't it?
    dropFocus :: a -> a


data Zipper tag a = Zipper {
    _Left :: [TaggedTree tag a],
    _Right :: [TaggedTree tag a],
    _Ups :: [([TaggedTree tag a], tag, [TaggedTree tag a])],
    _Content :: TaggedTree tag a
}
deriving instance (Show tag, Show a) => Show (Zipper tag a)

instance MovesLikeZipper (Zipper tag a) where
    firstChild :: Zipper tag a -> Maybe (Zipper tag a)
    firstChild z = case _Content z of 
        TaggedLeaf _ _ -> Nothing
        TaggedBranch _ [] -> Nothing
        TaggedBranch tag (x:xs) -> Just Zipper {
            _Left = [],
            _Right = xs,
            _Ups = (_Left z, tag, _Right z):_Ups z,
            _Content = x
        }
    left z = case _Left z of
        [] -> Nothing
        tree:rest -> Just Zipper {
            _Left = rest,
            _Right = _Content z:_Right z,
            _Ups = _Ups z,
            _Content = tree
        }
    right z = case _Right z of
        [] -> Nothing
        tree:rest -> Just Zipper {
            _Left = _Content z:_Left z,
            _Right = rest,
            _Ups = _Ups z,
            _Content = tree
        }
    up z = let lz = leftmost z in case _Ups lz of
        [] -> Nothing
        (leftPath, tag, rightPath):rest -> Just Zipper {
            _Left = leftPath,
            _Right = rightPath,
            _Ups = rest,
            _Content = TaggedBranch tag $ _Content lz:_Right lz
        }
    dropFocus z = head $ catMaybes [
            dropRight <$> left z, 
            right z >>= up . dropLeft,
            nullContent <$> up z,
            Just $ nullContent z
        ] where dropRight lz = lz { _Right = tail (_Right lz) }
                dropLeft rz = rz { _Left = tail (_Left rz) }
                nullContent z' = z' { _Content = TaggedBranch (getTag $ _Content z') [] }


-- Zipper combinators --
-- All of these just rely on the movement primitives defined above

-- | Gives the next tree element in DFS order, or the tip element
nextDfs :: MovesLikeZipper a => a -> a
nextDfs z = head $ catMaybes [firstChild z, right z, Just $ keepTryingUntil up right z]

leftmost :: MovesLikeZipper a => a -> a
leftmost = most left

upmost :: MovesLikeZipper a => a -> a
upmost = most up


-- Zipper inspection --

look :: Zipper tag a -> TaggedTree tag a
look = _Content

put :: Zipper tag a -> TaggedTree tag a -> Zipper tag a
put z t = z { _Content = t }

hasChildren :: Zipper tag a -> Bool
hasChildren z = case _Content z of 
    (TaggedLeaf _ _) -> False
    (TaggedBranch _ []) -> False
    (TaggedBranch _ _) -> True


-- Zipper creation and modification --
zipperFromTrees :: tag -> [Tree a] -> Zipper tag a
zipperFromTrees tag trees = Zipper { _Left = [], _Right = [], _Ups = [], _Content = TaggedBranch tag $ toTagged tag <$> trees }

zipperFromTree :: tag -> Tree a -> Zipper tag a
zipperFromTree tag tree = Zipper { _Left = [], _Right = [], _Ups = [], _Content = toTagged tag tree }

treeFromZipper :: Zipper tag a -> TaggedTree tag a
treeFromZipper = look . upmost

-- Modify a zipper, splicing in a bunch of trees in place of and to the left of the focus
-- Note that calling `nextDfs` or `right` will not give back any of the spliced in data, as it's all to the left
spliceIn :: Zipper tag a -> [TaggedTree tag a] -> Zipper tag a
spliceIn z [] = z
spliceIn z [tree] = z { _Content = tree }
spliceIn z (tree:trees) = z { _Left = tree:_Left z } `spliceIn` trees

-- Modify the focus and the right of a zipper, such that calling `nextDfs` will give back the spliced in data
spliceRight :: Zipper tag a -> [TaggedTree tag a] -> Zipper tag a
spliceRight z' = go z' . reverse
    where go z [] = z
          go z [tree] = z { _Content = tree }
          go z (tree:trees) = z { _Right = tree:_Right z } `go` trees


