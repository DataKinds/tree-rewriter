module Zipper where
import Core
import Data.Maybe (fromMaybe, catMaybes)

data Zipper a = Zipper {
    _Left :: [Tree a],
    _Right :: [Tree a],
    _Ups :: [([Tree a], [Tree a])],
    _Content :: Tree a
} deriving (Show)

look :: Zipper a -> Tree a
look = _Content

put :: Zipper a -> Tree a -> Zipper a
put z t = z { _Content = t }

hasChildren :: Zipper a -> Bool
hasChildren z = case _Content z of 
    (Leaf _) -> False
    (Branch []) -> False
    (Branch _) -> True


-- Zipper creation --

zipperFromTrees :: [Tree a] -> Zipper a
zipperFromTrees trees = Zipper { _Left = [], _Right = [], _Ups = [], _Content = Branch trees }

zipperFromTree :: Tree a -> Zipper a
zipperFromTree tree = Zipper { _Left = [], _Right = [], _Ups = [], _Content = tree }

treeFromZipper :: Zipper a -> Tree a
treeFromZipper = look . upmost

-- Modify a zipper, splicing in a bunch of trees in place of and to the left of the focus
-- Note that calling `nextDfs` or `right` will not give back any of the spliced in data, as it's all to the left
spliceIn :: Zipper a -> [Tree a] -> Zipper a
spliceIn z [] = z
spliceIn z [tree] = z { _Content = tree }
spliceIn z (tree:trees) = z { _Left = tree:_Left z } `spliceIn` trees

spliceRight :: Zipper a -> [Tree a] -> Zipper a
spliceRight z' = go z' . reverse
    where go z [] = z
          go z [tree] = z { _Content = tree }
          go z (tree:trees) = z { _Right = tree:_Right z } `go` trees

-- Drop the focused tree and give back a zipper looking to the right, left, or above the previous focus
dropFocus :: Zipper a -> Zipper a
dropFocus z = tryLeft . tryRightUp . tryUp $ z
    where dropRight lz = lz { _Right = tail (_Right lz) }
          dropLeft rz = rz { _Left = tail (_Left rz) }
          nullContent z' = z' { _Content = Branch [] }
          tryLeft fallback = maybe fallback dropRight (left z)
          tryRightUp fallback = fromMaybe fallback $ up (maybe fallback dropLeft (right z))
          tryUp fallback = nullContent $ fromMaybe fallback (up z)


-- Zipper combinators --

-- See https://hackage.haskell.org/package/zippers-0.3.2/docs/src/Control.Zipper.Internal.html#farthest
-- Combinator for going as far as you can in one direction of the zipper
most :: (a -> Maybe a) -> a -> a
most f = go 
    where go a = maybe a go (f a)

-- See https://hackage.haskell.org/package/zippers-0.3.2/docs/src/Control.Zipper.Internal.html#tug
-- Combinator for trying to go in a direction
tug :: (a -> Maybe a) -> a -> a
tug f x = fromMaybe x (f x)

keepTryingUntil :: (a -> Maybe a) -> (a -> Maybe a) -> a -> a
keepTryingUntil f end = go
    where go x = case end x of
            Just y -> y
            Nothing -> maybe x go (f x)

-- Gives the next tree element in DFS order, or the tip element
nextDfs :: Zipper a -> Zipper a
nextDfs z = head $ catMaybes [firstChild z, right z, Just $ keepTryingUntil up right z]

-- Zipper navigation primitives --

-- Refocuses the zipper on its first child, if it exists
firstChild :: Zipper a -> Maybe (Zipper a)
firstChild z = case _Content z of 
    Leaf _ -> Nothing
    Branch [] -> Nothing
    Branch (x:xs) -> Just Zipper {
        _Left = [],
        _Right = xs,
        _Ups = (_Left z, _Right z):_Ups z,
        _Content = x
    }

left :: Zipper a -> Maybe (Zipper a)
left z = case _Left z of
    [] -> Nothing
    tree:rest -> Just Zipper {
        _Left = rest,
        _Right = _Content z:_Right z,
        _Ups = _Ups z,
        _Content = tree
    }
leftmost :: Zipper a -> Zipper a
leftmost = most left

right :: Zipper a -> Maybe (Zipper a)
right z = case _Right z of
    [] -> Nothing
    tree:rest -> Just Zipper {
        _Left = _Content z:_Left z,
        _Right = rest,
        _Ups = _Ups z,
        _Content = tree
    }

up :: Zipper a -> Maybe (Zipper a)
up z = let lz = leftmost z in case _Ups lz of
    [] -> Nothing
    (leftPath, rightPath):rest -> Just Zipper {
        _Left = leftPath,
        _Right = rightPath,
        _Ups = rest,
        _Content = Branch $ _Content lz:_Right lz
    }
upmost :: Zipper a -> Zipper a
upmost = most up