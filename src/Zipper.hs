module Zipper where
import Core
import Data.Maybe (fromMaybe)

data Zipper a = Zipper {
    _Left :: [Tree a],
    _Right :: [Tree a],
    _Ups :: [([Tree a], [Tree a])],
    _Content :: Tree a
}

look :: Zipper a -> Tree a
look = _Content

put :: Zipper a -> Tree a -> Zipper a
put z t = z { _Content = t }

zipperFromTrees :: [Tree a] -> Zipper a
zipperFromTrees trees = Zipper { _Left = [], _Right = [], _Ups = [], _Content = Branch trees }

zipperFromTree :: Tree a -> Zipper a
zipperFromTree tree = Zipper { _Left = [], _Right = [], _Ups = [], _Content = tree }

treeFromZipper :: Zipper a -> Tree a
treeFromZipper = look . upmost

hasChildren :: Zipper a -> Bool
hasChildren z = case _Content z of 
    (Leaf _) -> False
    (Branch []) -> False
    (Branch _) -> True

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
keepTryingUntil tryThis untilThisSucceeds onThis = undefined 

firstChild :: Zipper a -> Maybe (Zipper a)
firstChild z = case _Content z of 
    Leaf _ -> Nothing
    Branch [] -> Nothing
    Branch (x:xs) -> Just Zipper {
        _Left = [],
        _Right = xs,
        _Ups = (reverse $ _Left z, _Right z):_Ups z,
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
up z = case _Ups $ leftmost z of
    [] -> Nothing
    (leftPath, rightPath):rest -> Just Zipper {
        _Left = leftPath,
        _Right = rightPath,
        _Ups = rest,
        _Content = Branch $ _Right z
    }
upmost :: Zipper a -> Zipper a
upmost = most up