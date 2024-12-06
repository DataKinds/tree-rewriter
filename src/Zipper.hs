module Zipper where
import Core

data Zipper a = Zipper {
    _Left :: [Tree a],
    _Right :: [Tree a],
    _Ups :: [([Tree a], [Tree a])],
    _Content :: Tree a
}

zipperFromTree :: Tree a -> Zipper a
zipperFromTree tree = Zipper { _Left = [], _Right = [], _Ups = [], _Content = tree }

