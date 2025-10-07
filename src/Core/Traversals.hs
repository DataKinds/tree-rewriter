module Core.Traversals where
import Core (Tree)

data Treeversal a = Nest | Unnest | Value a

toCanonical :: Tree a -> [Treeversal a]
toCanonical tree = undefined -- use nextDfs

fromCanonical :: [Treeversal a] -> Tree a
fromCanonical tree = undefined -- unsure if even needed?