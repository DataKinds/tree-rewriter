module Core.DSL where
import Core (Tree (..), RValue (..), PVar)
import qualified Data.Text as T
import Data.Text.ICU (regex', ParseError)

-- Tree leaf values
sym :: String -> Tree RValue
sym = Leaf . RSymbol . T.pack
str :: String -> Tree RValue
str = Leaf . RString . T.pack
tstr :: T.Text -> Tree RValue
tstr = Leaf . RString
num :: Integral i => i -> Tree RValue
num = Leaf . RNumber . fromIntegral
regex :: String -> Either ParseError (Tree RValue)
regex = fmap (Leaf . RRegex) . regex' [] . T.pack
-- Pattern variables 
pvar :: PVar -> Tree RValue
pvar = Leaf . RVariable
-- Runtime branch
branch :: [Tree a] -> Tree a
branch = Branch
