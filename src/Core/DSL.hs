module Core.DSL where
import Core (Tree (..), RValue (..), PVar, defaultTag)
import qualified Data.Text as T
import Data.Text.ICU (regex', ParseError)

-- Tree leaf values
sym :: String -> Tree RValue
sym = Leaf defaultTag . RSymbol . T.pack
str :: String -> Tree RValue
str = Leaf defaultTag . RString . T.pack
tstr :: T.Text -> Tree RValue
tstr = Leaf defaultTag . RString
num :: Integral i => i -> Tree RValue
num = Leaf defaultTag . RNumber . fromIntegral
regex :: String -> Either ParseError (Tree RValue)
regex = fmap (Leaf defaultTag . RRegex) . regex' [] . T.pack
-- Pattern variables 
pvar :: PVar -> Tree RValue
pvar = Leaf defaultTag . RVariable
-- Runtime branch
branch :: [Tree a] -> Tree a
branch = Branch defaultTag
