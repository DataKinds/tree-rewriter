module Main (main) where

import Core
import DSL

rules :: Rules
rules = makeRules $ do
    rule (psym "hello") (psym "world")
    rule (pbranch [pnum 1, pvar "a", pnum 3]) (pbranch [pnum 3, pvar "a", pnum 1])

oneRule = case makeRules (rule (psym "hello") (psym "world")) of
    Rules rs -> head rs

input :: Tree RValue
input = Branch [Leaf (RSymbol "hello"), Leaf (RSymbol "world!")]

main :: IO ()
main = do
    print rules
    putStrLn "Applying to input:"
    print input
    putStrLn "Outcome:"
    print $ run rules input