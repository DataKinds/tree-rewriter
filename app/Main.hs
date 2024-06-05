module Main (main) where

import Core
import DSL

rules :: Rules
rules = makeRules $ do
    rule (psym "hello") (psym "world")
    rule (pbranch [pnum 1, pvar "a", pnum 3]) (pbranch [pnum 3, pvar "a", pnum 1])

input :: Tree RValue
input = rbranch [rsym "hello", rsym "world!", rbranch [rnum 4, rnum 1, rnum 2, rnum 3, rnum 5]]

main :: IO ()
main = do
    print rules
    putStrLn "Applying to input:"
    print input
    putStrLn "Outcome:"
    print $ run rules input