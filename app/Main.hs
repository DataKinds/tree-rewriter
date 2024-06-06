module Main (main) where

import Core
import DSL
import TH

rules :: Rules
rules = makeRules $ do
    [a|hello|] ~> [a|world|]
    pbranch [[a|.1|], [a|:a|], [a|.3|]] ~> pbranch [[a|.3|], [a|:a|], [a|.1|]]

input :: Tree RValue
input = rbranch [rsym "hello", rsym "world!", rbranch [rnum 4, rnum 1, rnum 2, rnum 3, rnum 5]]

main :: IO ()
main = do
    print rules
    putStrLn "Applying to input:"
    print input
    putStrLn "Outcome:"
    print $ run rules input