module Main (main) where

import Core
import DSL
import TH
import Data.Maybe (fromJust)

rules :: Rules
rules = makeRules $ do
    [a|hello ~> world|]
    [a|(.1 :asd) ~> (.3 :asd)|]
    [a|(.1 :a .3) ~> (.3 :a .1)|]
    [a|[.1 .2 three four :a :b] ~> [:a :b]|]
    [a|[.1 .2 three four :a ,:b] ~> [:a ,:b]|]
    [a|(if true then :a else :b) ~> :a|]
    [a|(if false then :a else :b) ~> :b|]
    [a|(true) ~> true|]
    [a|(false) ~> false|]

input :: Tree RValue
input = rbranch [rsym "hello", rsym "world!", rbranch [rsym "true"], rsym "reverse this", rbranch [rnum 4, rnum 1, rnum 2, rnum 3, rnum 5]]
input2 :: Tree RValue
input2 = fromJust . unpattern $ [a|(hello world)|]

main :: IO ()
main = do
    print rules
    putStrLn "Applying to input:"
    print input
    putStrLn "\nOutcome:"
    print $ run rules input