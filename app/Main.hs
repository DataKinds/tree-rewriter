module Main (main) where

import Core
import DSL

main :: IO ()
main = print . makeRules $ do
    rule (psym "hello") (psym "world")
    rule (pbranch [pnum 1, pvar "a", pnum 3]) (pbranch [pnum 3, pvar "a", pnum 1])
