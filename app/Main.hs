module Main (main) where

import Core
import Runtime
import Parser
import TH
import Data.Maybe (fromJust)
import Control.Monad (guard)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as I
import Text.Parsec (runParserT)
import Control.Monad.Trans.Accum (runAccum)


rules1 :: Rules
rules1 = makeRules $ do
    [a|hello ~> world|]
    [a|(.1 :asd) ~> (.3 :asd)|]
    [a|(.1 :a .3) ~> (.3 :a .1)|]
    [a|[.1 .2 three four :a :b] ~> [:a :b]|]
    [a|[.1 .2 three four :a ..:b] ~> [:a ..:b]|]
    [a|(if true then :a else :b) ~> :a|]
    [a|(if false then :a else :b) ~> :b|]
    [a|(true) ~> true|]
    [a|(false) ~> false|]

input :: Tree RValue
input = rbranch [rsym "hello", rsym "world!", rbranch [rsym "true"], rsym "reverse this", rbranch [rnum 4, rnum 1, rnum 2, rnum 3, rnum 5]]
input1 =  [a|(hello world! (true) reversethis (.4 .1  .2 .3 .5))|]
input2 :: Tree RValue
input2 =  [a|(hello world)|]
input3 = [a|(hello :world)|]

test :: IO ()
test = do
    print rules1
    putStrLn "Applying to input:"
    print input
    putStrLn "\nOutcome:"
    print $ run rules1 input

runProg :: T.Text -> String -> IO () 
runProg prog filepath = let
    parseM = runParserT programParser () filepath (T.unpack prog)
    (parsed, rules) = runAccum parseM mempty
    in case parsed of 
        Left err -> fail . show $ err
        Right rvals -> do
            print parsed
            print rules
            let rvals' = run rules <$> rvals
            (mapM_ . mapM_) (putStrLn . sexprprint) rvals'

main :: IO ()
main = do
    args <- getArgs
    let progPath = unwords args
    putStrLn $ "reading " ++ progPath
    prog <- I.readFile progPath
    -- putStrLn . T.unpack $ prog
    runProg prog progPath
