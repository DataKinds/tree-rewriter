module Main (main) where

import Core
import Runtime
import Parser
import TH
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Text.Parsec (runParserT)
import Data.Functor.Identity ( Identity(runIdentity) )
import Options.Applicative
import Control.Monad (when)
import Data.Char (isSpace)


ensureTHCompilation :: [Tree RValue]
ensureTHCompilation = [
        [a|hello ~> world|],
        [a|(+1 :asd) ~> (+3 :asd)|],
        [a|(+1 :a +3) ~> (+3 :a +1)|],
        [a|[+1 +2 three four :a :b] ~> [:a :b]|],
        [a|[+1 +2 three four :a ..:b] ~> [:a ..:b]|],
        [a|(if true then :a else :b) ~> :a|],
        [a|(if false then :a else :b) ~> :b|],
        [a|(true) ~> true|],
        [a|(false) ~> false|],
        [a|(hello :world)|],
        [a|(hello world! (true) reversethis (+4 +1  +2 +3 +5))|],
        [a|(hello world)|],
        rbranch [rsym "hello", rsym "world!", rbranch [rsym "true"], rsym "reverse this", rbranch [rnum 4, rnum 1, rnum 2, rnum 3, rnum 5]]
    ]

testTHCompilation :: IO ()
testTHCompilation = run mempty ensureTHCompilation >>= print

runProg :: T.Text -> OwO -> IO ()
runProg prog (OwO filepath printOutput extraVerbose) = let
    -- TODO: parser should take a Text directly
    parsed = runIdentity $ runParserT programParser () filepath (T.unpack prog)
    in case parsed of 
        Left err -> fail . show $ err
        Right rvals -> do
            when extraVerbose $ do 
                putStrLn ""
                putStrLn "+-------------------+"
                putStrLn "| Parsed from input |"
                putStrLn "+-------------------+"
                mapM_ (putStrLn . sexprprint) rvals
            (rvals', defs) <- run mempty rvals
            when extraVerbose $ do
                putStrLn ""
                print defs
            when printOutput $ do 
                putStrLn ""
                putStrLn "+-----------------+"
                putStrLn "| Final transform |"
                putStrLn "+-----------------+"
                mapM_ (putStrLn . sexprprint) rvals'

runFileProg :: OwO -> IO () 
runFileProg owo = do
    prog <- TI.readFile . owoInputFile $ owo
    runProg prog owo

runStdinProg :: OwO -> IO () 
runStdinProg owo = do
    prog <- TI.getContents
    runProg prog owo

data OwO = OwO
  { owoInputFile :: String
  , owoPrintOutput :: Bool
  , owoExtraVerbose :: Bool
  }

cli :: Parser OwO
cli = OwO
      <$> strArgument
          ( metavar "INFILE"
         <> help "Rosin file to read and interpret" 
         <> value "" )
      <*> switch
          ( long "print-output"
         <> short 'p'
         <> help "Whether to print the final state of the input tree." )
      <*> switch
          ( long "verbose"
         <> short 'v'
         <> help "Whether to print verbose debugging information. Implies -p." )

imply :: OwO -> OwO
imply (OwO inputFile _ True) = OwO inputFile True True
imply x = x

main :: IO ()
main = do
    config <- imply <$> execParser cliParser
    if dropWhile isSpace (owoInputFile config) == "" then do
        -- we're reading from standard in
        when (owoExtraVerbose config) $ putStrLn "reading from stdin"
        runStdinProg config
    else do
        -- we're reading from a file
        when (owoExtraVerbose config) (putStrLn $ "reading " ++ (owoInputFile config))
        runFileProg config
    where 
        cliParser = info (cli <**> helper) (
               fullDesc
            <> progDesc "Invoke Rosin on input, either from standard input or from a file." 
            <> header "Rosin (they/them) is a tree rewriting language" 
            <> footer "Created by at/DataKinds in 2024. Comes with one warranty: if you can prove that Rosin caused you physical or otherwise material injury, the current maintainer will arrive and dispense one (1) sad platitude regarding your condition."
            )
