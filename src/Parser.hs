module Parser where

import Core
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import DSL
import Text.Parsec
import Text.Parsec.String
import Data.Char
import Control.Monad
import Control.Applicative (some)

-- A parser that records rule definitions as they come.
type RuleParser = Parsec String Rules

flex :: ParsecT String u m a -> ParsecT String u m a
flex p = p <* spaces

psymStartParser :: RuleParser Char
psymStartParser = satisfy (`notElem` ("()[]~-" :: String))

psymCharParser :: RuleParser Char
psymCharParser = satisfy (\c -> not (isSpace c) && (c `notElem` ("()[]~-" :: String)))

psymRawParser :: RuleParser String
psymRawParser = do
    start <- psymStartParser
    rest <- many psymCharParser
    return (start:rest)

psymParser :: RuleParser (Tree (Pattern RValue))
psymParser = flex $ do
    sym <- psymRawParser
    return $ psym sym

pvarParser :: RuleParser (Tree (Pattern RValue))
pvarParser = flex $ do
    _ <- char ':'
    sym <- psymRawParser
    return $ pvar sym 

pstrParser :: RuleParser (Tree (Pattern RValue))
pstrParser = flex $ do
    _ <- char '/'
    sym <- psymRawParser
    return $ pstr sym 

pnumParser :: RuleParser (Tree (Pattern RValue))
pnumParser = flex $ do
    _ <- char '.'
    num <- many1 $ satisfy isDigit
    return (pnum . read $ num)

patternLiteralParser :: RuleParser (Tree (Pattern RValue))
patternLiteralParser = choice [branchParser, pvarParser, pstrParser, pnumParser, psymParser]

branchParser :: RuleParser (Tree (Pattern RValue))
branchParser = flex $ do
    _ <- flex . string $ "("
    lits <- many patternLiteralParser
    _ <- flex . string $ ")"
    return $ pbranch lits

listParser :: RuleParser (Tree (Pattern RValue))
listParser = flex $ do
    _ <- flex . string $ "["
    lits <- many patternLiteralParser
    _ <- flex . string $ "]"
    let empty = pbranch []
        cons x xs = pbranch [x, xs]
    return $ foldr cons empty lits

patternParser :: RuleParser (Tree (Pattern RValue))
patternParser = try branchParser <|> try patternLiteralParser

ruleParser :: RuleParser (Tree (Pattern RValue))
ruleParser = flex $ do
    pat <- patternParser
    _ <- flex $ string "~>"
    template <- patternParser
    modifyState (\rules -> (pat ~> template):rules)
    return $ pbranch [pat, psym "~>", template]

parsePattern :: String -> Either String (Tree (Pattern RValue))
parsePattern pat = case (runParser parser mempty "" pat) of
    Left err -> Left $ show err
    Right pat -> Right pat
    where
        parser = spaces *> (try ruleParser <|> try patternParser)
