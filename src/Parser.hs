module Parser where

import Text.Parsec
    ( char,
      satisfy,
      spaces,
      string,
      choice,
      (<|>),
      many,
      many1,
      try,
      ParsecT )
import DSL ( WithRuleset, psym, pstr, pnum, pbranch, pvar )
import Core ( Pattern, Tree, RValue, Rewrite (Rewrite) ) 
import Data.Char ( isSpace, isDigit )



type RuleParser = ParsecT String () WithRuleset

flex :: RuleParser a -> RuleParser a
flex p = p <* spaces

psymStartParser :: RuleParser Char
psymStartParser = satisfy (`notElem` (":/.()[]~-" :: String))

psymCharParser :: RuleParser Char
psymCharParser = satisfy (\c -> not (isSpace c) && (c `notElem` (":/.()[]~-" :: String)))

psymRawParser :: RuleParser String
psymRawParser = do
    start <- psymStartParser
    rest <- many psymCharParser
    return (start:rest)

psymParser :: RuleParser (Tree (Pattern RValue))
psymParser = flex (psym <$> psymRawParser)

pvarParser :: RuleParser (Tree (Pattern RValue))
pvarParser = flex (char ':' *> (pvar <$> psymRawParser))

pstrParser :: RuleParser (Tree (Pattern RValue))
pstrParser = flex (char '/' *> (pstr <$> psymRawParser))

pnumParser :: RuleParser (Tree (Pattern RValue))
pnumParser = try$flex (char '.' *> (pnum . read <$> many1 (satisfy isDigit)))

patternLiteralParser :: RuleParser (Tree (Pattern RValue))
patternLiteralParser = choice [pbranchParser, plistParser, pvarParser, pstrParser, pnumParser, psymParser]

pbranchParser :: RuleParser (Tree (Pattern RValue))
pbranchParser = flex $ do
    _ <- flex . string $ "("
    lits <- many patternLiteralParser
    _ <- flex . string $ ")"
    return $ pbranch lits

-- parses [1 2 3 4 ..:a]
ptailListParser :: RuleParser (Tree (Pattern RValue))
ptailListParser = flex $ do
    _ <- flex . string $ "["
    lits <- many patternLiteralParser
    tail <- string ".." *> pvarParser
    _ <- flex . string $ "]"
    return $ foldr cons tail lits
        where
            cons x xs = pbranch [x, xs]

-- parses [1 2 (3) :a 4]
plistParser :: RuleParser (Tree (Pattern RValue))
plistParser = flex $ do
    _ <- flex . string $ "["
    lits <- many patternLiteralParser
    _ <- flex . string $ "]"
    return $ foldr cons (pbranch []) lits
        where
            cons x xs = pbranch [x, xs]

patternParser :: RuleParser (Tree (Pattern RValue))
patternParser = try pbranchParser <|> try ptailListParser <|> try patternLiteralParser

patternRuleParser :: RuleParser (Rewrite RValue)
patternRuleParser = flex $ do
    pat <- patternParser
    _ <- flex $ string "~>"
    Rewrite pat <$> patternParser -- template
