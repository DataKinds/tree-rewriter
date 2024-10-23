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
      unexpected,
      ParsecT, skipMany, oneOf )
import Runtime ( WithRuleset, psym, pstr, pnum, pbranch, pvar, addRule )
import Core ( Tree, RValue, Rewrite (Rewrite) ) 
import Data.Char ( isSpace, isDigit )
import Control.Applicative (Alternative(some))
import Control.Monad (liftM, join)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity)



type RuleParser = ParsecT String () Identity

flex :: RuleParser a -> RuleParser a
flex p = p <* spaces

-- one line flex
olFlex :: RuleParser a -> RuleParser a
olFlex p = p <* skipMany (satisfy (\c -> isSpace c && (c `notElem` ("\n\r" :: String))))

psymCharParser :: RuleParser Char
psymCharParser = satisfy (\c -> not (isSpace c) && (c `notElem` (":/()[]" :: String)))

psymRawParser :: RuleParser String
psymRawParser = some psymCharParser

psymParser :: RuleParser (Tree RValue)
psymParser = psym <$> psymRawParser

pvarParser :: RuleParser (Tree RValue)
pvarParser = char ':' *> (pvar <$> psymRawParser)

-- TODO
pstrParser :: RuleParser (Tree RValue)
pstrParser = char '/' *> (pstr <$> psymRawParser)

pnumParser :: RuleParser (Tree RValue)
pnumParser = try (char '+' *> (pnum . read <$> many1 (oneOf "0123456789-")))

-- parses (1 2 (4 (5 6 7 (8))) :a 5 6)
pbranchParser :: RuleParser (Tree RValue)
pbranchParser = do
    _ <- flex . string $ "("
    lits <- many . flex $ patternLiteralParser
    _ <- string ")"
    return $ pbranch lits

-- parses [1 2 3 4 ..:a]
ptailListParser :: RuleParser (Tree RValue)
ptailListParser = try $ do
    _ <- flex . string $ "["
    lits <- many . flex $ patternLiteralParser
    tail' <- string ".." *> pvarParser
    _ <- string "]"
    return $ foldr cons tail' lits
        where
            cons x xs = pbranch [x, xs]

-- parses [1 2 (3) :a 4]
plistParser :: RuleParser (Tree RValue)
plistParser = do
    _ <- flex . string $ "["
    lits <- many . flex $ patternLiteralParser
    _ <- string "]"
    return $ foldr cons (pbranch []) lits
        where
            cons x xs = pbranch [x, xs]

patternLiteralParser :: RuleParser (Tree RValue)
patternLiteralParser = choice [pbranchParser, ptailListParser, plistParser, pvarParser, pstrParser, pnumParser, psymParser]

patternParser = try patternLiteralParser

-- patternRuleParser :: RuleParser (Rewrite RValue)
-- patternRuleParser = flex $ do
--     pattern <- olFlex patternParser
--     _ <- olFlex $ string "~>"
--     templates <- many $ olFlex patternParser
--     let rule = Rewrite pattern templates
--     lift $ addRule rule
--     pure rule

programParser :: RuleParser [Tree RValue]
programParser = (fmap join . many . flex) (some . flex $ patternParser)
