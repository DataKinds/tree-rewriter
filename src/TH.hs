module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import DSL
import Text.Parsec
import Data.Char
import Control.Monad
import Control.Applicative (some)
import Core 
import Language.Haskell.TH.Syntax
import Control.Monad.Trans.Accum


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
pnumParser = flex (char '.' *> (pnum . read <$> many1 (satisfy isDigit)))

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
    -- out <- foldr (\lit acc -> pbranch)
    return $ pbranch lits -- TODO

patternParser :: RuleParser (Tree (Pattern RValue))
patternParser = try branchParser <|> try patternLiteralParser

patternRuleParser :: RuleParser ()
patternRuleParser = flex $ do
    pat <- patternParser
    _ <- flex $ string "~>"
    template <- patternParser
    _ <- return $ pat ~> template
    pure ()

-- Quasiquote helpers --
treeToQ :: Lift a => RuleParser a -> RuleParser (Q Exp)
treeToQ par = do 
    out <- par
    return [| out |]

quoteRuleParser :: RuleParser (Q Exp)
quoteRuleParser = flex $ do
    pat <- treeToQ patternParser
    _ <- flex $ string "~>"
    template <- treeToQ patternParser
    pure [| $(pat) ~> $(template) |]

quotePattern :: String -> Q Exp
quotePattern pat = let
    parseM = runParserT parser () "" pat
    (parsed, _) = runAccum parseM mempty
    in case parsed of 
        Left err -> error . show $ err
        Right q -> q
    where
        parser1 = try quoteRuleParser 
        parser2 = treeToQ $ try patternParser 
        parser = spaces *> (parser1 <|> parser2)

-- pAttern (p was taken...)
a :: QuasiQuoter
a = QuasiQuoter {
    quoteExp = quotePattern,
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
}