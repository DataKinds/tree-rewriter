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
psymStartParser = satisfy (`notElem` (",:/.()[]~-" :: String))

psymCharParser :: RuleParser Char
psymCharParser = satisfy (\c -> not (isSpace c) && (c `notElem` (":/.()[]~-" :: String)))

psymRawParser :: RuleParser String
psymRawParser = do
    start <- psymStartParser
    rest <- many psymCharParser
    return (start:rest)

symParser :: RuleParser String
symParser = flex psymRawParser

varParser :: RuleParser String
varParser = flex (char ':' *> psymRawParser)

strParser :: RuleParser String
strParser = flex (char '/' *> psymRawParser)

numParser :: RuleParser String
numParser = flex (char '.' *> many1 (satisfy isDigit))

branchParser :: RuleParser a -> RuleParser [a]
branchParser innerLit = flex $ do
    _ <- flex . string $ "("
    lits <- many innerLit
    _ <- flex . string $ ")"
    return lits

listParser :: RuleParser a -> RuleParser ([a], Maybe a)
listParser innerLit = flex $ do
    _ <- flex . string $ "["
    lits <- many (try innerLit)
    endLit <- optionMaybe (string "," *> innerLit)
    _ <- flex . string $ "]"
    return (lits, endLit)


pbranchParser = pbranch <$> branchParser patternLiteralParser
plistParser = do
    (lits, optFinal) <- listParser patternLiteralParser
    let final = case optFinal of
            Just final' -> final'
            Nothing -> pbranch []
    return $ foldr (\lit cons -> pbranch [lit, cons]) final lits
psymParser = psym <$> symParser
pvarParser = pvar <$> varParser
pstrParser = pstr <$> strParser
pnumParser = pnum . read <$> numParser

rsymParser = rsym <$> symParser
rstrParser = rstr <$> strParser
rnumParser = rnum . read <$> numParser

patternLiteralParser :: RuleParser (Tree (Pattern RValue))
patternLiteralParser = choice [pbranchParser, plistParser, pvarParser, pstrParser, pnumParser, psymParser]

patternRuleParser :: RuleParser ()
patternRuleParser = flex $ do
    pat <- patternLiteralParser
    _ <- flex $ string "~>"
    template <- patternLiteralParser
    _ <- return $ pat ~> template
    pure ()

-- Quasiquote helpers --
treeToQ :: Lift a => RuleParser a -> RuleParser (Q Exp)
treeToQ par = do 
    out <- par
    return [| out |]

quoteRuleParser :: RuleParser (Q Exp)
quoteRuleParser = flex $ do
    pat <- treeToQ patternLiteralParser
    _ <- flex $ string "~>"
    template <- treeToQ patternLiteralParser
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
        parser2 = treeToQ $ try patternLiteralParser 
        parser = spaces *> (parser1 <|> parser2)

-- pAttern (p was taken...)
a :: QuasiQuoter
a = QuasiQuoter {
    quoteExp = quotePattern,
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
}