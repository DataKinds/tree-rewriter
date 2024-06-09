module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import DSL
import Text.Parsec
import Text.Parsec.String
import Data.Char
import Control.Monad
import Control.Applicative (some)

flex :: Parser a -> Parser a
flex p = p <* spaces

psymStartParser :: Parser Char
psymStartParser = satisfy (`notElem` ("()[]~-" :: String))

psymCharParser :: Parser Char
psymCharParser = satisfy (\c -> not (isSpace c) && (c `notElem` ("()[]~-" :: String)))

psymRawParser :: Parser String
psymRawParser = do
    start <- psymStartParser
    rest <- many psymCharParser
    return (start:rest)

psymParser :: Parser (Q Exp)
psymParser = flex $ do
    sym <- psymRawParser
    return [| psym sym |]

pvarParser :: Parser (Q Exp)
pvarParser = flex $ do
    _ <- char ':'
    sym <- psymRawParser
    return [| pvar sym |]

pstrParser :: Parser (Q Exp)
pstrParser = flex $ do
    _ <- char '/'
    sym <- psymRawParser
    return [| pstr sym |]

pnumParser :: Parser (Q Exp)
pnumParser = flex $ do
    _ <- char '.'
    num <- many1 $ satisfy isDigit
    return [| pnum . read $ num |]

patternLiteralParser :: Parser (Q Exp)
patternLiteralParser = choice [branchParser, pvarParser, pstrParser, pnumParser, psymParser]

branchParser :: Parser (Q Exp)
branchParser = flex $ do
    _ <- flex . string $ "("
    lits <- many patternLiteralParser
    _ <- flex . string $ ")"
    return [| pbranch $(listE lits) |]

listParser :: Parser (Q Exp)
listParser = flex $ do
    _ <- flex . string $ "["
    lits <- many patternLiteralParser
    _ <- flex . string $ "]"
    -- out <- foldr (\lit acc -> pbranch)
    return [| pbranch $(listE lits) |]

patternParser :: Parser (Q Exp)
patternParser = try branchParser <|> try patternLiteralParser

patternRuleParser :: Parser (Q Exp)
patternRuleParser = flex $ do
    pat <- patternParser
    _ <- flex $ string "~>"
    template <- patternParser
    return [| $(pat) ~> $(template) |]

-- Quasiquote helpers --
parsePattern :: String -> Q Exp
parsePattern pat = case (parse parser "" pat) of 
    Left err -> error . show $ err
    Right q -> q
    where
        parser = spaces *> (try patternRuleParser <|> try patternParser)

-- pAttern (p was taken...)
a :: QuasiQuoter
a = QuasiQuoter {
    quoteExp = parsePattern,
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
}