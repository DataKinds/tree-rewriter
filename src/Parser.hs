module Parser (module Parser) where

import Text.Parsec
    ( char,
      satisfy,
      spaces,
      choice,
      (<|>),
      many,
      many1,
      try,
      optionMaybe,
      ParsecT, skipMany, oneOf, notFollowedBy, anyChar, manyTill, eof, parserFail, runParserT, ParseError )
import Core ( sym, str, num, branch, pvar, regex,
    Tree, RValue(..), PVarTag (..), PVar (..), SpecialAccumTag (..) ) 
import Data.Char ( isSpace )
import Control.Applicative (Alternative(some))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (isJust)
import qualified Data.Text as T



type RuleParser = ParsecT String () Identity

flex :: RuleParser a -> RuleParser a
flex p = p <* spaces

-- one line flex
olFlex :: RuleParser a -> RuleParser a
olFlex p = p <* skipMany (satisfy (\c -> isSpace c && (c `notElem` ("\n\r" :: String))))

psymCharParser :: RuleParser Char
psymCharParser = satisfy (\c -> not (isSpace c) && (c `notElem` (":/()[]\"" :: String)))

psymRawParser :: RuleParser String
psymRawParser = some psymCharParser

psymParser :: RuleParser (Tree RValue)
psymParser = sym <$> psymRawParser

-- parses $!hello or :hello or ?+ or ?!@
pvarParser :: RuleParser (Tree RValue)
pvarParser = try specialAccum <|> normalVar 
    where
        mkAccumParser :: Char -> SpecialAccumTag -> RuleParser (SpecialAccumTag, Char)
        mkAccumParser accumName accumTag = char accumName >> pure (accumTag, accumName)
        specialAccum = do
            _ <- char '?'
            eager <- isJust <$> optionMaybe (char '!')
            (accumTag, accumName) <- choice -- parse the accumulator
                [ mkAccumParser '+' SASum
                , mkAccumParser '-' SANegate
                , mkAccumParser '*' SAProduct
                , mkAccumParser '>' SAOutput 
                , mkAccumParser '<' SAInput  
                , mkAccumParser '@' SAPack   
                , mkAccumParser '%' SAUnpack 
                ]
            pure . pvar $ PVar {
                pvarEager = eager,
                pvarName = T.singleton accumName,
                pvarTag = PVarSpecialAccum accumTag
            }
        normalVar = do
            sigilTag <- choice -- parse the sigil
                [ char ':' >> pure PVarNothingSpecial
                , char '$' >> pure PVarRegexGroup]
            eager <- isJust <$> optionMaybe (char '!')
            varBody <- psymRawParser
            pure . pvar $ PVar{
                pvarEager = eager,
                pvarName = T.pack varBody,
                pvarTag = sigilTag
            }

-- parses "hello world"
pstrParser :: RuleParser (Tree RValue)
pstrParser = do
    flex . char $ '"'
    str <$> manyTill anyEscapedChar (try $ char '"')
    where
        anyEscapedChar = (try $ char '\\' *> anyChar) <|> anyChar

-- parses /hello world/
pregexParser :: RuleParser (Tree RValue)
pregexParser = do
    flex . char $ '/'
    parsedRegex <- regex <$> manyTill escapedSlashOrAny (try $ char '/')
    case parsedRegex of
        Right compiledRegex -> pure compiledRegex
        Left parserError -> parserFail (show parserError)
    where
        escapedSlashOrAny = try (char '\\' *> char '/') <|> anyChar

-- parses 10 or -45 or +222
pnumParser :: RuleParser (Tree RValue)
pnumParser = do
    sign <- optionMaybe $ char '+' <|> char '-'
    digits <- many1 (oneOf "0123456789")
    let reader = case sign of
            Just '+' -> num . read
            Just '-' -> num . negate . read
            Nothing ->  num . read
    pure $ reader digits

-- parses (1 2 (4 (5 6 7 (8))) :a 5 6)
pbranchParser :: RuleParser (Tree RValue)
pbranchParser = do
    _ <- flex . char $ '('
    lits <- many . flex $ literalParser
    _ <- char ')'
    return $ branch lits

-- parses [1 2 3 4 ::a]
ptailListParser :: RuleParser (Tree RValue)
ptailListParser = try $ do
    _ <- flex . char $ '['
    let tailParser = char ':' *> pvarParser
    lits <- many $ do 
        notFollowedBy tailParser
        flex literalParser
    tailLit <- tailParser
    _ <- char ']'
    return $ foldr cons tailLit lits
        where
            cons x xs = branch [x, xs]

-- parses [1 2 (3) :a 4]
plistParser :: RuleParser (Tree RValue)
plistParser = do
    _ <- flex . char $ '['
    lits <- many . flex $ literalParser
    _ <- char ']'
    return $ foldr cons (branch []) lits
        where
            cons x xs = branch [x, xs]

literalParser :: RuleParser (Tree RValue)
literalParser = choice 
    [ pbranchParser
    , ptailListParser
    , plistParser
    , pvarParser
    , pstrParser
    , pregexParser
    , try pnumParser
    , psymParser
    ]

programParser :: RuleParser [Tree RValue]
programParser = spaces *> (some . flex . try $ literalParser) <* eof

parse :: String -> String -> Either ParseError [Tree RValue]
parse program filepath = runIdentity $ runParserT programParser () filepath program