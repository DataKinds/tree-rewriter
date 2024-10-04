module TH where

import Language.Haskell.TH
import Text.Parsec
    ( spaces,
      (<|>),
      try, runParserT )
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Control.Monad.Trans.Accum
import Parser (RuleParser, flex, patternRuleParser, patternLiteralParser)

-- Quasiquote helpers --
treeToQ :: Lift a => RuleParser a -> RuleParser (Q Exp)
treeToQ par = do 
    out <- par
    return [| out |]

quoteRuleParser :: RuleParser (Q Exp)
quoteRuleParser = flex $ do
    rewrite <- treeToQ patternRuleParser 
    pure [| addRule $(rewrite) |]

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