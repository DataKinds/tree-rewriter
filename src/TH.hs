module TH where

import Language.Haskell.TH
import Text.Parsec
    ( spaces,
      try, runParserT )
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Parser (RuleParser, literalParser)
import Data.Functor.Identity ( Identity(runIdentity) )

-- Quasiquote helpers --
treeToQ :: Lift a => RuleParser a -> RuleParser (Q Exp)
treeToQ par = do 
    out <- par
    return [| out |]

quotePattern :: String -> Q Exp
quotePattern pat = let
    parsed = runIdentity $ runParserT parser () "" pat
    in case parsed of 
        Left err -> error . show $ err
        Right q -> q
    where
        parser2 = treeToQ $ try literalParser 
        parser = spaces *> parser2

-- pAttern (p was taken...)
a :: QuasiQuoter
a = QuasiQuoter {
    quoteExp = quotePattern,
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
}