module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import DSL
import Text.Parsec

patternLiteral :: Parser String u (Q Exp)
patternLiteral = undefined

-- Quasiquote helpers --
patternParser :: String -> Q Exp
patternParser (':':name) = [| pvar name |]
patternParser ('/':name) = [| pstr name |]
patternParser ('.':name) = [| pnum . read $ name |]
patternParser name = [| psym name |]

-- pAttern (p was taken...)
a :: QuasiQuoter
a = QuasiQuoter {
    quoteExp = patternParser,
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
}