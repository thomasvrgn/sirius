module Language.Sirius.Parser.Modules.Pattern where

import qualified Data.Functor                           as F
import qualified Language.Sirius.CST.Expression         as C
import qualified Language.Sirius.CST.Modules.Annoted    as C
import qualified Language.Sirius.Parser.Lexer           as L
import           Language.Sirius.Parser.Modules.Literal (parseLiteral,
                                                         parseNamespaced)
import           Language.Sirius.Parser.Modules.Type    (parseType)
import qualified Text.Parsec                            as P

parsePattern :: Monad m => L.Sirius m C.Pattern
parsePattern =
  L.lexeme $
  P.choice
    [ C.PLiteral <$> parseLiteral
    , C.PWildcard F.<$ L.reserved "_"
    , P.try parseApp
    , C.PVariable <$> parseNamespaced
    , parseStruct
    ]

parseApp :: Monad m => L.Parser m C.Pattern
parseApp = do
  name <- parseNamespaced
  args <- L.parens $ L.commaSep parsePattern
  return $ C.PApp name args

parseStruct :: Monad m => L.Parser m C.Pattern
parseStruct = do
  name <- parseType
  fields <-
    L.braces $
    L.commaSep $ C.Annoted <$> L.identifier <* L.reservedOp ":" <*> parsePattern
  return $ C.PStruct name fields
