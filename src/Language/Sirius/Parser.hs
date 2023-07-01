module Language.Sirius.Parser where

import qualified Language.Sirius.CST.Expression      as C
import qualified Language.Sirius.CST.Modules.Located as C
import qualified Language.Sirius.Parser.Expression   as E
import qualified Language.Sirius.Parser.Lexer        as L
import qualified Language.Sirius.Parser.Toplevel     as T
import qualified Text.Parsec                         as P

type SourceFile = String

parseSirius ::
     Monad m
  => SourceFile
  -> Text
  -> m (Either P.ParseError [C.Located C.Toplevel])
parseSirius =
  P.runParserT (L.whiteSpace *> T.parseToplevels E.parseExpression <* P.eof) ()
