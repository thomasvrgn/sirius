module Language.Sirius.Parser.Modules.Literal where

import qualified Data.Functor                       as F
import qualified Language.Sirius.CST.Modules.Literal as C
import qualified Language.Sirius.Parser.Lexer        as L
import qualified Text.Parsec                        as P
import qualified Language.Sirius.CST.Modules.Namespaced as D

parseLiteral :: Monad m => L.Parser m C.Literal
parseLiteral =
  P.choice
    [ 
      P.try $ C.Float <$> L.float
    , C.Int <$> L.integer
    , C.Bool <$>
      (L.reserved "true" F.$> True P.<|> L.reserved "false" F.$> False)
    , C.Char <$> L.charLiteral
    , C.String <$> L.stringLiteral
    ]

parseNamespaced :: Monad m => L.Parser m D.Namespaced
parseNamespaced = P.try (do
    names <- P.sepBy1 L.identifier (L.reservedOp "::")
    case names of
      [name] -> return $ D.Simple name
      _:_ -> do
        let x = viaNonEmpty init names >>= \x' -> viaNonEmpty last names >>= \y -> return (y, x')
        case x of
          Just (n, ns) -> return $ D.Namespaced ns n
          Nothing -> fail "Impossible"
      _ -> fail "Impossible")
  P.<|> D.Simple <$> L.identifier