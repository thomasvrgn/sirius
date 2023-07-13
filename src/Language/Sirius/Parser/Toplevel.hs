module Language.Sirius.Parser.Toplevel where

import qualified Language.Sirius.CST.Expression      as C
import qualified Language.Sirius.CST.Modules.Annoted as C
import qualified Language.Sirius.CST.Modules.Located as C
import           Language.Sirius.CST.Modules.Type    (Type (TypeFunction))
import qualified Language.Sirius.Parser.Lexer        as L
import qualified Language.Sirius.Parser.Modules.Type as T
import qualified Text.Parsec                         as P

parseImport :: Monad m => L.Sirius m C.Toplevel
parseImport =
  L.lexeme $ do
    L.reserved "use"
    name <- (:) <$> L.identifier <*> P.many (P.string "." *> L.identifier)
    return $ C.TUse name

parseModuleDefinition :: Monad m => L.Sirius m C.Expression -> L.Sirius m C.Toplevel
parseModuleDefinition e =
  L.lexeme $ do
    L.reserved "mod"
    name <- L.identifier
    C.TNamespace name <$> L.braces (P.many (parseToplevel e))

parseTypeAlias :: Monad m => L.Sirius m C.Toplevel
parseTypeAlias =
  L.lexeme $ do
    L.reserved "type"
    name <- L.identifier
    gens <- P.option [] $ L.brackets $ L.commaSep L.identifier
    L.reservedOp "="
    C.TTypeAlias (C.Annoted name gens) <$> T.parseType

parseEnumeration :: Monad m => L.Sirius m C.Toplevel
parseEnumeration =
  L.lexeme $ do
    L.reserved "enum"
    name <- L.identifier
    gens <- P.option [] $ L.brackets $ L.commaSep L.identifier
    variants <-
      L.braces $
      L.commaSep
        (C.Annoted <$> L.identifier <*>
         (P.try (L.parens (L.commaSep T.parseType)) <|> pure []))
    return $ C.TEnumeration (C.Annoted name gens) variants

parseProperty :: Monad m => L.Sirius m C.Toplevel
parseProperty =
  L.lexeme $ do
    L.reserved "property"
    prop <-
      L.parens $ C.Annoted <$> L.identifier <* L.reservedOp ":" <*> T.parseType
    name <- L.identifier
    gens <- P.option [] $ L.brackets $ L.commaSep L.identifier
    args <-
      L.parens $
      L.commaSep $
      C.Annoted <$> L.identifier <* L.reservedOp ":" <*> T.parseType
    L.reservedOp ":"
    ty <- T.parseType
    return $ C.TProperty gens prop (C.Annoted name ty) args

parseFunction :: Monad m => L.Sirius m C.Expression -> L.Sirius m C.Toplevel
parseFunction p =
  L.lexeme $ do
    L.reserved "fn"
    name <- L.identifier <|> L.operator
    gens <- P.option [] $ L.brackets $ L.commaSep L.identifier
    args <-
      L.parens $
      L.commaSep $
      C.Annoted <$> L.identifier <* L.reservedOp ":" <*> T.parseType
    L.reservedOp ":"
    ty <- T.parseType
    body <- L.reservedOp "=" *> p
    return $ C.TFunction gens (C.Annoted name ty) args body

parsePropFunction :: Monad m => L.Sirius m C.Expression -> L.Sirius m C.Toplevel
parsePropFunction p =
  L.lexeme $ do
    L.reserved "fn"
    prop <-
      L.parens $ C.Annoted <$> L.identifier <* L.reservedOp ":" <*> T.parseType
    name <- L.identifier <|> L.operator
    gens <- P.option [] $ L.brackets $ L.commaSep L.identifier
    args <-
      L.parens $
      L.commaSep $
      C.Annoted <$> L.identifier <* L.reservedOp ":" <*> T.parseType
    L.reservedOp ":"
    ty <- T.parseType
    body <- L.reservedOp "=" *> p
    return $ C.TFunctionProp gens prop (C.Annoted name ty) args body

parseStruct :: Monad m => L.Sirius m C.Toplevel
parseStruct =
  L.lexeme $ do
    L.reserved "struct"
    name <- L.identifier
    gens <- P.option [] $ L.brackets $ L.commaSep L.identifier
    fields <-
      L.braces $
      P.sepBy
        (C.Annoted <$> L.identifier <* L.reservedOp ":" <*> T.parseType)
        L.comma
    return $ C.TStruct (C.Annoted name gens) fields

parseStructInlined :: Monad m => L.Sirius m C.Toplevel
parseStructInlined =
  L.lexeme $ do
    L.reserved "struct"
    name <- L.identifier
    gens <- P.option [] $ L.brackets $ L.commaSep L.identifier
    fields <-
      L.parens $
      P.sepBy
        (C.Annoted <$> L.identifier <* L.reservedOp ":" <*> T.parseType)
        L.comma
    return $ C.TStruct (C.Annoted name gens) fields

parseExternFn :: Monad m => L.Sirius m C.Toplevel
parseExternFn =
  L.lexeme $ do
    L.reserved "extern"
    name <- L.identifier
    gens <- P.option [] $ L.brackets $ L.commaSep L.identifier
    args <-
      L.parens $ L.commaSep $ L.identifier >> L.reservedOp ":" >> T.parseType
    L.reservedOp ":"
    C.TExtern gens . C.Annoted name . TypeFunction args <$> T.parseType

parseExternVar :: Monad m => L.Sirius m C.Toplevel
parseExternVar =
  L.lexeme $ do
    L.reserved "extern"
    name <- L.identifier
    gens <- P.option [] $ L.brackets $ L.commaSep L.identifier
    L.reservedOp ":"
    C.TExtern gens . C.Annoted name <$> T.parseType

parseAnnotation :: Monad m => L.Sirius m C.Expression -> L.Sirius m C.Toplevel
parseAnnotation e =
  L.lexeme $ do
    L.reserved "with"
    name <-
      fromString <$>
      many (P.letter <|> P.digit <|> P.char '_' <|> P.char '.' <|> P.char '-') <*
      L.whiteSpace
    C.TAnnotation name <$> parseToplevel e

parseToplevel :: Monad m => L.Sirius m C.Expression -> L.Sirius m C.Toplevel
parseToplevel p =
  P.choice
    [ parseImport
    , parseAnnotation p
    , parseModuleDefinition p
    , parseTypeAlias
    , parseEnumeration
    , P.try $ parsePropFunction p
    , parseFunction p
    , P.try parseStructInlined
    , parseStruct
    , P.try parseExternFn
    , parseExternVar
    , parseProperty
    ] <*
  P.optionMaybe L.semi

parseToplevels ::
     Monad m => L.Sirius m C.Expression -> L.Parser m [C.Located C.Toplevel]
parseToplevels p = P.many (parseToplevel p)
